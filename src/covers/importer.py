from typing import Any, Optional
from .covers import Covers, __version__
from . import branch as br
from .covers_core import FileMatcher
from pathlib import Path
import sys

from importlib.abc import MetaPathFinder, Loader
from importlib import machinery
from importlib.resources.abc import TraversableResources


class CoversLoader(Loader):
    def __init__(self, sci: Covers, orig_loader: Loader, origin: str):
        self.sci = sci  # Covers object measuring coverage
        self.orig_loader = orig_loader  # original loader we're wrapping
        self.origin = Path(origin)  # module origin (source file for a source loader)

        # loadlib checks for this attribute to see if we support it... keep in sync with orig_loader
        if not getattr(self.orig_loader, "get_resource_reader", None):
            delattr(self, "get_resource_reader")

    # for compability with loaders supporting resources, used e.g. by sklearn
    def get_resource_reader(self, fullname: str) -> Optional[TraversableResources]:
        if hasattr(self.orig_loader, "get_resource_reader"):
            return self.orig_loader.get_resource_reader(fullname)
        return None

    def create_module(self, spec):
        return self.orig_loader.create_module(spec)

    def get_code(self, name):  # expected by pyrun
        return self.orig_loader.get_code(name)  # type: ignore[attr-defined]

    def exec_module(self, module):
        # branch coverage requires pre-instrumentation from source
        if (
            self.sci.branch
            and isinstance(self.orig_loader, machinery.SourceFileLoader)
            and self.origin.exists()
        ):
            source = self.origin.read_bytes().decode("utf-8")
            t = br.preinstrument(source)
            code = compile(t, str(self.origin), "exec")
        else:
            code = self.orig_loader.get_code(module.__name__)  # type: ignore[attr-defined]

        self.sci.register_module(module)
        code = self.sci.instrument(code)
        exec(code, module.__dict__)


# FileMatcher is now implemented in Rust (see src_rust/file_matcher.rs)
# It's imported from covers_core at the top of this file


class MatchEverything:
    def __init__(self):
        pass

    def matches(self, filename: Path):
        return True


class CoversMetaPathFinder(MetaPathFinder):
    def __init__(self, sci, file_matcher, debug=False):
        self.debug = debug
        self.sci = sci
        self.file_matcher = file_matcher

    def find_spec(self, fullname, path, target=None):
        if self.debug:
            print(f"Looking for {fullname}")

        for f in sys.meta_path:
            # skip ourselves
            if isinstance(f, CoversMetaPathFinder):
                continue

            if not hasattr(f, "find_spec"):
                continue

            spec = f.find_spec(fullname, path, target)
            if spec is None or spec.loader is None:
                continue

            # can't instrument extension files
            if isinstance(spec.loader, machinery.ExtensionFileLoader):
                return None

            if spec.origin and self.file_matcher.matches(spec.origin):
                if self.debug:
                    print(f"instrumenting {fullname} from {spec.origin}")
                spec.loader = CoversLoader(self.sci, spec.loader, spec.origin)

            return spec

        return None


class ImportManager:
    """A context manager that enables instrumentation while active."""

    def __init__(
        self,
        sci: Covers,
        file_matcher: Optional[FileMatcher] = None,
        debug: bool = False,
    ):
        self.mpf = CoversMetaPathFinder(
            sci, file_matcher if file_matcher else MatchEverything(), debug
        )

    def __enter__(self) -> "ImportManager":
        sys.meta_path.insert(0, self.mpf)
        return self

    def __exit__(self, *args: Any) -> None:
        i = 0
        while i < len(sys.meta_path):
            if sys.meta_path[i] is self.mpf:
                sys.meta_path.pop(i)
                break
            i += 1


def wrap_pytest(sci: Covers, file_matcher: FileMatcher):
    # Store branch information and source for files processed by pytest
    # This is needed because pytest's assertion rewriter strips branch markers from the AST
    pytest_branches = {}  # filename -> list of (from_line, to_line) tuples
    pytest_sources = {}  # filename -> source code

    def redirect_calls(module, funcName, funcWrapperName):
        """Redirects calls to the given function to a wrapper function in the same module."""
        import ast
        import types

        assert funcWrapperName not in module.__dict__, (
            f"function {funcWrapperName} already defined"
        )

        with open(module.__file__) as f:
            t = ast.parse(f.read())

        funcNames = set()  # names of the functions we modified
        for n in ast.walk(t):
            if isinstance(n, (ast.FunctionDef, ast.AsyncFunctionDef)):
                for s in ast.walk(n):
                    if (
                        isinstance(s, ast.Call)
                        and isinstance(s.func, ast.Name)
                        and s.func.id == funcName
                    ):
                        s.func.id = funcWrapperName
                        funcNames.add(n.name)

        code = compile(t, module.__file__, "exec")

        # It's tempting to just exec(code, module.__dict__) here, but the code often times has side effects...
        # So instead of we find the new code object(s) and replace them in the loaded module.

        replacement = dict()  # replacement code objects

        def find_replacements(co):
            for c in co.co_consts:
                if isinstance(c, types.CodeType):
                    if c.co_name in funcNames:
                        replacement[c.co_name] = c
                    find_replacements(c)

        find_replacements(code)

        visited: set = set()
        for f in Covers.find_functions(module.__dict__.values(), visited):
            if repl := replacement.get(f.__code__.co_name, None):
                assert f.__code__.co_firstlineno == repl.co_firstlineno  # sanity check
                f.__code__ = repl

    try:
        import _pytest.assertion.rewrite as pyrewrite
    except ModuleNotFoundError:
        return

    redirect_calls(pyrewrite, "exec", "_Covers_exec_wrapper")

    def exec_wrapper(obj, g):
        if hasattr(obj, "co_filename") and file_matcher.matches(obj.co_filename):
            filename = obj.co_filename
            # If we have stored source for this file, recompile with branch markers
            # This is needed because pytest strips branch markers from the AST
            if sci.branch and filename in pytest_sources:
                # Preinstrument and recompile the source with branch markers
                preinstrumented_ast = br.preinstrument(pytest_sources[filename])
                obj = compile(preinstrumented_ast, filename, "exec")
            obj = sci.instrument(obj)
        exec(obj, g)

    pyrewrite._Covers_exec_wrapper = exec_wrapper  # type: ignore[attr-defined]

    if sci.branch:
        import inspect

        expected_sigs = {
            "rewrite_asserts": ["mod", "source", "module_path", "config"],
            "_read_pyc": ["source", "pyc", "trace"],
            "_write_pyc": ["state", "co", "source_stat", "pyc"],
        }

        for fun, expected in expected_sigs.items():
            sig = inspect.signature(pyrewrite.__dict__[fun])
            if list(sig.parameters) != expected:
                import warnings

                warnings.warn(
                    f"Unable to activate pytest branch coverage: unexpected {fun} signature {str(sig)}"
                    + "; please open an issue at https://github.com/your-repo/covers .",
                    RuntimeWarning,
                )
                return

        orig_rewrite_asserts = pyrewrite.rewrite_asserts

        def rewrite_asserts_wrapper(*args):
            # FIXME we should normally subject pre-instrumentation to file_matcher matching...
            # but the filename isn't clearly available. So here we instead always pre-instrument
            # (pytest instrumented) files. Our pre-instrumentation adds global assignments that
            # *should* be innocuous if not followed by sci.instrument.
            # args[0] is mod (AST), args[1] is source, args[2] is module_path
            # preinstrument now takes source and returns modified AST
            # Convert bytes to string if necessary
            source = args[1]
            if isinstance(source, bytes):
                source = source.decode("utf-8")

            # Calculate and store branch information and source for this file
            # Since pytest strips branch markers, we need to store this separately
            # and recompile later in exec_wrapper
            module_path = str(args[2]) if len(args) > 2 else None
            if module_path:
                from .covers_core import analyze_branches_ts

                branch_data = analyze_branches_ts(source)
                # Convert branch_data dict to list of (from_line, to_line) tuples
                # branch_data format: {branch_line: [(insert_line, to_line), ...]}
                # - if insert_line != 0: branch goes to insert_line
                # - if insert_line == 0: branch goes to to_line (else/exit)
                branches = []
                for branch_line, markers in branch_data.items():
                    for insert_line, to_line in markers:
                        if insert_line != 0:
                            branches.append((branch_line, insert_line))
                        else:
                            branches.append((branch_line, to_line))
                if branches:
                    pytest_branches[module_path] = branches
                    pytest_sources[module_path] = source

            # Don't preinstrument here - pytest will strip the markers anyway
            # We'll recompile with markers in exec_wrapper instead
            return orig_rewrite_asserts(*args)

        def adjust_name(fn: Path) -> Path:
            return fn.parent / (fn.stem + "-covers-" + __version__ + fn.suffix)

        orig_read_pyc = pyrewrite._read_pyc

        def read_pyc(*args, **kwargs):
            return orig_read_pyc(*args[:1], adjust_name(args[1]), *args[2:], **kwargs)  # type: ignore[call-arg]

        orig_write_pyc = pyrewrite._write_pyc

        def write_pyc(*args, **kwargs):
            return orig_write_pyc(*args[:3], adjust_name(args[3]), *args[4:], **kwargs)  # type: ignore[call-arg]

        pyrewrite._read_pyc = read_pyc
        pyrewrite._write_pyc = write_pyc
        pyrewrite.rewrite_asserts = rewrite_asserts_wrapper
