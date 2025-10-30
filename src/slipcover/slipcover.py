from __future__ import annotations

import dis
import sys
import threading
import types
from collections import defaultdict
from typing import TYPE_CHECKING

from pathlib import Path

from . import branch as br
from .version import __version__
from .xmlreport import XmlReporter

# FIXME provide __all__

# Python 3.13 returns 'None' lines;
# Python 3.11+ generates a line just for RESUME or RETURN_GENERATOR, POP_TOP, RESUME;
# Python 3.11 generates a 0th line
_op_RESUME = dis.opmap["RESUME"]
_op_RETURN_GENERATOR = dis.opmap["RETURN_GENERATOR"]

def findlinestarts(co: types.CodeType):
    for off, line in dis.findlinestarts(co):
        if line and co.co_code[off] not in (_op_RESUME, _op_RETURN_GENERATOR):
            yield off, line

if TYPE_CHECKING:
    from typing import Dict, Iterable, Iterator, List, Optional, Tuple

    from .schemas import Coverage

class SlipcoverError(Exception):
    pass


class PathSimplifier:
    def __init__(self):
        self.cwd = Path.cwd()

    def simplify(self, path : str) -> str:
        f = Path(path)
        try:
            return str(f.relative_to(self.cwd))
        except ValueError:
            return path 


def format_missing(missing_lines: List[int], executed_lines: List[int],
                   missing_branches: List[tuple]) -> str:
    """Formats ranges of missing lines, including non-code (e.g., comments) ones that fall
       between missed ones"""

    missing_set = set(missing_lines)
    missing_branches = [(a,b) for a,b in missing_branches if a not in missing_set and b not in missing_set]

    def format_branch(br):
        return f"{br[0]}->exit" if br[1] == 0 else f"{br[0]}->{br[1]}"

    def find_ranges():
        executed = set(executed_lines)
        it = iter(missing_lines)    # assumed sorted
        a = next(it, None)
        while a is not None:
            while missing_branches and missing_branches[0][0] < a:
                yield format_branch(missing_branches.pop(0))

            b = a
            n = next(it, None)
            while n is not None:
                if any(l in executed for l in range(b+1, n+1)):
                    break

                b = n
                n = next(it, None)

            yield str(a) if a == b else f"{a}-{b}"

            a = n

        while missing_branches:
            yield format_branch(missing_branches.pop(0))

    return ", ".join(find_ranges())

def print_xml(
    coverage: Coverage,
    source_paths: Iterable[str],
    *,
    with_branches: bool = False,
    xml_package_depth: int = 99,
    outfile=sys.stdout
) -> None:
    XmlReporter(
        coverage=coverage,
        source=source_paths,
        with_branches=with_branches,
        xml_package_depth=xml_package_depth,
    ).report(outfile=outfile)


def print_coverage(coverage, *, outfile=sys.stdout, missing_width=None, skip_covered=False) -> None:
    """Prints coverage information for human consumption."""
    from tabulate import tabulate

    if not coverage.get('files', None): # includes empty coverage['files']
        return

    branch_coverage = coverage.get('meta', {}).get('branch_coverage', False)

    def table():
        for f, f_info in sorted(coverage['files'].items()):
            exec_l = len(f_info['executed_lines'])
            miss_l = len(f_info['missing_lines'])

            if branch_coverage:
                exec_b = len(f_info['executed_branches'])
                miss_b = len(f_info['missing_branches'])
                pct_b = 100*exec_b/(exec_b+miss_b) if (exec_b+miss_b) else 0

            pct = f_info['summary']['percent_covered']

            if skip_covered and pct == 100.0:
                continue

            yield [f, exec_l+miss_l, miss_l,
                   *([exec_b+miss_b, miss_b, round(pct_b)] if branch_coverage else []),
                   round(pct),
                   format_missing(f_info['missing_lines'], f_info['executed_lines'],
                                  f_info['missing_branches'] if 'missing_branches' in f_info else [])]

        if len(coverage['files']) > 1:
            yield ['---'] + [''] * (6 if branch_coverage else 4)

            s = coverage['summary']

            if branch_coverage:
                exec_b = s['covered_branches']
                miss_b = s['missing_branches']
                pct_b = 100*exec_b/(exec_b+miss_b) if (exec_b+miss_b) else 0

            yield ['(summary)', s['covered_lines']+s['missing_lines'], s['missing_lines'],
                   *([exec_b+miss_b, miss_b, round(pct_b)] if branch_coverage else []),
                   round(s['percent_covered']), '']



    print("", file=outfile)
    headers = ["File", "#lines", "#l.miss",
               *(["#br.", "#br.miss", "brCov%", "totCov%"] if branch_coverage else ["Cover%"]),
               "Missing"]
    maxcolwidths = [None] * (len(headers)-1) + [missing_width]
    print(tabulate(table(), headers=headers, maxcolwidths=maxcolwidths), file=outfile)


def add_summaries(cov: dict) -> None:
    """Adds (or updates) 'summary' entries in coverage information."""
    # global summary
    g_summary : dict = defaultdict(int)
    g_nom = g_den = 0

    if 'files' in cov:
        for f_cov in cov['files'].values():
            summary : dict = { # per-file summary
                'covered_lines': len(f_cov['executed_lines']),
                'missing_lines': len(f_cov['missing_lines']),
            }

            nom = summary['covered_lines']
            den = nom + summary['missing_lines']

            if 'executed_branches' in f_cov:
                summary.update({
                    'covered_branches': len(f_cov['executed_branches']),
                    'missing_branches': len(f_cov['missing_branches'])
                })

                nom += summary['covered_branches']
                den += summary['covered_branches'] + summary['missing_branches']

            summary['percent_covered'] = 100.0 if den == 0 else 100*nom/den
            f_cov['summary'] = summary

            for k in summary:
                g_summary[k] += summary[k]
            g_nom += nom
            g_den += den

    g_summary['percent_covered'] = 100.0 if g_den == 0 else 100*g_nom/g_den
    g_summary['percent_covered_display'] = str(int(round(g_summary['percent_covered'], 0)))
    cov['summary'] = g_summary


def merge_coverage(a: dict, b: dict) -> dict:
    """Merges coverage result 'b' into 'a'."""

    if a.get('meta', {}).get('software', None) != 'slipcover':
        raise SlipcoverError('Cannot merge coverage: only SlipCover format supported.')

    if a.get('meta', {}).get('show_contexts', False) or \
       b.get('meta', {}).get('show_contexts', False):
        raise SlipcoverError('Merging coverage with show_contexts=True unsupported')

    branch_coverage = a.get('meta', {}).get('branch_coverage', False)
    if branch_coverage and not b.get('meta', {}).get('branch_coverage', False):
        raise SlipcoverError('Cannot merge coverage: branch coverage missing')

    a_files = a['files']
    b_files = b['files']

    def both(f, field):
        return (a_files[f][field] if f in a_files else []) + b_files[f][field]

    for f in b_files:
        executed_lines = set(both(f, 'executed_lines'))
        missing_lines = set(both(f, 'missing_lines'))
        missing_lines -= executed_lines
        update = {
            'executed_lines': sorted(executed_lines),
            'missing_lines': sorted(missing_lines)
        }

        if branch_coverage:
            executed_branches = set(tuple(br) for br in both(f, 'executed_branches'))
            missing_branches = set(tuple(br) for br in both(f, 'missing_branches'))
            missing_branches -= executed_branches
            update.update({
                'executed_branches': sorted(list(br) for br in executed_branches),
                'missing_branches': sorted(list(br) for br in missing_branches)
            })

        a_files[f] = update

    add_summaries(a)
    return a


class Slipcover:
    def __init__(self, immediate: bool = False,
                 d_miss_threshold: int = 50, branch: bool = False,
                 disassemble: bool = False, source: Optional[List[str]] = None):
        self.immediate = immediate
        self.d_miss_threshold = d_miss_threshold
        self.branch = branch
        self.disassemble = disassemble
        self.source = source

        # mutex protecting this state
        self.lock = threading.RLock()

        # notes which code lines have been instrumented
        self.code_lines: Dict[str, set] = defaultdict(set)
        self.code_branches: Dict[str, set] = defaultdict(set)

        # notes which lines and branches have been seen.
        self.all_seen: Dict[str, set] = defaultdict(set)

        # notes lines/branches seen since last de-instrumentation
        self._get_newly_seen()

        def handle_line(code, line):
            if br.is_branch(line):
                self.newly_seen[code.co_filename].add(br.decode_branch(line))
            elif line:
                self.newly_seen[code.co_filename].add(line)
            return sys.monitoring.DISABLE

        if sys.monitoring.get_tool(sys.monitoring.COVERAGE_ID) != "SlipCover":
            sys.monitoring.use_tool_id(sys.monitoring.COVERAGE_ID, "SlipCover") # FIXME add free_tool_id

        sys.monitoring.register_callback(sys.monitoring.COVERAGE_ID,
                                         sys.monitoring.events.LINE, handle_line)

        self.modules : list = []

    def _get_newly_seen(self):
        """Returns the current set of ``new'' lines, leaving a new container in place."""

        # We trust that assigning to self.newly_seen is atomic, as it is triggered
        # by a STORE_NAME or similar opcode and Python synchronizes those.  We rely on
        # C extensions' atomicity for updates within self.newly_seen.  The lock here
        # is just to protect callers of this method (so that the exchange is atomic).

        with self.lock:
            newly_seen = self.newly_seen if hasattr(self, "newly_seen") else None
            self.newly_seen: Dict[str, set] = defaultdict(set)

        return newly_seen


    @staticmethod
    def lines_from_code(co: types.CodeType) -> Iterator[int]:
        for c in co.co_consts:
            if isinstance(c, types.CodeType):
                yield from Slipcover.lines_from_code(c)

        yield from (line for _, line in findlinestarts(co) if not br.is_branch(line))


    @staticmethod
    def branches_from_code(co: types.CodeType) -> Iterator[Tuple[int, int]]:
        for c in co.co_consts:
            if isinstance(c, types.CodeType):
                yield from Slipcover.branches_from_code(c)

        yield from (br.decode_branch(line) for _, line in findlinestarts(co) if br.is_branch(line))


    def instrument(self, co: types.CodeType, parent: Optional[types.CodeType] = None) -> types.CodeType:
        """Instruments a code object for coverage detection.

        If invoked on a function, instruments its code.
        """

        if isinstance(co, types.FunctionType):
            co = co.__code__

        assert isinstance(co, types.CodeType)
        # print(f"instrumenting {co.co_name}")

        sys.monitoring.set_local_events(sys.monitoring.COVERAGE_ID, co, sys.monitoring.events.LINE)

        # handle functions-within-functions
        for c in co.co_consts:
            if isinstance(c, types.CodeType):
                self.instrument(c, co)

        if not parent:
            with self.lock:
                self.code_lines[co.co_filename].update(Slipcover.lines_from_code(co))
                self.code_branches[co.co_filename].update(Slipcover.branches_from_code(co))

        return co


    def _add_unseen_source_files(self, source: List[str]):
        import ast

        dirs = [Path(d).resolve() for d in source]

        while dirs:
            p = dirs.pop()
            for file in p.iterdir():
                if file.is_dir():
                    dirs.append(file)   # walk this directory, too

                elif file.is_file() and file.suffix.lower() == '.py':
                    file = file.absolute()
                    filename = str(file)
                    try:
                        if filename not in self.code_lines:
                            t = ast.parse(file.read_text())
                            if self.branch:
                                t = br.preinstrument(t)
                            code = compile(t, filename, "exec")
                            self.code_lines[filename] = set(Slipcover.lines_from_code(code))
                            if self.branch:
                                self.code_branches[filename] = set(Slipcover.branches_from_code(code))

                    except Exception as e: # for SyntaxError and such... FIXME curate list and catch only those
                        print(f"Warning: unable to include {filename}: {e}")


    @staticmethod
    def _make_meta(branch_coverage: bool) -> dict:
        import datetime

        return {
            'software': 'slipcover',
            'version': __version__,
            'timestamp': datetime.datetime.now().isoformat(),
            'branch_coverage': branch_coverage,
            'show_contexts': False
        }


    def signal_child_process(self):
        self.source = None  # only the parent process needs to run _add_unseen_source_files
        with self.lock:
            self._get_newly_seen()
            self.all_seen.clear()


    def get_coverage(self):
        """Returns coverage information collected."""

        with self.lock:
            # FIXME calling _get_newly_seen will prevent de-instrumentation if still running!
            newly_seen = self._get_newly_seen()

            for file, lines in newly_seen.items():
                self.all_seen[file].update(lines)

            if self.source:
                self._add_unseen_source_files(self.source)

            simp = PathSimplifier()

            files = dict()
            for f, f_code_lines in self.code_lines.items():
                if f in self.all_seen:
                    branches_seen = {x for x in self.all_seen[f] if isinstance(x, tuple)}
                    lines_seen = self.all_seen[f] - branches_seen
                else:
                    lines_seen = branches_seen = set()

                f_files = {
                    'executed_lines': sorted(lines_seen),
                    'missing_lines': sorted(f_code_lines - lines_seen),
                }

                if self.branch:
                    f_files['executed_branches'] = sorted(branches_seen)
                    f_files['missing_branches'] = sorted(self.code_branches[f] - branches_seen)

                files[simp.simplify(f)] = f_files

            cov = {
                'meta': Slipcover._make_meta(self.branch),
                'files': files
            }

            add_summaries(cov)
            return cov


    # @deprecated
    def print_coverage(self, outfile=sys.stdout, *, missing_width=None) -> None:
        """Prints the coveage collected by this Slipcover."""
        print_coverage(self.get_coverage(), outfile=outfile, missing_width=missing_width)


    @staticmethod
    def find_functions(items, visited : set):
        # Don't use isinstance() or inspect.isfunction, as isinstance as may call __class__,
        # which may have side effects (e.g., using Celery https://github.com/celery/celery).
        def is_patchable_function(func):
            # PyPy has no "builtin functions" like CPython. instead, it uses
            # regular functions, with a special type of code object.
            # the second condition is always True on CPython
            return issubclass(type(func), types.FunctionType) and type(func.__code__) is types.CodeType

        def find_funcs(root):
            if is_patchable_function(root):
                if root not in visited:
                    visited.add(root)
                    yield root

            # Prefer isinstance(x,type) over isclass(x) because many many
            # things, such as str(), are classes
            elif issubclass(type(root), type):
                if root not in visited:
                    visited.add(root)

                    # Don't use inspect.getmembers(root) since that invokes getattr(),
                    # which causes any descriptors to be invoked, which results in either
                    # additional (unintended) coverage and/or errors because __get__ is
                    # invoked in an unexpected way.
                    obj_names = dir(root)
                    for obj_key in obj_names:
                        mro = (root,) + root.__mro__
                        for base in mro:
                            if (base == root or base not in visited) and obj_key in base.__dict__:
                                yield from find_funcs(base.__dict__[obj_key])
                                break

            elif (issubclass(type(root), classmethod) or issubclass(type(root), staticmethod)) and \
                 is_patchable_function(root.__func__):
                if root.__func__ not in visited:
                    visited.add(root.__func__)
                    yield root.__func__

        # FIXME this may yield "dictionary changed size during iteration"
        return [f for it in items for f in find_funcs(it)]


    def register_module(self, m):
        self.modules.append(m)
