import ast
from typing import List, Union

# Import from Rust implementation
from .covers_core import encode_branch, analyze_branches_ts

EXIT = 0


def preinstrument(source: Union[str, ast.Module]) -> ast.Module:
    """Prepares an AST for coverage instrumentation using tree-sitter analysis.

    Args:
        source: The Python source code to analyze (string or AST Module)

    Returns:
        Modified AST with branch markers inserted
    """
    import textwrap

    # Handle both string and AST Module inputs
    if isinstance(source, ast.Module):
        # Unparse the AST to get source code for tree-sitter analysis
        source_str = ast.unparse(source)
        tree = source
    else:
        # Remove common leading whitespace from source string
        source_str = textwrap.dedent(source)
        tree = ast.parse(source_str)

    # Analyze source with tree-sitter (implemented in Rust)
    # Returns dict mapping branch_line -> [(insert_line, to_line), ...]
    # Note: This doesn't import ast in Rust, it's tree-sitter based!
    branch_data = analyze_branches_ts(source_str)

    class SlipcoverTransformer(ast.NodeTransformer):
        def __init__(self, branch_info):
            self.branch_info = branch_info
            self.source_lines = source_str.splitlines()

        def _mark_branch(self, from_line: int, to_line: int) -> List[ast.stmt]:
            # Using a constant Expr allows the compiler to optimize this to a NOP
            mark = ast.Expr(ast.Constant(None))
            for node in ast.walk(mark):
                node.lineno = node.end_lineno = encode_branch(from_line, to_line)  # type: ignore[attr-defined]
                # Leaving the columns unitialized can lead to invalid positions despite
                # our use of ast.fix_missing_locations
                node.col_offset = node.end_col_offset = -1  # type: ignore[attr-defined]

            return [mark]

        def _mark_branches_from_info(
            self, node: Union[ast.If, ast.For, ast.AsyncFor, ast.While], branch_markers
        ) -> ast.AST:
            # branch_markers is [(insert_line, to_line), ...]
            for idx, (insert_line, to_line) in enumerate(branch_markers):
                if insert_line == 0:
                    # This means append to orelse
                    node.orelse = self._mark_branch(node.lineno, to_line)
                elif idx == 0:
                    # First marker goes to body
                    node.body = self._mark_branch(node.lineno, insert_line) + node.body
                else:
                    # Subsequent markers go to orelse
                    if node.orelse:
                        node.orelse = (
                            self._mark_branch(node.lineno, insert_line) + node.orelse
                        )
                    else:
                        node.orelse = self._mark_branch(node.lineno, insert_line)

            super().generic_visit(node)
            return node

        def visit_If(self, node: ast.If) -> ast.AST:
            if node.lineno in self.branch_info:
                return self._mark_branches_from_info(
                    node, self.branch_info[node.lineno]
                )
            super().generic_visit(node)
            return node

        def visit_For(self, node: ast.For) -> ast.AST:
            if node.lineno in self.branch_info:
                return self._mark_branches_from_info(
                    node, self.branch_info[node.lineno]
                )
            super().generic_visit(node)
            return node

        def visit_AsyncFor(self, node: ast.AsyncFor) -> ast.AST:
            if node.lineno in self.branch_info:
                return self._mark_branches_from_info(
                    node, self.branch_info[node.lineno]
                )
            super().generic_visit(node)
            return node

        def visit_While(self, node: ast.While) -> ast.AST:
            if node.lineno in self.branch_info:
                return self._mark_branches_from_info(
                    node, self.branch_info[node.lineno]
                )
            super().generic_visit(node)
            return node

        def visit_Match(self, node: ast.Match) -> ast.Match:
            if node.lineno in self.branch_info:
                branch_markers = self.branch_info[node.lineno]

                # First markers go to each case body
                for idx, (insert_line, to_line) in enumerate(branch_markers):
                    if insert_line == 0:
                        # This is the wildcard case
                        node.cases.append(
                            ast.match_case(
                                ast.MatchAs(),
                                body=self._mark_branch(node.lineno, to_line),
                            )
                        )
                    elif idx < len(node.cases):
                        # Add marker to existing case
                        node.cases[idx].body = (
                            self._mark_branch(node.lineno, insert_line)
                            + node.cases[idx].body
                        )

            super().generic_visit(node)
            return node

    tree = SlipcoverTransformer(branch_data).visit(tree)
    ast.fix_missing_locations(tree)
    return tree


def preinstrument_and_compile(source: str, filename: str, branch: bool):
    """Parse, preinstrument, and compile Python source code.

    This function is called from Rust code to avoid importing ast in Rust.

    Args:
        source: Python source code
        filename: Filename for the code object
        branch: Whether to enable branch coverage

    Returns:
        Compiled code object
    """
    if branch:
        tree = preinstrument(source)
    else:
        tree = ast.parse(source, filename)

    return compile(tree, filename, "exec")
