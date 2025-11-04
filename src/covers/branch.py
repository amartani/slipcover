import ast
from typing import List, Union, Optional

# Import from Rust implementation
from .covers_core import encode_branch, analyze_branches_ts

EXIT = 0


def preinstrument(source, tree: Optional[ast.Module] = None) -> ast.Module:
    """Prepares an AST for coverage instrumentation using tree-sitter analysis.

    Args:
        source: The Python source code to analyze (str) or AST Module (for backward compatibility)
        tree: Optional pre-parsed AST. If not provided, will parse from source.

    Returns:
        Modified AST with branch markers inserted
    """
    # Handle backward compatibility: if source is an AST, use AST-based analysis
    use_ast_based_analysis = isinstance(source, ast.Module)

    if use_ast_based_analysis:
        # When passed an AST directly (for backward compat), use AST-based analysis
        # to preserve original line numbers
        return _preinstrument_ast_based(source)
    else:
        # When passed source code, use tree-sitter analysis
        if tree is None:
            tree = ast.parse(source)
        return _preinstrument_tree_sitter(source, tree)


def _preinstrument_tree_sitter(source: str, tree: ast.Module) -> ast.Module:
    """Tree-sitter based preinstrumentation (used when source code is available)."""
    # Analyze source with tree-sitter (implemented in Rust)
    # Returns dict mapping branch_line -> [(insert_line, to_line), ...]
    # Note: This doesn't import ast in Rust, it's tree-sitter based!
    branch_data = analyze_branches_ts(source)

    class SlipcoverTransformer(ast.NodeTransformer):
        def __init__(self, branch_info):
            self.branch_info = branch_info
            self.source_lines = source.splitlines()

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
    tree = ast.parse(source, filename)

    if branch:
        tree = preinstrument(source, tree)

    return compile(tree, filename, "exec")


def _preinstrument_ast_based(tree: ast.Module) -> ast.Module:
    """AST-based preinstrumentation (for backward compatibility when no source code is available)."""

    class SlipcoverTransformer(ast.NodeTransformer):
        def __init__(self):
            pass

        def _mark_branch(self, from_line: int, to_line: int) -> List[ast.stmt]:
            # Using a constant Expr allows the compiler to optimize this to a NOP
            mark = ast.Expr(ast.Constant(None))
            for node in ast.walk(mark):
                node.lineno = node.end_lineno = encode_branch(from_line, to_line)  # type: ignore[attr-defined]
                # Leaving the columns unitialized can lead to invalid positions despite
                # our use of ast.fix_missing_locations
                node.col_offset = node.end_col_offset = -1  # type: ignore[attr-defined]

            return [mark]

        def _mark_branches(
            self, node: Union[ast.If, ast.For, ast.AsyncFor, ast.While]
        ) -> ast.AST:
            node.body = self._mark_branch(node.lineno, node.body[0].lineno) + node.body

            if node.orelse:
                node.orelse = (
                    self._mark_branch(node.lineno, node.orelse[0].lineno) + node.orelse
                )
            else:
                to_line = node.next_node.lineno if node.next_node else EXIT  # type: ignore[union-attr]
                node.orelse = self._mark_branch(node.lineno, to_line)

            super().generic_visit(node)
            return node

        def visit_If(self, node: ast.If) -> ast.AST:
            return self._mark_branches(node)

        def visit_For(self, node: ast.For) -> ast.AST:
            return self._mark_branches(node)

        def visit_AsyncFor(self, node: ast.AsyncFor) -> ast.AST:
            return self._mark_branches(node)

        def visit_While(self, node: ast.While) -> ast.AST:
            return self._mark_branches(node)

        def visit_Match(self, node: ast.Match) -> ast.Match:
            for case in node.cases:
                case.body = (
                    self._mark_branch(node.lineno, case.body[0].lineno) + case.body
                )

            last_pattern = case.pattern  # case is node.cases[-1]
            while isinstance(last_pattern, ast.MatchOr):
                last_pattern = last_pattern.patterns[-1]

            has_wildcard = (
                case.guard is None
                and isinstance(last_pattern, ast.MatchAs)
                and last_pattern.pattern is None
            )
            if not has_wildcard:
                to_line = node.next_node.lineno if node.next_node else EXIT  # type: ignore[attr-defined]
                node.cases.append(
                    ast.match_case(
                        ast.MatchAs(), body=self._mark_branch(node.lineno, to_line)
                    )
                )

            super().generic_visit(node)
            return node

    match_type = ast.Match
    try_type = (ast.Try, ast.TryStar)

    # Compute the "next" statement in case a branch flows control out of a node.
    # We need a parent node's "next" computed before its siblings, so we compute it here, in BFS;
    # note that visit() doesn't guarantee any specific order.
    tree.next_node = None  # type: ignore[attr-defined]
    for node in ast.walk(tree):
        if isinstance(node, (ast.FunctionDef, ast.AsyncFunctionDef)):
            # no next node, yields (..., 0), i.e., "->exit" branch
            node.next_node = None  # type: ignore[union-attr]

        for name, field in ast.iter_fields(node):
            if isinstance(field, ast.AST):
                # if a field is just a node, any execution continues after our node
                field.next_node = node.next_node  # type: ignore[attr-defined]
            elif isinstance(node, match_type) and name == "cases":
                # each case continues after the 'match'
                for item in field:
                    item.next_node = node.next_node  # type: ignore[attr-defined]
            elif isinstance(node, try_type) and name == "handlers":
                # each 'except' continues either in 'finally', or after the 'try'
                for h in field:
                    h.next_node = (
                        node.finalbody[0] if node.finalbody else node.next_node
                    )  # type: ignore[attr-defined,union-attr]
            elif isinstance(field, list):
                # if a field is a list, each item but the last one continues with the next item
                prev = None
                for item in field:
                    if isinstance(item, ast.AST):
                        if prev:
                            prev.next_node = item  # type: ignore[attr-defined]
                        prev = item

                if prev:
                    if isinstance(node, (ast.For, ast.While)):
                        # loops back
                        prev.next_node = node  # type: ignore[attr-defined]
                    elif isinstance(node, try_type) and (name in ("body", "orelse")):
                        if name == "body" and node.orelse:
                            prev.next_node = node.orelse[0]  # type: ignore[attr-defined]
                        elif node.finalbody:
                            prev.next_node = node.finalbody[0]  # type: ignore[attr-defined]
                        else:
                            prev.next_node = node.next_node  # type: ignore[attr-defined, union-attr]
                    else:
                        prev.next_node = node.next_node  # type: ignore[attr-defined]

    tree = SlipcoverTransformer().visit(tree)
    ast.fix_missing_locations(tree)
    return tree
