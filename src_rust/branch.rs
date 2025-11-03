use pyo3::prelude::*;
use pyo3::types::PyModule as PyModuleType;
use std::ffi::CString;

const EXIT: i32 = 0;

/// Python-facing preinstrument function.
/// Takes a Python AST Module and returns a modified Python AST Module.
/// This is a direct translation of the Python preinstrument() function to Rust.
#[pyfunction]
pub fn preinstrument_rust(py: Python, tree: Bound<PyAny>) -> PyResult<Py<PyAny>> {
    // Import AST module
    let ast = PyModule::import(py, "ast")?;
    let covers_core = PyModule::import(py, "covers.covers_core")?;
    let encode_branch = covers_core.getattr("encode_branch")?;

    // Step 1: Compute next_node for all nodes using BFS traversal
    // Set tree.next_node = None
    tree.setattr("next_node", py.None())?;

    // Walk the tree and compute next_node relationships
    compute_next_nodes(py, &ast, &tree)?;

    // Step 2: Transform the tree using a NodeTransformer
    let transformer = create_transformer(py, &ast, &encode_branch)?;
    let visited = transformer.call_method1("visit", (tree.clone(),))?;

    // Step 3: Fix missing locations
    ast.call_method1("fix_missing_locations", (visited.clone(),))?;

    Ok(visited.into())
}

/// Compute next_node relationships for all nodes in the AST.
/// This implements the BFS traversal logic from the Python version.
fn compute_next_nodes(py: Python, ast: &Bound<PyModuleType>, node: &Bound<PyAny>) -> PyResult<()> {
    let match_type = ast.getattr("Match")?;
    let try_type = ast.getattr("Try")?;
    let trystar_type = ast.getattr("TryStar").ok();

    // BFS walk using ast.walk()
    let walk = ast.call_method1("walk", (node,))?;

    for item in walk.try_iter()? {
        let item = item?;

        // Handle function and async function definitions
        if is_instance(py, &item, &["FunctionDef", "AsyncFunctionDef"])? {
            item.setattr("next_node", py.None())?;
        }

        // Iterate over fields
        let iter_fields = ast.call_method1("iter_fields", (&item,))?;

        for field_item in iter_fields.try_iter()? {
            let field_tuple = field_item?;
            let field_tuple = field_tuple.cast::<pyo3::types::PyTuple>()?;

            let name: String = field_tuple.get_item(0)?.extract()?;
            let field = field_tuple.get_item(1)?;

            // Get the next_node from the current item
            let item_next = item.getattr("next_node")?;

            // If field is an AST node
            if has_attr(&field, "_fields")? {
                field.setattr("next_node", item_next.clone())?;
            }
            // If this is Match node and field is "cases"
            else if item.is_instance(&match_type)? && name == "cases" {
                for case in field.try_iter()? {
                    case?.setattr("next_node", item_next.clone())?;
                }
            }
            // If this is Try/TryStar node and field is "handlers"
            else if (item.is_instance(&try_type)?
                || (trystar_type.is_some() && item.is_instance(trystar_type.as_ref().unwrap())?))
                && name == "handlers"
            {
                let finalbody = item.getattr("finalbody")?;
                let handler_next = if finalbody.len()? > 0 {
                    finalbody.get_item(0)?
                } else {
                    item_next.clone()
                };

                for handler in field.try_iter()? {
                    handler?.setattr("next_node", handler_next.clone())?;
                }
            }
            // If field is a list of AST nodes
            else if field.hasattr("__iter__")? {
                let mut prev: Option<Bound<PyAny>> = None;

                for list_item in field.try_iter()? {
                    let list_item = list_item?;

                    if has_attr(&list_item, "_fields")? {
                        if let Some(p) = &prev {
                            p.setattr("next_node", list_item.clone())?;
                        }
                        prev = Some(list_item);
                    }
                }

                if let Some(prev_node) = prev {
                    // Check if this is a loop (For or While)
                    if is_instance(py, &item, &["For", "While"])? {
                        // Loops back
                        prev_node.setattr("next_node", item.clone())?;
                    }
                    // Check if this is Try/TryStar and field is "body" or "orelse"
                    else if (item.is_instance(&try_type)?
                        || (trystar_type.is_some()
                            && item.is_instance(trystar_type.as_ref().unwrap())?))
                        && (name == "body" || name == "orelse")
                    {
                        let orelse = item.getattr("orelse")?;
                        let finalbody = item.getattr("finalbody")?;

                        let next = if name == "body" && orelse.len()? > 0 {
                            orelse.get_item(0)?
                        } else if finalbody.len()? > 0 {
                            finalbody.get_item(0)?
                        } else {
                            item_next.clone()
                        };

                        prev_node.setattr("next_node", next)?;
                    } else {
                        prev_node.setattr("next_node", item_next.clone())?;
                    }
                }
            }
        }
    }

    Ok(())
}

/// Create the NodeTransformer class in Python.
fn create_transformer<'py>(
    py: Python<'py>,
    _ast: &Bound<PyModuleType>,
    encode_branch: &Bound<'py, PyAny>,
) -> PyResult<Bound<'py, PyAny>> {
    let transformer_code = format!(
        r#"
import ast
from typing import List, Union

EXIT = {}

class SlipcoverTransformer(ast.NodeTransformer):
    def __init__(self, encode_branch_fn):
        self.encode_branch = encode_branch_fn

    def _mark_branch(self, from_line: int, to_line: int) -> List[ast.stmt]:
        mark = ast.Expr(ast.Constant(None))
        for node in ast.walk(mark):
            node.lineno = node.end_lineno = self.encode_branch(from_line, to_line)
            node.col_offset = node.end_col_offset = -1
        return [mark]

    def _mark_branches(self, node: Union[ast.If, ast.For, ast.AsyncFor, ast.While]) -> ast.AST:
        node.body = self._mark_branch(node.lineno, node.body[0].lineno) + node.body

        if node.orelse:
            node.orelse = self._mark_branch(node.lineno, node.orelse[0].lineno) + node.orelse
        else:
            to_line = node.next_node.lineno if node.next_node else EXIT
            node.orelse = self._mark_branch(node.lineno, to_line)

        self.generic_visit(node)
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
            case.body = self._mark_branch(node.lineno, case.body[0].lineno) + case.body

        last_case = node.cases[-1]
        last_pattern = last_case.pattern
        while isinstance(last_pattern, ast.MatchOr):
            last_pattern = last_pattern.patterns[-1]

        has_wildcard = (
            last_case.guard is None
            and isinstance(last_pattern, ast.MatchAs)
            and last_pattern.pattern is None
        )
        if not has_wildcard:
            to_line = node.next_node.lineno if node.next_node else EXIT
            node.cases.append(
                ast.match_case(
                    ast.MatchAs(), body=self._mark_branch(node.lineno, to_line)
                )
            )

        self.generic_visit(node)
        return node
"#,
        EXIT
    );

    let code_cstr = CString::new(transformer_code)?;
    let file_cstr = CString::new("transformer.py")?;
    let name_cstr = CString::new("transformer")?;

    let locals = PyModule::from_code(py, &code_cstr, &file_cstr, &name_cstr)?;
    let transformer_class = locals.getattr("SlipcoverTransformer")?;
    let transformer = transformer_class.call1((encode_branch,))?;

    Ok(transformer)
}

/// Helper to check if an object is an instance of any of the given type names.
fn is_instance(py: Python, obj: &Bound<PyAny>, type_names: &[&str]) -> PyResult<bool> {
    let ast = PyModule::import(py, "ast")?;

    for type_name in type_names {
        if let Ok(type_obj) = ast.getattr(*type_name) {
            if obj.is_instance(&type_obj)? {
                return Ok(true);
            }
        }
    }

    Ok(false)
}

/// Helper to check if an object has an attribute.
fn has_attr(obj: &Bound<PyAny>, attr: &str) -> PyResult<bool> {
    obj.hasattr(attr)
}
