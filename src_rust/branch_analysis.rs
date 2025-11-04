use tree_sitter::{Parser, Node};
use std::collections::HashMap;
use tree_sitter_python::LANGUAGE;

/// Information about a branch point
#[derive(Debug, Clone)]
pub struct BranchInfo {
    /// The line number where the branch occurs (1-indexed)
    pub branch_line: usize,
    /// List of (insert_before_line, to_line) pairs for branch markers to insert
    /// insert_before_line: where to insert the marker in the body
    /// to_line: the target line for the branch (or 0 for EXIT)
    pub markers: Vec<(usize, usize)>,
}

/// Analyzes Python source code to identify branch points for coverage instrumentation
pub fn analyze_branches(source: &str) -> Result<Vec<BranchInfo>, String> {
    let mut parser = Parser::new();
    parser.set_language(&LANGUAGE.into())
        .map_err(|e| format!("Error loading Python grammar: {:?}", e))?;

    let tree = parser.parse(source, None)
        .ok_or("Failed to parse source code")?;

    let mut branches = Vec::new();
    let root_node = tree.root_node();

    // Compute "next node" information (what statement comes after each node)
    let next_nodes = compute_next_nodes(&root_node, source);

    // Walk the tree to find branch statements
    find_branches_recursive(&root_node, source, &next_nodes, &mut branches)?;

    Ok(branches)
}

/// Computes the "next node" line number for each node (what executes after this node completes)
fn compute_next_nodes(root: &Node, source: &str) -> HashMap<usize, usize> {
    let mut next_nodes = HashMap::new();
    compute_next_nodes_recursive(root, source, 0, &mut next_nodes);
    next_nodes
}

#[allow(clippy::only_used_in_recursion)]
fn compute_next_nodes_recursive(
    node: &Node,
    source: &str,
    parent_next: usize,
    next_nodes: &mut HashMap<usize, usize>
) {
    let node_id = node.id();
    let kind = node.kind();

    // For function definitions, there's no next node (exit)
    if kind == "function_definition" || kind == "async_function_definition" {
        next_nodes.insert(node_id, 0); // 0 = EXIT

        // Process children but don't inherit parent's next
        for i in 0..node.child_count() {
            if let Some(child) = node.child(i) {
                compute_next_nodes_recursive(&child, source, 0, next_nodes);
            }
        }
        return;
    }

    // Default: this node's next is the parent's next
    next_nodes.insert(node_id, parent_next);

    // Handle block structures (lists of statements)
    if kind == "block" || kind == "module" {
        let mut stmts: Vec<Node> = Vec::new();
        for i in 0..node.child_count() {
            if let Some(child) = node.child(i)
                && is_statement(&child) {
                    stmts.push(child);
                }
        }

        // Each statement's next is the following statement, except the last
        for i in 0..stmts.len() {
            let stmt = &stmts[i];
            let stmt_id = stmt.id();

            if i + 1 < stmts.len() {
                // Next statement exists
                let next_stmt = &stmts[i + 1];
                let next_line = next_stmt.start_position().row + 1; // Convert to 1-indexed
                next_nodes.insert(stmt_id, next_line);
                compute_next_nodes_recursive(stmt, source, next_line, next_nodes);
            } else {
                // Last statement in block - inherits parent's next
                next_nodes.insert(stmt_id, parent_next);
                compute_next_nodes_recursive(stmt, source, parent_next, next_nodes);
            }
        }
    } else {
        // For other nodes, recurse into children
        for i in 0..node.child_count() {
            if let Some(child) = node.child(i) {
                compute_next_nodes_recursive(&child, source, parent_next, next_nodes);
            }
        }
    }
}

/// Recursively finds branch statements (if, for, while, match)
fn find_branches_recursive(
    node: &Node,
    source: &str,
    next_nodes: &HashMap<usize, usize>,
    branches: &mut Vec<BranchInfo>
) -> Result<(), String> {
    let kind = node.kind();

    match kind {
        "if_statement" => handle_if_statement(node, source, next_nodes, branches)?,
        "for_statement" | "while_statement" => handle_loop_statement(node, source, next_nodes, branches)?,
        "match_statement" => handle_match_statement(node, source, next_nodes, branches)?,
        _ => {
            // Recurse into children
            for i in 0..node.child_count() {
                if let Some(child) = node.child(i) {
                    find_branches_recursive(&child, source, next_nodes, branches)?;
                }
            }
        }
    }

    Ok(())
}

fn handle_if_statement(
    node: &Node,
    source: &str,
    next_nodes: &HashMap<usize, usize>,
    branches: &mut Vec<BranchInfo>
) -> Result<(), String> {
    let branch_line = node.start_position().row + 1; // 1-indexed
    let mut markers = Vec::new();

    // Find the consequence (if body)
    if let Some(consequence) = node.child_by_field_name("consequence") {
        let body_first_line = find_first_statement_line(&consequence);
        if let Some(first_line) = body_first_line {
            markers.push((first_line, first_line));
        }
    }

    // Find the alternative (else/elif)
    if let Some(alternative) = node.child_by_field_name("alternative") {
        let alt_first_line = find_first_statement_line(&alternative);
        if let Some(first_line) = alt_first_line {
            markers.push((first_line, first_line));
        }
    } else {
        // No else clause - branch to next statement after if
        let next_line = next_nodes.get(&node.id()).copied().unwrap_or(0);
        markers.push((0, next_line)); // 0 means "append to orelse"
    }

    branches.push(BranchInfo {
        branch_line,
        markers,
    });

    // Recurse into children to find nested branches
    for i in 0..node.child_count() {
        if let Some(child) = node.child(i) {
            find_branches_recursive(&child, source, next_nodes, branches)?;
        }
    }

    Ok(())
}

fn handle_loop_statement(
    node: &Node,
    source: &str,
    next_nodes: &HashMap<usize, usize>,
    branches: &mut Vec<BranchInfo>
) -> Result<(), String> {
    let branch_line = node.start_position().row + 1; // 1-indexed
    let mut markers = Vec::new();

    // Find the loop body
    if let Some(body) = node.child_by_field_name("body") {
        let body_first_line = find_first_statement_line(&body);
        if let Some(first_line) = body_first_line {
            markers.push((first_line, first_line));
        }
    }

    // Find the else clause (for/while can have else)
    if let Some(alternative) = node.child_by_field_name("alternative") {
        let alt_first_line = find_first_statement_line(&alternative);
        if let Some(first_line) = alt_first_line {
            markers.push((first_line, first_line));
        }
    } else {
        // No else clause - branch to next statement
        let next_line = next_nodes.get(&node.id()).copied().unwrap_or(0);
        markers.push((0, next_line)); // 0 means "append to orelse"
    }

    branches.push(BranchInfo {
        branch_line,
        markers,
    });

    // Recurse into children
    for i in 0..node.child_count() {
        if let Some(child) = node.child(i) {
            find_branches_recursive(&child, source, next_nodes, branches)?;
        }
    }

    Ok(())
}

fn handle_match_statement(
    node: &Node,
    source: &str,
    next_nodes: &HashMap<usize, usize>,
    branches: &mut Vec<BranchInfo>
) -> Result<(), String> {
    let branch_line = node.start_position().row + 1; // 1-indexed
    let mut markers = Vec::new();
    let mut has_wildcard = false;

    // Find all case clauses
    if let Some(body) = node.child_by_field_name("body") {
        for i in 0..body.child_count() {
            if let Some(case) = body.child(i)
                && case.kind() == "case_clause" {
                    // Each case gets a marker
                    let case_first_line = find_first_statement_line(&case);
                    if let Some(first_line) = case_first_line {
                        markers.push((first_line, first_line));
                    }

                    // Check if this is a wildcard case
                    if let Some(pattern) = case.child_by_field_name("pattern")
                        && is_wildcard_pattern(&pattern) && !has_guard(&case) {
                            has_wildcard = true;
                        }
                }
        }
    }

    // If no wildcard, add a fallthrough branch
    if !has_wildcard {
        let next_line = next_nodes.get(&node.id()).copied().unwrap_or(0);
        markers.push((0, next_line)); // Will need to add a wildcard case
    }

    branches.push(BranchInfo {
        branch_line,
        markers,
    });

    // Recurse into children
    for i in 0..node.child_count() {
        if let Some(child) = node.child(i) {
            find_branches_recursive(&child, source, next_nodes, branches)?;
        }
    }

    Ok(())
}

fn find_first_statement_line(node: &Node) -> Option<usize> {
    if is_statement(node) {
        return Some(node.start_position().row + 1);
    }

    // If it's a block, find the first statement child
    if node.kind() == "block" {
        for i in 0..node.child_count() {
            if let Some(child) = node.child(i)
                && is_statement(&child) {
                    return Some(child.start_position().row + 1);
                }
        }
    }

    // Check all children
    for i in 0..node.child_count() {
        if let Some(child) = node.child(i)
            && let Some(line) = find_first_statement_line(&child) {
                return Some(line);
            }
    }

    None
}

fn is_statement(node: &Node) -> bool {
    let kind = node.kind();
    !kind.contains("comment") && !kind.contains("newline") && node.is_named()
        && !matches!(kind, "block" | ":" | "(" | ")" | "[" | "]" | "{" | "}")
}

fn is_wildcard_pattern(node: &Node) -> bool {
    // A wildcard pattern is "_" or a bare identifier used as a capture
    if node.kind() == "wildcard_pattern" {
        return true;
    }
    if node.kind() == "as_pattern" {
        // Check if it's an as_pattern without a specific pattern (just a name)
        if let Some(pattern) = node.child_by_field_name("pattern") {
            return pattern.kind() == "wildcard_pattern";
        }
        // If no pattern field, it might be a bare name, which is a wildcard
        return true;
    }
    false
}

fn has_guard(case_node: &Node) -> bool {
    case_node.child_by_field_name("guard").is_some()
}
