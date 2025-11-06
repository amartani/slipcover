use ahash::AHashMap;
use tree_sitter::{Node, Parser};
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
    parser
        .set_language(&LANGUAGE.into())
        .map_err(|e| format!("Error loading Python grammar: {:?}", e))?;

    let tree = parser
        .parse(source, None)
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
fn compute_next_nodes(root: &Node, source: &str) -> AHashMap<usize, usize> {
    let mut next_nodes = AHashMap::new();
    compute_next_nodes_recursive(root, source, 0, &mut next_nodes);
    next_nodes
}

#[allow(clippy::only_used_in_recursion)]
fn compute_next_nodes_recursive(
    node: &Node,
    source: &str,
    parent_next: usize,
    next_nodes: &mut AHashMap<usize, usize>,
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

    // Special handling for loops: the loop body should loop back to the loop header
    // Note: async for does NOT loop back (matching Python's old implementation)
    if kind == "for_statement" {
        // Check if this is async for by looking for an 'async' child
        let is_async =
            (0..node.child_count()).any(|i| node.child(i).map(|c| c.kind()) == Some("async"));

        if is_async {
            // Async for: body does NOT loop back
            if let Some(body) = node.child_by_field_name("body") {
                compute_next_nodes_recursive(&body, source, parent_next, next_nodes);
            }
        } else {
            // Regular for: body loops back
            let loop_line = node.start_position().row + 1;
            if let Some(body) = node.child_by_field_name("body") {
                compute_next_nodes_recursive(&body, source, loop_line, next_nodes);
            }
        }

        // Process other children (condition, alternative) with parent's next
        for i in 0..node.child_count() {
            if let Some(child) = node.child(i)
                && node.field_name_for_child(i as u32) != Some("body")
            {
                compute_next_nodes_recursive(&child, source, parent_next, next_nodes);
            }
        }
        return;
    }

    // Special handling for while loops: body loops back
    if kind == "while_statement" {
        let loop_line = node.start_position().row + 1;

        // Process the loop body with the loop line as the "next"
        if let Some(body) = node.child_by_field_name("body") {
            compute_next_nodes_recursive(&body, source, loop_line, next_nodes);
        }

        // Process other children (condition, alternative) with parent's next
        for i in 0..node.child_count() {
            if let Some(child) = node.child(i)
                && node.field_name_for_child(i as u32) != Some("body")
            {
                compute_next_nodes_recursive(&child, source, parent_next, next_nodes);
            }
        }
        return;
    }

    // Special handling for match_statement: all cases should use match's next, not next case
    if kind == "match_statement" {
        // Find the body block and process each case_clause with parent_next
        if let Some(body) = node.child_by_field_name("body") {
            // Process each case_clause in the body with parent_next
            for i in 0..body.child_count() {
                if let Some(case) = body.child(i)
                    && case.kind() == "case_clause"
                {
                    compute_next_nodes_recursive(&case, source, parent_next, next_nodes);
                }
            }
        }

        // Process non-body children
        for i in 0..node.child_count() {
            if let Some(child) = node.child(i)
                && node.field_name_for_child(i as u32) != Some("body")
            {
                compute_next_nodes_recursive(&child, source, parent_next, next_nodes);
            }
        }
        return;
    }

    // Special handling for case_clause: process case body with parent_next
    if kind == "case_clause" {
        // Process case body with parent_next (next after match, not next case)
        if let Some(body) = node.child_by_field_name("consequence") {
            compute_next_nodes_recursive(&body, source, parent_next, next_nodes);
        }
        return;
    }

    // Special handling for try_statement: try body should go to else (if exists) then finally
    if kind == "try_statement" {
        // Find else and finally clauses
        let mut else_line = None;
        let mut finally_line = None;

        for i in 0..node.child_count() {
            if let Some(child) = node.child(i) {
                match child.kind() {
                    "else_clause" => {
                        else_line = find_first_statement_line(&child);
                    }
                    "finally_clause" => {
                        finally_line = find_first_statement_line(&child);
                    }
                    _ => {}
                }
            }
        }

        // Try body branches to: else (if exists), then finally (if exists), then parent's next
        let try_next = else_line.or(finally_line).unwrap_or(parent_next);

        // Process try body with else/finally/parent's next
        if let Some(body) = node.child_by_field_name("body") {
            compute_next_nodes_recursive(&body, source, try_next, next_nodes);
        }

        // Process except handlers - they skip else and go to finally or parent's next
        let except_next = finally_line.unwrap_or(parent_next);
        for i in 0..node.child_count() {
            if let Some(child) = node.child(i) {
                let child_kind = child.kind();
                if child_kind == "except_clause" {
                    compute_next_nodes_recursive(&child, source, except_next, next_nodes);
                } else if child_kind == "else_clause" {
                    // Else clause branches to finally (if exists) or parent's next
                    let else_next = finally_line.unwrap_or(parent_next);
                    compute_next_nodes_recursive(&child, source, else_next, next_nodes);
                } else if child_kind == "finally_clause" {
                    compute_next_nodes_recursive(&child, source, parent_next, next_nodes);
                }
            }
        }
        return;
    }

    // Handle block structures (lists of statements)
    if kind == "block" || kind == "module" {
        let mut stmts: Vec<Node> = Vec::new();
        for i in 0..node.child_count() {
            if let Some(child) = node.child(i)
                && is_statement(&child)
            {
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
    next_nodes: &AHashMap<usize, usize>,
    branches: &mut Vec<BranchInfo>,
) -> Result<(), String> {
    let kind = node.kind();

    match kind {
        "if_statement" => handle_if_statement(node, source, next_nodes, branches)?,
        "elif_clause" => {
            // Handle elif as an if_statement
            // Find the condition and body
            handle_elif_as_if(node, source, next_nodes, branches)?;
        }
        "for_statement" | "while_statement" | "async_for_statement" => {
            handle_loop_statement(node, source, next_nodes, branches)?
        }
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

fn handle_elif_as_if(
    node: &Node,
    source: &str,
    next_nodes: &AHashMap<usize, usize>,
    branches: &mut Vec<BranchInfo>,
) -> Result<(), String> {
    // elif_clause has similar structure to if_statement
    // It has condition and consequence fields, and optionally alternative
    let branch_line = node.start_position().row + 1; // 1-indexed
    let mut markers = Vec::new();

    // Find the consequence (elif body)
    if let Some(consequence) = node.child_by_field_name("consequence") {
        let body_first_line = find_first_statement_line(&consequence);
        if let Some(first_line) = body_first_line {
            markers.push((first_line, first_line));
        }
    }

    // elif clauses in tree-sitter-python are children of if_statement, not nested
    // The else clause (if any) is a sibling of the elif, not a child
    // We need to look at the parent to find sibling alternatives
    if let Some(parent) = node.parent() {
        let mut found_else = false;
        let elif_index =
            (0..parent.child_count()).find(|&i| parent.child(i).map(|c| c.id()) == Some(node.id()));

        if let Some(start_idx) = elif_index {
            // Look for siblings after this elif
            for i in (start_idx + 1)..parent.child_count() {
                if let Some(sibling) = parent.child(i)
                    && parent.field_name_for_child(i as u32) == Some("alternative")
                {
                    let sibling_kind = sibling.kind();

                    if sibling_kind == "else_clause" {
                        // Found an else clause - add marker to first statement
                        let else_first_line = find_first_statement_line(&sibling);
                        if let Some(first_line) = else_first_line {
                            markers.push((first_line, first_line));
                            found_else = true;
                        }
                        break;
                    } else if sibling_kind == "elif_clause" {
                        // Found another elif - add marker pointing to it
                        let next_elif_line = sibling.start_position().row + 1;
                        markers.push((next_elif_line, next_elif_line));
                        found_else = true;
                        break;
                    }
                }
            }
        }

        if !found_else {
            // No else clause - branch to next statement after entire if/elif chain
            let next_line = next_nodes.get(&parent.id()).copied().unwrap_or(0);
            markers.push((0, next_line)); // 0 means "append to orelse"
        }
    } else {
        // No parent (shouldn't happen) - use next after elif
        let next_line = next_nodes.get(&node.id()).copied().unwrap_or(0);
        markers.push((0, next_line));
    }

    branches.push(BranchInfo {
        branch_line,
        markers,
    });

    // Recurse into consequence and alternative
    if let Some(consequence) = node.child_by_field_name("consequence") {
        find_branches_recursive(&consequence, source, next_nodes, branches)?;
    }

    if let Some(alternative) = node.child_by_field_name("alternative") {
        find_branches_recursive(&alternative, source, next_nodes, branches)?;
    }

    Ok(())
}

fn handle_if_statement(
    node: &Node,
    source: &str,
    next_nodes: &AHashMap<usize, usize>,
    branches: &mut Vec<BranchInfo>,
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

    // Find the FIRST alternative (else/elif)
    // Note: tree-sitter can have multiple alternatives (elif and else), but
    // the if should only branch to the first one. The elif will handle the rest.
    let mut found_alternative = false;
    for i in 0..node.child_count() {
        if let Some(child) = node.child(i)
            && node.field_name_for_child(i as u32) == Some("alternative")
        {
            found_alternative = true;
            let alt_kind = child.kind();

            if alt_kind == "elif_clause" {
                // elif - add marker pointing to the elif line
                // The elif will create its own branches when processed recursively
                let elif_line = child.start_position().row + 1;
                markers.push((elif_line, elif_line));
            } else {
                // Regular else clause - add marker to first statement
                let alt_first_line = find_first_statement_line(&child);
                if let Some(first_line) = alt_first_line {
                    markers.push((first_line, first_line));
                }
            }
            break; // Only process the FIRST alternative
        }
    }

    if !found_alternative {
        // No else clause - branch to next statement after if
        let next_line = next_nodes.get(&node.id()).copied().unwrap_or(0);
        markers.push((0, next_line)); // 0 means "append to orelse"
    }

    branches.push(BranchInfo {
        branch_line,
        markers,
    });

    // Recurse into consequence to find nested branches
    if let Some(consequence) = node.child_by_field_name("consequence") {
        find_branches_recursive(&consequence, source, next_nodes, branches)?;
    }

    // Recurse into alternative to find nested branches (including elif if_statements)
    if let Some(alternative) = node.child_by_field_name("alternative") {
        find_branches_recursive(&alternative, source, next_nodes, branches)?;
    }

    Ok(())
}

fn handle_loop_statement(
    node: &Node,
    source: &str,
    next_nodes: &AHashMap<usize, usize>,
    branches: &mut Vec<BranchInfo>,
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
    next_nodes: &AHashMap<usize, usize>,
    branches: &mut Vec<BranchInfo>,
) -> Result<(), String> {
    let branch_line = node.start_position().row + 1; // 1-indexed
    let mut markers = Vec::new();
    let mut has_wildcard = false;

    // Find all case clauses
    if let Some(body) = node.child_by_field_name("body") {
        for i in 0..body.child_count() {
            if let Some(case) = body.child(i)
                && case.kind() == "case_clause"
            {
                // Each case gets a marker
                let case_first_line = find_first_statement_line(&case);
                if let Some(first_line) = case_first_line {
                    markers.push((first_line, first_line));
                }

                // Check if this is a wildcard case
                for j in 0..case.child_count() {
                    if let Some(child) = case.child(j) {
                        let kind = child.kind();
                        if kind == "case_pattern" {
                            // Check if the pattern contains a wildcard (recursively)
                            eprintln!("DEBUG: checking case_pattern for wildcard");
                            let has_wild = contains_wildcard(&child);
                            eprintln!("  contains_wildcard returned: {}", has_wild);
                            if has_wild && !has_guard(&case) {
                                has_wildcard = true;
                            }
                            break;
                        }
                    }
                }
            }
        }
    }

    // If no wildcard, add a fallthrough branch
    eprintln!(
        "DEBUG: match at line {} has_wildcard={}",
        branch_line, has_wildcard
    );
    if !has_wildcard {
        let next_line = next_nodes.get(&node.id()).copied().unwrap_or(0);
        eprintln!("  adding fallthrough case to line {}", next_line);
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
    let kind = node.kind();

    // Special handling for structural nodes that contain blocks
    if matches!(kind, "else_clause" | "elif_clause" | "finally_clause") {
        // These nodes have a body - recurse into it
        for i in 0..node.child_count() {
            if let Some(child) = node.child(i)
                && let Some(line) = find_first_statement_line(&child)
            {
                return Some(line);
            }
        }
        return None;
    }

    // Special handling for case_clause - look for the "consequence" field
    if kind == "case_clause" {
        if let Some(consequence) = node.child_by_field_name("consequence") {
            return find_first_statement_line(&consequence);
        }
        return None;
    }

    // If it's a block, find the first statement child
    if kind == "block" {
        for i in 0..node.child_count() {
            if let Some(child) = node.child(i)
                && is_statement(&child)
            {
                return Some(child.start_position().row + 1);
            }
        }
        return None;
    }

    // If this is a statement itself, return its line
    if is_statement(node) {
        return Some(node.start_position().row + 1);
    }

    // Check all children
    for i in 0..node.child_count() {
        if let Some(child) = node.child(i)
            && let Some(line) = find_first_statement_line(&child)
        {
            return Some(line);
        }
    }

    None
}

fn is_statement(node: &Node) -> bool {
    let kind = node.kind();
    !kind.contains("comment")
        && !kind.contains("newline")
        && node.is_named()
        && !matches!(kind, "block" | ":" | "(" | ")" | "[" | "]" | "{" | "}")
}

fn is_wildcard_pattern(node: &Node) -> bool {
    // A wildcard pattern is "_" or a bare identifier used as a capture
    let kind = node.kind();
    if kind == "wildcard_pattern" || kind == "_" {
        return true;
    }
    if kind == "as_pattern" {
        // Check if it's an as_pattern without a specific pattern (just a name)
        if let Some(pattern) = node.child_by_field_name("pattern") {
            let pattern_kind = pattern.kind();
            return pattern_kind == "wildcard_pattern" || pattern_kind == "_";
        }
        // If no pattern field, it might be a bare name, which is a wildcard
        return true;
    }
    false
}

fn contains_wildcard(node: &Node) -> bool {
    eprintln!(
        "    contains_wildcard: kind={}, named_children={}",
        node.kind(),
        node.named_child_count()
    );
    // Check if this node or any descendant is a wildcard pattern
    if is_wildcard_pattern(node) {
        eprintln!("      is_wildcard!");
        return true;
    }

    // For case_pattern nodes with a single name child, treat as wildcard
    // A bare identifier like `case y:` is a capture pattern (matches anything)
    if node.kind() == "case_pattern"
        && node.named_child_count() == 1
        && let Some(child) = node.named_child(0)
    {
        let child_kind = child.kind();
        eprintln!("      single child kind={}", child_kind);
        if child_kind == "identifier" || child_kind == "dotted_name" {
            eprintln!("      is capture!");
            return true;
        }
    }

    // Recursively check all children
    for i in 0..node.child_count() {
        if let Some(child) = node.child(i)
            && contains_wildcard(&child)
        {
            return true;
        }
    }

    false
}

fn has_guard(case_node: &Node) -> bool {
    case_node.child_by_field_name("guard").is_some()
}
