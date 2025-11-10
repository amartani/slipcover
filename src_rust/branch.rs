// Branch encoding and decoding functionality
// Based on the original Python branch.py module

use crate::branch_analysis::analyze_branches;
use ahash::AHashMap;
use pyo3::exceptions::PyAssertionError;
use pyo3::prelude::*;
use pyo3::types::{PyDict, PyList, PyTuple};

// Branch encoding constants
pub const BRANCH_MARKER: i32 = 1 << 30;
pub const LINE_MASK: i32 = 0x7FFF;

// EXIT constant used in Python branch.py module
#[allow(dead_code)]
pub const EXIT: i32 = 0;

/// Check if a line number is actually a branch marker
#[pyfunction]
pub fn is_branch(line: i32) -> bool {
    (line & BRANCH_MARKER) != 0
}

/// Encode a branch from one line to another
#[pyfunction]
pub fn encode_branch(from_line: i32, to_line: i32) -> PyResult<i32> {
    if from_line > LINE_MASK {
        return Err(PyAssertionError::new_err(format!(
            "Line number {} too high, unable to add branch tracking",
            from_line
        )));
    }
    if to_line > LINE_MASK {
        return Err(PyAssertionError::new_err(format!(
            "Line number {} too high, unable to add branch tracking",
            to_line
        )));
    }
    Ok(BRANCH_MARKER | ((from_line & LINE_MASK) << 15) | (to_line & LINE_MASK))
}

/// Decode a branch marker into (from_line, to_line)
#[pyfunction]
pub fn decode_branch(line: i32) -> (i32, i32) {
    ((line >> 15) & LINE_MASK, line & LINE_MASK)
}

/// Analyze Python source code to find branch points using tree-sitter (internal implementation)
pub fn analyze_branches_ts_impl(
    source: &str,
) -> Result<AHashMap<usize, Vec<(usize, usize)>>, String> {
    let branch_info_list = analyze_branches(source)?;

    // Build a native Rust structure
    let mut branches_map: AHashMap<usize, Vec<(usize, usize)>> = AHashMap::new();

    for info in branch_info_list {
        branches_map.insert(info.branch_line, info.markers);
    }

    Ok(branches_map)
}

/// Analyze Python source code to find branch points using tree-sitter (Python API)
#[pyfunction]
pub fn analyze_branches_ts(py: Python, source: String) -> PyResult<Py<PyDict>> {
    // Call the internal implementation
    let branches_map = analyze_branches_ts_impl(&source)
        .map_err(PyErr::new::<pyo3::exceptions::PyValueError, _>)?;

    // Convert to Python structures
    let result = PyDict::new(py);

    for (branch_line, markers) in branches_map {
        let markers_list = PyList::empty(py);
        for (insert_line, to_line) in markers {
            markers_list.append(PyTuple::new(py, [insert_line, to_line])?)?;
        }
        result.set_item(branch_line, markers_list)?;
    }

    Ok(result.into())
}
