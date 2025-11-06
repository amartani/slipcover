// Code object analysis - extracting lines and branches
// Based on the original Python bytecode.py module

use crate::branch::is_branch;
use pyo3::prelude::*;
use pyo3::types::{PyAny, PyModule, PyTuple};

/// Extract line numbers from a code object (non-branch lines)
#[pyfunction]
pub fn lines_from_code(py: Python, co: &Bound<PyAny>) -> PyResult<Vec<i32>> {
    let mut lines = Vec::new();

    // Recursively process co_consts
    let consts = co.getattr("co_consts")?;

    for c in consts.try_iter()? {
        let item = c?;
        // Check if it's a code object by checking for co_code attribute
        if item.hasattr("co_code")? {
            let sub_lines = lines_from_code(py, &item)?;
            lines.extend(sub_lines);
        }
    }

    // Get lines from this code object using dis.findlinestarts
    let dis_module = PyModule::import(py, "dis")?;
    let findlinestarts = dis_module.getattr("findlinestarts")?;
    let line_starts = findlinestarts.call1((co,))?;

    // Get opmap for RESUME and RETURN_GENERATOR
    let opmap = dis_module.getattr("opmap")?;
    let op_resume: u8 = opmap.get_item("RESUME")?.extract()?;
    let op_return_generator: u8 = opmap.get_item("RETURN_GENERATOR")?.extract()?;
    let co_code = co.getattr("co_code")?;
    let co_code_bytes: &[u8] = co_code.extract()?;

    for item in line_starts.try_iter()? {
        let bound_item = item?;
        let tuple: &Bound<PyTuple> = bound_item.cast()?;
        let off: usize = tuple.get_item(0)?.extract()?;

        // Check if line is None (Python 3.13 can return None)
        let line_obj = tuple.get_item(1)?;
        if line_obj.is_none() {
            continue;
        }
        let line: i32 = line_obj.extract()?;

        // Filter out None lines, RESUME, and RETURN_GENERATOR
        if line != 0
            && off < co_code_bytes.len()
            && co_code_bytes[off] != op_resume
            && co_code_bytes[off] != op_return_generator
            && !is_branch(line)
        {
            lines.push(line);
        }
    }

    Ok(lines)
}

/// Extract branch tuples from a code object
#[pyfunction]
pub fn branches_from_code(py: Python, co: &Bound<PyAny>) -> PyResult<Vec<(i32, i32)>> {
    let mut branches = Vec::new();

    // Recursively process co_consts
    let consts = co.getattr("co_consts")?;

    for c in consts.try_iter()? {
        let item = c?;
        // Check if it's a code object
        if item.hasattr("co_code")? {
            let sub_branches = branches_from_code(py, &item)?;
            branches.extend(sub_branches);
        }
    }

    // Get branches from this code object using dis.findlinestarts
    let dis_module = PyModule::import(py, "dis")?;
    let findlinestarts = dis_module.getattr("findlinestarts")?;
    let line_starts = findlinestarts.call1((co,))?;

    for item in line_starts.try_iter()? {
        let bound_item = item?;
        let tuple: &Bound<PyTuple> = bound_item.cast()?;

        // Check if line is None (Python 3.13 can return None)
        let line_obj = tuple.get_item(1)?;
        if line_obj.is_none() {
            continue;
        }
        let line: i32 = line_obj.extract()?;

        if is_branch(line) {
            branches.push(crate::branch::decode_branch(line));
        }
    }

    Ok(branches)
}
