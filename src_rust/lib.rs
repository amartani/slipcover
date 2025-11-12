// Covers - Code coverage tool for Python
// Main library file - module organization and PyO3 bindings

use pyo3::prelude::*;

// Module declarations
mod branch;
mod branch_analysis;
mod bytecode;
mod cli;
mod code_analysis;
mod covers;
mod file_matcher;
mod lcovreport;
mod path;
mod reporting;
mod schemas;
mod tracker;
mod xmlreport;

// Re-export main types and functions for the Python module
use branch::{analyze_branches_ts, decode_branch, encode_branch, is_branch};
use bytecode::{Branch, Editor, ExceptionTableEntry, LineEntry};
use cli::{main_cli, parse_args};
use code_analysis::{branches_from_code, lines_from_code};
use covers::{Covers, VERSION};
use file_matcher::FileMatcher;
use lcovreport::print_lcov;
use path::PathSimplifier;
use reporting::{add_summaries, format_missing_py, merge_coverage, print_coverage};
use tracker::CoverageTracker;
use xmlreport::print_xml;

// Create a custom CoversError exception
// This will be available to other modules via `crate::CoversError`
pyo3::create_exception!(covers_core, CoversError, pyo3::exceptions::PyException);

/// Module definition
#[pymodule]
fn covers_core(_py: Python, m: &Bound<PyModule>) -> PyResult<()> {
    // CLI functions
    m.add_function(wrap_pyfunction!(main_cli, m)?)?;
    m.add_function(wrap_pyfunction!(parse_args, m)?)?;

    // Branch functions
    m.add_function(wrap_pyfunction!(is_branch, m)?)?;
    m.add_function(wrap_pyfunction!(encode_branch, m)?)?;
    m.add_function(wrap_pyfunction!(decode_branch, m)?)?;
    m.add_function(wrap_pyfunction!(analyze_branches_ts, m)?)?;

    // Code analysis functions
    m.add_function(wrap_pyfunction!(lines_from_code, m)?)?;
    m.add_function(wrap_pyfunction!(branches_from_code, m)?)?;

    // Reporting functions
    m.add_function(wrap_pyfunction!(add_summaries, m)?)?;
    m.add_function(wrap_pyfunction!(format_missing_py, m)?)?;
    m.add_function(wrap_pyfunction!(merge_coverage, m)?)?;
    m.add_function(wrap_pyfunction!(print_coverage, m)?)?;
    m.add_function(wrap_pyfunction!(print_xml, m)?)?;
    m.add_function(wrap_pyfunction!(print_lcov, m)?)?;

    // Classes
    m.add_class::<CoverageTracker>()?;
    m.add_class::<PathSimplifier>()?;
    m.add_class::<Covers>()?;
    m.add_class::<FileMatcher>()?;
    m.add_class::<schemas::CoverageData>()?;

    // Bytecode classes
    m.add_class::<Branch>()?;
    m.add_class::<Editor>()?;
    m.add_class::<ExceptionTableEntry>()?;
    m.add_class::<LineEntry>()?;

    // Version
    m.add("__version__", VERSION)?;

    // Exceptions
    m.add("CoversError", m.py().get_type::<CoversError>())?;

    Ok(())
}
