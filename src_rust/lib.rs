// Covers - Code coverage tool for Python
// Main library file - module organization and PyO3 bindings

use pyo3::prelude::*;

// Module declarations
mod branch;
mod branch_analysis;
mod code_analysis;
mod covers;
mod lcovreport;
mod path;
mod reporting;
mod schemas;
mod tracker;
mod xmlreport;

// Re-export main types and functions for the Python module
use branch::{analyze_branches_ts, decode_branch, encode_branch, is_branch};
use code_analysis::{branches_from_code, lines_from_code};
use covers::{Covers, VERSION};
use lcovreport::print_lcov;
use path::PathSimplifier;
use reporting::{add_summaries, print_coverage};
use tracker::CoverageTracker;
use xmlreport::print_xml;

/// Module definition
#[pymodule]
fn covers_core(_py: Python, m: &Bound<PyModule>) -> PyResult<()> {
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
    m.add_function(wrap_pyfunction!(print_coverage, m)?)?;
    m.add_function(wrap_pyfunction!(print_xml, m)?)?;
    m.add_function(wrap_pyfunction!(print_lcov, m)?)?;

    // Classes
    m.add_class::<CoverageTracker>()?;
    m.add_class::<PathSimplifier>()?;
    m.add_class::<Covers>()?;

    // Version
    m.add("__version__", VERSION)?;

    Ok(())
}
