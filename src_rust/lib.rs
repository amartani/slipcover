use pyo3::prelude::*;
use pyo3::types::{PyDict, PyList, PySet, PyTuple, PyCFunction};
use pyo3::exceptions::PyAssertionError;
use pyo3::Py;
use std::collections::HashSet;
use ahash::AHashMap;
use std::sync::{Arc, Mutex};
use std::path::{Path, PathBuf};
use ahash::AHashSet;
use tabled::{Table, Tabled, settings::Style};
use chrono::prelude::*;
mod branch_analysis;
use branch_analysis::analyze_branches;

// Branch encoding constants
const BRANCH_MARKER: i32 = 1 << 30;
const LINE_MASK: i32 = 0x7FFF;

/// Check if a line number is actually a branch marker
#[pyfunction]
fn is_branch(line: i32) -> bool {
    (line & BRANCH_MARKER) != 0
}

/// Encode a branch from one line to another
#[pyfunction]
fn encode_branch(from_line: i32, to_line: i32) -> PyResult<i32> {
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
fn decode_branch(line: i32) -> (i32, i32) {
    ((line >> 15) & LINE_MASK, line & LINE_MASK)
}

/// LineOrBranch represents either a line number or a branch tuple
#[derive(Clone, Debug, Hash, Eq, PartialEq)]
enum LineOrBranch {
    Line(i32),
    Branch(i32, i32),
}

/// Native Rust structures for coverage data (to avoid Python conversions)
#[derive(Clone, Debug)]
struct FileCoverageData {
    executed_lines: Vec<i32>,
    missing_lines: Vec<i32>,
    executed_branches: Vec<(i32, i32)>,
    missing_branches: Vec<(i32, i32)>,
}

#[derive(Clone, Debug)]
struct FileSummary {
    covered_lines: i32,
    missing_lines: i32,
    covered_branches: Option<i32>,
    missing_branches: Option<i32>,
    percent_covered: f64,
}

#[derive(Clone, Debug)]
#[allow(dead_code)]
struct FileData {
    coverage: FileCoverageData,
    summary: FileSummary,
}

#[derive(Clone, Debug)]
struct MetaData {
    software: String,
    version: String,
    timestamp: String,
    branch_coverage: bool,
    show_contexts: bool,
}

#[derive(Clone, Debug)]
#[allow(dead_code)]
struct CoverageData {
    meta: MetaData,
    files: AHashMap<String, FileData>,
    summary: FileSummary,
}

/// Core coverage tracking structure
/// This is the performance-critical data structure that tracks which lines/branches have been executed
#[pyclass]
struct CoverageTracker {
    // Protects all the data structures below
    inner: Arc<Mutex<CoverageTrackerInner>>,
}

struct CoverageTrackerInner {
    // Notes which code lines have been instrumented
    code_lines: AHashMap<String, AHashSet<i32>>,
    code_branches: AHashMap<String, AHashSet<(i32, i32)>>,

    // Notes which lines and branches have been seen
    all_seen: AHashMap<String, AHashSet<LineOrBranch>>,

    // Notes lines/branches seen since last get_newly_seen
    newly_seen: AHashMap<String, AHashSet<LineOrBranch>>,
}

#[pymethods]
impl CoverageTracker {
    #[new]
    fn new() -> Self {
        CoverageTracker {
            inner: Arc::new(Mutex::new(CoverageTrackerInner {
                code_lines: AHashMap::new(),
                code_branches: AHashMap::new(),
                all_seen: AHashMap::new(),
                newly_seen: AHashMap::new(),
            })),
        }
    }

    /// Handle a line execution event from sys.monitoring
    /// This is the performance-critical callback function
    fn handle_line(&self, filename: String, line: i32) {
        // Work with the data structures
        let mut inner = self.inner.lock().unwrap();
        let seen_set = inner.newly_seen.entry(filename).or_default();

        if is_branch(line) {
            let (from_line, to_line) = decode_branch(line);
            seen_set.insert(LineOrBranch::Branch(from_line, to_line));
        } else if line != 0 {
            seen_set.insert(LineOrBranch::Line(line));
        }
    }

    /// Get and clear the newly seen lines/branches
    fn get_newly_seen(&self, py: Python) -> PyResult<Py<PyAny>> {
        let mut inner = self.inner.lock().unwrap();

        // Create a new empty HashMap for newly_seen and swap it with the current one
        let old_newly_seen = std::mem::take(&mut inner.newly_seen);

        // Convert to Python dict - consume the data instead of iterating over references
        let result = PyDict::new(py);
        for (filename, items) in old_newly_seen.into_iter() {
            let py_set = PySet::empty(py)?;
            for item in items {
                match item {
                    LineOrBranch::Line(line) => {
                        py_set.add(line)?;
                    }
                    LineOrBranch::Branch(from_line, to_line) => {
                        py_set.add(PyTuple::new(py, [from_line, to_line])?)?;
                    }
                }
            }
            result.set_item(filename, py_set)?;
        }

        Ok(result.into())
    }

    /// Update all_seen with the contents of newly_seen
    fn merge_newly_seen(&self) {
        let mut inner = self.inner.lock().unwrap();

        // Clone the newly_seen data first to avoid borrow checker issues
        let newly_seen_data: Vec<(String, AHashSet<LineOrBranch>)> = inner
            .newly_seen
            .iter()
            .map(|(k, v)| (k.clone(), v.clone()))
            .collect();

        for (filename, new_items) in newly_seen_data {
            let all_set = inner.all_seen.entry(filename).or_default();
            for item in new_items {
                all_set.insert(item);
            }
        }

        inner.newly_seen.clear();
    }

    /// Add instrumented lines for a file
    fn add_code_lines(&self, filename: String, lines: Vec<i32>) {
        let mut inner = self.inner.lock().unwrap();
        let lines_set = inner.code_lines.entry(filename).or_default();
        lines_set.extend(lines);
    }

    /// Add instrumented branches for a file
    fn add_code_branches(&self, filename: String, branches: Vec<(i32, i32)>) {
        let mut inner = self.inner.lock().unwrap();
        let branches_set = inner.code_branches.entry(filename).or_default();
        branches_set.extend(branches);
    }

    /// Get coverage data for all files (legacy Python API)
    fn get_coverage_data(&self, py: Python, with_branches: bool) -> PyResult<Py<PyAny>> {
        let files_data = self.get_coverage_data_native(with_branches);

        // Convert to Python structures
        let files_dict = PyDict::new(py);

        for (filename, file_data) in files_data {
            let file_dict = PyDict::new(py);

            file_dict.set_item("executed_lines", PyList::new(py, &file_data.executed_lines)?)?;
            file_dict.set_item("missing_lines", PyList::new(py, &file_data.missing_lines)?)?;

            if with_branches {
                // Convert branch tuples to Python
                let exec_br_list = PyList::empty(py);
                for (from_line, to_line) in file_data.executed_branches {
                    exec_br_list.append(PyTuple::new(py, [from_line, to_line])?)?;
                }

                let miss_br_list = PyList::empty(py);
                for (from_line, to_line) in file_data.missing_branches {
                    miss_br_list.append(PyTuple::new(py, [from_line, to_line])?)?;
                }

                file_dict.set_item("executed_branches", exec_br_list)?;
                file_dict.set_item("missing_branches", miss_br_list)?;
            }

            files_dict.set_item(filename, file_dict)?;
        }

        Ok(files_dict.into())
    }

    /// Clear all coverage data (for child processes)
    fn clear_all_seen(&self) {
        let mut inner = self.inner.lock().unwrap();
        inner.all_seen.clear();
        inner.newly_seen.clear();
    }

    /// Get all instrumented files
    fn get_instrumented_files(&self, py: Python) -> PyResult<Py<PyAny>> {
        let inner = self.inner.lock().unwrap();
        let files: Vec<&String> = inner.code_lines.keys().collect();
        Ok(PyList::new(py, files)?.into())
    }

    /// Check if a file has been instrumented
    fn has_file(&self, filename: String) -> bool {
        let inner = self.inner.lock().unwrap();
        inner.code_lines.contains_key(&filename)
    }
}

// Internal methods for CoverageTracker (not exposed to Python)
impl CoverageTracker {
    /// Get coverage data for all files (native Rust structures)
    fn get_coverage_data_native(&self, with_branches: bool) -> AHashMap<String, FileCoverageData> {
        let inner = self.inner.lock().unwrap();
        let mut files_data: AHashMap<String, FileCoverageData> = AHashMap::new();

        for (filename, code_lines) in inner.code_lines.iter() {
            // Get the seen lines and branches for this file
            let (lines_seen, branches_seen) = if let Some(all_seen) = inner.all_seen.get(filename) {
                let mut lines = AHashSet::new();
                let mut branches = AHashSet::new();

                for item in all_seen {
                    match item {
                        LineOrBranch::Line(line) => {
                            lines.insert(*line);
                        }
                        LineOrBranch::Branch(from_line, to_line) => {
                            branches.insert((*from_line, *to_line));
                        }
                    }
                }

                (lines, branches)
            } else {
                (AHashSet::new(), AHashSet::new())
            };

            // Calculate executed and missing lines
            let mut executed_lines: Vec<i32> = lines_seen.iter().copied().collect();
            executed_lines.sort_unstable();

            let mut missing_lines: Vec<i32> = code_lines
                .iter()
                .filter(|line| !lines_seen.contains(line))
                .copied()
                .collect();
            missing_lines.sort_unstable();

            // Calculate executed and missing branches if requested
            let (executed_branches, missing_branches) = if with_branches {
                let code_branches = inner.code_branches.get(filename);

                let mut executed_branches: Vec<(i32, i32)> = branches_seen.iter().copied().collect();
                executed_branches.sort_unstable();

                let mut missing_branches: Vec<(i32, i32)> = if let Some(cb) = code_branches {
                    cb.iter()
                        .filter(|branch| !branches_seen.contains(branch))
                        .copied()
                        .collect()
                } else {
                    Vec::new()
                };
                missing_branches.sort_unstable();

                (executed_branches, missing_branches)
            } else {
                (Vec::new(), Vec::new())
            };

            files_data.insert(
                filename.clone(),
                FileCoverageData {
                    executed_lines,
                    missing_lines,
                    executed_branches,
                    missing_branches,
                },
            );
        }

        files_data
    }
}

/// PathSimplifier - Simplifies file paths relative to current working directory
#[pyclass]
struct PathSimplifier {
    cwd: PathBuf,
}

#[pymethods]
impl PathSimplifier {
    #[new]
    fn new() -> PyResult<Self> {
        let cwd = dunce::canonicalize(std::env::current_dir()?)
            .map_err(|e| PyErr::new::<pyo3::exceptions::PyOSError, _>(format!("Failed to get cwd: {}", e)))?;
        Ok(PathSimplifier { cwd })
    }

    fn simplify(&self, path: String) -> String {
        let p = Path::new(&path);
        match p.strip_prefix(&self.cwd) {
            Ok(relative) => relative.to_string_lossy().to_string(),
            Err(_) => path,
        }
    }
}

/// Extract line numbers from a code object (non-branch lines)
#[pyfunction]
fn lines_from_code(py: Python, co: &Bound<PyAny>) -> PyResult<Vec<i32>> {
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
        if line != 0 &&
           off < co_code_bytes.len() &&
           co_code_bytes[off] != op_resume &&
           co_code_bytes[off] != op_return_generator &&
           !is_branch(line) {
            lines.push(line);
        }
    }

    Ok(lines)
}

/// Extract branch tuples from a code object
#[pyfunction]
fn branches_from_code(py: Python, co: &Bound<PyAny>) -> PyResult<Vec<(i32, i32)>> {
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
            branches.push(decode_branch(line));
        }
    }

    Ok(branches)
}

/// Version constant
const VERSION: &str = "1.0.17";

/// Format missing lines and branches as a string
fn format_missing(
    missing_lines: &[i32],
    executed_lines: &[i32],
    missing_branches: &[(i32, i32)],
) -> String {
    let missing_set: HashSet<i32> = missing_lines.iter().copied().collect();
    let executed_set: HashSet<i32> = executed_lines.iter().copied().collect();

    // Filter out branches where both endpoints are missing
    let mut branches: Vec<(i32, i32)> = missing_branches
        .iter()
        .filter(|(a, b)| !missing_set.contains(a) && !missing_set.contains(b))
        .copied()
        .collect();
    branches.sort_unstable();

    let format_branch = |br: (i32, i32)| -> String {
        if br.1 == 0 {
            format!("{}->exit", br.0)
        } else {
            format!("{}->{}", br.0, br.1)
        }
    };

    let mut result = Vec::new();
    let mut lines_iter = missing_lines.iter().copied().peekable();
    let mut branch_idx = 0;

    while let Some(a) = lines_iter.next() {
        // Add branches that come before this line
        while branch_idx < branches.len() && branches[branch_idx].0 < a {
            result.push(format_branch(branches[branch_idx]));
            branch_idx += 1;
        }

        // Find the end of this range
        let mut b = a;
        while let Some(&n) = lines_iter.peek() {
            // Check if there's any executed line between b and n
            let has_executed = (b + 1..=n).any(|line| executed_set.contains(&line));
            if has_executed {
                break;
            }
            b = n;
            lines_iter.next();
        }

        if a == b {
            result.push(a.to_string());
        } else {
            result.push(format!("{}-{}", a, b));
        }
    }

    // Add remaining branches
    while branch_idx < branches.len() {
        result.push(format_branch(branches[branch_idx]));
        branch_idx += 1;
    }

    result.join(", ")
}

/// Row structure for the coverage table
#[derive(Tabled)]
struct CoverageRow {
    #[tabled(rename = "File")]
    file: String,
    #[tabled(rename = "#lines")]
    lines: String,
    #[tabled(rename = "#l.miss")]
    lines_miss: String,
    #[tabled(rename = "#br.")]
    branches: String,
    #[tabled(rename = "#br.miss")]
    branches_miss: String,
    #[tabled(rename = "brCov%")]
    branch_cov: String,
    #[tabled(rename = "totCov%")]
    total_cov: String,
    #[tabled(rename = "Missing")]
    missing: String,
}

/// Row structure for the coverage table without branch coverage
#[derive(Tabled)]
struct SimpleCoverageRow {
    #[tabled(rename = "File")]
    file: String,
    #[tabled(rename = "#lines")]
    lines: String,
    #[tabled(rename = "#l.miss")]
    lines_miss: String,
    #[tabled(rename = "Cover%")]
    coverage: String,
    #[tabled(rename = "Missing")]
    missing: String,
}

/// Print coverage information
#[pyfunction]
#[pyo3(signature = (coverage, outfile=None, missing_width=None, skip_covered=false))]
fn print_coverage(
    py: Python,
    coverage: &Bound<PyDict>,
    outfile: Option<Py<PyAny>>,
    missing_width: Option<usize>,
    skip_covered: bool,
) -> PyResult<()> {
    // Get files from coverage
    let files = match coverage.get_item("files")? {
        Some(f) if !f.is_empty()? => f,
        _ => return Ok(()), // No files to report
    };
    let files_dict: &Bound<PyDict> = files.cast()?;

    // Check if branch coverage is enabled
    let mut branch_coverage = false;
    if let Some(meta) = coverage.get_item("meta")?
        && let Ok(bc) = meta.get_item("branch_coverage")
    {
        branch_coverage = bc.extract::<bool>().unwrap_or(false);
    }

    // Collect rows
    let mut files_vec: Vec<(String, Py<PyAny>)> = files_dict
        .iter()
        .map(|(k, v)| (k.extract::<String>().unwrap(), v.into()))
        .collect();
    files_vec.sort_by(|a, b| a.0.cmp(&b.0));

    let table_str = if branch_coverage {
        let mut rows: Vec<CoverageRow> = Vec::new();

        for (filename, f_info_py) in files_vec {
            let f_info: &Bound<PyDict> = f_info_py.bind(py).cast()?;

            let exec_l = f_info.get_item("executed_lines")?.unwrap().len()?;
            let miss_l = f_info.get_item("missing_lines")?.unwrap().len()?;

            let exec_b = f_info.get_item("executed_branches")?.unwrap().len()?;
            let miss_b = f_info.get_item("missing_branches")?.unwrap().len()?;
            let total_b = exec_b + miss_b;
            let pct_b = if total_b > 0 {
                (100 * exec_b) / total_b
            } else {
                0
            };

            let summary = f_info.get_item("summary")?.unwrap();
            let summary_dict: &Bound<PyDict> = summary.cast()?;
            let pct: f64 = summary_dict.get_item("percent_covered")?.unwrap().extract()?;

            if skip_covered && (pct - 100.0).abs() < 0.01 {
                continue;
            }

            // Get missing info
            let missing_lines_list = f_info.get_item("missing_lines")?.unwrap();
            let missing_lines: Vec<i32> = missing_lines_list.extract()?;

            let executed_lines_list = f_info.get_item("executed_lines")?.unwrap();
            let executed_lines: Vec<i32> = executed_lines_list.extract()?;

            let missing_branches = if let Some(mb) = f_info.get_item("missing_branches")? {
                let mb_list: Vec<Vec<i32>> = mb.extract()?;
                mb_list.into_iter().map(|v| (v[0], v[1])).collect()
            } else {
                Vec::new()
            };

            let missing_str = format_missing(&missing_lines, &executed_lines, &missing_branches);
            let truncated_missing = if let Some(width) = missing_width {
                if missing_str.len() > width {
                    format!("{}...", &missing_str[..width.saturating_sub(3)])
                } else {
                    missing_str
                }
            } else {
                missing_str
            };

            rows.push(CoverageRow {
                file: filename,
                lines: (exec_l + miss_l).to_string(),
                lines_miss: miss_l.to_string(),
                branches: total_b.to_string(),
                branches_miss: miss_b.to_string(),
                branch_cov: pct_b.to_string(),
                total_cov: pct.round().to_string(),
                missing: truncated_missing,
            });
        }

        // Add summary if multiple files
        if files_dict.len() > 1 {
            let summary = coverage.get_item("summary")?.unwrap();
            let summary_dict: &Bound<PyDict> = summary.cast()?;

            let s_covered_lines: i32 = summary_dict.get_item("covered_lines")?.unwrap().extract()?;
            let s_missing_lines: i32 = summary_dict.get_item("missing_lines")?.unwrap().extract()?;
            let s_covered_branches: i32 = summary_dict.get_item("covered_branches")?.unwrap().extract()?;
            let s_missing_branches: i32 = summary_dict.get_item("missing_branches")?.unwrap().extract()?;
            let s_percent: f64 = summary_dict.get_item("percent_covered")?.unwrap().extract()?;

            let total_b = s_covered_branches + s_missing_branches;
            let pct_b = if total_b > 0 {
                (100 * s_covered_branches) / total_b
            } else {
                0
            };

            rows.push(CoverageRow {
                file: "---".to_string(),
                lines: String::new(),
                lines_miss: String::new(),
                branches: String::new(),
                branches_miss: String::new(),
                branch_cov: String::new(),
                total_cov: String::new(),
                missing: String::new(),
            });

            rows.push(CoverageRow {
                file: "(summary)".to_string(),
                lines: (s_covered_lines + s_missing_lines).to_string(),
                lines_miss: s_missing_lines.to_string(),
                branches: total_b.to_string(),
                branches_miss: s_missing_branches.to_string(),
                branch_cov: pct_b.to_string(),
                total_cov: s_percent.round().to_string(),
                missing: String::new(),
            });
        }

        let mut table = Table::new(rows);
        table.with(Style::empty());

        // Note: missing_width parameter truncates strings before adding to table
        // so width constraint is already handled

        table.to_string()
    } else {
        let mut rows: Vec<SimpleCoverageRow> = Vec::new();

        for (filename, f_info_py) in files_vec {
            let f_info: &Bound<PyDict> = f_info_py.bind(py).cast()?;

            let exec_l = f_info.get_item("executed_lines")?.unwrap().len()?;
            let miss_l = f_info.get_item("missing_lines")?.unwrap().len()?;

            let summary = f_info.get_item("summary")?.unwrap();
            let summary_dict: &Bound<PyDict> = summary.cast()?;
            let pct: f64 = summary_dict.get_item("percent_covered")?.unwrap().extract()?;

            if skip_covered && (pct - 100.0).abs() < 0.01 {
                continue;
            }

            // Get missing info
            let missing_lines_list = f_info.get_item("missing_lines")?.unwrap();
            let missing_lines: Vec<i32> = missing_lines_list.extract()?;

            let executed_lines_list = f_info.get_item("executed_lines")?.unwrap();
            let executed_lines: Vec<i32> = executed_lines_list.extract()?;

            let missing_branches: Vec<(i32, i32)> = Vec::new();

            let missing_str = format_missing(&missing_lines, &executed_lines, &missing_branches);
            let truncated_missing = if let Some(width) = missing_width {
                if missing_str.len() > width {
                    format!("{}...", &missing_str[..width.saturating_sub(3)])
                } else {
                    missing_str
                }
            } else {
                missing_str
            };

            rows.push(SimpleCoverageRow {
                file: filename,
                lines: (exec_l + miss_l).to_string(),
                lines_miss: miss_l.to_string(),
                coverage: pct.round().to_string(),
                missing: truncated_missing,
            });
        }

        // Add summary if multiple files
        if files_dict.len() > 1 {
            let summary = coverage.get_item("summary")?.unwrap();
            let summary_dict: &Bound<PyDict> = summary.cast()?;

            let s_covered_lines: i32 = summary_dict.get_item("covered_lines")?.unwrap().extract()?;
            let s_missing_lines: i32 = summary_dict.get_item("missing_lines")?.unwrap().extract()?;
            let s_percent: f64 = summary_dict.get_item("percent_covered")?.unwrap().extract()?;

            rows.push(SimpleCoverageRow {
                file: "---".to_string(),
                lines: String::new(),
                lines_miss: String::new(),
                coverage: String::new(),
                missing: String::new(),
            });

            rows.push(SimpleCoverageRow {
                file: "(summary)".to_string(),
                lines: (s_covered_lines + s_missing_lines).to_string(),
                lines_miss: s_missing_lines.to_string(),
                coverage: s_percent.round().to_string(),
                missing: String::new(),
            });
        }

        let mut table = Table::new(rows);
        table.with(Style::empty());

        // Note: missing_width parameter truncates strings before adding to table
        // so width constraint is already handled

        table.to_string()
    };

    // Write output
    let output = format!("\n{}\n", table_str);

    if let Some(outfile_py) = outfile {
        let outfile_bound = outfile_py.bind(py);
        let write_method = outfile_bound.getattr("write")?;
        write_method.call1((output,))?;
    } else {
        // Default to stdout
        let sys_module = PyModule::import(py, "sys")?;
        let stdout = sys_module.getattr("stdout")?;
        let write_method = stdout.getattr("write")?;
        write_method.call1((output,))?;
    }

    Ok(())
}

/// Adds summaries to coverage data (native Rust structures)
fn add_summaries_native(
    files_data: &mut AHashMap<String, FileCoverageData>,
    with_branches: bool,
) -> (AHashMap<String, FileSummary>, FileSummary) {
    let mut file_summaries: AHashMap<String, FileSummary> = AHashMap::new();
    let mut g_covered_lines = 0;
    let mut g_missing_lines = 0;
    let mut g_covered_branches = 0;
    let mut g_missing_branches = 0;

    for (filename, file_data) in files_data.iter() {
        let covered_lines = file_data.executed_lines.len() as i32;
        let missing_lines_count = file_data.missing_lines.len() as i32;

        let mut nom = covered_lines;
        let mut den = nom + missing_lines_count;

        let (covered_branches, missing_branches_count) = if with_branches {
            let cb = file_data.executed_branches.len() as i32;
            let mb = file_data.missing_branches.len() as i32;
            nom += cb;
            den += cb + mb;
            (Some(cb), Some(mb))
        } else {
            (None, None)
        };

        let percent_covered = if den == 0 { 100.0 } else { 100.0 * nom as f64 / den as f64 };

        file_summaries.insert(
            filename.clone(),
            FileSummary {
                covered_lines,
                missing_lines: missing_lines_count,
                covered_branches,
                missing_branches: missing_branches_count,
                percent_covered,
            },
        );

        g_covered_lines += covered_lines;
        g_missing_lines += missing_lines_count;
        if let Some(cb) = covered_branches {
            g_covered_branches += cb;
        }
        if let Some(mb) = missing_branches_count {
            g_missing_branches += mb;
        }
    }

    // Calculate global summary
    let g_nom = if with_branches {
        g_covered_lines + g_covered_branches
    } else {
        g_covered_lines
    };
    let g_den = if with_branches {
        g_covered_lines + g_missing_lines + g_covered_branches + g_missing_branches
    } else {
        g_covered_lines + g_missing_lines
    };
    let g_percent_covered = if g_den == 0 { 100.0 } else { 100.0 * g_nom as f64 / g_den as f64 };

    let global_summary = FileSummary {
        covered_lines: g_covered_lines,
        missing_lines: g_missing_lines,
        covered_branches: if with_branches { Some(g_covered_branches) } else { None },
        missing_branches: if with_branches { Some(g_missing_branches) } else { None },
        percent_covered: g_percent_covered,
    };

    (file_summaries, global_summary)
}

/// Adds (or updates) 'summary' entries in coverage information (legacy Python API)
#[pyfunction]
fn add_summaries(py: Python, cov: &Bound<PyDict>) -> PyResult<()> {
    let mut g_summary_data: AHashMap<String, i32> = AHashMap::new();
    let mut g_nom = 0;
    let mut g_den = 0;

    // Process files if they exist
    if let Ok(Some(files)) = cov.get_item("files") {
        let files_dict: &Bound<PyDict> = files.cast()?;

        for (_filename, f_cov_obj) in files_dict.iter() {
            let f_cov: &Bound<PyDict> = f_cov_obj.cast()?;

            // Get executed and missing lines
            let executed_lines = f_cov.get_item("executed_lines")?.unwrap();
            let missing_lines = f_cov.get_item("missing_lines")?.unwrap();

            let covered_lines = executed_lines.len()? as i32;
            let missing_lines_count = missing_lines.len()? as i32;

            let mut nom = covered_lines;
            let mut den = nom + missing_lines_count;

            // Create summary dict
            let summary = PyDict::new(py);
            summary.set_item("covered_lines", covered_lines)?;
            summary.set_item("missing_lines", missing_lines_count)?;

            // Handle branches if present
            if let Ok(Some(executed_branches)) = f_cov.get_item("executed_branches") {
                let missing_branches = f_cov.get_item("missing_branches")?.unwrap();

                let covered_branches = executed_branches.len()? as i32;
                let missing_branches_count = missing_branches.len()? as i32;

                summary.set_item("covered_branches", covered_branches)?;
                summary.set_item("missing_branches", missing_branches_count)?;

                nom += covered_branches;
                den += covered_branches + missing_branches_count;

                // Update global summary for branches
                *g_summary_data.entry("covered_branches".to_string()).or_insert(0) += covered_branches;
                *g_summary_data.entry("missing_branches".to_string()).or_insert(0) += missing_branches_count;
            }

            // Calculate percent covered
            let percent_covered = if den == 0 { 100.0 } else { 100.0 * nom as f64 / den as f64 };
            summary.set_item("percent_covered", percent_covered)?;

            // Set summary on file
            f_cov.set_item("summary", summary)?;

            // Update global summary for lines
            *g_summary_data.entry("covered_lines".to_string()).or_insert(0) += covered_lines;
            *g_summary_data.entry("missing_lines".to_string()).or_insert(0) += missing_lines_count;

            g_nom += nom;
            g_den += den;
        }
    }

    // Create global summary
    let g_summary = PyDict::new(py);
    for (k, v) in g_summary_data {
        g_summary.set_item(k, v)?;
    }

    let g_percent_covered = if g_den == 0 { 100.0 } else { 100.0 * g_nom as f64 / g_den as f64 };
    g_summary.set_item("percent_covered", g_percent_covered)?;
    g_summary.set_item("percent_covered_display", format!("{}", g_percent_covered.round() as i32))?;

    cov.set_item("summary", g_summary)?;

    Ok(())
}

/// Main Covers class
#[pyclass]
struct Covers {
    immediate: bool,
    #[allow(dead_code)]
    d_miss_threshold: i32,
    branch: bool,
    #[allow(dead_code)]
    disassemble: bool,
    source: Option<Vec<String>>,
    instrumented_code_ids: Arc<Mutex<HashSet<usize>>>,
    tracker: Py<CoverageTracker>,
    modules: Vec<Py<PyAny>>,
}

#[pymethods]
impl Covers {
    #[new]
    #[pyo3(signature = (immediate=false, d_miss_threshold=50, branch=false, disassemble=false, source=None))]
    fn new(
        py: Python,
        immediate: bool,
        d_miss_threshold: i32,
        branch: bool,
        disassemble: bool,
        source: Option<Vec<String>>,
    ) -> PyResult<Py<Self>> {
        let tracker = Py::new(py, CoverageTracker::new())?;
        let instrumented_code_ids = Arc::new(Mutex::new(HashSet::new()));

        let slf = Py::new(
            py,
            Covers {
                immediate,
                d_miss_threshold,
                branch,
                disassemble,
                source,
                instrumented_code_ids: instrumented_code_ids.clone(),
                tracker: tracker.clone_ref(py),
                modules: Vec::new(),
            },
        )?;

        // Set up sys.monitoring callback
        let sys_module = PyModule::import(py, "sys")?;
        let monitoring = sys_module.getattr("monitoring")?;

        // Check if tool is already registered
        let coverage_id = monitoring.getattr("COVERAGE_ID")?;
        let current_tool = monitoring.call_method1("get_tool", (&coverage_id,))?;

        if current_tool.is_none() || current_tool.extract::<String>().ok() != Some("Covers".to_string()) {
            monitoring.call_method1("use_tool_id", (&coverage_id, "Covers"))?;
        }

        // Create the handle_line callback
        let tracker_ref = tracker.clone_ref(py);
        let ids_ref = instrumented_code_ids.clone();

        let handle_line = PyCFunction::new_closure(
            py,
            None,
            None,
            move |args: &Bound<PyTuple>, _kwargs: Option<&Bound<PyDict>>| -> PyResult<Py<PyAny>> {
                Python::attach(|py| {
                    let code = args.get_item(0)?;
                    let line: i32 = args.get_item(1)?.extract()?;

                    // Get code object ID using builtins.id()
                    let code_id = code.as_ptr() as usize;

                    // Check if this code object was instrumented by this instance
                    {
                        let ids = ids_ref.lock().unwrap();
                        if !ids.contains(&code_id) {
                            // Return DISABLE constant
                            let sys_module = PyModule::import(py, "sys")?;
                            let monitoring = sys_module.getattr("monitoring")?;
                            return Ok(monitoring.getattr("DISABLE")?.into());
                        }
                    }

                    // Call tracker.handle_line
                    let filename: String = code.getattr("co_filename")?.extract()?;
                    tracker_ref.bind(py).borrow().handle_line(filename, line);

                    // Return DISABLE constant
                    let sys_module = PyModule::import(py, "sys")?;
                    let monitoring = sys_module.getattr("monitoring")?;
                    Ok(monitoring.getattr("DISABLE")?.into())
                })
            },
        )?;

        // Register the callback
        let events = monitoring.getattr("events")?;
        let line_event = events.getattr("LINE")?;
        monitoring.call_method1("register_callback", (&coverage_id, &line_event, handle_line))?;

        Ok(slf)
    }

    #[pyo3(signature = (co, parent=None))]
    fn instrument(&mut self, py: Python, co: Py<PyAny>, parent: Option<Py<PyAny>>) -> PyResult<Py<PyAny>> {
        let co_bound = co.bind(py);

        // If it's a function, get its __code__
        let code_obj = if co_bound.hasattr("__code__")? {
            co_bound.getattr("__code__")?.into()
        } else {
            co.clone_ref(py)
        };

        let code_bound = code_obj.bind(py);

        // Get code object ID and track it
        let code_id = code_bound.as_ptr() as usize;

        {
            let mut ids = self.instrumented_code_ids.lock().unwrap();
            ids.insert(code_id);
        }

        // Set up monitoring for this code object
        let sys_module = PyModule::import(py, "sys")?;
        let monitoring = sys_module.getattr("monitoring")?;
        let coverage_id = monitoring.getattr("COVERAGE_ID")?;
        let events = monitoring.getattr("events")?;
        let line_event = events.getattr("LINE")?;

        monitoring.call_method1("set_local_events", (coverage_id, code_bound, line_event))?;
        monitoring.call_method0("restart_events")?;

        // Recursively instrument nested code objects
        let consts = code_bound.getattr("co_consts")?;

        for c in consts.try_iter()? {
            let item = c?;
            if item.hasattr("co_code")? {
                self.instrument(py, item.into(), Some(code_obj.clone_ref(py)))?;
            }
        }

        // If this is a top-level code object (no parent), register lines and branches
        if parent.is_none() {
            let filename: String = code_bound.getattr("co_filename")?.extract()?;

            let lines = lines_from_code(py, code_bound)?;
            let branches = branches_from_code(py, code_bound)?;

            let tracker = self.tracker.bind(py).borrow();
            tracker.add_code_lines(filename.clone(), lines);
            if !branches.is_empty() {
                tracker.add_code_branches(filename, branches);
            }
        }

        Ok(code_obj)
    }

    fn get_coverage(&mut self, py: Python) -> PyResult<Py<PyDict>> {
        // Merge newly seen into all_seen
        let tracker = self.tracker.bind(py).borrow();
        tracker.merge_newly_seen();

        // Add unseen source files if source is specified
        if let Some(ref source_paths) = self.source {
            self._add_unseen_source_files_internal(py, source_paths.clone())?;
        }

        // Simplify paths
        let simp = PathSimplifier::new()?;

        // Get coverage data from tracker using native structures
        let files_data = tracker.get_coverage_data_native(self.branch);

        // Simplify file paths
        let mut simplified_files_data: AHashMap<String, FileCoverageData> = AHashMap::new();
        for (path, file_data) in files_data {
            let simplified = simp.simplify(path);
            simplified_files_data.insert(simplified, file_data);
        }

        // Add summaries using native Rust structures
        let (file_summaries, global_summary) = add_summaries_native(&mut simplified_files_data, self.branch);

        // Create meta using native structures
        let meta_data = Self::_make_meta_native(self.branch);

        // Now convert everything to Python at once
        let cov = PyDict::new(py);

        // Convert meta
        let meta_dict = PyDict::new(py);
        meta_dict.set_item("software", meta_data.software)?;
        meta_dict.set_item("version", meta_data.version)?;
        meta_dict.set_item("timestamp", meta_data.timestamp)?;
        meta_dict.set_item("branch_coverage", meta_data.branch_coverage)?;
        meta_dict.set_item("show_contexts", meta_data.show_contexts)?;
        cov.set_item("meta", meta_dict)?;

        // Convert files with their summaries
        let files_dict = PyDict::new(py);
        for (filename, file_data) in simplified_files_data {
            let file_dict = PyDict::new(py);

            file_dict.set_item("executed_lines", PyList::new(py, &file_data.executed_lines)?)?;
            file_dict.set_item("missing_lines", PyList::new(py, &file_data.missing_lines)?)?;

            if self.branch {
                let exec_br_list = PyList::empty(py);
                for (from_line, to_line) in file_data.executed_branches {
                    exec_br_list.append(PyTuple::new(py, [from_line, to_line])?)?;
                }

                let miss_br_list = PyList::empty(py);
                for (from_line, to_line) in file_data.missing_branches {
                    miss_br_list.append(PyTuple::new(py, [from_line, to_line])?)?;
                }

                file_dict.set_item("executed_branches", exec_br_list)?;
                file_dict.set_item("missing_branches", miss_br_list)?;
            }

            // Add file summary
            if let Some(summary) = file_summaries.get(&filename) {
                let summary_dict = PyDict::new(py);
                summary_dict.set_item("covered_lines", summary.covered_lines)?;
                summary_dict.set_item("missing_lines", summary.missing_lines)?;
                if let Some(cb) = summary.covered_branches {
                    summary_dict.set_item("covered_branches", cb)?;
                }
                if let Some(mb) = summary.missing_branches {
                    summary_dict.set_item("missing_branches", mb)?;
                }
                summary_dict.set_item("percent_covered", summary.percent_covered)?;
                file_dict.set_item("summary", summary_dict)?;
            }

            files_dict.set_item(filename, file_dict)?;
        }
        cov.set_item("files", files_dict)?;

        // Add global summary
        let g_summary_dict = PyDict::new(py);
        g_summary_dict.set_item("covered_lines", global_summary.covered_lines)?;
        g_summary_dict.set_item("missing_lines", global_summary.missing_lines)?;
        if let Some(cb) = global_summary.covered_branches {
            g_summary_dict.set_item("covered_branches", cb)?;
        }
        if let Some(mb) = global_summary.missing_branches {
            g_summary_dict.set_item("missing_branches", mb)?;
        }
        g_summary_dict.set_item("percent_covered", global_summary.percent_covered)?;
        g_summary_dict.set_item("percent_covered_display", format!("{}", global_summary.percent_covered.round() as i32))?;
        cov.set_item("summary", g_summary_dict)?;

        Ok(cov.into())
    }

    fn signal_child_process(&mut self, py: Python) -> PyResult<()> {
        self.source = None;
        let tracker = self.tracker.bind(py).borrow();
        tracker.get_newly_seen(py)?;
        tracker.clear_all_seen();
        Ok(())
    }

    #[staticmethod]
    fn find_functions(py: Python, items: Py<PyAny>, visited: Py<PySet>) -> PyResult<Vec<Py<PyAny>>> {
        // Import types module
        let types_module = PyModule::import(py, "types")?;
        let function_type = types_module.getattr("FunctionType")?;
        let code_type = types_module.getattr("CodeType")?;

        let visited_set = visited.bind(py);
        let mut results = Vec::new();

        // Use native Rust HashSet for faster lookups (store Python object pointer addresses)
        let mut visited_native = AHashSet::new();

        // Populate native set with existing visited items from Python set
        for item in visited_set.iter() {
            visited_native.insert(item.as_ptr() as usize);
        }

        // Convert to list first to handle dict_values and other iterables
        let builtins = PyModule::import(py, "builtins")?;
        let list_fn = builtins.getattr("list")?;
        let items_list: Vec<Py<PyAny>> = list_fn.call1((items,))?.extract()?;

        for item in items_list {
            Self::find_funcs_recursive(py, item, &mut visited_native, &mut results, &function_type, &code_type)?;
        }

        // Sync back to Python set for API consistency
        for item in &results {
            visited_set.add(item)?;
        }

        Ok(results)
    }

    fn register_module(&mut self, module: Py<PyAny>) {
        self.modules.push(module);
    }

    #[staticmethod]
    fn lines_from_code(py: Python, co: Py<PyAny>) -> PyResult<Vec<i32>> {
        lines_from_code(py, co.bind(py))
    }

    #[staticmethod]
    fn branches_from_code(py: Python, co: Py<PyAny>) -> PyResult<Vec<(i32, i32)>> {
        branches_from_code(py, co.bind(py))
    }

    #[pyo3(signature = (outfile=None, missing_width=None))]
    fn print_coverage(&mut self, py: Python, outfile: Option<Py<PyAny>>, missing_width: Option<usize>) -> PyResult<()> {
        // Get coverage first
        let cov = self.get_coverage(py)?;
        let cov_bound = cov.bind(py);

        // Call the Rust print_coverage function
        print_coverage(py, cov_bound, outfile, missing_width, false)?;
        Ok(())
    }

    fn __str__(&self, _py: Python) -> PyResult<String> {
        Ok(format!("Covers(branch={}, immediate={})", self.branch, self.immediate))
    }

    // Property getters
    #[getter]
    fn branch(&self) -> bool {
        self.branch
    }

    #[getter]
    fn immediate(&self) -> bool {
        self.immediate
    }

    #[getter]
    fn d_miss_threshold(&self) -> i32 {
        self.d_miss_threshold
    }

    #[getter]
    fn disassemble(&self) -> bool {
        self.disassemble
    }
}

// Helper methods implementation
impl Covers {
    fn _make_meta_native(branch_coverage: bool) -> MetaData {
        let now = Local::now();
        let timestamp = now.to_rfc3339_opts(SecondsFormat::Micros, true);

        MetaData {
            software: "covers".to_string(),
            version: VERSION.to_string(),
            timestamp,
            branch_coverage,
            show_contexts: false,
        }
    }

    fn _make_meta(py: Python, branch_coverage: bool) -> PyResult<Py<PyDict>> {
        let meta_data = Self::_make_meta_native(branch_coverage);

        let meta = PyDict::new(py);
        meta.set_item("software", meta_data.software)?;
        meta.set_item("version", meta_data.version)?;
        meta.set_item("timestamp", meta_data.timestamp)?;
        meta.set_item("branch_coverage", meta_data.branch_coverage)?;
        meta.set_item("show_contexts", meta_data.show_contexts)?;

        Ok(meta.into())
    }

    fn _add_unseen_source_files_internal(&self, py: Python, source: Vec<String>) -> PyResult<()> {
        let mut dirs: Vec<PathBuf> = Vec::new();
        for d in source {
            let p = PathBuf::from(d);
            match dunce::canonicalize(&p) {
                Ok(resolved) => dirs.push(resolved),
                Err(e) => return Err(PyErr::new::<pyo3::exceptions::PyOSError, _>(format!("Failed to resolve path {:?}: {}", p, e))),
            }
        }

        let tracker = self.tracker.bind(py).borrow();

        while let Some(p) = dirs.pop() {
            let entries = match std::fs::read_dir(&p) {
                Ok(entries) => entries,
                Err(e) => {
                    println!("Warning: unable to read directory {:?}: {}", p, e);
                    continue;
                }
            };

            for entry_result in entries {
                let entry = match entry_result {
                    Ok(entry) => entry,
                    Err(e) => {
                        println!("Warning: unable to read directory entry in {:?}: {}", p, e);
                        continue;
                    }
                };

                let path = entry.path();
                if path.is_dir() {
                    dirs.push(path);
                } else if path.is_file() && let Some(ext) = path.extension() && ext.to_string_lossy().to_lowercase() == "py" {
                    let filename = path.to_string_lossy().to_string();

                    // Check if file has been instrumented
                    if !tracker.has_file(filename.clone()) {
                        // Try to parse and compile
                        match self._try_add_file_from_path(py, &path, &filename, &tracker) {
                            Ok(_) => {},
                            Err(e) => {
                                println!("Warning: unable to include {}: {}", filename, e);
                            }
                        }
                    }
                }
            }
        }

        Ok(())
    }

    fn _try_add_file_from_path(
        &self,
        py: Python,
        path: &Path,
        filename: &str,
        tracker: &CoverageTracker,
    ) -> PyResult<()> {
        let content = std::fs::read_to_string(path)
            .map_err(|e| PyErr::new::<pyo3::exceptions::PyIOError, _>(format!("Failed to read file {}: {}", filename, e)))?;

        // Call Python's preinstrument_and_compile from covers.branch module
        let branch_module = PyModule::import(py, "covers.branch")?;
        let preinstrument_and_compile = branch_module.getattr("preinstrument_and_compile")?;
        let code = preinstrument_and_compile.call1((content, filename, self.branch))?;

        // Extract lines and branches
        let lines = lines_from_code(py, &code)?;
        if !lines.is_empty() {
            tracker.add_code_lines(filename.to_string(), lines);
        }

        if self.branch {
            let branches = branches_from_code(py, &code)?;
            if !branches.is_empty() {
                tracker.add_code_branches(filename.to_string(), branches);
            }
        }

        Ok(())
    }

    fn find_funcs_recursive(
        py: Python,
        root: Py<PyAny>,
        visited: &mut AHashSet<usize>,
        results: &mut Vec<Py<PyAny>>,
        function_type: &Bound<PyAny>,
        code_type: &Bound<PyAny>,
    ) -> PyResult<()> {
        let root_bound = root.bind(py);
        let root_ptr = root_bound.as_ptr() as usize;
        let root_type = root_bound.get_type();

        // Check if it's a patchable function
        if root_type.is_subclass(function_type)? {
            let code_obj = root_bound.getattr("__code__")?;
            let code_obj_type = code_obj.get_type();

            if code_obj_type.is(code_type) {
                if !visited.contains(&root_ptr) {
                    visited.insert(root_ptr);
                    results.push(root.clone_ref(py));
                }
                return Ok(());
            }
        }

        // Check if it's a type/class
        let type_type = py.get_type::<pyo3::types::PyType>();
        if root_type.is_subclass(&type_type)? {
            if !visited.contains(&root_ptr) {
                visited.insert(root_ptr);

                // Get dir() of the object
                let builtins = PyModule::import(py, "builtins")?;
                let dir_fn = builtins.getattr("dir")?;
                let dir_result = dir_fn.call1((root_bound,))?;
                let obj_names: Vec<String> = dir_result.extract()?;

                // Build MRO
                let mro = root_bound.getattr("__mro__")?;
                let mro_list: &Bound<PyTuple> = mro.cast()?;

                for obj_key in obj_names {
                    for base in mro_list.iter() {
                        let is_root = base.is(root_bound);
                        let base_ptr = base.as_ptr() as usize;
                        let base_visited = visited.contains(&base_ptr);

                        if is_root || !base_visited {
                            let base_dict = base.getattr("__dict__")?;
                            if base_dict.contains(&obj_key)? {
                                let item = base_dict.get_item(&obj_key)?;
                                Self::find_funcs_recursive(py, item.into(), visited, results, function_type, code_type)?;
                                break;
                            }
                        }
                    }
                }
            }
            return Ok(());
        }

        // Check if it's a classmethod or staticmethod
        let classmethod_type = py.get_type::<pyo3::types::PyType>().call1(("classmethod",))?;
        let staticmethod_type = py.get_type::<pyo3::types::PyType>().call1(("staticmethod",))?;

        if (root_type.is_subclass(&classmethod_type)? || root_type.is_subclass(&staticmethod_type)?)
            && let Ok(func) = root_bound.getattr("__func__") {
            let func_type = func.get_type();
            if func_type.is_subclass(function_type)? {
                let func_code = func.getattr("__code__")?;
                let func_code_type = func_code.get_type();
                let func_ptr = func.as_ptr() as usize;

                if func_code_type.is(code_type)
                    && !visited.contains(&func_ptr) {
                    visited.insert(func_ptr);
                    let func_py: Py<PyAny> = func.into();
                    results.push(func_py);
                }
            }
        }

        Ok(())
    }
}

/// Analyze Python source code to find branch points using tree-sitter
#[pyfunction]
fn analyze_branches_ts(py: Python, source: String) -> PyResult<Py<PyDict>> {
    let branch_info_list = analyze_branches(&source)
        .map_err(PyErr::new::<pyo3::exceptions::PyValueError, _>)?;

    // Build a native Rust structure first
    let mut branches_map: AHashMap<usize, Vec<(usize, usize)>> = AHashMap::new();

    for info in branch_info_list {
        branches_map.insert(info.branch_line, info.markers);
    }

    // Now convert to Python structures in one pass
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

/// Module definition
#[pymodule]
fn covers_core(_py: Python, m: &Bound<PyModule>) -> PyResult<()> {
    m.add_function(wrap_pyfunction!(is_branch, m)?)?;
    m.add_function(wrap_pyfunction!(encode_branch, m)?)?;
    m.add_function(wrap_pyfunction!(decode_branch, m)?)?;
    m.add_function(wrap_pyfunction!(lines_from_code, m)?)?;
    m.add_function(wrap_pyfunction!(branches_from_code, m)?)?;
    m.add_function(wrap_pyfunction!(add_summaries, m)?)?;
    m.add_function(wrap_pyfunction!(print_coverage, m)?)?;
    m.add_function(wrap_pyfunction!(analyze_branches_ts, m)?)?;
    m.add_class::<CoverageTracker>()?;
    m.add_class::<PathSimplifier>()?;
    m.add_class::<Covers>()?;
    m.add("__version__", VERSION)?;
    Ok(())
}
