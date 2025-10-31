use pyo3::prelude::*;
use pyo3::types::{PyDict, PyList, PySet, PyTuple};
use pyo3::exceptions::PyAssertionError;
use std::collections::HashMap;
use std::sync::{Arc, Mutex};
use ahash::AHashSet;

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

/// Core coverage tracking structure
/// This is the performance-critical data structure that tracks which lines/branches have been executed
#[pyclass]
struct CoverageTracker {
    // Protects all the data structures below
    inner: Arc<Mutex<CoverageTrackerInner>>,
}

struct CoverageTrackerInner {
    // Notes which code lines have been instrumented
    code_lines: HashMap<String, AHashSet<i32>>,
    code_branches: HashMap<String, AHashSet<(i32, i32)>>,

    // Notes which lines and branches have been seen
    all_seen: HashMap<String, AHashSet<LineOrBranch>>,

    // Notes lines/branches seen since last get_newly_seen
    newly_seen: HashMap<String, AHashSet<LineOrBranch>>,
}

#[pymethods]
impl CoverageTracker {
    #[new]
    fn new() -> Self {
        CoverageTracker {
            inner: Arc::new(Mutex::new(CoverageTrackerInner {
                code_lines: HashMap::new(),
                code_branches: HashMap::new(),
                all_seen: HashMap::new(),
                newly_seen: HashMap::new(),
            })),
        }
    }

    /// Handle a line execution event from sys.monitoring
    /// This is the performance-critical callback function
    fn handle_line(&self, py: Python, filename: String, line: i32) {
        // Release the GIL while we're working with the data structures
        py.allow_threads(|| {
            let mut inner = self.inner.lock().unwrap();
            let seen_set = inner.newly_seen.entry(filename).or_insert_with(AHashSet::new);

            if is_branch(line) {
                let (from_line, to_line) = decode_branch(line);
                seen_set.insert(LineOrBranch::Branch(from_line, to_line));
            } else if line != 0 {
                seen_set.insert(LineOrBranch::Line(line));
            }
        });
    }

    /// Get and clear the newly seen lines/branches
    fn get_newly_seen(&self, py: Python) -> PyResult<PyObject> {
        let mut inner = self.inner.lock().unwrap();

        // Create a new empty HashMap for newly_seen and swap it with the current one
        let old_newly_seen = std::mem::replace(&mut inner.newly_seen, HashMap::new());

        // Convert to Python dict
        let result = PyDict::new_bound(py);
        for (filename, items) in old_newly_seen.iter() {
            let py_set = PySet::empty_bound(py)?;
            for item in items {
                match item {
                    LineOrBranch::Line(line) => {
                        py_set.add(*line)?;
                    }
                    LineOrBranch::Branch(from_line, to_line) => {
                        let tuple = PyTuple::new_bound(py, &[*from_line, *to_line]);
                        py_set.add(tuple)?;
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
            let all_set = inner.all_seen.entry(filename).or_insert_with(AHashSet::new);
            for item in new_items {
                all_set.insert(item);
            }
        }

        inner.newly_seen.clear();
    }

    /// Add instrumented lines for a file
    fn add_code_lines(&self, filename: String, lines: Vec<i32>) {
        let mut inner = self.inner.lock().unwrap();
        let lines_set = inner.code_lines.entry(filename).or_insert_with(AHashSet::new);
        lines_set.extend(lines);
    }

    /// Add instrumented branches for a file
    fn add_code_branches(&self, filename: String, branches: Vec<(i32, i32)>) {
        let mut inner = self.inner.lock().unwrap();
        let branches_set = inner.code_branches.entry(filename).or_insert_with(AHashSet::new);
        branches_set.extend(branches);
    }

    /// Get coverage data for all files
    fn get_coverage_data(&self, py: Python, with_branches: bool) -> PyResult<PyObject> {
        let inner = self.inner.lock().unwrap();

        let files_dict = PyDict::new_bound(py);

        for (filename, code_lines) in inner.code_lines.iter() {
            let file_dict = PyDict::new_bound(py);

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

            file_dict.set_item("executed_lines", PyList::new_bound(py, executed_lines))?;
            file_dict.set_item("missing_lines", PyList::new_bound(py, missing_lines))?;

            // Handle branch coverage if requested
            if with_branches {
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

                // Convert to list of tuples for Python
                let exec_br_list = PyList::empty_bound(py);
                for (from_line, to_line) in executed_branches {
                    exec_br_list.append(PyTuple::new_bound(py, &[from_line, to_line]))?;
                }

                let miss_br_list = PyList::empty_bound(py);
                for (from_line, to_line) in missing_branches {
                    miss_br_list.append(PyTuple::new_bound(py, &[from_line, to_line]))?;
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
    fn get_instrumented_files(&self, py: Python) -> PyResult<PyObject> {
        let inner = self.inner.lock().unwrap();
        let files: Vec<&String> = inner.code_lines.keys().collect();
        Ok(PyList::new_bound(py, files).into())
    }

    /// Check if a file has been instrumented
    fn has_file(&self, filename: String) -> bool {
        let inner = self.inner.lock().unwrap();
        inner.code_lines.contains_key(&filename)
    }
}

/// Module definition
#[pymodule]
fn slipcover_core(_py: Python, m: &Bound<PyModule>) -> PyResult<()> {
    m.add_function(wrap_pyfunction!(is_branch, m)?)?;
    m.add_function(wrap_pyfunction!(encode_branch, m)?)?;
    m.add_function(wrap_pyfunction!(decode_branch, m)?)?;
    m.add_class::<CoverageTracker>()?;
    Ok(())
}
