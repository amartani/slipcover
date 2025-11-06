// Coverage tracking functionality
// Based on the original Python covers.py module (CoverageTracker part)

use crate::branch::{decode_branch, is_branch};
use crate::schemas::{FileCoverageData, LineOrBranch};
use ahash::{AHashMap, AHashSet};
use pyo3::prelude::*;
use pyo3::types::{PyAny, PyDict, PyList, PySet, PyTuple};
use std::sync::{Arc, Mutex};

/// Core coverage tracking structure
/// This is the performance-critical data structure that tracks which lines/branches have been executed
#[pyclass]
pub struct CoverageTracker {
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

impl Default for CoverageTracker {
    fn default() -> Self {
        Self::new()
    }
}

#[pymethods]
impl CoverageTracker {
    #[new]
    pub fn new() -> Self {
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
    pub fn handle_line(&self, filename: String, line: i32) {
        // Work with the data structures
        let mut inner = self.inner.lock().expect("CoverageTracker mutex poisoned");
        let seen_set = inner.newly_seen.entry(filename).or_default();

        if is_branch(line) {
            let (from_line, to_line) = decode_branch(line);
            seen_set.insert(LineOrBranch::Branch(from_line, to_line));
        } else if line != 0 {
            seen_set.insert(LineOrBranch::Line(line));
        }
    }

    /// Get and clear the newly seen lines/branches
    pub fn get_newly_seen(&self, py: Python) -> PyResult<Py<PyAny>> {
        let mut inner = self.inner.lock().expect("CoverageTracker mutex poisoned");

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
    pub fn merge_newly_seen(&self) {
        let mut inner = self.inner.lock().expect("CoverageTracker mutex poisoned");

        // Take ownership of newly_seen and replace with empty map, avoiding clones
        let newly_seen = std::mem::take(&mut inner.newly_seen);
        for (filename, new_items) in newly_seen {
            inner
                .all_seen
                .entry(filename)
                .or_default()
                .extend(new_items);
        }
    }

    /// Add instrumented lines for a file
    pub fn add_code_lines(&self, filename: String, lines: Vec<i32>) {
        let mut inner = self.inner.lock().expect("CoverageTracker mutex poisoned");
        let lines_set = inner.code_lines.entry(filename).or_default();
        lines_set.extend(lines);
    }

    /// Add instrumented branches for a file
    pub fn add_code_branches(&self, filename: String, branches: Vec<(i32, i32)>) {
        let mut inner = self.inner.lock().expect("CoverageTracker mutex poisoned");
        let branches_set = inner.code_branches.entry(filename).or_default();
        branches_set.extend(branches);
    }

    /// Get coverage data for all files (legacy Python API)
    pub fn get_coverage_data(&self, py: Python, with_branches: bool) -> PyResult<Py<PyAny>> {
        let files_data = self.get_coverage_data_native(with_branches);

        // Convert to Python structures
        let files_dict = PyDict::new(py);

        for (filename, file_data) in files_data {
            let file_dict = PyDict::new(py);

            file_dict.set_item(
                "executed_lines",
                PyList::new(py, &file_data.executed_lines)?,
            )?;
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
    pub fn clear_all_seen(&self) {
        let mut inner = self.inner.lock().expect("CoverageTracker mutex poisoned");
        inner.all_seen.clear();
        inner.newly_seen.clear();
    }

    /// Get all instrumented files
    pub fn get_instrumented_files(&self, py: Python) -> PyResult<Py<PyAny>> {
        let inner = self.inner.lock().expect("CoverageTracker mutex poisoned");
        let files: Vec<&String> = inner.code_lines.keys().collect();
        Ok(PyList::new(py, files)?.into())
    }

    /// Check if a file has been instrumented
    pub fn has_file(&self, filename: String) -> bool {
        let inner = self.inner.lock().expect("CoverageTracker mutex poisoned");
        inner.code_lines.contains_key(&filename)
    }
}

// Internal methods for CoverageTracker (not exposed to Python)
impl CoverageTracker {
    /// Get coverage data for all files (native Rust structures)
    pub fn get_coverage_data_native(
        &self,
        with_branches: bool,
    ) -> AHashMap<String, FileCoverageData> {
        let inner = self.inner.lock().expect("CoverageTracker mutex poisoned");
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

                let mut executed_branches: Vec<(i32, i32)> =
                    branches_seen.iter().copied().collect();
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
