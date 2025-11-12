// Data structures for coverage information
// Based on the original Python schemas.py module (TypedDict definitions)

use ahash::AHashMap;
use pyo3::prelude::*;
use pyo3::types::{PyDict, PyList, PyTuple};

/// LineOrBranch represents either a line number or a branch tuple
#[derive(Clone, Debug, Hash, Eq, PartialEq)]
pub enum LineOrBranch {
    Line(i32),
    Branch(i32, i32),
}

/// Native Rust structures for coverage data (to avoid Python conversions)
#[derive(Clone, Debug)]
pub struct FileCoverageData {
    pub executed_lines: Vec<i32>,
    pub missing_lines: Vec<i32>,
    pub executed_branches: Vec<(i32, i32)>,
    pub missing_branches: Vec<(i32, i32)>,
}

#[derive(Clone, Debug)]
pub struct FileSummary {
    pub covered_lines: i32,
    pub missing_lines: i32,
    pub covered_branches: Option<i32>,
    pub missing_branches: Option<i32>,
    pub percent_covered: f64,
}

#[derive(Clone, Debug)]
pub struct FileData {
    pub coverage: FileCoverageData,
    pub summary: FileSummary,
}

#[derive(Clone, Debug)]
pub struct MetaData {
    pub software: String,
    pub version: String,
    pub timestamp: String,
    pub branch_coverage: bool,
    pub show_contexts: bool,
}

/// Native Rust structure for coverage results
/// This struct uses only Rust-native fields internally and exposes Python-friendly getters
#[pyclass(name = "CoverageData")]
#[derive(Clone, Debug)]
pub struct CoverageData {
    pub meta: MetaData,
    pub files: AHashMap<String, FileData>,
    pub summary: FileSummary,
}

#[pymethods]
impl CoverageData {
    /// Create CoverageData from a Python dictionary
    #[staticmethod]
    pub fn load_from_dict(dict_obj: &Bound<PyAny>) -> PyResult<Self> {
        let dict = dict_obj.cast::<PyDict>()?;

        // Extract meta
        let meta_dict = dict
            .get_item("meta")?
            .ok_or_else(|| PyErr::new::<pyo3::exceptions::PyKeyError, _>("Missing 'meta' key"))?;
        let meta_dict: &Bound<PyDict> = meta_dict.cast()?;

        let meta = MetaData {
            software: meta_dict.get_item("software")?.unwrap().extract()?,
            version: meta_dict.get_item("version")?.unwrap().extract()?,
            timestamp: meta_dict.get_item("timestamp")?.unwrap().extract()?,
            branch_coverage: meta_dict.get_item("branch_coverage")?.unwrap().extract()?,
            show_contexts: meta_dict.get_item("show_contexts")?.unwrap().extract()?,
        };

        // Extract files
        let files_dict = dict
            .get_item("files")?
            .ok_or_else(|| PyErr::new::<pyo3::exceptions::PyKeyError, _>("Missing 'files' key"))?;
        let files_dict: &Bound<PyDict> = files_dict.cast()?;

        let mut files_map: AHashMap<String, FileData> = AHashMap::new();
        for (filename_obj, file_data_obj) in files_dict.iter() {
            let filename: String = filename_obj.extract().map_err(|e| {
                PyErr::new::<pyo3::exceptions::PyTypeError, _>(format!(
                    "Failed to extract filename: {}",
                    e
                ))
            })?;
            let file_dict: &Bound<PyDict> = file_data_obj.cast().map_err(|e| {
                PyErr::new::<pyo3::exceptions::PyTypeError, _>(format!(
                    "Failed to cast file_data for {}: {}",
                    filename, e
                ))
            })?;

            let executed_lines: Vec<i32> = file_dict
                .get_item("executed_lines")?
                .unwrap()
                .extract()
                .map_err(|e| {
                    PyErr::new::<pyo3::exceptions::PyTypeError, _>(format!(
                        "Failed to extract executed_lines for {}: {}",
                        filename, e
                    ))
                })?;
            let missing_lines: Vec<i32> = file_dict
                .get_item("missing_lines")?
                .unwrap()
                .extract()
                .map_err(|e| {
                PyErr::new::<pyo3::exceptions::PyTypeError, _>(format!(
                    "Failed to extract missing_lines for {}: {}",
                    filename, e
                ))
            })?;

            let (executed_branches, missing_branches) = if meta.branch_coverage
                && file_dict.contains("executed_branches")?
                && file_dict.contains("missing_branches")?
            {
                // JSON deserializes arrays as lists of lists, not lists of tuples
                // Extract as Vec<Vec<i32>> first, then convert to Vec<(i32, i32)>
                let exec_br_list: Vec<Vec<i32>> = file_dict
                    .get_item("executed_branches")?
                    .unwrap()
                    .extract()
                    .map_err(|e| {
                        PyErr::new::<pyo3::exceptions::PyTypeError, _>(format!(
                            "Failed to extract executed_branches for {}: {}",
                            filename, e
                        ))
                    })?;

                let miss_br_list: Vec<Vec<i32>> = file_dict
                    .get_item("missing_branches")?
                    .unwrap()
                    .extract()
                    .map_err(|e| {
                        PyErr::new::<pyo3::exceptions::PyTypeError, _>(format!(
                            "Failed to extract missing_branches for {}: {}",
                            filename, e
                        ))
                    })?;

                (
                    exec_br_list.into_iter().map(|v| (v[0], v[1])).collect(),
                    miss_br_list.into_iter().map(|v| (v[0], v[1])).collect(),
                )
            } else {
                (Vec::new(), Vec::new())
            };

            let summary_dict = file_dict.get_item("summary")?.unwrap();
            let summary_dict: &Bound<PyDict> = summary_dict.cast()?;

            let summary = FileSummary {
                covered_lines: summary_dict.get_item("covered_lines")?.unwrap().extract()?,
                missing_lines: summary_dict.get_item("missing_lines")?.unwrap().extract()?,
                covered_branches: summary_dict
                    .get_item("covered_branches")?
                    .map(|v| v.extract())
                    .transpose()?,
                missing_branches: summary_dict
                    .get_item("missing_branches")?
                    .map(|v| v.extract())
                    .transpose()?,
                percent_covered: summary_dict
                    .get_item("percent_covered")?
                    .unwrap()
                    .extract()?,
            };

            files_map.insert(
                filename,
                FileData {
                    coverage: FileCoverageData {
                        executed_lines,
                        missing_lines,
                        executed_branches,
                        missing_branches,
                    },
                    summary,
                },
            );
        }

        // Extract summary
        let summary_dict = dict.get_item("summary")?.ok_or_else(|| {
            PyErr::new::<pyo3::exceptions::PyKeyError, _>("Missing 'summary' key")
        })?;
        let summary_dict: &Bound<PyDict> = summary_dict.cast()?;

        let summary = FileSummary {
            covered_lines: summary_dict.get_item("covered_lines")?.unwrap().extract()?,
            missing_lines: summary_dict.get_item("missing_lines")?.unwrap().extract()?,
            covered_branches: summary_dict
                .get_item("covered_branches")?
                .map(|v| v.extract())
                .transpose()?,
            missing_branches: summary_dict
                .get_item("missing_branches")?
                .map(|v| v.extract())
                .transpose()?,
            percent_covered: summary_dict
                .get_item("percent_covered")?
                .unwrap()
                .extract()?,
        };

        Ok(CoverageData {
            meta,
            files: files_map,
            summary,
        })
    }

    /// Convert CoverageData to a Python dictionary
    fn to_dict(&self, py: Python) -> PyResult<Py<PyDict>> {
        let cov = PyDict::new(py);

        // Add meta
        let meta_dict = PyDict::new(py);
        meta_dict.set_item("software", &self.meta.software)?;
        meta_dict.set_item("version", &self.meta.version)?;
        meta_dict.set_item("timestamp", &self.meta.timestamp)?;
        meta_dict.set_item("branch_coverage", self.meta.branch_coverage)?;
        meta_dict.set_item("show_contexts", self.meta.show_contexts)?;
        cov.set_item("meta", meta_dict)?;

        // Add files
        let files_dict = PyDict::new(py);
        for (filename, file_data) in &self.files {
            let file_dict = PyDict::new(py);

            file_dict.set_item(
                "executed_lines",
                PyList::new(py, &file_data.coverage.executed_lines)?,
            )?;
            file_dict.set_item(
                "missing_lines",
                PyList::new(py, &file_data.coverage.missing_lines)?,
            )?;

            if self.meta.branch_coverage {
                let exec_br_list = PyList::empty(py);
                for (from_line, to_line) in &file_data.coverage.executed_branches {
                    exec_br_list.append(PyTuple::new(py, [from_line, to_line])?)?;
                }

                let miss_br_list = PyList::empty(py);
                for (from_line, to_line) in &file_data.coverage.missing_branches {
                    miss_br_list.append(PyTuple::new(py, [from_line, to_line])?)?;
                }

                file_dict.set_item("executed_branches", exec_br_list)?;
                file_dict.set_item("missing_branches", miss_br_list)?;
            }

            // Add file summary
            let summary_dict = PyDict::new(py);
            summary_dict.set_item("covered_lines", file_data.summary.covered_lines)?;
            summary_dict.set_item("missing_lines", file_data.summary.missing_lines)?;
            if let Some(cb) = file_data.summary.covered_branches {
                summary_dict.set_item("covered_branches", cb)?;
            }
            if let Some(mb) = file_data.summary.missing_branches {
                summary_dict.set_item("missing_branches", mb)?;
            }
            summary_dict.set_item("percent_covered", file_data.summary.percent_covered)?;
            file_dict.set_item("summary", summary_dict)?;

            files_dict.set_item(filename, file_dict)?;
        }
        cov.set_item("files", files_dict)?;

        // Add global summary
        let summary_dict = PyDict::new(py);
        summary_dict.set_item("covered_lines", self.summary.covered_lines)?;
        summary_dict.set_item("missing_lines", self.summary.missing_lines)?;
        if let Some(cb) = self.summary.covered_branches {
            summary_dict.set_item("covered_branches", cb)?;
        }
        if let Some(mb) = self.summary.missing_branches {
            summary_dict.set_item("missing_branches", mb)?;
        }
        summary_dict.set_item("percent_covered", self.summary.percent_covered)?;
        summary_dict.set_item(
            "percent_covered_display",
            format!("{}", self.summary.percent_covered.round() as i32),
        )?;
        cov.set_item("summary", summary_dict)?;

        Ok(cov.into())
    }

    /// Check if a key exists in the coverage data
    fn __contains__(&self, key: &str) -> bool {
        matches!(key, "meta" | "files" | "summary")
    }

    /// Get the metadata as a Python dictionary
    fn __getitem__(&self, py: Python, key: &str) -> PyResult<Py<PyAny>> {
        match key {
            "meta" => {
                let meta_dict = PyDict::new(py);
                meta_dict.set_item("software", &self.meta.software)?;
                meta_dict.set_item("version", &self.meta.version)?;
                meta_dict.set_item("timestamp", &self.meta.timestamp)?;
                meta_dict.set_item("branch_coverage", self.meta.branch_coverage)?;
                meta_dict.set_item("show_contexts", self.meta.show_contexts)?;
                Ok(meta_dict.into())
            }
            "files" => {
                let files_dict = PyDict::new(py);
                for (filename, file_data) in &self.files {
                    let file_dict = PyDict::new(py);

                    file_dict.set_item(
                        "executed_lines",
                        PyList::new(py, &file_data.coverage.executed_lines)?,
                    )?;
                    file_dict.set_item(
                        "missing_lines",
                        PyList::new(py, &file_data.coverage.missing_lines)?,
                    )?;

                    if self.meta.branch_coverage {
                        let exec_br_list = PyList::empty(py);
                        for (from_line, to_line) in &file_data.coverage.executed_branches {
                            exec_br_list.append(PyTuple::new(py, [from_line, to_line])?)?;
                        }

                        let miss_br_list = PyList::empty(py);
                        for (from_line, to_line) in &file_data.coverage.missing_branches {
                            miss_br_list.append(PyTuple::new(py, [from_line, to_line])?)?;
                        }

                        file_dict.set_item("executed_branches", exec_br_list)?;
                        file_dict.set_item("missing_branches", miss_br_list)?;
                    }

                    // Add file summary
                    let summary_dict = PyDict::new(py);
                    summary_dict.set_item("covered_lines", file_data.summary.covered_lines)?;
                    summary_dict.set_item("missing_lines", file_data.summary.missing_lines)?;
                    if let Some(cb) = file_data.summary.covered_branches {
                        summary_dict.set_item("covered_branches", cb)?;
                    }
                    if let Some(mb) = file_data.summary.missing_branches {
                        summary_dict.set_item("missing_branches", mb)?;
                    }
                    summary_dict.set_item("percent_covered", file_data.summary.percent_covered)?;
                    file_dict.set_item("summary", summary_dict)?;

                    files_dict.set_item(filename, file_dict)?;
                }
                Ok(files_dict.into())
            }
            "summary" => {
                let summary_dict = PyDict::new(py);
                summary_dict.set_item("covered_lines", self.summary.covered_lines)?;
                summary_dict.set_item("missing_lines", self.summary.missing_lines)?;
                if let Some(cb) = self.summary.covered_branches {
                    summary_dict.set_item("covered_branches", cb)?;
                }
                if let Some(mb) = self.summary.missing_branches {
                    summary_dict.set_item("missing_branches", mb)?;
                }
                summary_dict.set_item("percent_covered", self.summary.percent_covered)?;
                summary_dict.set_item(
                    "percent_covered_display",
                    format!("{}", self.summary.percent_covered.round() as i32),
                )?;
                Ok(summary_dict.into())
            }
            _ => Err(PyErr::new::<pyo3::exceptions::PyKeyError, _>(format!(
                "Key '{}' not found",
                key
            ))),
        }
    }
}
