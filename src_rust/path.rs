// Path simplification utilities
// Part of the original Python covers.py module

use pyo3::exceptions::PyOSError;
use pyo3::prelude::*;
use std::path::{Path, PathBuf};

/// PathSimplifier - Simplifies file paths relative to current working directory
#[pyclass]
pub struct PathSimplifier {
    cwd: PathBuf,
}

#[pymethods]
impl PathSimplifier {
    #[new]
    pub fn new() -> PyResult<Self> {
        let cwd = dunce::canonicalize(std::env::current_dir()?)
            .map_err(|e| PyOSError::new_err(format!("Failed to get cwd: {}", e)))?;
        Ok(PathSimplifier { cwd })
    }

    pub fn simplify(&self, path: String) -> String {
        let p = Path::new(&path);
        match p.strip_prefix(&self.cwd) {
            Ok(relative) => relative.to_string_lossy().to_string(),
            Err(_) => path,
        }
    }
}
