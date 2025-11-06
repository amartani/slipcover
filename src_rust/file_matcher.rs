// File matcher for determining which files to instrument
// Rust implementation of the FileMatcher class from importer.py

use pyo3::exceptions::PyOSError;
use pyo3::prelude::*;
use std::path::{Path, PathBuf};

/// FileMatcher determines which files should be instrumented for coverage
#[pyclass]
pub struct FileMatcher {
    cwd: PathBuf,
    sources: Vec<PathBuf>,
    omit: Vec<String>,
    pylib_paths: Vec<PathBuf>,
}

#[pymethods]
impl FileMatcher {
    #[new]
    fn new(py: Python) -> PyResult<Self> {
        // Get current working directory
        let cwd = std::env::current_dir()
            .map_err(|e| PyOSError::new_err(format!("Failed to get current directory: {}", e)))?;

        // Get Python library paths using sysconfig
        let sysconfig = py.import("sysconfig")?;

        let stdlib_path = sysconfig
            .call_method1("get_path", ("stdlib",))?
            .extract::<String>()?;

        let purelib_path = sysconfig
            .call_method1("get_path", ("purelib",))?
            .extract::<String>()?;

        // Canonicalize and simplify Python library paths
        let pylib_paths = vec![
            PathBuf::from(stdlib_path)
                .canonicalize()
                .map(|p| dunce::simplified(&p).to_path_buf())
                .unwrap_or_default(),
            PathBuf::from(purelib_path)
                .canonicalize()
                .map(|p| dunce::simplified(&p).to_path_buf())
                .unwrap_or_default(),
        ];

        // Canonicalize and simplify cwd (remove Windows UNC prefixes)
        let cwd = match cwd.canonicalize() {
            Ok(canonical) => dunce::simplified(&canonical).to_path_buf(),
            Err(_) => cwd,
        };

        Ok(FileMatcher {
            cwd,
            sources: Vec::new(),
            omit: Vec::new(),
            pylib_paths,
        })
    }

    /// Add a source directory to match files against
    #[pyo3(name = "addSource")]
    fn add_source(&mut self, source: Bound<PyAny>) -> PyResult<()> {
        // Try to extract as string first
        let source_str = if let Ok(s) = source.extract::<String>() {
            s
        } else {
            // Try to get the path from a Path-like object (__fspath__ method)
            if let Ok(path_obj) = source.call_method0("__fspath__") {
                path_obj.extract::<String>()?
            } else {
                // Try to convert to string
                source.str()?.extract::<String>()?
            }
        };

        let path = PathBuf::from(source_str);
        let resolved = self.resolve_path(&path);
        self.sources.push(resolved);
        Ok(())
    }

    /// Add an omit pattern (glob pattern)
    #[pyo3(name = "addOmit")]
    fn add_omit(&mut self, omit: &str) -> PyResult<()> {
        let pattern = if omit.starts_with('*') {
            // It's a glob pattern, use as-is
            omit.to_string()
        } else {
            // It's a relative path, resolve it relative to cwd
            let path = self.cwd.join(omit);
            path.to_string_lossy().to_string()
        };
        self.omit.push(pattern);
        Ok(())
    }

    /// Check if a file matches the criteria for instrumentation
    fn matches(&self, filename: Option<Bound<PyAny>>) -> PyResult<bool> {
        // Handle None
        let filename = match filename {
            None => return Ok(false),
            Some(f) => f,
        };

        // Check if it's a string
        if let Ok(s) = filename.extract::<String>() {
            // Special case: "built-in" can't be instrumented
            if s == "built-in" {
                return Ok(false);
            }

            // Convert to PathBuf
            let path = PathBuf::from(s);
            return self.matches_path(&path);
        }

        // Try to get the path from a Path-like object (__fspath__ method)
        if let Ok(path_obj) = filename.call_method0("__fspath__")
            && let Ok(path_str) = path_obj.extract::<String>()
        {
            let path = PathBuf::from(path_str);
            return self.matches_path(&path);
        }

        // If we can't extract a path, just return false
        Ok(false)
    }
}

impl FileMatcher {
    /// Normalize a path by removing "." and ".." components
    /// This mimics Python's Path.resolve() behavior without requiring the path to exist
    fn normalize_path(path: &Path) -> PathBuf {
        use std::path::Component;

        let mut components = Vec::new();
        for component in path.components() {
            match component {
                Component::CurDir => {} // Skip "."
                Component::ParentDir => {
                    // Go up one level, but don't pop past prefix/root
                    // Only pop if the last component is a Normal path segment
                    if let Some(&Component::Normal(_)) = components.last() {
                        components.pop();
                    }
                }
                // Keep prefix, root, and normal components
                comp => components.push(comp),
            }
        }
        components.iter().collect()
    }

    /// Resolve a path to an absolute, normalized path
    /// Like Python's Path.resolve() - doesn't require the path to exist
    fn resolve_path(&self, path: &Path) -> PathBuf {
        // First try to canonicalize (handles symlinks and confirms existence)
        if let Ok(canonical) = path.canonicalize() {
            // Use dunce to simplify Windows UNC paths
            return dunce::simplified(&canonical).to_path_buf();
        }

        // Path doesn't exist, so manually resolve it
        let absolute = if path.is_absolute() {
            path.to_path_buf()
        } else {
            self.cwd.join(path)
        };

        // Normalize by removing ".." and "." components
        let normalized = Self::normalize_path(&absolute);

        // Use dunce to simplify Windows UNC paths
        dunce::simplified(&normalized).to_path_buf()
    }

    /// Internal method to check if a path matches
    fn matches_path(&self, path: &Path) -> PyResult<bool> {
        // Check for DLL/shared library extensions
        if let Some(ext) = path.extension()
            && (ext == "pyd" || ext == "so")
        {
            return Ok(false); // Can't instrument DLLs
        }

        // Resolve the path
        let resolved = self.resolve_path(path);

        // Check omit patterns
        if !self.omit.is_empty() {
            let path_str = resolved.to_string_lossy();
            for pattern in &self.omit {
                if Self::fnmatch(&path_str, pattern) {
                    return Ok(false);
                }
            }
        }

        // If sources are specified, check if file is relative to any source
        if !self.sources.is_empty() {
            for source in &self.sources {
                if Self::is_relative_to(&resolved, source) {
                    return Ok(true);
                }
            }
            return Ok(false);
        }

        // Check if file is in Python library paths (stdlib or site-packages)
        for pylib_path in &self.pylib_paths {
            if Self::is_relative_to(&resolved, pylib_path) {
                return Ok(false);
            }
        }

        // Otherwise, check if file is relative to cwd
        Ok(Self::is_relative_to(&resolved, &self.cwd))
    }

    /// Check if path is relative to base (like Path.is_relative_to in Python 3.9+)
    fn is_relative_to(path: &Path, base: &Path) -> bool {
        path.starts_with(base)
    }

    /// Simple fnmatch implementation for glob patterns
    /// This is a basic implementation that handles the patterns used in the tests
    fn fnmatch(path: &str, pattern: &str) -> bool {
        // Use the glob crate's pattern matching
        if let Ok(glob_pattern) = glob::Pattern::new(pattern) {
            return glob_pattern.matches(path);
        }
        false
    }
}
