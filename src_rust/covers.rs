// Main Covers class
// Based on the original Python covers.py module

use crate::code_analysis::{branches_from_code, lines_from_code};
use crate::path::PathSimplifier;
use crate::reporting::{add_summaries_native, print_coverage};
use crate::schemas::{CoverageData, FileCoverageData, FileData, MetaData};
use crate::tracker::CoverageTracker;
use ahash::AHashMap;
use ahash::AHashSet;
use chrono::SecondsFormat;
use chrono::prelude::*;
use pyo3::exceptions::{PyIOError, PyOSError};
use pyo3::prelude::*;
use pyo3::types::{PyAny, PyCFunction, PyCode, PyCodeInput, PyDict, PyModule, PySet, PyTuple};
use std::ffi::CString;
use std::path::{Path, PathBuf};
use std::sync::{Arc, Mutex};

/// Version constant (from Cargo.toml)
pub const VERSION: &str = env!("CARGO_PKG_VERSION");

/// Main Covers class
#[pyclass]
pub struct Covers {
    immediate: bool,
    // Reserved for future de-instrumentation feature (exposed via getter for API compatibility)
    #[allow(dead_code)]
    d_miss_threshold: i32,
    branch: bool,
    // Reserved for future disassembly feature (exposed via getter for API compatibility)
    #[allow(dead_code)]
    disassemble: bool,
    source: Option<Vec<String>>,
    instrumented_code_ids: Arc<Mutex<AHashSet<usize>>>,
    tracker: Arc<CoverageTracker>,
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
        let tracker = Arc::new(CoverageTracker::new());
        let instrumented_code_ids = Arc::new(Mutex::new(AHashSet::new()));

        let slf = Py::new(
            py,
            Covers {
                immediate,
                d_miss_threshold,
                branch,
                disassemble,
                source,
                instrumented_code_ids: instrumented_code_ids.clone(),
                tracker: tracker.clone(),
                modules: Vec::new(),
            },
        )?;

        // Set up sys.monitoring callback
        let sys_module = PyModule::import(py, "sys")?;
        let monitoring = sys_module.getattr("monitoring")?;

        // Check if tool is already registered
        let coverage_id = monitoring.getattr("COVERAGE_ID")?;
        let current_tool = monitoring.call_method1("get_tool", (&coverage_id,))?;

        if current_tool.is_none()
            || current_tool.extract::<String>().ok() != Some("Covers".to_string())
        {
            monitoring.call_method1("use_tool_id", (&coverage_id, "Covers"))?;
        }

        // Create the handle_line callback
        let tracker_ref = tracker.clone();
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
                        let ids = ids_ref
                            .lock()
                            .expect("instrumented_code_ids mutex poisoned");
                        if !ids.contains(&code_id) {
                            // Return DISABLE constant
                            let sys_module = PyModule::import(py, "sys")?;
                            let monitoring = sys_module.getattr("monitoring")?;
                            return Ok(monitoring.getattr("DISABLE")?.into());
                        }
                    }

                    // Call tracker.handle_line directly (no Python overhead)
                    let filename: String = code.getattr("co_filename")?.extract()?;
                    tracker_ref.handle_line(filename, line);

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
        monitoring.call_method1(
            "register_callback",
            (&coverage_id, &line_event, handle_line),
        )?;

        Ok(slf)
    }

    #[pyo3(signature = (co, parent=None))]
    fn instrument(
        &mut self,
        py: Python,
        co: Py<PyAny>,
        parent: Option<Py<PyAny>>,
    ) -> PyResult<Py<PyAny>> {
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
            let mut ids = self
                .instrumented_code_ids
                .lock()
                .expect("instrumented_code_ids mutex poisoned");
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

            self.tracker.add_code_lines(filename.clone(), lines);
            if !branches.is_empty() {
                self.tracker.add_code_branches(filename, branches);
            }
        }

        Ok(code_obj)
    }

    fn get_coverage(&mut self, _py: Python) -> PyResult<CoverageData> {
        // Merge newly seen into all_seen
        self.tracker.merge_newly_seen();

        // Add unseen source files if source is specified
        if let Some(ref source_paths) = self.source {
            self._add_unseen_source_files_internal(_py, source_paths.clone())?;
        }

        // Simplify paths
        let simp = PathSimplifier::new()?;

        // Get coverage data from tracker using native structures
        let files_data = self.tracker.get_coverage_data_native(self.branch);

        // Simplify file paths
        let mut simplified_files_data: AHashMap<String, FileCoverageData> = AHashMap::new();
        for (path, file_data) in files_data {
            let simplified = simp.simplify(path);
            simplified_files_data.insert(simplified, file_data);
        }

        // Add summaries using native Rust structures
        let (file_summaries, global_summary) =
            add_summaries_native(&mut simplified_files_data, self.branch);

        // Create meta using native structures
        let meta_data = Self::_make_meta_native(self.branch);

        // Build the files map with FileData
        let mut files_map: AHashMap<String, FileData> = AHashMap::new();
        for (filename, coverage) in simplified_files_data {
            let summary = file_summaries.get(&filename).unwrap().clone();
            files_map.insert(filename, FileData { coverage, summary });
        }

        // Create and return the CoverageData struct
        Ok(CoverageData {
            meta: meta_data,
            files: files_map,
            summary: global_summary,
        })
    }

    fn signal_child_process(&mut self, py: Python) -> PyResult<()> {
        self.source = None;
        self.tracker.get_newly_seen(py)?;
        self.tracker.clear_all_seen();
        Ok(())
    }

    #[staticmethod]
    fn find_functions(
        py: Python,
        items: Py<PyAny>,
        visited: Py<PySet>,
    ) -> PyResult<Vec<Py<PyAny>>> {
        // Import types module
        let types_module = PyModule::import(py, "types")?;
        let function_type = types_module.getattr("FunctionType")?;
        let code_type = types_module.getattr("CodeType")?;

        let visited_set = visited.bind(py);
        let mut results = Vec::new();

        // Use native Rust AHashSet for faster lookups (store Python object pointer addresses)
        let mut visited_native = AHashSet::new();

        // Populate native set with existing visited items from Python set
        for item in visited_set.iter() {
            visited_native.insert(item.as_ptr() as usize);
        }

        // Convert to list first to handle dict_values and other iterables
        // Use native iteration instead of calling builtins.list
        let items_list: Vec<Py<PyAny>> = items
            .bind(py)
            .try_iter()?
            .map(|item| item.map(|i| i.unbind()))
            .collect::<PyResult<Vec<_>>>()?;

        for item in items_list {
            Self::find_funcs_recursive(
                py,
                item,
                &mut visited_native,
                &mut results,
                &function_type,
                &code_type,
            )?;
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

    /// Add code branches for a file (for pytest integration)
    fn add_code_branches(
        &self,
        _py: Python,
        filename: String,
        branches: Vec<(i32, i32)>,
    ) -> PyResult<()> {
        self.tracker.add_code_branches(filename, branches);
        Ok(())
    }

    #[pyo3(signature = (outfile=None, missing_width=None))]
    fn print_coverage(
        &mut self,
        py: Python,
        outfile: Option<Py<PyAny>>,
        missing_width: Option<usize>,
    ) -> PyResult<()> {
        // Get coverage first
        let cov = self.get_coverage(py)?;

        // Call the Rust print_coverage function
        print_coverage(py, &cov, outfile, missing_width, false)?;
        Ok(())
    }

    fn __str__(&self, _py: Python) -> PyResult<String> {
        Ok(format!(
            "Covers(branch={}, immediate={})",
            self.branch, self.immediate
        ))
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

    fn _add_unseen_source_files_internal(&self, py: Python, source: Vec<String>) -> PyResult<()> {
        let mut dirs: Vec<PathBuf> = Vec::new();
        for d in source {
            let p = PathBuf::from(d);
            match dunce::canonicalize(&p) {
                Ok(resolved) => dirs.push(resolved),
                Err(e) => {
                    return Err(PyOSError::new_err(format!(
                        "Failed to resolve path {:?}: {}",
                        p, e
                    )));
                }
            }
        }

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
                } else if path.is_file()
                    && let Some(ext) = path.extension()
                    && ext.to_string_lossy().to_lowercase() == "py"
                {
                    let filename = path.to_string_lossy().to_string();

                    // Check if file has been instrumented
                    if !self.tracker.has_file(filename.clone()) {
                        // Try to parse and compile
                        match self._try_add_file_from_path(py, &path, &filename) {
                            Ok(_) => {}
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

    fn _try_add_file_from_path(&self, py: Python, path: &Path, filename: &str) -> PyResult<()> {
        let content = std::fs::read_to_string(path)
            .map_err(|e| PyIOError::new_err(format!("Failed to read file {}: {}", filename, e)))?;

        // Compile the source using PyCode::compile (safe PyO3 API)
        let c_content = CString::new(content.as_str())
            .map_err(|e| PyIOError::new_err(format!("Invalid source content: {}", e)))?;
        let c_filename = CString::new(filename)
            .map_err(|e| PyIOError::new_err(format!("Invalid filename: {}", e)))?;

        let code = PyCode::compile(py, &c_content, &c_filename, PyCodeInput::File)?;

        // Extract lines using lines_from_code
        let lines = lines_from_code(py, &code)?;
        if !lines.is_empty() {
            self.tracker.add_code_lines(filename.to_string(), lines);
        }

        // For branches, use tree-sitter analysis directly instead of bytecode analysis
        if self.branch {
            use crate::branch::analyze_branches_ts_impl;

            // Use tree-sitter to analyze branches (using native Rust types)
            let branch_data = analyze_branches_ts_impl(&content)
                .map_err(|e| PyIOError::new_err(format!("Failed to analyze branches: {}", e)))?;

            // Extract branches from the tree-sitter analysis result
            let mut branches = Vec::new();
            for (branch_line, markers) in branch_data {
                for (_insert_line, to_line) in markers {
                    // Add the branch (from branch_line to to_line)
                    // Skip branches where to_line is 0 (these are inserted into orelse and handled separately)
                    if to_line != 0 {
                        branches.push((branch_line as i32, to_line as i32));
                    }
                }
            }

            if !branches.is_empty() {
                self.tracker
                    .add_code_branches(filename.to_string(), branches);
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

                // Get dir() of the object using native PyO3 method
                let obj_names: Vec<String> = root_bound.dir()?.extract()?;

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
                                Self::find_funcs_recursive(
                                    py,
                                    item.into(),
                                    visited,
                                    results,
                                    function_type,
                                    code_type,
                                )?;
                                break;
                            }
                        }
                    }
                }
            }
            return Ok(());
        }

        // Check if it's a classmethod or staticmethod
        let classmethod_type = py
            .get_type::<pyo3::types::PyType>()
            .call1(("classmethod",))?;
        let staticmethod_type = py
            .get_type::<pyo3::types::PyType>()
            .call1(("staticmethod",))?;

        if (root_type.is_subclass(&classmethod_type)?
            || root_type.is_subclass(&staticmethod_type)?)
            && let Ok(func) = root_bound.getattr("__func__")
        {
            let func_type = func.get_type();
            if func_type.is_subclass(function_type)? {
                let func_code = func.getattr("__code__")?;
                let func_code_type = func_code.get_type();
                let func_ptr = func.as_ptr() as usize;

                if func_code_type.is(code_type) && !visited.contains(&func_ptr) {
                    visited.insert(func_ptr);
                    let func_py: Py<PyAny> = func.into();
                    results.push(func_py);
                }
            }
        }

        Ok(())
    }
}
