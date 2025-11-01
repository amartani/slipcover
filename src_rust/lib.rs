use pyo3::prelude::*;
use pyo3::types::{PyDict, PyList, PySet, PyTuple, PyCFunction, PyModule as PyModuleType};
use pyo3::exceptions::PyAssertionError;
use pyo3::Py;
use std::collections::{HashMap, HashSet};
use std::sync::{Arc, Mutex};
use std::path::{Path, PathBuf};
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

        // Convert to Python dict
        let result = PyDict::new(py);
        for (filename, items) in old_newly_seen.iter() {
            let py_set = PySet::empty(py)?;
            for item in items {
                match item {
                    LineOrBranch::Line(line) => {
                        py_set.add(*line)?;
                    }
                    LineOrBranch::Branch(from_line, to_line) => {
                        let tuple = PyTuple::new(py, [*from_line, *to_line])?;
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

    /// Get coverage data for all files
    fn get_coverage_data(&self, py: Python, with_branches: bool) -> PyResult<Py<PyAny>> {
        let inner = self.inner.lock().unwrap();

        let files_dict = PyDict::new(py);

        for (filename, code_lines) in inner.code_lines.iter() {
            let file_dict = PyDict::new(py);

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

            file_dict.set_item("executed_lines", PyList::new(py, executed_lines)?)?;
            file_dict.set_item("missing_lines", PyList::new(py, missing_lines)?)?;

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
                let exec_br_list = PyList::empty(py);
                for (from_line, to_line) in executed_branches {
                    exec_br_list.append(PyTuple::new(py, [from_line, to_line])?)?;
                }

                let miss_br_list = PyList::empty(py);
                for (from_line, to_line) in missing_branches {
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

/// PathSimplifier - Simplifies file paths relative to current working directory
#[pyclass]
struct PathSimplifier {
    cwd: PathBuf,
}

#[pymethods]
impl PathSimplifier {
    #[new]
    fn new() -> PyResult<Self> {
        let cwd = std::env::current_dir()
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

/// Main Slipcover class
#[pyclass]
struct Slipcover {
    immediate: bool,
    branch: bool,
    source: Option<Vec<String>>,
    instrumented_code_ids: Arc<Mutex<HashSet<usize>>>,
    tracker: Py<CoverageTracker>,
    modules: Vec<Py<PyAny>>,
}

#[pymethods]
impl Slipcover {
    #[new]
    #[pyo3(signature = (immediate=false, branch=false, source=None))]
    fn new(
        py: Python,
        immediate: bool,
        branch: bool,
        source: Option<Vec<String>>,
    ) -> PyResult<Py<Self>> {
        let tracker = Py::new(py, CoverageTracker::new())?;
        let instrumented_code_ids = Arc::new(Mutex::new(HashSet::new()));

        let slf = Py::new(
            py,
            Slipcover {
                immediate,
                branch,
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

        if current_tool.is_none() || current_tool.extract::<String>().ok() != Some("SlipCover".to_string()) {
            monitoring.call_method1("use_tool_id", (&coverage_id, "SlipCover"))?;
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
                    let builtins = PyModule::import(py, "builtins")?;
                    let id_fn = builtins.getattr("id")?;
                    let code_id: usize = id_fn.call1((&code,))?.extract()?;

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
        let builtins = PyModule::import(py, "builtins")?;
        let id_fn = builtins.getattr("id")?;
        let code_id: usize = id_fn.call1((code_bound,))?.extract()?;

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

        // Get coverage data from tracker
        let files_data = tracker.get_coverage_data(py, self.branch)?;
        let files_dict_raw: &Bound<PyDict> = files_data.bind(py).cast()?;

        // Simplify file paths
        let files_dict = PyDict::new(py);
        for (key, value) in files_dict_raw.iter() {
            let path: String = key.extract()?;
            let simplified = simp.simplify(path);
            files_dict.set_item(simplified, value)?;
        }

        // Create meta dict
        let meta = Self::_make_meta(py, self.branch)?;

        // Create coverage dict
        let cov = PyDict::new(py);
        cov.set_item("meta", meta)?;
        cov.set_item("files", files_dict)?;

        // Add summaries
        let slipcover_module = PyModule::import(py, "slipcover.slipcover")?;
        let add_summaries = slipcover_module.getattr("add_summaries")?;
        add_summaries.call1((&cov,))?;

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
    fn find_functions(py: Python, items: Vec<Py<PyAny>>, visited: Py<PySet>) -> PyResult<Vec<Py<PyAny>>> {
        // Import types module
        let types_module = PyModule::import(py, "types")?;
        let function_type = types_module.getattr("FunctionType")?;
        let code_type = types_module.getattr("CodeType")?;

        let visited_set = visited.bind(py);
        let mut results = Vec::new();

        for item in items {
            Self::find_funcs_recursive(py, item, visited_set, &mut results, &function_type, &code_type)?;
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

        // Import the print_coverage function
        let slipcover_module = PyModule::import(py, "slipcover.slipcover")?;
        let print_coverage_fn = slipcover_module.getattr("print_coverage")?;

        // Build kwargs
        let kwargs = PyDict::new(py);
        if let Some(of) = outfile {
            kwargs.set_item("outfile", of)?;
        } else {
            // Default to sys.stdout
            let sys_module = PyModule::import(py, "sys")?;
            let stdout = sys_module.getattr("stdout")?;
            kwargs.set_item("outfile", stdout)?;
        }
        if let Some(mw) = missing_width {
            kwargs.set_item("missing_width", mw)?;
        }

        // Call the Python function
        print_coverage_fn.call((&cov,), Some(&kwargs))?;
        Ok(())
    }

    fn __str__(&self, _py: Python) -> PyResult<String> {
        Ok(format!("Slipcover(branch={}, immediate={})", self.branch, self.immediate))
    }
}

// Helper methods implementation
impl Slipcover {
    fn _make_meta(py: Python, branch_coverage: bool) -> PyResult<Py<PyDict>> {
        let datetime_module = PyModule::import(py, "datetime")?;
        let datetime_class = datetime_module.getattr("datetime")?;
        let now = datetime_class.call_method0("now")?;
        let timestamp: String = now.call_method0("isoformat")?.extract()?;

        let version_module = PyModule::import(py, "slipcover.version")?;
        let version: String = version_module.getattr("__version__")?.extract()?;

        let meta = PyDict::new(py);
        meta.set_item("software", "slipcover")?;
        meta.set_item("version", version)?;
        meta.set_item("timestamp", timestamp)?;
        meta.set_item("branch_coverage", branch_coverage)?;
        meta.set_item("show_contexts", false)?;

        Ok(meta.into())
    }

    fn _add_unseen_source_files_internal(&self, py: Python, source: Vec<String>) -> PyResult<()> {
        let ast_module = PyModule::import(py, "ast")?;
        let pathlib_module = PyModule::import(py, "pathlib")?;
        let path_class = pathlib_module.getattr("Path")?;

        let mut dirs: Vec<Py<PyAny>> = Vec::new();
        for d in source {
            let p = path_class.call1((d,))?;
            let resolved = p.call_method0("resolve")?;
            dirs.push(resolved.into());
        }

        let tracker = self.tracker.bind(py).borrow();

        while let Some(p) = dirs.pop() {
            let p_bound = p.bind(py);
            let iter_dir = p_bound.call_method0("iterdir")?;

            for file_result in iter_dir.try_iter()? {
                let file = file_result?;

                if file.call_method0("is_dir")?.extract::<bool>()? {
                    dirs.push(file.into());
                } else if file.call_method0("is_file")?.extract::<bool>()? {
                    let suffix: String = file.call_method0("suffix")?.extract()?;
                    if suffix.to_lowercase() == ".py" {
                        let absolute = file.call_method0("absolute")?;
                        let filename: String = absolute.str()?.extract()?;

                        // Check if file has been instrumented
                        if !tracker.has_file(filename.clone()) {
                            // Try to parse and compile
                            match self._try_add_file(py, &file, &filename, &ast_module, &tracker) {
                                Ok(_) => {},
                                Err(e) => {
                                    println!("Warning: unable to include {}: {}", filename, e);
                                }
                            }
                        }
                    }
                }
            }
        }

        Ok(())
    }

    fn _try_add_file(
        &self,
        py: Python,
        file: &Bound<PyAny>,
        filename: &str,
        ast_module: &Bound<PyModuleType>,
        tracker: &CoverageTracker,
    ) -> PyResult<()> {
        let content: String = file.call_method0("read_text")?.extract()?;
        let mut tree = ast_module.call_method1("parse", (content,))?;

        // If branch coverage, preinstrument
        if self.branch {
            let branch_module = PyModule::import(py, "slipcover.branch")?;
            let preinstrument = branch_module.getattr("preinstrument")?;
            tree = preinstrument.call1((tree,))?;
        }

        // Compile
        let builtins = PyModule::import(py, "builtins")?;
        let compile_fn = builtins.getattr("compile")?;
        let code = compile_fn.call1((tree, filename, "exec"))?;

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
        visited: &Bound<PySet>,
        results: &mut Vec<Py<PyAny>>,
        function_type: &Bound<PyAny>,
        code_type: &Bound<PyAny>,
    ) -> PyResult<()> {
        let root_bound = root.bind(py);
        let root_type = root_bound.get_type();

        // Check if it's a patchable function
        if root_type.is_subclass(function_type)? {
            let code_obj = root_bound.getattr("__code__")?;
            let code_obj_type = code_obj.get_type();

            if code_obj_type.is(code_type) {
                if !visited.contains(&root)? {
                    visited.add(&root)?;
                    results.push(root.clone_ref(py));
                }
                return Ok(());
            }
        }

        // Check if it's a type/class
        let type_type = py.get_type::<pyo3::types::PyType>();
        if root_type.is_subclass(&type_type)? {
            if !visited.contains(&root)? {
                visited.add(&root)?;

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
                        let base_visited = visited.contains(&base)?;

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

                if func_code_type.is(code_type)
                    && !visited.contains(&func)? {
                    visited.add(&func)?;
                    let func_py: Py<PyAny> = func.into();
                    results.push(func_py);
                }
            }
        }

        Ok(())
    }
}

/// Module definition
#[pymodule]
fn slipcover_core(_py: Python, m: &Bound<PyModule>) -> PyResult<()> {
    m.add_function(wrap_pyfunction!(is_branch, m)?)?;
    m.add_function(wrap_pyfunction!(encode_branch, m)?)?;
    m.add_function(wrap_pyfunction!(decode_branch, m)?)?;
    m.add_function(wrap_pyfunction!(lines_from_code, m)?)?;
    m.add_function(wrap_pyfunction!(branches_from_code, m)?)?;
    m.add_class::<CoverageTracker>()?;
    m.add_class::<PathSimplifier>()?;
    m.add_class::<Slipcover>()?;
    Ok(())
}
