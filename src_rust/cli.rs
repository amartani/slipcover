// CLI argument parsing and execution logic using clap
// This module provides Rust implementations of CLI functionality

use clap::Parser;
use pyo3::prelude::*;
use pyo3::types::{IntoPyDict, PyDict, PyList, PyModule};
use std::fs::File;
use std::io::BufReader;
use std::path::{Path, PathBuf};

// Import coverage reporting functions
use crate::lcovreport::print_lcov;
use crate::reporting::{merge_coverage, print_coverage};
use crate::xmlreport::print_xml;

const VERSION: &str = env!("CARGO_PKG_VERSION");

#[derive(Parser, Debug)]
#[command(name = "Covers")]
#[command(version = VERSION)]
#[command(about = "Near Zero-Overhead Python Code Coverage", long_about = None)]
struct Cli {
    /// Measure both branch and line coverage
    #[arg(long)]
    branch: bool,

    /// Select JSON output
    #[arg(long)]
    json: bool,

    /// Pretty-print JSON output
    #[arg(long)]
    pretty_print: bool,

    /// Select XML output
    #[arg(long)]
    xml: bool,

    /// Select LCOV output
    #[arg(long)]
    lcov: bool,

    /// Controls which directories are identified as packages in XML reports
    #[arg(long, default_value = "99")]
    xml_package_depth: i32,

    /// Specify output file name
    #[arg(long)]
    out: Option<String>,

    /// Specify directories to cover (comma-separated)
    #[arg(long)]
    source: Option<String>,

    /// Specify file(s) to omit (comma-separated)
    #[arg(long)]
    omit: Option<String>,

    /// Request immediate de-instrumentation
    #[arg(long)]
    immediate: bool,

    /// Omit fully covered files from text output
    #[arg(long)]
    skip_covered: bool,

    /// Fail execution with RC 2 if overall coverage is below this percentage
    #[arg(long, default_value = "0.0")]
    fail_under: f64,

    /// Threshold for de-instrumentation (if not immediate)
    #[arg(long, default_value = "50")]
    threshold: i32,

    /// Maximum width for 'missing' column
    #[arg(long, default_value = "80")]
    missing_width: i32,

    /// Silent mode (no output)
    #[arg(long, hide = true)]
    silent: bool,

    /// Disassemble mode (for development)
    #[arg(long, hide = true)]
    dis: bool,

    /// Debug mode (for development)
    #[arg(long, hide = true)]
    debug: bool,

    /// Don't wrap pytest (for development)
    #[arg(long, hide = true)]
    dont_wrap_pytest: bool,

    /// Run given module as __main__
    #[arg(
        short = 'm',
        num_args = 1,
        conflicts_with = "script",
        conflicts_with = "merge"
    )]
    module: Option<Vec<String>>,

    /// Merge JSON coverage files, saving to --out
    #[arg(long, num_args = 1.., conflicts_with = "script", conflicts_with = "module")]
    merge: Option<Vec<String>>,

    /// The script to run
    #[arg(
        value_name = "SCRIPT",
        conflicts_with = "merge",
        conflicts_with = "module"
    )]
    script: Option<String>,

    /// Arguments for the script or module
    #[arg(
        value_name = "ARGS",
        trailing_var_arg = true,
        allow_hyphen_values = true
    )]
    script_or_module_args: Vec<String>,
}

impl Cli {
    /// Convert CLI arguments to a Python dictionary
    fn to_pydict<'py>(&self, py: Python<'py>) -> PyResult<Bound<'py, PyDict>> {
        let dict = PyDict::new(py);

        dict.set_item("branch", self.branch)?;
        dict.set_item("json", self.json)?;
        dict.set_item("pretty_print", self.pretty_print)?;
        dict.set_item("xml", self.xml)?;
        dict.set_item("lcov", self.lcov)?;
        dict.set_item("xml_package_depth", self.xml_package_depth)?;
        dict.set_item("immediate", self.immediate)?;
        dict.set_item("skip_covered", self.skip_covered)?;
        dict.set_item("fail_under", self.fail_under)?;
        dict.set_item("threshold", self.threshold)?;
        dict.set_item("missing_width", self.missing_width)?;
        dict.set_item("silent", self.silent)?;
        dict.set_item("dis", self.dis)?;
        dict.set_item("debug", self.debug)?;
        dict.set_item("dont_wrap_pytest", self.dont_wrap_pytest)?;

        // Optional fields
        if let Some(ref out) = self.out {
            dict.set_item("out", out)?;
        }
        if let Some(ref source) = self.source {
            dict.set_item("source", source)?;
        }
        if let Some(ref omit) = self.omit {
            dict.set_item("omit", omit)?;
        }
        if let Some(ref module) = self.module {
            dict.set_item("module", module.clone())?;
        }
        if let Some(ref merge) = self.merge {
            dict.set_item("merge", merge.clone())?;
        }
        if let Some(ref script) = self.script {
            dict.set_item("script", script)?;
        }

        dict.set_item("script_or_module_args", self.script_or_module_args.clone())?;

        Ok(dict)
    }
}

/// Parse command-line arguments and run the coverage tool
/// This is the main entry point called from Python's __main__.py
#[pyfunction]
#[pyo3(signature = (argv))]
pub fn main_cli(py: Python, argv: Vec<String>) -> PyResult<i32> {
    // Special handling for -m flag to properly consume remaining args
    // This is needed because clap's trailing_var_arg doesn't work well with -m
    let (args_to_parse, module_args) = if let Some(m_pos) = argv.iter().position(|x| x == "-m") {
        if m_pos + 1 < argv.len() {
            // Split: everything up to and including the module name goes to clap,
            // everything after becomes script_or_module_args
            let split_point = m_pos + 2;
            let clap_args = argv[..split_point].to_vec();
            let remaining = argv[split_point..].to_vec();
            (clap_args, Some(remaining))
        } else {
            (argv, None)
        }
    } else {
        (argv, None)
    };

    // Parse arguments using clap
    let mut cli = match Cli::try_parse_from(args_to_parse) {
        Ok(cli) => cli,
        Err(e) => {
            // clap will print error/help message to stderr
            eprintln!("{}", e);
            return Ok(
                if e.kind() == clap::error::ErrorKind::DisplayHelp
                    || e.kind() == clap::error::ErrorKind::DisplayVersion
                {
                    0
                } else {
                    1
                },
            );
        }
    };

    // Override script_or_module_args if we split at -m
    if let Some(module_args) = module_args {
        cli.script_or_module_args = module_args;
    }

    // Check if this is a merge operation
    if cli.merge.is_some() {
        if cli.out.is_none() {
            return Err(pyo3::exceptions::PyValueError::new_err(
                "--out is required with --merge",
            ));
        }
        return merge_coverage_files(py, &cli);
    }

    // Validate that we have either script or module
    if cli.script.is_none() && cli.module.is_none() {
        eprintln!("error: Must specify either a script or -m module");
        return Ok(1);
    }

    // Otherwise, run with coverage
    run_with_coverage(py, &cli)
}

/// Parse command-line arguments into a Python dictionary
/// This provides compatibility with the previous interface
#[pyfunction]
#[pyo3(signature = (argv))]
pub fn parse_args(py: Python, argv: Vec<String>) -> PyResult<Bound<PyDict>> {
    // Use the same special handling for -m as in main_cli
    let (args_to_parse, module_args) = if let Some(m_pos) = argv.iter().position(|x| x == "-m") {
        if m_pos + 1 < argv.len() {
            let split_point = m_pos + 2;
            let clap_args = argv[..split_point].to_vec();
            let remaining = argv[split_point..].to_vec();
            (clap_args, Some(remaining))
        } else {
            (argv, None)
        }
    } else {
        (argv, None)
    };

    let mut cli = Cli::try_parse_from(args_to_parse).map_err(|e| {
        pyo3::exceptions::PyValueError::new_err(format!("Argument parsing error: {}", e))
    })?;

    if let Some(module_args) = module_args {
        cli.script_or_module_args = module_args;
    }

    cli.to_pydict(py)
}

fn merge_coverage_files(py: Python, cli: &Cli) -> PyResult<i32> {
    // Get merge files from cli
    let merge_files_list = cli
        .merge
        .as_ref()
        .ok_or_else(|| pyo3::exceptions::PyValueError::new_err("merge files not specified"))?;

    if merge_files_list.is_empty() {
        return Err(pyo3::exceptions::PyValueError::new_err(
            "no merge files specified",
        ));
    }

    // Convert string paths to Path objects
    let merge_files: Vec<PathBuf> = merge_files_list.iter().map(PathBuf::from).collect();

    let base_path = merge_files[0]
        .parent()
        .unwrap_or_else(|| Path::new("."))
        .to_string_lossy()
        .to_string();

    // Load the first file as the base coverage using native Rust file I/O and serde_json
    let first_file_str = merge_files[0].to_string_lossy().to_string();
    let first_file = File::open(&merge_files[0]).map_err(|e| {
        pyo3::exceptions::PyIOError::new_err(format!("Failed to open {}: {}", first_file_str, e))
    })?;
    let first_reader = BufReader::new(first_file);
    let first_json: serde_json::Value = serde_json::from_reader(first_reader).map_err(|e| {
        pyo3::exceptions::PyValueError::new_err(format!("Error parsing {}: {}", first_file_str, e))
    })?;

    // Convert serde_json::Value to Python dict using pythonize
    let merged_dict: Py<PyDict> = pythonize::pythonize(py, &first_json)
        .map_err(|e| {
            pyo3::exceptions::PyValueError::new_err(format!(
                "Error converting {} to Python dict: {}",
                first_file_str, e
            ))
        })?
        .extract()?;

    // Merge additional files
    for merge_file in merge_files.iter().skip(1) {
        let merge_file_str = merge_file.to_string_lossy().to_string();
        let file = File::open(merge_file).map_err(|e| {
            pyo3::exceptions::PyIOError::new_err(format!(
                "Failed to open {}: {}",
                merge_file_str, e
            ))
        })?;
        let reader = BufReader::new(file);
        let json_value: serde_json::Value = serde_json::from_reader(reader).map_err(|e| {
            pyo3::exceptions::PyValueError::new_err(format!(
                "Error parsing {}: {}",
                merge_file_str, e
            ))
        })?;

        // Convert to Python dict
        let coverage_dict: Py<PyDict> = pythonize::pythonize(py, &json_value)
            .map_err(|e| {
                pyo3::exceptions::PyValueError::new_err(format!(
                    "Error converting {} to Python dict: {}",
                    merge_file_str, e
                ))
            })?
            .extract()?;

        // Merge into merged_dict using the Rust merge_coverage function
        merge_coverage(py, merged_dict.bind(py), coverage_dict.bind(py))?;
    }

    // Get output file path
    let out_file_str = cli
        .out
        .as_ref()
        .ok_or_else(|| pyo3::exceptions::PyValueError::new_err("--out is required"))?;

    // Determine output format and write
    let branch = cli.branch;
    let xml_package_depth = cli.xml_package_depth;

    // Open output file for writing (use Python file object for compatibility with print functions)
    let builtins = PyModule::import(py, "builtins")?;
    let open_fn = builtins.getattr("open")?;
    let out_handle = open_fn.call(
        (out_file_str.as_str(), "w"),
        Some(&[("encoding", "utf-8")].into_py_dict(py)?),
    )?;

    // Convert merged_dict to CoverageData for output functions
    let coverage_data = crate::schemas::CoverageData::load_from_dict(merged_dict.bind(py))?;

    if cli.xml {
        // XML output using Rust print_xml function
        print_xml(
            py,
            &coverage_data,
            vec![base_path.clone()],
            branch,
            xml_package_depth,
            Some(out_handle.clone().into()),
        )?;
    } else if cli.lcov {
        // LCOV output using Rust print_lcov function
        print_lcov(
            py,
            &coverage_data,
            vec![base_path.clone()],
            branch,
            Some(out_handle.clone().into()),
        )?;
    } else {
        // JSON output using serde_json
        let pretty_print = cli.pretty_print;

        // Convert PyDict to serde_json::Value using pythonize
        let json_value: serde_json::Value =
            pythonize::depythonize(merged_dict.bind(py)).map_err(|e| {
                pyo3::exceptions::PyValueError::new_err(format!(
                    "Error converting coverage to JSON: {}",
                    e
                ))
            })?;

        // Write JSON to file using serde_json
        let json_str = if pretty_print {
            serde_json::to_string_pretty(&json_value).map_err(|e| {
                pyo3::exceptions::PyIOError::new_err(format!("Error serializing JSON: {}", e))
            })?
        } else {
            serde_json::to_string(&json_value).map_err(|e| {
                pyo3::exceptions::PyIOError::new_err(format!("Error serializing JSON: {}", e))
            })?
        };

        // Write to Python file object
        out_handle.call_method1("write", (json_str,))?;
    }

    out_handle.call_method0("close")?;

    // Print human-readable table unless silent
    if !cli.silent {
        let skip_covered = cli.skip_covered;
        let missing_width = cli.missing_width;

        // Get stdout from sys module
        let sys_module = PyModule::import(py, "sys")?;
        let stdout = sys_module.getattr("stdout")?;

        // Convert merged_dict to CoverageData
        let coverage_data = crate::schemas::CoverageData::load_from_dict(merged_dict.bind(py))?;

        // Use Rust print_coverage function
        print_coverage(
            py,
            &coverage_data,
            Some(stdout.into()),
            Some(missing_width as usize),
            skip_covered,
        )?;
    }

    // Check fail_under threshold
    if cli.fail_under > 0.0 {
        let merged_dict_ref = merged_dict.bind(py);
        let summary = merged_dict_ref
            .get_item("summary")?
            .ok_or_else(|| pyo3::exceptions::PyValueError::new_err("coverage has no summary"))?;
        let summary_dict: &Bound<PyDict> = summary.cast()?;
        let percent_covered: f64 = summary_dict
            .get_item("percent_covered")?
            .ok_or_else(|| {
                pyo3::exceptions::PyValueError::new_err("summary has no percent_covered")
            })?
            .extract()?;

        if percent_covered < cli.fail_under {
            return Ok(2);
        }
    }

    Ok(0)
}

fn run_with_coverage(py: Python, cli: &Cli) -> PyResult<i32> {
    use pyo3::types::PyModule;
    use std::path::PathBuf;

    // Determine base path - always resolve to absolute path
    let base_path = if let Some(ref script_str) = cli.script {
        let script_path = PathBuf::from(script_str);
        let parent = script_path
            .parent()
            .unwrap_or_else(|| std::path::Path::new("."));

        // Resolve parent to absolute path
        dunce::canonicalize(parent).unwrap_or_else(|_| parent.to_path_buf())
    } else {
        std::env::current_dir().unwrap_or_else(|_| PathBuf::from("."))
    };

    // Set up file matcher
    let file_matcher_class = py.import("covers")?.getattr("FileMatcher")?;
    let file_matcher = file_matcher_class.call0()?;

    // Add sources
    if let Some(ref source_str) = cli.source {
        for s in source_str.split(',') {
            file_matcher.call_method1("addSource", (s,))?;
        }
    } else if let Some(ref script_str) = cli.script {
        let script_parent = PathBuf::from(script_str)
            .parent()
            .map(|p| p.to_path_buf())
            .unwrap_or_else(|| PathBuf::from("."));
        file_matcher.call_method1("addSource", (script_parent.to_string_lossy().as_ref(),))?;
    }

    // Add omit patterns
    if let Some(ref omit_str) = cli.omit {
        for o in omit_str.split(',') {
            file_matcher.call_method1("addOmit", (o,))?;
        }
    }

    // Extract source list for Covers constructor
    let source_list = cli.source.as_ref().map(|source_str| {
        source_str
            .split(',')
            .map(|s| s.to_string())
            .collect::<Vec<_>>()
    });

    // Create Covers instance
    let covers_class = py.import("covers")?.getattr("Covers")?;
    let sci = covers_class.call1((
        cli.immediate,
        cli.threshold,
        cli.branch,
        cli.dis,
        source_list,
    ))?;

    // Wrap pytest if not disabled
    if !cli.dont_wrap_pytest {
        let covers_module = PyModule::import(py, "covers")?;
        let wrap_pytest = covers_module.getattr("wrap_pytest")?;
        wrap_pytest.call1((&sci, &file_matcher))?;
    }

    // Set up fork handling on non-Windows platforms
    let platform_module = PyModule::import(py, "platform")?;
    let system: String = platform_module.call_method0("system")?.extract()?;

    if system != "Windows" {
        // Set up fork and exit shims
        setup_fork_handling(py, &sci)?;
    }

    // Set up atexit handler for coverage output
    setup_atexit_handler(py, &sci, cli, &base_path)?;

    // Run script or module
    if cli.script.is_some() {
        run_script(py, &sci, &file_matcher, cli)?;
    } else {
        run_module(py, &sci, &file_matcher, cli)?;
    }

    // Check fail_under threshold
    if cli.fail_under > 0.0 {
        let cov = sci.call_method0("get_coverage")?;
        let summary = cov.get_item("summary")?;
        let percent_covered: f64 = summary.get_item("percent_covered")?.extract()?;
        if percent_covered < cli.fail_under {
            return Ok(2);
        }
    }

    Ok(0)
}

fn setup_fork_handling(py: Python, sci: &Bound<PyAny>) -> PyResult<()> {
    // Import the runner module for fork/exit shims
    let runner_module = PyModule::import(py, "covers.runner")?;
    let fork_shim = runner_module.getattr("fork_shim")?;
    let exit_shim = runner_module.getattr("exit_shim")?;

    // Get os module
    let os_module = PyModule::import(py, "os")?;

    // Set up fork and exit shims
    let fork_wrapper = fork_shim.call1((sci,))?;
    let exit_wrapper = exit_shim.call1((sci,))?;

    os_module.setattr("fork", fork_wrapper)?;
    os_module.setattr("_exit", exit_wrapper)?;

    Ok(())
}

fn setup_atexit_handler(
    py: Python,
    sci: &Bound<PyAny>,
    cli: &Cli,
    base_path: &std::path::Path,
) -> PyResult<()> {
    use pyo3::types::PyModule;
    use std::ffi::CString;

    // Convert Cli to PyDict for the Python callback
    let args_dict = cli.to_pydict(py)?;

    // Create the atexit callback
    let sci_clone = sci.clone();
    let base_path_str = base_path.to_string_lossy().to_string();

    // Import atexit module
    let atexit_module = PyModule::import(py, "atexit")?;

    // Create Python callback by defining it with proper closure
    // Pass base_path as a variable to avoid Windows path escaping issues
    let code_str = r#"
import sys
import json
import covers as sc
from covers.runner import get_coverage

def sci_atexit():
    def printit(coverage, outfile):
        # Convert CoverageData to dict for JSON serialization
        if hasattr(coverage, 'to_dict'):
            coverage_dict = coverage.to_dict()
        elif hasattr(coverage, '__class__') and coverage.__class__.__name__ == 'CoverageData':
            # Fallback for older implementations
            coverage_dict = {
                'meta': coverage['meta'],
                'files': coverage['files'],
                'summary': coverage['summary']
            }
        else:
            coverage_dict = coverage

        if _args.get("json"):
            print(
                json.dumps(
                    coverage_dict, indent=(4 if _args.get("pretty_print") else None)
                ),
                file=outfile,
            )
        elif _args.get("xml"):
            sc.print_xml(
                coverage,
                source_paths=[_base_path],
                with_branches=_args.get("branch", False),
                xml_package_depth=_args.get("xml_package_depth", 99),
                outfile=outfile,
            )
        elif _args.get("lcov"):
            sc.print_lcov(
                coverage,
                source_paths=[_base_path],
                with_branches=_args.get("branch", False),
                outfile=outfile,
            )
        else:
            sc.print_coverage(
                coverage,
                outfile=outfile,
                skip_covered=_args.get("skip_covered", False),
                missing_width=_args.get("missing_width", 80),
            )

    if not _args.get("silent"):
        coverage = get_coverage(_sci)
        if _args.get("out"):
            with open(_args["out"], "w") as outfile:
                printit(coverage, outfile)
        else:
            printit(coverage, sys.stdout)
"#;

    let callback_code = CString::new(code_str).unwrap();

    // Execute the callback definition with args, sci, and base_path in the namespace
    let globals = pyo3::types::PyDict::new(py);
    globals.set_item("_args", &args_dict)?;
    globals.set_item("_sci", &sci_clone)?;
    globals.set_item("_base_path", base_path_str)?;

    py.run(&callback_code, Some(&globals), Some(&globals))?;

    let callback = globals.get_item("sci_atexit")?.unwrap();

    // Register the callback
    atexit_module.call_method1("register", (callback,))?;

    Ok(())
}

fn run_script(
    py: Python,
    sci: &Bound<PyAny>,
    file_matcher: &Bound<PyAny>,
    cli: &Cli,
) -> PyResult<()> {
    use pyo3::types::{PyDict, PyString};
    use std::fs;
    use std::path::PathBuf;

    let script_str = cli
        .script
        .as_ref()
        .ok_or_else(|| pyo3::exceptions::PyValueError::new_err("script not specified"))?;
    let script_path = PathBuf::from(script_str);

    // Python globals for the script being executed
    let script_globals = PyDict::new(py);
    script_globals.set_item("__name__", "__main__")?;
    script_globals.set_item("__file__", script_str.as_str())?;

    // Set up sys.argv
    let sys_module = PyModule::import(py, "sys")?;
    let argv = PyList::new(py, &[PyString::new(py, script_str.as_str())])?;

    for arg in &cli.script_or_module_args {
        argv.append(PyString::new(py, arg))?;
    }
    sys_module.setattr("argv", argv)?;

    // Modify sys.path
    let sys_path_obj = sys_module.getattr("path")?;
    let sys_path = sys_path_obj.cast::<PyList>()?;
    sys_path.del_item(0)?;
    let base_path = script_path
        .parent()
        .unwrap_or_else(|| std::path::Path::new("."));
    sys_path.insert(0, base_path.to_string_lossy().as_ref())?;

    // Read and compile the script
    let source = fs::read_to_string(&script_path).map_err(|e| {
        pyo3::exceptions::PyIOError::new_err(format!("Failed to read script: {}", e))
    })?;

    // Check if we need to apply branch pre-instrumentation
    let matches: bool = file_matcher
        .call_method1("matches", (script_str.as_str(),))?
        .extract()?;

    let code = if cli.branch && matches {
        // Apply branch pre-instrumentation
        let branch_module = PyModule::import(py, "covers.branch")?;
        let preinstrument = branch_module.getattr("preinstrument")?;
        let ast_tree = preinstrument.call1((source.as_str(),))?;

        // Compile the AST
        let builtins = PyModule::import(py, "builtins")?;
        let compile = builtins.getattr("compile")?;
        compile.call1((ast_tree, script_path.to_string_lossy().as_ref(), "exec"))?
    } else {
        // Parse and compile normally
        let ast_module = PyModule::import(py, "ast")?;
        let ast_tree = ast_module.call_method1("parse", (source.as_str(),))?;

        let builtins = PyModule::import(py, "builtins")?;
        let compile = builtins.getattr("compile")?;
        compile.call1((ast_tree, script_path.to_string_lossy().as_ref(), "exec"))?
    };

    // Instrument if matches
    let instrumented_code = if matches {
        sci.call_method1("instrument", (code,))?
    } else {
        code.clone()
    };

    // Execute with ImportManager context
    let covers_module = PyModule::import(py, "covers")?;
    let import_manager_class = covers_module.getattr("ImportManager")?;
    let import_manager = import_manager_class.call1((sci, file_matcher))?;

    // Enter context
    import_manager.call_method0("__enter__")?;

    // Execute the code - use Python's exec function
    let builtins = PyModule::import(py, "builtins")?;
    let exec_fn = builtins.getattr("exec")?;
    let exec_result = exec_fn.call1((instrumented_code, script_globals));

    // Exit context
    import_manager.call_method1("__exit__", (py.None(), py.None(), py.None()))?;

    exec_result?;

    Ok(())
}

fn run_module(
    py: Python,
    sci: &Bound<PyAny>,
    file_matcher: &Bound<PyAny>,
    cli: &Cli,
) -> PyResult<()> {
    use pyo3::types::{PyList, PyString};

    // Get module name
    let module_list = cli
        .module
        .as_ref()
        .ok_or_else(|| pyo3::exceptions::PyValueError::new_err("module not specified"))?;
    let module_name = module_list
        .first()
        .ok_or_else(|| pyo3::exceptions::PyValueError::new_err("module list is empty"))?;

    // Set up sys.argv
    let sys_module = PyModule::import(py, "sys")?;
    let argv = PyList::new(py, &[PyString::new(py, module_name)])?;

    for arg in &cli.script_or_module_args {
        argv.append(PyString::new(py, arg))?;
    }
    sys_module.setattr("argv", argv)?;

    // Import runpy and run the module
    let runpy_module = PyModule::import(py, "runpy")?;

    // Execute with ImportManager context
    let covers_module = PyModule::import(py, "covers")?;
    let import_manager_class = covers_module.getattr("ImportManager")?;
    let import_manager = import_manager_class.call1((sci, file_matcher))?;

    // Enter context
    import_manager.call_method0("__enter__")?;

    // Run the module
    let kwargs = [("run_name", "__main__"), ("alter_sys", "True")].into_py_dict(py)?;
    let run_result = runpy_module.call_method("run_module", (module_name,), Some(&kwargs));

    // Exit context
    import_manager.call_method1("__exit__", (py.None(), py.None(), py.None()))?;

    run_result?;

    Ok(())
}
