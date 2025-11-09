// CLI argument parsing and execution logic
// This module provides Rust implementations of CLI functionality

use pyo3::prelude::*;
use pyo3::types::PyDict;

/// Parse command-line arguments and run the coverage tool
/// This is the main entry point called from Python's __main__.py
#[pyfunction]
#[pyo3(signature = (argv))]
pub fn main_cli(py: Python, argv: Vec<String>) -> PyResult<i32> {
    // Parse arguments
    let args = parse_args(py, argv)?;

    // Check if this is a merge operation
    if args.get_item("merge")?.is_some() {
        if args.get_item("out")?.is_none() {
            return Err(pyo3::exceptions::PyValueError::new_err(
                "--out is required with --merge",
            ));
        }
        return merge_coverage_files(py, &args);
    }

    // Otherwise, run with coverage
    run_with_coverage(py, &args)
}

/// Parse command-line arguments into a Python dictionary
/// This provides a Rust-based alternative to Python's argparse
#[pyfunction]
#[pyo3(signature = (argv))]
pub fn parse_args(py: Python, argv: Vec<String>) -> PyResult<Bound<PyDict>> {
    let args = PyDict::new(py);

    // Default values
    args.set_item("branch", false)?;
    args.set_item("json", false)?;
    args.set_item("pretty_print", false)?;
    args.set_item("xml", false)?;
    args.set_item("lcov", false)?;
    args.set_item("xml_package_depth", 99)?;
    args.set_item("immediate", false)?;
    args.set_item("skip_covered", false)?;
    args.set_item("fail_under", 0.0)?;
    args.set_item("threshold", 50)?;
    args.set_item("missing_width", 80)?;
    args.set_item("silent", false)?;
    args.set_item("dis", false)?;
    args.set_item("debug", false)?;
    args.set_item("dont_wrap_pytest", false)?;

    let mut i = 1; // Skip program name
    let mut script_or_module_args: Vec<String> = Vec::new();
    let mut found_script_or_module = false;

    while i < argv.len() {
        let arg = &argv[i];

        if found_script_or_module {
            // Everything after script/module goes to script_or_module_args
            script_or_module_args.push(arg.clone());
            i += 1;
            continue;
        }

        match arg.as_str() {
            "--branch" => args.set_item("branch", true)?,
            "--json" => args.set_item("json", true)?,
            "--pretty-print" => args.set_item("pretty_print", true)?,
            "--xml" => args.set_item("xml", true)?,
            "--lcov" => args.set_item("lcov", true)?,
            "--immediate" => args.set_item("immediate", true)?,
            "--skip-covered" => args.set_item("skip_covered", true)?,
            "--silent" => args.set_item("silent", true)?,
            "--dis" => args.set_item("dis", true)?,
            "--debug" => args.set_item("debug", true)?,
            "--dont-wrap-pytest" => args.set_item("dont_wrap_pytest", true)?,
            "--version" => {
                let version = crate::VERSION;
                println!("Covers v{}", version);
                std::process::exit(0);
            }
            "--help" | "-h" => {
                print_help();
                std::process::exit(0);
            }

            "--xml-package-depth" => {
                i += 1;
                if i >= argv.len() {
                    return Err(pyo3::exceptions::PyValueError::new_err(
                        "--xml-package-depth requires a value",
                    ));
                }
                let val: i32 = argv[i].parse().map_err(|_| {
                    pyo3::exceptions::PyValueError::new_err("Invalid value for --xml-package-depth")
                })?;
                args.set_item("xml_package_depth", val)?;
            }

            "--out" => {
                i += 1;
                if i >= argv.len() {
                    return Err(pyo3::exceptions::PyValueError::new_err(
                        "--out requires a value",
                    ));
                }
                args.set_item("out", &argv[i])?;
            }

            "--source" => {
                i += 1;
                if i >= argv.len() {
                    return Err(pyo3::exceptions::PyValueError::new_err(
                        "--source requires a value",
                    ));
                }
                args.set_item("source", &argv[i])?;
            }

            "--omit" => {
                i += 1;
                if i >= argv.len() {
                    return Err(pyo3::exceptions::PyValueError::new_err(
                        "--omit requires a value",
                    ));
                }
                args.set_item("omit", &argv[i])?;
            }

            "--fail-under" => {
                i += 1;
                if i >= argv.len() {
                    return Err(pyo3::exceptions::PyValueError::new_err(
                        "--fail-under requires a value",
                    ));
                }
                let val: f64 = argv[i].parse().map_err(|_| {
                    pyo3::exceptions::PyValueError::new_err("Invalid value for --fail-under")
                })?;
                args.set_item("fail_under", val)?;
            }

            "--threshold" => {
                i += 1;
                if i >= argv.len() {
                    return Err(pyo3::exceptions::PyValueError::new_err(
                        "--threshold requires a value",
                    ));
                }
                let val: i32 = argv[i].parse().map_err(|_| {
                    pyo3::exceptions::PyValueError::new_err("Invalid value for --threshold")
                })?;
                args.set_item("threshold", val)?;
            }

            "--missing-width" => {
                i += 1;
                if i >= argv.len() {
                    return Err(pyo3::exceptions::PyValueError::new_err(
                        "--missing-width requires a value",
                    ));
                }
                let val: i32 = argv[i].parse().map_err(|_| {
                    pyo3::exceptions::PyValueError::new_err("Invalid value for --missing-width")
                })?;
                args.set_item("missing_width", val)?;
            }

            "-m" => {
                i += 1;
                if i >= argv.len() {
                    return Err(pyo3::exceptions::PyValueError::new_err(
                        "-m requires a module name",
                    ));
                }
                args.set_item("module", vec![argv[i].clone()])?;
                found_script_or_module = true;
            }

            "--merge" => {
                let mut merge_files: Vec<String> = Vec::new();
                i += 1;
                while i < argv.len() && !argv[i].starts_with('-') {
                    merge_files.push(argv[i].clone());
                    i += 1;
                }
                if merge_files.is_empty() {
                    return Err(pyo3::exceptions::PyValueError::new_err(
                        "--merge requires at least one file",
                    ));
                }
                args.set_item("merge", merge_files)?;
                i -= 1; // Back up one since we'll increment at the end
            }

            _ => {
                if arg.starts_with('-') {
                    return Err(pyo3::exceptions::PyValueError::new_err(format!(
                        "Unknown option: {}",
                        arg
                    )));
                } else {
                    // This is the script
                    args.set_item("script", arg.clone())?;
                    found_script_or_module = true;
                }
            }
        }

        i += 1;
    }

    args.set_item("script_or_module_args", script_or_module_args)?;

    // Validate that we have either script, module, or merge
    if args.get_item("script")?.is_none()
        && args.get_item("module")?.is_none()
        && args.get_item("merge")?.is_none()
    {
        return Err(pyo3::exceptions::PyValueError::new_err(
            "Must specify either a script, -m module, or --merge",
        ));
    }

    Ok(args)
}

fn print_help() {
    println!(
        r#"Covers - Near Zero-Overhead Python Code Coverage

USAGE:
    covers [OPTIONS] <SCRIPT> [ARGS...]
    covers [OPTIONS] -m <MODULE> [ARGS...]
    covers [OPTIONS] --merge <FILE>... --out <OUTPUT>

OPTIONS:
    --branch                    Measure both branch and line coverage
    --json                      Select JSON output
    --pretty-print              Pretty-print JSON output
    --xml                       Select XML output
    --lcov                      Select LCOV output
    --xml-package-depth <N>     Package depth for XML reports (default: 99)
    --out <FILE>                Specify output file name
    --source <PATHS>            Specify directories to cover (comma-separated)
    --omit <PATTERNS>           Specify file(s) to omit (comma-separated)
    --immediate                 Request immediate de-instrumentation
    --skip-covered              Omit fully covered files from text output
    --fail-under <PCT>          Fail if coverage is below this percentage
    --threshold <N>             Threshold for de-instrumentation (default: 50)
    --missing-width <N>         Maximum width for 'missing' column (default: 80)
    -m <MODULE>                 Run given module as __main__
    --merge <FILES>...          Merge JSON coverage files
    --version                   Print version information
    --help, -h                  Print this help message
"#
    );
}

fn merge_coverage_files(py: Python, args: &Bound<PyDict>) -> PyResult<i32> {
    // Import the runner module which has the merge logic
    let runner_module = PyModule::import(py, "covers.__runner__")?;
    let merge_fn = runner_module.getattr("merge_coverage_files")?;

    // Call the Python merge function
    let result = merge_fn.call1((args,))?;
    result.extract::<i32>()
}

fn run_with_coverage(py: Python, args: &Bound<PyDict>) -> PyResult<i32> {
    // Import the runner module
    let runner_module = PyModule::import(py, "covers.__runner__")?;
    let run_fn = runner_module.getattr("run_with_coverage")?;

    // Call the Python runner function
    let result = run_fn.call1((args,))?;
    result.extract::<i32>()
}
