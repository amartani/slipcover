// CLI argument parsing and execution logic using clap
// This module provides Rust implementations of CLI functionality

use clap::Parser;
use pyo3::prelude::*;
use pyo3::types::PyDict;

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

    // Convert to Python dictionary
    let args = cli.to_pydict(py)?;

    // Check if this is a merge operation
    if args.get_item("merge")?.is_some() {
        if args.get_item("out")?.is_none() {
            return Err(pyo3::exceptions::PyValueError::new_err(
                "--out is required with --merge",
            ));
        }
        return merge_coverage_files(py, &args);
    }

    // Validate that we have either script or module
    if args.get_item("script")?.is_none() && args.get_item("module")?.is_none() {
        eprintln!("error: Must specify either a script or -m module");
        return Ok(1);
    }

    // Otherwise, run with coverage
    run_with_coverage(py, &args)
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
