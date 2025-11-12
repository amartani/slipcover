use crate::schemas::CoverageData;
use ahash::AHashMap;
use pyo3::prelude::*;
use std::io::Write;
use std::path::Path;

/// Calculate relative path from source_path to file_path
fn get_relative_path(file_path: &str, source_paths: &[String]) -> String {
    let file_path = Path::new(file_path);

    // Try to find a matching source path and make the file path relative to it
    for source_path in source_paths {
        let source = Path::new(source_path);
        if let Ok(relative) = file_path.strip_prefix(source) {
            return relative.to_string_lossy().to_string();
        }
    }

    // If no source path matches, return the original path
    file_path.to_string_lossy().to_string()
}

/// Get branch data organized by line number
fn get_branch_data_by_line(
    executed_branches: &[(i32, i32)],
    missing_branches: &[(i32, i32)],
) -> AHashMap<i32, Vec<(i32, i32, bool)>> {
    let mut branch_map: AHashMap<i32, Vec<(i32, i32, bool)>> = AHashMap::new();

    // Add executed branches
    for &(from_line, to_line) in executed_branches {
        branch_map
            .entry(from_line)
            .or_default()
            .push((from_line, to_line, true));
    }

    // Add missing branches
    for &(from_line, to_line) in missing_branches {
        branch_map
            .entry(from_line)
            .or_default()
            .push((from_line, to_line, false));
    }

    // Sort branches for consistent output
    for branches in branch_map.values_mut() {
        branches.sort();
    }

    branch_map
}

/// Print coverage data in LCOV format
///
/// Args:
///     coverage: Dictionary containing coverage data
///     source_paths: List of source paths for relative path resolution
///     with_branches: Include branch coverage (default: false)
///     outfile: Output file path (default: stdout)
///
/// LCOV format specification:
///     TN: Test name
///     SF: Source file path
///     DA: Line number, execution count
///     LF: Number of lines found
///     LH: Number of lines hit
///     BRDA: Line number, block number, branch number, taken count (or '-' for not taken)
///     BRF: Number of branches found
///     BRH: Number of branches hit
///     end_of_record
#[pyfunction(signature = (coverage, source_paths, *, with_branches=false, outfile=None))]
pub fn print_lcov(
    py: Python,
    coverage: &CoverageData,
    source_paths: Vec<String>,
    with_branches: bool,
    outfile: Option<Py<PyAny>>,
) -> PyResult<()> {
    // Prepare output writer
    let mut output: Vec<u8> = Vec::new();

    // Write LCOV data for each file
    for (file_path, file_data) in &coverage.files {
        // Get relative path
        let relative_path = get_relative_path(file_path, &source_paths);

        // TN: Test name (optional, we'll use a generic name)
        writeln!(output, "TN:").unwrap();

        // SF: Source file
        writeln!(output, "SF:{}", relative_path).unwrap();

        // Combine executed and missing lines to get all line numbers with execution counts
        let mut line_data: AHashMap<i32, i32> = AHashMap::new();

        // Executed lines have count > 0 (we'll use 1 since we don't track actual execution count)
        for &line in &file_data.coverage.executed_lines {
            line_data.insert(line, 1);
        }

        // Missing lines have count 0
        for &line in &file_data.coverage.missing_lines {
            line_data.insert(line, 0);
        }

        // Sort line numbers
        let mut line_numbers: Vec<i32> = line_data.keys().copied().collect();
        line_numbers.sort();

        // DA: Line data (line number, execution count)
        for line_num in &line_numbers {
            let count = line_data.get(line_num).unwrap();
            writeln!(output, "DA:{},{}", line_num, count).unwrap();
        }

        // LF: Lines found, LH: Lines hit
        let lines_found = line_numbers.len();
        let lines_hit = file_data.coverage.executed_lines.len();
        writeln!(output, "LF:{}", lines_found).unwrap();
        writeln!(output, "LH:{}", lines_hit).unwrap();

        // Branch coverage (if enabled)
        if with_branches {
            let branch_map = get_branch_data_by_line(
                &file_data.coverage.executed_branches,
                &file_data.coverage.missing_branches,
            );

            // Get sorted line numbers that have branches
            let mut branch_lines: Vec<i32> = branch_map.keys().copied().collect();
            branch_lines.sort();

            let mut branch_num = 0;
            let mut branches_hit = 0;
            let mut branches_found = 0;

            // BRDA: Branch data (line, block, branch, taken)
            for line_num in branch_lines {
                let branches = branch_map.get(&line_num).unwrap();
                let block_num = 0; // We use a single block per line

                for (_from, _to, taken) in branches {
                    let taken_str = if *taken { "1" } else { "-" };
                    writeln!(
                        output,
                        "BRDA:{},{},{},{}",
                        line_num, block_num, branch_num, taken_str
                    )
                    .unwrap();

                    branches_found += 1;
                    if *taken {
                        branches_hit += 1;
                    }
                    branch_num += 1;
                }
            }

            // BRF: Branches found, BRH: Branches hit
            if branches_found > 0 {
                writeln!(output, "BRF:{}", branches_found).unwrap();
                writeln!(output, "BRH:{}", branches_hit).unwrap();
            }
        }

        // end_of_record
        writeln!(output, "end_of_record").unwrap();
    }

    // Write to file or stdout
    let output_str = String::from_utf8(output).unwrap();

    if let Some(outfile_obj) = outfile {
        let outfile_bound = outfile_obj.bind(py);
        let write_method = outfile_bound.getattr("write")?;
        write_method.call1((output_str,))?;
    } else {
        // Default to stdout
        let sys_module = pyo3::types::PyModule::import(py, "sys")?;
        let stdout = sys_module.getattr("stdout")?;
        let write_method = stdout.getattr("write")?;
        write_method.call1((output_str,))?;
    }

    Ok(())
}
