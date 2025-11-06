use ahash::AHashMap;
use pyo3::prelude::*;
use pyo3::types::PyDict;
use std::io::Write;
use std::path::Path;

/// File data structure for LCOV
#[derive(Debug, Clone)]
struct FileInfo {
    file_path: String,
    executed_lines: Vec<i32>,
    missing_lines: Vec<i32>,
    executed_branches: Vec<(i32, i32)>,
    missing_branches: Vec<(i32, i32)>,
}

/// Sort strings the way humans expect (natural sorting)
fn human_sorted(mut strings: Vec<String>) -> Vec<String> {
    strings.sort_by(|a, b| {
        let a_parts: Vec<_> = a
            .split(|c: char| !c.is_alphanumeric())
            .map(|s| {
                s.parse::<i64>()
                    .map(|n| (false, n, String::new()))
                    .unwrap_or((true, 0, s.to_string()))
            })
            .collect();
        let b_parts: Vec<_> = b
            .split(|c: char| !c.is_alphanumeric())
            .map(|s| {
                s.parse::<i64>()
                    .map(|n| (false, n, String::new()))
                    .unwrap_or((true, 0, s.to_string()))
            })
            .collect();
        a_parts.cmp(&b_parts)
    });
    strings
}

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
    coverage: &Bound<'_, PyDict>,
    source_paths: Vec<String>,
    with_branches: bool,
    outfile: Option<Py<PyAny>>,
) -> PyResult<()> {
    // Parse coverage data
    let files_dict = coverage.get_item("files")?.ok_or_else(|| {
        PyErr::new::<pyo3::exceptions::PyKeyError, _>("'files' key not found in coverage")
    })?;
    let files_dict: &Bound<'_, PyDict> = files_dict
        .cast()
        .map_err(|_| PyErr::new::<pyo3::exceptions::PyTypeError, _>("'files' must be a dict"))?;

    let mut file_infos: Vec<FileInfo> = Vec::new();

    for (file_path_obj, file_data_obj) in files_dict.iter() {
        let file_path: String = file_path_obj.extract()?;
        let file_data: &Bound<'_, PyDict> = file_data_obj.cast().map_err(|_| {
            PyErr::new::<pyo3::exceptions::PyTypeError, _>("file data must be a dict")
        })?;

        let executed_lines: Vec<i32> = file_data
            .get_item("executed_lines")?
            .ok_or_else(|| {
                PyErr::new::<pyo3::exceptions::PyKeyError, _>("'executed_lines' not found")
            })?
            .extract()?;

        let missing_lines: Vec<i32> = file_data
            .get_item("missing_lines")?
            .ok_or_else(|| {
                PyErr::new::<pyo3::exceptions::PyKeyError, _>("'missing_lines' not found")
            })?
            .extract()?;

        let (executed_branches, missing_branches) = if with_branches {
            let exec_branches: Vec<(i32, i32)> = file_data
                .get_item("executed_branches")?
                .map(|v| v.extract())
                .transpose()?
                .unwrap_or_default();

            let miss_branches: Vec<(i32, i32)> = file_data
                .get_item("missing_branches")?
                .map(|v| v.extract())
                .transpose()?
                .unwrap_or_default();

            (exec_branches, miss_branches)
        } else {
            (Vec::new(), Vec::new())
        };

        file_infos.push(FileInfo {
            file_path,
            executed_lines,
            missing_lines,
            executed_branches,
            missing_branches,
        });
    }

    // Sort files naturally
    let file_paths: Vec<String> = file_infos.iter().map(|f| f.file_path.clone()).collect();
    let sorted_paths = human_sorted(file_paths);

    // Create a map for quick lookup
    let mut file_map: AHashMap<String, FileInfo> = AHashMap::new();
    for file_info in file_infos {
        file_map.insert(file_info.file_path.clone(), file_info);
    }

    // Prepare output writer
    let mut output: Vec<u8> = Vec::new();

    // Write LCOV data for each file
    for file_path in sorted_paths {
        let file_info = file_map.get(&file_path).unwrap();

        // Get relative path
        let relative_path = get_relative_path(&file_info.file_path, &source_paths);

        // TN: Test name (optional, we'll use a generic name)
        writeln!(output, "TN:").unwrap();

        // SF: Source file
        writeln!(output, "SF:{}", relative_path).unwrap();

        // Combine executed and missing lines to get all line numbers with execution counts
        let mut line_data: AHashMap<i32, i32> = AHashMap::new();

        // Executed lines have count > 0 (we'll use 1 since we don't track actual execution count)
        for &line in &file_info.executed_lines {
            line_data.insert(line, 1);
        }

        // Missing lines have count 0
        for &line in &file_info.missing_lines {
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
        let lines_hit = file_info.executed_lines.len();
        writeln!(output, "LF:{}", lines_found).unwrap();
        writeln!(output, "LH:{}", lines_hit).unwrap();

        // Branch coverage (if enabled)
        if with_branches {
            let branch_map =
                get_branch_data_by_line(&file_info.executed_branches, &file_info.missing_branches);

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
