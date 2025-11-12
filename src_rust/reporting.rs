// Coverage reporting functionality
// Based on the original Python covers.py module (reporting part)

use crate::schemas::{CoverageData, FileCoverageData, FileSummary};
use ahash::{AHashMap, AHashSet};
use pyo3::prelude::*;
use pyo3::types::{PyAny, PyDict, PyModule};
use tabled::{Table, Tabled, settings::Style};

// Import CoversError from lib.rs
use crate::CoversError;

/// Format missing lines and branches as a string
pub fn format_missing(
    missing_lines: &[i32],
    executed_lines: &[i32],
    missing_branches: &[(i32, i32)],
) -> String {
    let missing_set: AHashSet<i32> = missing_lines.iter().copied().collect();
    let executed_set: AHashSet<i32> = executed_lines.iter().copied().collect();

    // Filter out branches where both endpoints are missing
    let mut branches: Vec<(i32, i32)> = missing_branches
        .iter()
        .filter(|(a, b)| !missing_set.contains(a) && !missing_set.contains(b))
        .copied()
        .collect();
    branches.sort_unstable();

    let format_branch = |br: (i32, i32)| -> String {
        if br.1 == 0 {
            format!("{}->exit", br.0)
        } else {
            format!("{}->{}", br.0, br.1)
        }
    };

    let mut result = Vec::new();
    let mut lines_iter = missing_lines.iter().copied().peekable();
    let mut branch_idx = 0;

    while let Some(a) = lines_iter.next() {
        // Add branches that come before this line
        while branch_idx < branches.len() && branches[branch_idx].0 < a {
            result.push(format_branch(branches[branch_idx]));
            branch_idx += 1;
        }

        // Find the end of this range
        let mut b = a;
        while let Some(&n) = lines_iter.peek() {
            // Check if there's any executed line between b and n
            let has_executed = (b + 1..=n).any(|line| executed_set.contains(&line));
            if has_executed {
                break;
            }
            b = n;
            lines_iter.next();
        }

        if a == b {
            result.push(a.to_string());
        } else {
            result.push(format!("{}-{}", a, b));
        }
    }

    // Add remaining branches
    while branch_idx < branches.len() {
        result.push(format_branch(branches[branch_idx]));
        branch_idx += 1;
    }

    result.join(", ")
}

/// Python-exposed version of format_missing
#[pyfunction]
#[pyo3(signature = (missing_lines, executed_lines, missing_branches))]
pub fn format_missing_py(
    missing_lines: Vec<i32>,
    executed_lines: Vec<i32>,
    missing_branches: Vec<(i32, i32)>,
) -> String {
    format_missing(&missing_lines, &executed_lines, &missing_branches)
}

/// Row structure for the coverage table
#[derive(Tabled)]
struct CoverageRow {
    #[tabled(rename = "File")]
    file: String,
    #[tabled(rename = "#lines")]
    lines: String,
    #[tabled(rename = "#l.miss")]
    lines_miss: String,
    #[tabled(rename = "#br.")]
    branches: String,
    #[tabled(rename = "#br.miss")]
    branches_miss: String,
    #[tabled(rename = "brCov%")]
    branch_cov: String,
    #[tabled(rename = "totCov%")]
    total_cov: String,
    #[tabled(rename = "Missing")]
    missing: String,
}

/// Row structure for the coverage table without branch coverage
#[derive(Tabled)]
struct SimpleCoverageRow {
    #[tabled(rename = "File")]
    file: String,
    #[tabled(rename = "#lines")]
    lines: String,
    #[tabled(rename = "#l.miss")]
    lines_miss: String,
    #[tabled(rename = "Cover%")]
    coverage: String,
    #[tabled(rename = "Missing")]
    missing: String,
}

/// Print coverage information
#[pyfunction]
#[pyo3(signature = (coverage, outfile=None, missing_width=None, skip_covered=false))]
pub fn print_coverage(
    py: Python,
    coverage: &CoverageData,
    outfile: Option<Py<PyAny>>,
    missing_width: Option<usize>,
    skip_covered: bool,
) -> PyResult<()> {
    // Return early if no files
    if coverage.files.is_empty() {
        return Ok(());
    }

    // Check if branch coverage is enabled
    let branch_coverage = coverage.meta.branch_coverage;

    // Collect and sort file names
    let mut files_vec: Vec<&String> = coverage.files.keys().collect();
    files_vec.sort();

    let table_str = if branch_coverage {
        let mut rows: Vec<CoverageRow> = Vec::new();

        for filename in files_vec {
            let file_data = &coverage.files[filename];

            let exec_l = file_data.coverage.executed_lines.len();
            let miss_l = file_data.coverage.missing_lines.len();

            let exec_b = file_data.coverage.executed_branches.len();
            let miss_b = file_data.coverage.missing_branches.len();
            let total_b = exec_b + miss_b;
            let pct_b = if total_b > 0 {
                (100 * exec_b) / total_b
            } else {
                0
            };

            let pct = file_data.summary.percent_covered;

            if skip_covered && (pct - 100.0).abs() < 0.01 {
                continue;
            }

            // Get missing info
            let missing_str = format_missing(
                &file_data.coverage.missing_lines,
                &file_data.coverage.executed_lines,
                &file_data.coverage.missing_branches,
            );
            let truncated_missing = if let Some(width) = missing_width {
                if missing_str.len() > width {
                    format!("{}...", &missing_str[..width.saturating_sub(3)])
                } else {
                    missing_str
                }
            } else {
                missing_str
            };

            rows.push(CoverageRow {
                file: filename.clone(),
                lines: (exec_l + miss_l).to_string(),
                lines_miss: miss_l.to_string(),
                branches: total_b.to_string(),
                branches_miss: miss_b.to_string(),
                branch_cov: pct_b.to_string(),
                total_cov: pct.round().to_string(),
                missing: truncated_missing,
            });
        }

        // Add summary if multiple files
        if coverage.files.len() > 1 {
            let s_covered_lines = coverage.summary.covered_lines;
            let s_missing_lines = coverage.summary.missing_lines;
            let s_covered_branches = coverage.summary.covered_branches.unwrap_or(0);
            let s_missing_branches = coverage.summary.missing_branches.unwrap_or(0);
            let s_percent = coverage.summary.percent_covered;

            let total_b = s_covered_branches + s_missing_branches;
            let pct_b = if total_b > 0 {
                (100 * s_covered_branches) / total_b
            } else {
                0
            };

            rows.push(CoverageRow {
                file: "---".to_string(),
                lines: String::new(),
                lines_miss: String::new(),
                branches: String::new(),
                branches_miss: String::new(),
                branch_cov: String::new(),
                total_cov: String::new(),
                missing: String::new(),
            });

            rows.push(CoverageRow {
                file: "(summary)".to_string(),
                lines: (s_covered_lines + s_missing_lines).to_string(),
                lines_miss: s_missing_lines.to_string(),
                branches: total_b.to_string(),
                branches_miss: s_missing_branches.to_string(),
                branch_cov: pct_b.to_string(),
                total_cov: s_percent.round().to_string(),
                missing: String::new(),
            });
        }

        let mut table = Table::new(rows);
        table.with(Style::empty());

        // Note: missing_width parameter truncates strings before adding to table
        // so width constraint is already handled

        table.to_string()
    } else {
        let mut rows: Vec<SimpleCoverageRow> = Vec::new();

        for filename in files_vec {
            let file_data = &coverage.files[filename];

            let exec_l = file_data.coverage.executed_lines.len();
            let miss_l = file_data.coverage.missing_lines.len();

            let pct = file_data.summary.percent_covered;

            if skip_covered && (pct - 100.0).abs() < 0.01 {
                continue;
            }

            // Get missing info
            let missing_branches: Vec<(i32, i32)> = Vec::new();

            let missing_str = format_missing(
                &file_data.coverage.missing_lines,
                &file_data.coverage.executed_lines,
                &missing_branches,
            );
            let truncated_missing = if let Some(width) = missing_width {
                if missing_str.len() > width {
                    format!("{}...", &missing_str[..width.saturating_sub(3)])
                } else {
                    missing_str
                }
            } else {
                missing_str
            };

            rows.push(SimpleCoverageRow {
                file: filename.clone(),
                lines: (exec_l + miss_l).to_string(),
                lines_miss: miss_l.to_string(),
                coverage: pct.round().to_string(),
                missing: truncated_missing,
            });
        }

        // Add summary if multiple files
        if coverage.files.len() > 1 {
            let s_covered_lines = coverage.summary.covered_lines;
            let s_missing_lines = coverage.summary.missing_lines;
            let s_percent = coverage.summary.percent_covered;

            rows.push(SimpleCoverageRow {
                file: "---".to_string(),
                lines: String::new(),
                lines_miss: String::new(),
                coverage: String::new(),
                missing: String::new(),
            });

            rows.push(SimpleCoverageRow {
                file: "(summary)".to_string(),
                lines: (s_covered_lines + s_missing_lines).to_string(),
                lines_miss: s_missing_lines.to_string(),
                coverage: s_percent.round().to_string(),
                missing: String::new(),
            });
        }

        let mut table = Table::new(rows);
        table.with(Style::empty());

        // Note: missing_width parameter truncates strings before adding to table
        // so width constraint is already handled

        table.to_string()
    };

    // Write output
    let output = format!("\n{}\n", table_str);

    if let Some(outfile_py) = outfile {
        let outfile_bound = outfile_py.bind(py);
        let write_method = outfile_bound.getattr("write")?;
        write_method.call1((output,))?;
    } else {
        // Default to stdout
        let sys_module = PyModule::import(py, "sys")?;
        let stdout = sys_module.getattr("stdout")?;
        let write_method = stdout.getattr("write")?;
        write_method.call1((output,))?;
    }

    Ok(())
}

/// Adds summaries to coverage data (native Rust structures)
pub fn add_summaries_native(
    files_data: &mut AHashMap<String, FileCoverageData>,
    with_branches: bool,
) -> (AHashMap<String, FileSummary>, FileSummary) {
    let mut file_summaries: AHashMap<String, FileSummary> = AHashMap::new();
    let mut g_covered_lines = 0;
    let mut g_missing_lines = 0;
    let mut g_covered_branches = 0;
    let mut g_missing_branches = 0;

    for (filename, file_data) in files_data.iter() {
        let covered_lines = file_data.executed_lines.len() as i32;
        let missing_lines_count = file_data.missing_lines.len() as i32;

        let mut nom = covered_lines;
        let mut den = nom + missing_lines_count;

        let (covered_branches, missing_branches_count) = if with_branches {
            let cb = file_data.executed_branches.len() as i32;
            let mb = file_data.missing_branches.len() as i32;
            nom += cb;
            den += cb + mb;
            (Some(cb), Some(mb))
        } else {
            (None, None)
        };

        let percent_covered = if den == 0 {
            100.0
        } else {
            100.0 * nom as f64 / den as f64
        };

        file_summaries.insert(
            filename.clone(),
            FileSummary {
                covered_lines,
                missing_lines: missing_lines_count,
                covered_branches,
                missing_branches: missing_branches_count,
                percent_covered,
            },
        );

        g_covered_lines += covered_lines;
        g_missing_lines += missing_lines_count;
        if let Some(cb) = covered_branches {
            g_covered_branches += cb;
        }
        if let Some(mb) = missing_branches_count {
            g_missing_branches += mb;
        }
    }

    // Calculate global summary
    let g_nom = if with_branches {
        g_covered_lines + g_covered_branches
    } else {
        g_covered_lines
    };
    let g_den = if with_branches {
        g_covered_lines + g_missing_lines + g_covered_branches + g_missing_branches
    } else {
        g_covered_lines + g_missing_lines
    };
    let g_percent_covered = if g_den == 0 {
        100.0
    } else {
        100.0 * g_nom as f64 / g_den as f64
    };

    let global_summary = FileSummary {
        covered_lines: g_covered_lines,
        missing_lines: g_missing_lines,
        covered_branches: if with_branches {
            Some(g_covered_branches)
        } else {
            None
        },
        missing_branches: if with_branches {
            Some(g_missing_branches)
        } else {
            None
        },
        percent_covered: g_percent_covered,
    };

    (file_summaries, global_summary)
}

/// Adds (or updates) 'summary' entries in coverage information (legacy Python API)
#[pyfunction]
pub fn add_summaries(py: Python, cov: &Bound<PyDict>) -> PyResult<()> {
    let mut g_summary_data: AHashMap<String, i32> = AHashMap::new();
    let mut g_nom = 0;
    let mut g_den = 0;

    // Process files if they exist
    if let Ok(Some(files)) = cov.get_item("files") {
        let files_dict: &Bound<PyDict> = files.cast()?;

        for (_filename, f_cov_obj) in files_dict.iter() {
            let f_cov: &Bound<PyDict> = f_cov_obj.cast()?;

            // Get executed and missing lines
            let executed_lines = f_cov.get_item("executed_lines")?.unwrap();
            let missing_lines = f_cov.get_item("missing_lines")?.unwrap();

            let covered_lines = executed_lines.len()? as i32;
            let missing_lines_count = missing_lines.len()? as i32;

            let mut nom = covered_lines;
            let mut den = nom + missing_lines_count;

            // Create summary dict
            let summary = PyDict::new(py);
            summary.set_item("covered_lines", covered_lines)?;
            summary.set_item("missing_lines", missing_lines_count)?;

            // Handle branches if present
            if let Ok(Some(executed_branches)) = f_cov.get_item("executed_branches") {
                let missing_branches = f_cov.get_item("missing_branches")?.unwrap();

                let covered_branches = executed_branches.len()? as i32;
                let missing_branches_count = missing_branches.len()? as i32;

                summary.set_item("covered_branches", covered_branches)?;
                summary.set_item("missing_branches", missing_branches_count)?;

                nom += covered_branches;
                den += covered_branches + missing_branches_count;

                // Update global summary for branches
                *g_summary_data
                    .entry("covered_branches".to_string())
                    .or_insert(0) += covered_branches;
                *g_summary_data
                    .entry("missing_branches".to_string())
                    .or_insert(0) += missing_branches_count;
            }

            // Calculate percent covered
            let percent_covered = if den == 0 {
                100.0
            } else {
                100.0 * nom as f64 / den as f64
            };
            summary.set_item("percent_covered", percent_covered)?;

            // Set summary on file
            f_cov.set_item("summary", summary)?;

            // Update global summary for lines
            *g_summary_data
                .entry("covered_lines".to_string())
                .or_insert(0) += covered_lines;
            *g_summary_data
                .entry("missing_lines".to_string())
                .or_insert(0) += missing_lines_count;

            g_nom += nom;
            g_den += den;
        }
    }

    // Create global summary
    let g_summary = PyDict::new(py);
    for (k, v) in g_summary_data {
        g_summary.set_item(k, v)?;
    }

    let g_percent_covered = if g_den == 0 {
        100.0
    } else {
        100.0 * g_nom as f64 / g_den as f64
    };
    g_summary.set_item("percent_covered", g_percent_covered)?;
    g_summary.set_item(
        "percent_covered_display",
        format!("{}", g_percent_covered.round() as i32),
    )?;

    cov.set_item("summary", g_summary)?;

    Ok(())
}

/// Merge coverage data (native Rust implementation)
pub fn merge_coverage_impl(
    a_files: &mut AHashMap<String, FileCoverageData>,
    b_files: &AHashMap<String, FileCoverageData>,
    branch_coverage: bool,
) {
    for (filename, b_file_data) in b_files.iter() {
        // Get or create entry for this file in a
        let a_file_data = a_files
            .entry(filename.clone())
            .or_insert_with(|| FileCoverageData {
                executed_lines: Vec::new(),
                missing_lines: Vec::new(),
                executed_branches: Vec::new(),
                missing_branches: Vec::new(),
            });

        // Merge executed lines
        let mut executed_lines_set: AHashSet<i32> =
            a_file_data.executed_lines.iter().copied().collect();
        executed_lines_set.extend(b_file_data.executed_lines.iter().copied());

        // Merge missing lines
        let mut missing_lines_set: AHashSet<i32> =
            a_file_data.missing_lines.iter().copied().collect();
        missing_lines_set.extend(b_file_data.missing_lines.iter().copied());

        // Remove executed lines from missing lines
        missing_lines_set.retain(|line| !executed_lines_set.contains(line));

        // Sort the results
        let mut executed_lines_vec: Vec<i32> = executed_lines_set.into_iter().collect();
        executed_lines_vec.sort_unstable();
        let mut missing_lines_vec: Vec<i32> = missing_lines_set.into_iter().collect();
        missing_lines_vec.sort_unstable();

        a_file_data.executed_lines = executed_lines_vec;
        a_file_data.missing_lines = missing_lines_vec;

        // Handle branches if branch_coverage is enabled
        if branch_coverage {
            // Merge executed branches
            let mut executed_branches_set: AHashSet<(i32, i32)> =
                a_file_data.executed_branches.iter().copied().collect();
            executed_branches_set.extend(b_file_data.executed_branches.iter().copied());

            // Merge missing branches
            let mut missing_branches_set: AHashSet<(i32, i32)> =
                a_file_data.missing_branches.iter().copied().collect();
            missing_branches_set.extend(b_file_data.missing_branches.iter().copied());

            // Remove executed branches from missing branches
            missing_branches_set.retain(|br| !executed_branches_set.contains(br));

            // Sort and update
            let mut executed_branches_vec: Vec<(i32, i32)> =
                executed_branches_set.into_iter().collect();
            executed_branches_vec.sort_unstable();
            let mut missing_branches_vec: Vec<(i32, i32)> =
                missing_branches_set.into_iter().collect();
            missing_branches_vec.sort_unstable();

            a_file_data.executed_branches = executed_branches_vec;
            a_file_data.missing_branches = missing_branches_vec;
        }
    }
}

/// Merge coverage result 'b' into 'a' (Python API)
#[pyfunction]
pub fn merge_coverage(py: Python, a: &Bound<PyDict>, b: &Bound<PyDict>) -> PyResult<Py<PyDict>> {
    // Validate that both coverage files are in covers format
    let a_meta = a.get_item("meta")?.ok_or_else(|| {
        CoversError::new_err("Cannot merge coverage: missing 'meta' in first coverage")
    })?;
    let a_meta_dict: &Bound<PyDict> = a_meta.cast()?;

    let b_meta = b.get_item("meta")?.ok_or_else(|| {
        CoversError::new_err("Cannot merge coverage: missing 'meta' in second coverage")
    })?;
    let b_meta_dict: &Bound<PyDict> = b_meta.cast()?;

    // Check if software is "covers"
    if let Ok(Some(software)) = a_meta_dict.get_item("software") {
        if software.extract::<String>()? != "covers" {
            return Err(CoversError::new_err(
                "Cannot merge coverage: only Covers format supported.",
            ));
        }
    } else {
        return Err(CoversError::new_err(
            "Cannot merge coverage: only Covers format supported.",
        ));
    }

    // Check if show_contexts is enabled (not supported)
    let a_show_contexts = a_meta_dict
        .get_item("show_contexts")?
        .and_then(|v| v.extract::<bool>().ok())
        .unwrap_or(false);
    let b_show_contexts = b_meta_dict
        .get_item("show_contexts")?
        .and_then(|v| v.extract::<bool>().ok())
        .unwrap_or(false);

    if a_show_contexts || b_show_contexts {
        return Err(CoversError::new_err(
            "Merging coverage with show_contexts=True unsupported",
        ));
    }

    // Check branch_coverage compatibility
    let branch_coverage = a_meta_dict
        .get_item("branch_coverage")?
        .and_then(|v| v.extract::<bool>().ok())
        .unwrap_or(false);
    let b_branch_coverage = b_meta_dict
        .get_item("branch_coverage")?
        .and_then(|v| v.extract::<bool>().ok())
        .unwrap_or(false);

    if branch_coverage && !b_branch_coverage {
        return Err(CoversError::new_err(
            "Cannot merge coverage: branch coverage missing",
        ));
    }

    // Get files dictionaries
    let a_files = a.get_item("files")?.ok_or_else(|| {
        CoversError::new_err("Cannot merge coverage: missing 'files' in first coverage")
    })?;
    let a_files_dict: &Bound<PyDict> = a_files.cast()?;

    let b_files = b.get_item("files")?.ok_or_else(|| {
        CoversError::new_err("Cannot merge coverage: missing 'files' in second coverage")
    })?;
    let b_files_dict: &Bound<PyDict> = b_files.cast()?;

    // Convert PyDict files to native Rust structures
    let mut a_files_native: AHashMap<String, FileCoverageData> = AHashMap::new();
    for (filename, file_data) in a_files_dict.iter() {
        let filename_str = filename.extract::<String>()?;
        let file_dict: &Bound<PyDict> = file_data.cast()?;

        let executed_lines: Vec<i32> = file_dict.get_item("executed_lines")?.unwrap().extract()?;
        let missing_lines: Vec<i32> = file_dict.get_item("missing_lines")?.unwrap().extract()?;

        let (executed_branches, missing_branches) = if branch_coverage {
            let exec_br_list: Vec<Vec<i32>> = file_dict
                .get_item("executed_branches")?
                .unwrap()
                .extract()?;
            let miss_br_list: Vec<Vec<i32>> =
                file_dict.get_item("missing_branches")?.unwrap().extract()?;
            (
                exec_br_list.into_iter().map(|v| (v[0], v[1])).collect(),
                miss_br_list.into_iter().map(|v| (v[0], v[1])).collect(),
            )
        } else {
            (Vec::new(), Vec::new())
        };

        a_files_native.insert(
            filename_str,
            FileCoverageData {
                executed_lines,
                missing_lines,
                executed_branches,
                missing_branches,
            },
        );
    }

    let mut b_files_native: AHashMap<String, FileCoverageData> = AHashMap::new();
    for (filename, file_data) in b_files_dict.iter() {
        let filename_str = filename.extract::<String>()?;
        let file_dict: &Bound<PyDict> = file_data.cast()?;

        let executed_lines: Vec<i32> = file_dict.get_item("executed_lines")?.unwrap().extract()?;
        let missing_lines: Vec<i32> = file_dict.get_item("missing_lines")?.unwrap().extract()?;

        let (executed_branches, missing_branches) = if branch_coverage {
            let exec_br_list: Vec<Vec<i32>> = file_dict
                .get_item("executed_branches")?
                .unwrap()
                .extract()?;
            let miss_br_list: Vec<Vec<i32>> =
                file_dict.get_item("missing_branches")?.unwrap().extract()?;
            (
                exec_br_list.into_iter().map(|v| (v[0], v[1])).collect(),
                miss_br_list.into_iter().map(|v| (v[0], v[1])).collect(),
            )
        } else {
            (Vec::new(), Vec::new())
        };

        b_files_native.insert(
            filename_str,
            FileCoverageData {
                executed_lines,
                missing_lines,
                executed_branches,
                missing_branches,
            },
        );
    }

    // Call the native merge implementation
    merge_coverage_impl(&mut a_files_native, &b_files_native, branch_coverage);

    // Convert back to PyDict and update a
    for (filename, file_data) in a_files_native.iter() {
        let update = PyDict::new(py);
        update.set_item("executed_lines", file_data.executed_lines.clone())?;
        update.set_item("missing_lines", file_data.missing_lines.clone())?;

        if branch_coverage {
            let executed_branches_list: Vec<Vec<i32>> = file_data
                .executed_branches
                .iter()
                .map(|(a, b)| vec![*a, *b])
                .collect();
            let missing_branches_list: Vec<Vec<i32>> = file_data
                .missing_branches
                .iter()
                .map(|(a, b)| vec![*a, *b])
                .collect();

            update.set_item("executed_branches", executed_branches_list)?;
            update.set_item("missing_branches", missing_branches_list)?;
        }

        a_files_dict.set_item(filename, update)?;
    }

    // Add summaries using native implementation
    let (file_summaries, global_summary) =
        add_summaries_native(&mut a_files_native, branch_coverage);

    // Convert summaries to PyDict format
    for (filename, summary) in file_summaries.iter() {
        if let Ok(Some(file_data)) = a_files_dict.get_item(filename) {
            let file_dict: &Bound<PyDict> = file_data.cast()?;
            let summary_dict = PyDict::new(py);
            summary_dict.set_item("covered_lines", summary.covered_lines)?;
            summary_dict.set_item("missing_lines", summary.missing_lines)?;
            if let Some(cb) = summary.covered_branches {
                summary_dict.set_item("covered_branches", cb)?;
            }
            if let Some(mb) = summary.missing_branches {
                summary_dict.set_item("missing_branches", mb)?;
            }
            summary_dict.set_item("percent_covered", summary.percent_covered)?;
            file_dict.set_item("summary", summary_dict)?;
        }
    }

    // Set global summary
    let g_summary = PyDict::new(py);
    g_summary.set_item("covered_lines", global_summary.covered_lines)?;
    g_summary.set_item("missing_lines", global_summary.missing_lines)?;
    if let Some(cb) = global_summary.covered_branches {
        g_summary.set_item("covered_branches", cb)?;
    }
    if let Some(mb) = global_summary.missing_branches {
        g_summary.set_item("missing_branches", mb)?;
    }
    g_summary.set_item("percent_covered", global_summary.percent_covered)?;
    g_summary.set_item(
        "percent_covered_display",
        format!("{}", global_summary.percent_covered.round() as i32),
    )?;
    a.set_item("summary", g_summary)?;

    // Return the modified dict a
    Ok(a.clone().unbind())
}
