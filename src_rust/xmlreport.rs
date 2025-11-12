use crate::schemas::CoverageData;
use ahash::{AHashMap, AHashSet};
use pyo3::prelude::*;
use quick_xml::Writer;
use quick_xml::events::{BytesEnd, BytesStart, BytesText, Event};
use std::collections::BTreeMap;
use std::io::Write;

const DTD_URL: &str =
    "https://raw.githubusercontent.com/cobertura/web/master/htdocs/xml/coverage-04.dtd";
const VERSION: &str = "1.0.17";

/// Human-friendly sorting key for strings with numbers
fn human_key(s: &str) -> (Vec<HumanKeyComponent>, String) {
    let re = regex::Regex::new(r"(\d+)").unwrap();
    let mut parts = Vec::new();
    let mut last_end = 0;

    for cap in re.captures_iter(s) {
        let m = cap.get(0).unwrap();
        if last_end < m.start() {
            parts.push(HumanKeyComponent::Str(s[last_end..m.start()].to_string()));
        }
        if let Ok(num) = s[m.start()..m.end()].parse::<i64>() {
            parts.push(HumanKeyComponent::Num(num));
        }
        last_end = m.end();
    }
    if last_end < s.len() {
        parts.push(HumanKeyComponent::Str(s[last_end..].to_string()));
    }

    (parts, s.to_string())
}

#[derive(PartialEq, Eq, PartialOrd, Ord, Clone, Debug)]
enum HumanKeyComponent {
    Str(String),
    Num(i64),
}

/// Sort strings the way humans expect (natural sorting)
fn human_sorted(strings: &[String]) -> Vec<String> {
    let mut items: Vec<_> = strings.iter().map(|s| (human_key(s), s)).collect();
    items.sort_by(|a, b| a.0.cmp(&b.0));
    items.into_iter().map(|(_, s)| s.clone()).collect()
}

/// Calculate rate as a string
/// Matches Python's "%.4g" format (4 significant digits, trailing zeros removed)
fn rate(hit: i32, num: i32) -> String {
    if num == 0 {
        "1".to_string()
    } else {
        let value = (hit as f64) / (num as f64);
        // Format with 4 significant digits, similar to Python's %.4g
        let formatted = format!("{:.4}", value);
        // Remove trailing zeros after decimal point
        if formatted.contains('.') {
            let trimmed = formatted.trim_end_matches('0').trim_end_matches('.');
            trimmed.to_string()
        } else {
            formatted
        }
    }
}

/// File data structure
#[derive(Debug, Clone)]
struct FileInfo {
    file_path: String,
    executed_lines: Vec<i32>,
    missing_lines: Vec<i32>,
    executed_branches: Vec<(i32, i32)>,
    missing_branches: Vec<(i32, i32)>,
}

/// Package data structure
#[derive(Debug, Clone)]
struct PackageData {
    file_infos: Vec<FileInfo>,
    hits: i32,
    lines: i32,
    br_hits: i32,
    branches: i32,
}

impl PackageData {
    fn new() -> Self {
        PackageData {
            file_infos: Vec::new(),
            hits: 0,
            lines: 0,
            br_hits: 0,
            branches: 0,
        }
    }
}

/// Get missing branch arcs from file data
fn get_missing_branch_arcs(missing_branches: &[(i32, i32)]) -> AHashMap<i32, Vec<i32>> {
    let mut mba: AHashMap<i32, Vec<i32>> = AHashMap::new();
    for (from_line, to_line) in missing_branches {
        mba.entry(*from_line).or_default().push(*to_line);
    }
    mba
}

/// Get branch statistics
fn get_branch_stats(
    executed_branches: &[(i32, i32)],
    missing_branches: &[(i32, i32)],
    missing_arcs: &AHashMap<i32, Vec<i32>>,
) -> AHashMap<i32, (i32, i32)> {
    let mut all_branches: Vec<(i32, i32)> = Vec::new();
    all_branches.extend_from_slice(executed_branches);
    all_branches.extend_from_slice(missing_branches);
    all_branches.sort_unstable();

    let mut exits: AHashMap<i32, i32> = AHashMap::new();
    for (from_line, _) in &all_branches {
        *exits.entry(*from_line).or_insert(0) += 1;
    }

    let mut stats: AHashMap<i32, (i32, i32)> = AHashMap::new();
    for (from_line, _) in &all_branches {
        let total = *exits.get(from_line).unwrap_or(&0);
        let missing = missing_arcs.get(from_line).map(|v| v.len()).unwrap_or(0) as i32;
        let taken = total - missing;
        stats.insert(*from_line, (total, taken));
    }

    stats
}

/// Write a single file's class element to XML
fn write_file_xml<W: Write>(
    writer: &mut Writer<W>,
    file_info: &FileInfo,
    source_paths: &AHashSet<String>,
    xml_package_depth: i32,
    with_branches: bool,
) -> Result<(String, i32, i32, i32, i32, String), String> {
    // Determine relative name
    let filename = file_info.file_path.replace('\\', "/");
    let mut rel_name = None;
    let mut new_source_path = None;

    for source_path in source_paths {
        let sp = source_path.replace('\\', "/");
        if filename.starts_with(&format!("{}/", sp)) {
            rel_name = Some(filename[sp.len() + 1..].to_string());
            break;
        }
    }

    let rel_name = if let Some(rn) = rel_name {
        rn
    } else {
        // Fallback: compute relative path from cwd
        let full_path = std::path::Path::new(&file_info.file_path);
        let cwd = std::env::current_dir().map_err(|e| format!("Failed to get cwd: {}", e))?;
        let relative = full_path
            .strip_prefix(&cwd)
            .map(|p| p.to_string_lossy().to_string())
            .unwrap_or_else(|_| file_info.file_path.clone());

        // Calculate the source path
        let full = dunce::canonicalize(full_path).unwrap_or_else(|_| full_path.to_path_buf());
        let full_str = full.to_string_lossy().to_string();
        if full_str.len() > relative.len() {
            new_source_path = Some(
                full_str[..full_str.len() - relative.len()]
                    .trim_end_matches(['/', '\\'])
                    .to_string(),
            );
        }

        relative
    };

    let dirname = {
        let d = std::path::Path::new(&rel_name)
            .parent()
            .and_then(|p| p.to_str())
            .unwrap_or(".");
        if d.is_empty() {
            ".".to_string()
        } else {
            let parts: Vec<&str> = d.split('/').collect();
            let depth = xml_package_depth.min(parts.len() as i32) as usize;
            parts[..depth].join("/")
        }
    };

    let package_name = if dirname == "." {
        ".".to_string()
    } else {
        dirname.replace('/', ".")
    };

    // Calculate branch stats if needed
    let missing_branch_arcs = if with_branches {
        get_missing_branch_arcs(&file_info.missing_branches)
    } else {
        AHashMap::new()
    };

    let branch_stats = if with_branches {
        get_branch_stats(
            &file_info.executed_branches,
            &file_info.missing_branches,
            &missing_branch_arcs,
        )
    } else {
        AHashMap::new()
    };

    // Generate class element
    let mut class_elem = BytesStart::new("class");
    let class_name = std::path::Path::new(&rel_name)
        .strip_prefix(&dirname)
        .ok()
        .and_then(|p| p.to_str())
        .unwrap_or(&rel_name);
    class_elem.push_attribute(("name", class_name));
    class_elem.push_attribute(("filename", rel_name.replace('\\', "/").as_str()));

    // Calculate class statistics
    let mut all_lines: Vec<i32> = Vec::new();
    all_lines.extend_from_slice(&file_info.executed_lines);
    all_lines.extend_from_slice(&file_info.missing_lines);
    all_lines.sort_unstable();
    all_lines.dedup();

    let missing_set: AHashSet<i32> = file_info.missing_lines.iter().copied().collect();

    let class_lines = all_lines.len() as i32;
    let class_hits = class_lines - file_info.missing_lines.len() as i32;

    let (class_branches, class_br_hits) = if with_branches {
        let branches = branch_stats.values().map(|(t, _)| t).sum::<i32>();
        let missing = branch_stats.values().map(|(t, k)| t - k).sum::<i32>();
        (branches, branches - missing)
    } else {
        (0, 0)
    };

    class_elem.push_attribute(("line-rate", rate(class_hits, class_lines).as_str()));
    let branch_rate = if with_branches {
        rate(class_br_hits, class_branches)
    } else {
        "0".to_string()
    };
    class_elem.push_attribute(("branch-rate", branch_rate.as_str()));
    class_elem.push_attribute(("complexity", "0"));

    writer
        .write_event(Event::Start(class_elem.borrow()))
        .map_err(|e| e.to_string())?;

    // Write empty methods element
    writer
        .write_event(Event::Empty(BytesStart::new("methods")))
        .map_err(|e| e.to_string())?;

    // Write lines
    writer
        .write_event(Event::Start(BytesStart::new("lines")))
        .map_err(|e| e.to_string())?;

    for line in all_lines {
        let mut line_elem = BytesStart::new("line");
        line_elem.push_attribute(("number", line.to_string().as_str()));
        line_elem.push_attribute((
            "hits",
            if missing_set.contains(&line) {
                "0"
            } else {
                "1"
            },
        ));

        if with_branches {
            if let Some((total, taken)) = branch_stats.get(&line) {
                line_elem.push_attribute(("branch", "true"));
                let cond_cov = format!("{}% ({}/{})", 100 * taken / total, taken, total);
                line_elem.push_attribute(("condition-coverage", cond_cov.as_str()));
            }

            if let Some(missing_arcs) = missing_branch_arcs.get(&line) {
                let annlines: Vec<String> = missing_arcs
                    .iter()
                    .map(|b| {
                        if *b <= 0 {
                            "exit".to_string()
                        } else {
                            b.to_string()
                        }
                    })
                    .collect();
                line_elem.push_attribute(("missing-branches", annlines.join(",").as_str()));
            }
        }

        writer
            .write_event(Event::Empty(line_elem))
            .map_err(|e| e.to_string())?;
    }

    writer
        .write_event(Event::End(BytesEnd::new("lines")))
        .map_err(|e| e.to_string())?;
    writer
        .write_event(Event::End(BytesEnd::new("class")))
        .map_err(|e| e.to_string())?;

    Ok((
        package_name,
        class_hits,
        class_lines,
        class_br_hits,
        class_branches,
        new_source_path.unwrap_or_default(),
    ))
}

/// Generate XML report
#[pyfunction]
#[pyo3(signature = (coverage, source_paths, *, with_branches=false, xml_package_depth=99, outfile=None))]
pub fn print_xml(
    py: Python,
    coverage: &CoverageData,
    source_paths: Vec<String>,
    with_branches: bool,
    xml_package_depth: i32,
    outfile: Option<Py<PyAny>>,
) -> PyResult<()> {
    // Parse source paths
    let mut source_paths_set: AHashSet<String> = AHashSet::new();
    for src in source_paths {
        if std::path::Path::new(&src).exists() {
            source_paths_set.insert(src.trim_end_matches(['/', '\\']).to_string());
        }
    }

    // Process each file and collect into packages
    let mut packages: BTreeMap<String, PackageData> = BTreeMap::new();

    for (file_path, file_data) in &coverage.files {
        let (executed_branches, missing_branches) = if with_branches {
            (
                file_data.coverage.executed_branches.clone(),
                file_data.coverage.missing_branches.clone(),
            )
        } else {
            (Vec::new(), Vec::new())
        };

        let file_info = FileInfo {
            file_path: file_path.clone(),
            executed_lines: file_data.coverage.executed_lines.clone(),
            missing_lines: file_data.coverage.missing_lines.clone(),
            executed_branches,
            missing_branches,
        };

        // Determine package name
        let filename = file_info.file_path.replace('\\', "/");
        let mut rel_name = None;

        for source_path in &source_paths_set {
            let sp = source_path.replace('\\', "/");
            if filename.starts_with(&format!("{}/", sp)) {
                rel_name = Some(filename[sp.len() + 1..].to_string());
                break;
            }
        }

        let rel_name = if let Some(rn) = rel_name {
            rn
        } else {
            let full_path = std::path::Path::new(&file_info.file_path);
            let cwd = std::env::current_dir().map_err(|e| {
                PyErr::new::<pyo3::exceptions::PyRuntimeError, _>(format!(
                    "Failed to get cwd: {}",
                    e
                ))
            })?;
            let relative = full_path
                .strip_prefix(&cwd)
                .map(|p| p.to_string_lossy().to_string())
                .unwrap_or_else(|_| file_info.file_path.clone());

            let full = dunce::canonicalize(full_path).unwrap_or_else(|_| full_path.to_path_buf());
            let full_str = full.to_string_lossy().to_string();
            if full_str.len() > relative.len() {
                let new_source = full_str[..full_str.len() - relative.len()]
                    .trim_end_matches(['/', '\\'])
                    .to_string();
                source_paths_set.insert(new_source);
            }

            relative
        };

        let dirname = {
            let d = std::path::Path::new(&rel_name)
                .parent()
                .and_then(|p| p.to_str())
                .unwrap_or(".");
            if d.is_empty() {
                ".".to_string()
            } else {
                let parts: Vec<&str> = d.split('/').collect();
                let depth = xml_package_depth.min(parts.len() as i32) as usize;
                parts[..depth].join("/")
            }
        };

        let package_name = if dirname == "." {
            ".".to_string()
        } else {
            dirname.replace('/', ".")
        };

        // Calculate statistics
        let mut all_lines: Vec<i32> = Vec::new();
        all_lines.extend_from_slice(&file_info.executed_lines);
        all_lines.extend_from_slice(&file_info.missing_lines);
        all_lines.sort_unstable();
        all_lines.dedup();

        let class_lines = all_lines.len() as i32;
        let class_hits = class_lines - file_info.missing_lines.len() as i32;

        let (class_branches, class_br_hits) = if with_branches {
            let missing_branch_arcs = get_missing_branch_arcs(&file_info.missing_branches);
            let branch_stats = get_branch_stats(
                &file_info.executed_branches,
                &file_info.missing_branches,
                &missing_branch_arcs,
            );
            let branches = branch_stats.values().map(|(t, _)| t).sum::<i32>();
            let missing = branch_stats.values().map(|(t, k)| t - k).sum::<i32>();
            (branches, branches - missing)
        } else {
            (0, 0)
        };

        let package = packages
            .entry(package_name)
            .or_insert_with(PackageData::new);
        package.file_infos.push(file_info);
        package.hits += class_hits;
        package.lines += class_lines;
        package.br_hits += class_br_hits;
        package.branches += class_branches;
    }

    // Generate final XML
    let mut xml_output = String::new();

    // XML declaration
    xml_output.push_str(r#"<?xml version="1.0"?>"#);
    xml_output.push('\n');

    // DOCTYPE
    xml_output.push_str(&format!(
        r#"<!DOCTYPE coverage SYSTEM "{}">
"#,
        DTD_URL
    ));

    // Now write the rest using the writer
    let mut buffer = Vec::new();
    let mut writer = Writer::new_with_indent(&mut buffer, b' ', 2);

    // Calculate totals
    let mut lnum_tot = 0;
    let mut lhits_tot = 0;
    let mut bnum_tot = 0;
    let mut bhits_tot = 0;

    for pkg_data in packages.values() {
        lhits_tot += pkg_data.hits;
        lnum_tot += pkg_data.lines;
        bhits_tot += pkg_data.br_hits;
        bnum_tot += pkg_data.branches;
    }

    // Coverage element
    let timestamp = std::time::SystemTime::now()
        .duration_since(std::time::UNIX_EPOCH)
        .unwrap()
        .as_millis();

    let mut coverage_elem = BytesStart::new("coverage");
    coverage_elem.push_attribute(("version", VERSION));
    coverage_elem.push_attribute(("timestamp", timestamp.to_string().as_str()));
    coverage_elem.push_attribute(("lines-valid", lnum_tot.to_string().as_str()));
    coverage_elem.push_attribute(("lines-covered", lhits_tot.to_string().as_str()));
    coverage_elem.push_attribute(("line-rate", rate(lhits_tot, lnum_tot).as_str()));

    if with_branches {
        coverage_elem.push_attribute(("branches-valid", bnum_tot.to_string().as_str()));
        coverage_elem.push_attribute(("branches-covered", bhits_tot.to_string().as_str()));
        coverage_elem.push_attribute(("branch-rate", rate(bhits_tot, bnum_tot).as_str()));
    } else {
        coverage_elem.push_attribute(("branches-covered", "0"));
        coverage_elem.push_attribute(("branches-valid", "0"));
        coverage_elem.push_attribute(("branch-rate", "0"));
    }
    coverage_elem.push_attribute(("complexity", "0"));

    writer
        .write_event(Event::Start(coverage_elem.borrow()))
        .map_err(|e| PyErr::new::<pyo3::exceptions::PyRuntimeError, _>(e.to_string()))?;

    // Comments
    writer
        .write_event(Event::Comment(BytesText::new(" Generated by covers ")))
        .map_err(|e| PyErr::new::<pyo3::exceptions::PyRuntimeError, _>(e.to_string()))?;
    writer
        .write_event(Event::Comment(BytesText::new(&format!(
            " Based on {} ",
            DTD_URL
        ))))
        .map_err(|e| PyErr::new::<pyo3::exceptions::PyRuntimeError, _>(e.to_string()))?;

    // Sources
    writer
        .write_event(Event::Start(BytesStart::new("sources")))
        .map_err(|e| PyErr::new::<pyo3::exceptions::PyRuntimeError, _>(e.to_string()))?;

    let sorted_sources = human_sorted(&source_paths_set.iter().cloned().collect::<Vec<_>>());
    for path in sorted_sources {
        writer
            .write_event(Event::Start(BytesStart::new("source")))
            .map_err(|e| PyErr::new::<pyo3::exceptions::PyRuntimeError, _>(e.to_string()))?;
        writer
            .write_event(Event::Text(BytesText::new(&path)))
            .map_err(|e| PyErr::new::<pyo3::exceptions::PyRuntimeError, _>(e.to_string()))?;
        writer
            .write_event(Event::End(BytesEnd::new("source")))
            .map_err(|e| PyErr::new::<pyo3::exceptions::PyRuntimeError, _>(e.to_string()))?;
    }

    writer
        .write_event(Event::End(BytesEnd::new("sources")))
        .map_err(|e| PyErr::new::<pyo3::exceptions::PyRuntimeError, _>(e.to_string()))?;

    // Packages
    writer
        .write_event(Event::Start(BytesStart::new("packages")))
        .map_err(|e| PyErr::new::<pyo3::exceptions::PyRuntimeError, _>(e.to_string()))?;

    for (pkg_name, pkg_data) in packages.iter() {
        let mut package_elem = BytesStart::new("package");
        package_elem.push_attribute((
            "name",
            pkg_name.replace(std::path::MAIN_SEPARATOR, ".").as_str(),
        ));
        package_elem.push_attribute(("line-rate", rate(pkg_data.hits, pkg_data.lines).as_str()));
        let branch_rate = if with_branches {
            rate(pkg_data.br_hits, pkg_data.branches)
        } else {
            "0".to_string()
        };
        package_elem.push_attribute(("branch-rate", branch_rate.as_str()));
        package_elem.push_attribute(("complexity", "0"));

        writer
            .write_event(Event::Start(package_elem.borrow()))
            .map_err(|e| PyErr::new::<pyo3::exceptions::PyRuntimeError, _>(e.to_string()))?;

        writer
            .write_event(Event::Start(BytesStart::new("classes")))
            .map_err(|e| PyErr::new::<pyo3::exceptions::PyRuntimeError, _>(e.to_string()))?;

        // Sort files by name
        let file_paths: Vec<String> = pkg_data
            .file_infos
            .iter()
            .map(|fi| fi.file_path.clone())
            .collect();
        let sorted_paths = human_sorted(&file_paths);

        for file_path in sorted_paths {
            let file_info = pkg_data
                .file_infos
                .iter()
                .find(|fi| fi.file_path == file_path)
                .unwrap();
            write_file_xml(
                &mut writer,
                file_info,
                &source_paths_set,
                xml_package_depth,
                with_branches,
            )
            .map_err(PyErr::new::<pyo3::exceptions::PyRuntimeError, _>)?;
        }

        writer
            .write_event(Event::End(BytesEnd::new("classes")))
            .map_err(|e| PyErr::new::<pyo3::exceptions::PyRuntimeError, _>(e.to_string()))?;
        writer
            .write_event(Event::End(BytesEnd::new("package")))
            .map_err(|e| PyErr::new::<pyo3::exceptions::PyRuntimeError, _>(e.to_string()))?;
    }

    writer
        .write_event(Event::End(BytesEnd::new("packages")))
        .map_err(|e| PyErr::new::<pyo3::exceptions::PyRuntimeError, _>(e.to_string()))?;
    writer
        .write_event(Event::End(BytesEnd::new("coverage")))
        .map_err(|e| PyErr::new::<pyo3::exceptions::PyRuntimeError, _>(e.to_string()))?;

    let body_str = String::from_utf8(buffer)
        .map_err(|e| PyErr::new::<pyo3::exceptions::PyRuntimeError, _>(e.to_string()))?;

    xml_output.push_str(&body_str);
    let xml_str = xml_output;

    // Write output
    if let Some(outfile_py) = outfile {
        let outfile_bound = outfile_py.bind(py);
        let write_method = outfile_bound.getattr("write")?;
        write_method.call1((xml_str,))?;
    } else {
        // Default to stdout
        let sys_module = pyo3::types::PyModule::import(py, "sys")?;
        let stdout = sys_module.getattr("stdout")?;
        let write_method = stdout.getattr("write")?;
        write_method.call1((xml_str,))?;
    }

    Ok(())
}
