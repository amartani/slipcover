// Data structures for coverage information
// Based on the original Python schemas.py module (TypedDict definitions)

use ahash::AHashMap;

/// LineOrBranch represents either a line number or a branch tuple
#[derive(Clone, Debug, Hash, Eq, PartialEq)]
pub enum LineOrBranch {
    Line(i32),
    Branch(i32, i32),
}

/// Native Rust structures for coverage data (to avoid Python conversions)
#[derive(Clone, Debug)]
pub struct FileCoverageData {
    pub executed_lines: Vec<i32>,
    pub missing_lines: Vec<i32>,
    pub executed_branches: Vec<(i32, i32)>,
    pub missing_branches: Vec<(i32, i32)>,
}

#[derive(Clone, Debug)]
pub struct FileSummary {
    pub covered_lines: i32,
    pub missing_lines: i32,
    pub covered_branches: Option<i32>,
    pub missing_branches: Option<i32>,
    pub percent_covered: f64,
}

#[derive(Clone, Debug)]
#[allow(dead_code)]
pub struct FileData {
    pub coverage: FileCoverageData,
    pub summary: FileSummary,
}

#[derive(Clone, Debug)]
pub struct MetaData {
    pub software: String,
    pub version: String,
    pub timestamp: String,
    pub branch_coverage: bool,
    pub show_contexts: bool,
}

#[derive(Clone, Debug)]
#[allow(dead_code)]
pub struct CoverageData {
    pub meta: MetaData,
    pub files: AHashMap<String, FileData>,
    pub summary: FileSummary,
}
