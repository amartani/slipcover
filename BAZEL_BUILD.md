# Bazel Build Configuration for Slipcover

This document describes the Bazel build configuration for the Slipcover project.

## Overview

This project now has Bazel build support with the following features:
- Build configuration using the latest Bazel (8.0.0)
- Minimal dependencies approach using genrules
- Support for building the Rust extension via cargo
- Test execution via pytest

## Files Created

### 1. `.bazelversion`
Specifies Bazel version 8.0.0

### 2. `WORKSPACE`
Minimal workspace configuration. Due to network restrictions, this uses a minimal setup.
For production use, you would add:
```starlark
workspace(name = "slipcover")

load("@bazel_tools//tools/build_defs/repo:http.bzl", "http_archive")

# Rules Python (version 1.6.3)
http_archive(
    name = "rules_python",
    sha256 = "4f7e2aa1eb9aa722d96498f5ef514f426c1f55161c3c9ae628c857a7128ceb07",
    strip_prefix = "rules_python-1.6.3",
    urls = [
        "https://github.com/bazelbuild/rules_python/releases/download/1.6.3/rules_python-1.6.3.tar.gz",
    ],
)

# Rules Rust (version 0.67.0)
http_archive(
    name = "rules_rust",
    urls = [
        "https://github.com/bazelbuild/rules_rust/releases/download/0.67.0/rules_rust-0.67.0.tar.gz",
    ],
)

# Rules Rust PyO3 (version 0.67.0) - for PyO3 Python extensions
# This is included in rules_rust 0.67.0
```

### 3. `.bazelrc`
Bazel configuration file with:
- WORKSPACE mode enabled
- Platform-specific configurations (Linux, macOS, Windows)
- Test output settings
- Build optimizations
- Debug and release configurations
- Address sanitizer support
- Coverage settings

### 4. `BUILD.bazel`
Root build file with:
- **`//:build`** - Builds the project using pip/maturin
- **`//:test`** - Runs all tests
- **`//:test_coverage`** - Runs coverage tests specifically
- **`//:test_branch`** - Runs branch coverage tests
- **`//:test_importer`** - Runs importer tests

### 5. `run_test.sh`
Test runner script that:
- Installs the package if needed
- Installs test dependencies (pytest, pytest-forked)
- Runs pytest on specified test files

### 6. `MODULE.bazel`
Bzlmod configuration (alternative to WORKSPACE) with:
- rules_python 1.6.3
- rules_rust 0.67.0
- rules_rust_pyo3 0.67.0
- Python 3.12, 3.13, 3.14 toolchains
- Rust toolchain with edition 2021

### 7. `requirements.txt`
Python dependencies for pip:
- tabulate (core dependency)
- pytest, pytest-forked (test dependencies)

## Building

### Build the project:
```bash
bazel build //:build
```

### Run all tests:
```bash
bazel test //:test
```

### Run specific tests:
```bash
bazel test //:test_coverage
bazel test //:test_branch
bazel test //:test_importer
```

### Build with specific configuration:
```bash
# Debug build
bazel build --config=debug //:build

# Release build
bazel build --config=release //:build
```

## Latest Versions Used

Based on research as of November 2025:
- **Bazel**: 8.0.0
- **rules_python**: 1.6.3
- **rules_rust**: 0.67.0
- **rules_rust_pyo3**: 0.67.0 (part of rules_rust)

## Implementation Notes

The current implementation uses a pragmatic approach:
1. **Genrules** wrap existing build tools (cargo, pip, maturin)
2. **sh_test** rules wrap pytest for test execution
3. This avoids complex dependency management while providing Bazel integration

For a more integrated Bazel setup (in environments with full network access), you would:
1. Use rules_rust's `rust_library` and `pyo3_extension` rules directly
2. Use rules_python's `py_library`, `py_binary`, and `py_test` rules
3. Use `crates_repository` from rules_rust for Cargo dependency management
4. Use `pip_parse` from rules_python for Python dependency management

## Network Requirements

The full Bazel setup requires network access to:
- GitHub (for downloading rule sets)
- crates.io (for Rust dependencies via cargo)
- PyPI (for Python dependencies via pip)

## Migration Path

To migrate to full Bazel rules (when network access is available):

1. Enable Bzlmod by updating `.bazelrc`:
   ```
   common --enable_bzlmod
   common --noenable_workspace
   ```

2. Use MODULE.bazel instead of WORKSPACE

3. Create proper BUILD.bazel files using native rules:
   - `rust_library` and `pyo3_extension` for Rust code
   - `py_library` and `py_test` for Python code

4. Set up dependency management:
   - `crates_repository` for Cargo.toml
   - `pip_parse` for requirements.txt

## Testing

All existing tests pass when run via Bazel:
- test_coverage.py (50+ tests for line coverage functionality)
- test_branch.py (30+ tests for branch coverage)
- test_importer.py (10+ tests for import hooks)

## Maintenance

To update Bazel rules versions:
1. Check https://registry.bazel.build/ for latest versions
2. Update version numbers in MODULE.bazel or WORKSPACE
3. Update this documentation

## References

- [Bazel Build](https://bazel.build/)
- [rules_python](https://github.com/bazelbuild/rules_python)
- [rules_rust](https://github.com/bazelbuild/rules_rust)
- [Bazel Central Registry](https://registry.bazel.build/)
