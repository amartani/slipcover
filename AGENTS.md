# Workflow
- Run `mise run build` to build the project.
- Run `mise run lint` to run linters after every edit.
- Run `mise run test` to run the entire test suite.
- Run `mise run test tests/test_coverage.py` to run a specific test file
- Run `mise run test_all_versions` to run tests against all suported versions of Python. Only run this when explicitly dealing with differences in behavior between Python versions.

# Python to Rust Conversion Guidelines
When converting Python code to Rust, follow these guidelines:
- Always remove the original Python implementations after converting to Rust; do not keep a fallback Python implementation.
- Never call Python code from Rust; when converting a function, always convert all functions that are called from it.
- Avoid using PyDict / PyList and similar; prefer to use pure-Rust equivalents. Use ahash in place of dicts, tree-sitter in place of ast, and similar.
- When adding a crate, always verify that you are using the latest available version.