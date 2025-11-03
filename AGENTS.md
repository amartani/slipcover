# Workflow
- Run `mise run build` to build the project.
- Run `mise run lint` to run linters after every edit.
- Run `mise run test` to run the entire test suite.
- Run `mise run test tests/test_coverage.py` to run a specific test file
- Run `mise run test_all_versions` to run tests against all suported versions of Python. Only run this when explicitly dealing with differences in behavior between Python versions.