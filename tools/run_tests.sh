#!/bin/bash
# Test runner script for Bazel

set -e

# Install test dependencies
pip install -q pytest pytest-forked

# Run pytest
cd "$(dirname "$0")/.."
pytest tests/ -v

echo "All tests passed!"
