#!/bin/bash
# Test runner script for Bazel

set -e

# Install the package if not already installed
if ! python -c "import slipcover" 2>/dev/null; then
    echo "Installing slipcover..."
    pip install -q -e .
fi

# Install test dependencies
pip install -q pytest pytest-forked 2>/dev/null || true

# Run the requested test(s)
if [ $# -eq 0 ]; then
    # Run all tests
    pytest tests/ -v
else
    # Run specific test file
    pytest "$@" -v
fi
