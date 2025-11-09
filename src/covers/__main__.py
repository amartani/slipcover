"""Covers CLI entry point.

This module is a minimal wrapper around the Rust-based CLI implementation.
All heavy lifting (argument parsing, coverage collection, reporting) is done in Rust.
"""

import sys
from covers.covers_core import main_cli


def main():
    """Main entry point for the covers CLI.

    This function delegates to the Rust implementation which handles:
    - Argument parsing
    - Coverage file merging
    - Script/module execution with instrumentation
    - Coverage reporting

    Returns:
        Exit code (0 for success, 1 for error, 2 for fail-under threshold)
    """
    return main_cli(sys.argv)


if __name__ == "__main__":
    raise SystemExit(main())
