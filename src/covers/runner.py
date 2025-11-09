"""Minimal Python runner for covers - handles script execution with instrumentation.

This module is called from the Rust CLI and handles the parts that must be in Python:
- Import hooks for code instrumentation
- AST transformation for branch coverage
- Script/module execution in the correct Python context
- Coverage file merging
"""

import sys
import os
import functools
import tempfile
import json
import warnings
from pathlib import Path
from typing import Any, Dict

import covers as sc


# Used for fork() support
input_tmpfiles = []
output_tmpfile = None


def fork_shim(sci):
    """Shims os.fork(), preparing the child to write its coverage to a temporary file
    and the parent to read from that file, so as to report the full coverage obtained.
    """
    original_fork = os.fork

    @functools.wraps(original_fork)
    def wrapper(*pargs, **kwargs):
        global input_tmpfiles, output_tmpfile

        # Create temp file and immediately close the file object to avoid __del__ issues
        tmp_file = tempfile.NamedTemporaryFile(
            mode="r+", encoding="utf-8", delete=False
        )
        tmp_name = tmp_file.name
        tmp_file.close()  # Close the file object (not the FD) to prevent __del__ issues

        if pid := original_fork(*pargs, **kwargs):
            # Parent process - save filename for reading later
            input_tmpfiles.append(tmp_name)
        else:
            # Child process
            sci.signal_child_process()
            input_tmpfiles.clear()  # to be used by this process' children, if any
            output_tmpfile = tmp_name

        return pid

    return wrapper


def get_coverage(sci):
    """Combines this process' coverage with that of any previously forked children."""
    global input_tmpfiles, output_tmpfile

    cov = sci.get_coverage()
    if input_tmpfiles:
        for fname in input_tmpfiles:
            try:
                with open(fname, "r", encoding="utf-8") as f:
                    f.seek(0, os.SEEK_END)
                    # If the file is empty, it was likely closed, possibly upon exec
                    if f.tell() != 0:
                        f.seek(0)
                        sc.merge_coverage(cov, json.load(f))
            except (json.JSONDecodeError, OSError, ValueError) as e:
                # OSError/ValueError can occur if the file was corrupted or deleted
                if isinstance(e, json.JSONDecodeError):
                    warnings.warn(f"Error reading {fname}: {e}")
            finally:
                try:
                    os.remove(fname)
                except (FileNotFoundError, OSError):
                    pass

    return cov


def exit_shim(sci):
    """Shims os._exit(), so a previously forked child process writes its coverage to
    a temporary file read by the parent.
    """
    original_exit = os._exit

    @functools.wraps(original_exit)
    def wrapper(*pargs, **kwargs):
        global output_tmpfile

        if output_tmpfile:
            try:
                with open(output_tmpfile, "w", encoding="utf-8") as f:
                    json.dump(get_coverage(sci), f)
            except (OSError, ValueError):
                # File may not be writable if descriptor was closed (e.g., via closerange)
                pass

        original_exit(*pargs, **kwargs)

    return wrapper


# run_with_coverage has been moved to Rust (src_rust/cli.rs)
# This file now only contains helper functions used by the Rust implementation


def merge_coverage_files(args: Dict[str, Any]) -> int:
    """Merge coverage files and output the result.

    Args:
        args: Dictionary of command-line arguments from Rust CLI

    Returns:
        Exit code (0 for success, 1 for error, 2 for fail-under threshold)
    """
    merge_files = args.get("merge", [])
    if isinstance(merge_files, str):
        merge_files = [merge_files]

    # Convert string paths to Path objects if needed
    merge_files = [Path(f) if isinstance(f, str) else f for f in merge_files]

    base_path = merge_files[0].parent if merge_files else Path(".")

    try:
        with merge_files[0].open() as jf:
            merged = json.load(jf)
    except Exception as e:
        warnings.warn(f"Error reading {merge_files[0]}: {e}")
        return 1

    try:
        for f in merge_files[1:]:
            with f.open() as jf:
                sc.merge_coverage(merged, json.load(jf))
    except Exception as e:
        warnings.warn(f"Error merging {f}: {e}")
        return 1

    out_file = Path(args["out"]) if isinstance(args["out"], str) else args["out"]

    try:
        with out_file.open("w", encoding="utf-8") as jf:
            if args.get("xml"):
                sc.print_xml(
                    merged,
                    source_paths=[str(base_path)],
                    with_branches=args.get("branch", False),
                    xml_package_depth=args.get("xml_package_depth", 99),
                    outfile=jf,
                )
            elif args.get("lcov"):
                sc.print_lcov(
                    merged,
                    source_paths=[str(base_path)],
                    with_branches=args.get("branch", False),
                    outfile=jf,
                )
            else:
                json.dump(merged, jf, indent=(4 if args.get("pretty_print") else None))

        # Print human-readable table for merge results
        if not args.get("silent"):
            sc.print_coverage(
                merged,
                outfile=sys.stdout,
                skip_covered=args.get("skip_covered", False),
                missing_width=args.get("missing_width", 80),
            )

    except Exception as e:
        warnings.warn(str(e))
        return 1

    if args.get("fail_under", 0.0) > 0:
        if merged["summary"]["percent_covered"] < args["fail_under"]:
            return 2

    return 0
