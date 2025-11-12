"""Minimal Python runner for covers - handles script execution with instrumentation.

This module is called from the Rust CLI and handles the parts that must be in Python:
- Import hooks for code instrumentation
- AST transformation for branch coverage
- Script/module execution in the correct Python context
- Fork/exit handling for multiprocess coverage
"""

import os
import functools
import tempfile
import json
import warnings

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
        # Convert CoverageData to dict for merging
        if hasattr(cov, "to_dict"):
            cov_dict = cov.to_dict()
        else:
            cov_dict = cov

        for fname in input_tmpfiles:
            try:
                with open(fname, "r", encoding="utf-8") as f:
                    f.seek(0, os.SEEK_END)
                    # If the file is empty, it was likely closed, possibly upon exec
                    if f.tell() != 0:
                        f.seek(0)
                        cov_dict = sc.merge_coverage(cov_dict, json.load(f))
            except (json.JSONDecodeError, OSError, ValueError) as e:
                # OSError/ValueError can occur if the file was corrupted or deleted
                if isinstance(e, json.JSONDecodeError):
                    warnings.warn(f"Error reading {fname}: {e}")
            finally:
                try:
                    os.remove(fname)
                except (FileNotFoundError, OSError):
                    pass

        # Convert back to CoverageData if we had input files
        cov = sc.CoverageData.load_from_dict(cov_dict)

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
                    cov = get_coverage(sci)
                    # Convert CoverageResults to dict for JSON serialization
                    if hasattr(cov, "to_dict"):
                        cov = cov.to_dict()
                    json.dump(cov, f)
            except (OSError, ValueError):
                # File may not be writable if descriptor was closed (e.g., via closerange)
                pass

        original_exit(*pargs, **kwargs)

    return wrapper


# run_with_coverage and merge_coverage_files have been moved to Rust (src_rust/cli.rs)
# This file now only contains helper functions used by the Rust implementation
