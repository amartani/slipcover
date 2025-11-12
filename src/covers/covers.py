from __future__ import annotations

import dis
import types
from typing import TYPE_CHECKING

# Import from Rust
from .covers_core import (  # noqa: F401
    Covers,
    CoverageData,
    CoverageTracker,
    PathSimplifier,
    add_summaries,
    branches_from_code,
    decode_branch,
    encode_branch,
    format_missing_py as format_missing,
    is_branch,
    lines_from_code,
    merge_coverage,
    print_coverage,
    print_xml,
    print_lcov,
    __version__,
    # Bytecode classes
    Branch,
    Editor,
    ExceptionTableEntry,
    LineEntry,
    # Exceptions
    CoversError,
)

__all__ = [
    # Core classes (from Rust)
    "Covers",
    "CoverageTracker",
    "PathSimplifier",
    # Branch functions (from Rust)
    "encode_branch",
    "decode_branch",
    "is_branch",
    # Code analysis functions (from Rust)
    "lines_from_code",
    "branches_from_code",
    # Reporting functions (from Rust)
    "add_summaries",
    "print_coverage",
    "print_xml",
    "print_lcov",
    # Bytecode classes (from Rust)
    "Branch",
    "Editor",
    "ExceptionTableEntry",
    "LineEntry",
    # Python utilities
    "findlinestarts",
    "format_missing",
    "merge_coverage",
    # Exceptions
    "CoversError",
    # Version
    "__version__",
]

# Python 3.13 returns 'None' lines;
# Python 3.11+ generates a line just for RESUME or RETURN_GENERATOR, POP_TOP, RESUME;
# Python 3.11 generates a 0th line
_op_RESUME = dis.opmap["RESUME"]
_op_RETURN_GENERATOR = dis.opmap["RETURN_GENERATOR"]


def findlinestarts(co: types.CodeType):
    for off, line in dis.findlinestarts(co):
        if line and co.co_code[off] not in (_op_RESUME, _op_RETURN_GENERATOR):
            yield off, line


if TYPE_CHECKING:
    pass


# format_missing is now implemented in Rust (imported above)
# print_xml is now implemented in Rust (imported above)
# print_coverage is now implemented in Rust (imported above)
# merge_coverage is now implemented in Rust (imported above)
# CoversError is now implemented in Rust (imported above)


# The Covers class is now implemented in Rust (covers_core)
# All methods are available from the imported class above
