from __future__ import annotations

import dis
import types
from typing import TYPE_CHECKING

# Import from Rust
from .covers_core import (  # noqa: F401
    Covers,
    CoverageTracker,
    PathSimplifier,
    add_summaries,
    branches_from_code,
    decode_branch,
    encode_branch,
    is_branch,
    lines_from_code,
    print_coverage,
    print_xml,
    __version__,
)

# FIXME provide __all__

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
    from typing import List


class CoversError(Exception):
    pass


def format_missing(
    missing_lines: List[int], executed_lines: List[int], missing_branches: List[tuple]
) -> str:
    """Formats ranges of missing lines, including non-code (e.g., comments) ones that fall
    between missed ones"""

    missing_set = set(missing_lines)
    missing_branches = [
        (a, b)
        for a, b in missing_branches
        if a not in missing_set and b not in missing_set
    ]

    def format_branch(br):
        return f"{br[0]}->exit" if br[1] == 0 else f"{br[0]}->{br[1]}"

    def find_ranges():
        executed = set(executed_lines)
        it = iter(missing_lines)  # assumed sorted
        a = next(it, None)
        while a is not None:
            while missing_branches and missing_branches[0][0] < a:
                yield format_branch(missing_branches.pop(0))

            b = a
            n = next(it, None)
            while n is not None:
                if any(line in executed for line in range(b + 1, n + 1)):
                    break

                b = n
                n = next(it, None)

            yield str(a) if a == b else f"{a}-{b}"

            a = n

        while missing_branches:
            yield format_branch(missing_branches.pop(0))

    return ", ".join(find_ranges())


# print_xml is now implemented in Rust (imported above)


# print_coverage is now implemented in Rust (imported above)


def merge_coverage(a: dict, b: dict) -> dict:
    """Merges coverage result 'b' into 'a'."""

    if a.get("meta", {}).get("software", None) != "covers":
        raise CoversError("Cannot merge coverage: only Covers format supported.")

    if a.get("meta", {}).get("show_contexts", False) or b.get("meta", {}).get(
        "show_contexts", False
    ):
        raise CoversError("Merging coverage with show_contexts=True unsupported")

    branch_coverage = a.get("meta", {}).get("branch_coverage", False)
    if branch_coverage and not b.get("meta", {}).get("branch_coverage", False):
        raise CoversError("Cannot merge coverage: branch coverage missing")

    a_files = a["files"]
    b_files = b["files"]

    def both(f, field):
        return (a_files[f][field] if f in a_files else []) + b_files[f][field]

    for f in b_files:
        executed_lines = set(both(f, "executed_lines"))
        missing_lines = set(both(f, "missing_lines"))
        missing_lines -= executed_lines
        update = {
            "executed_lines": sorted(executed_lines),
            "missing_lines": sorted(missing_lines),
        }

        if branch_coverage:
            executed_branches = set(tuple(br) for br in both(f, "executed_branches"))
            missing_branches = set(tuple(br) for br in both(f, "missing_branches"))
            missing_branches -= executed_branches
            update.update(
                {
                    "executed_branches": sorted(list(br) for br in executed_branches),
                    "missing_branches": sorted(list(br) for br in missing_branches),
                }
            )

        a_files[f] = update

    add_summaries(a)
    return a


# The Covers class is now implemented in Rust (covers_core)
# All methods are available from the imported class above
