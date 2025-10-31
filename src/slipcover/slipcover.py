from __future__ import annotations

import sys
from collections import defaultdict
from typing import TYPE_CHECKING

from .version import __version__
from .xmlreport import XmlReporter

# Import from Rust
from .slipcover_core import (
    Slipcover,
    CoverageTracker,
    PathSimplifier,
    is_branch,
    encode_branch,
    decode_branch,
    lines_from_code,
    branches_from_code,
)

# FIXME provide __all__

if TYPE_CHECKING:
    from typing import Dict, Iterable, Iterator, List, Optional, Tuple

    from .schemas import Coverage

class SlipcoverError(Exception):
    pass 


def format_missing(missing_lines: List[int], executed_lines: List[int],
                   missing_branches: List[tuple]) -> str:
    """Formats ranges of missing lines, including non-code (e.g., comments) ones that fall
       between missed ones"""

    missing_set = set(missing_lines)
    missing_branches = [(a,b) for a,b in missing_branches if a not in missing_set and b not in missing_set]

    def format_branch(br):
        return f"{br[0]}->exit" if br[1] == 0 else f"{br[0]}->{br[1]}"

    def find_ranges():
        executed = set(executed_lines)
        it = iter(missing_lines)    # assumed sorted
        a = next(it, None)
        while a is not None:
            while missing_branches and missing_branches[0][0] < a:
                yield format_branch(missing_branches.pop(0))

            b = a
            n = next(it, None)
            while n is not None:
                if any(l in executed for l in range(b+1, n+1)):
                    break

                b = n
                n = next(it, None)

            yield str(a) if a == b else f"{a}-{b}"

            a = n

        while missing_branches:
            yield format_branch(missing_branches.pop(0))

    return ", ".join(find_ranges())

def print_xml(
    coverage: Coverage,
    source_paths: Iterable[str],
    *,
    with_branches: bool = False,
    xml_package_depth: int = 99,
    outfile=sys.stdout
) -> None:
    XmlReporter(
        coverage=coverage,
        source=source_paths,
        with_branches=with_branches,
        xml_package_depth=xml_package_depth,
    ).report(outfile=outfile)


def print_coverage(coverage, *, outfile=sys.stdout, missing_width=None, skip_covered=False) -> None:
    """Prints coverage information for human consumption."""
    from tabulate import tabulate

    if not coverage.get('files', None): # includes empty coverage['files']
        return

    branch_coverage = coverage.get('meta', {}).get('branch_coverage', False)

    def table():
        for f, f_info in sorted(coverage['files'].items()):
            exec_l = len(f_info['executed_lines'])
            miss_l = len(f_info['missing_lines'])

            if branch_coverage:
                exec_b = len(f_info['executed_branches'])
                miss_b = len(f_info['missing_branches'])
                pct_b = 100*exec_b/(exec_b+miss_b) if (exec_b+miss_b) else 0

            pct = f_info['summary']['percent_covered']

            if skip_covered and pct == 100.0:
                continue

            yield [f, exec_l+miss_l, miss_l,
                   *([exec_b+miss_b, miss_b, round(pct_b)] if branch_coverage else []),
                   round(pct),
                   format_missing(f_info['missing_lines'], f_info['executed_lines'],
                                  f_info['missing_branches'] if 'missing_branches' in f_info else [])]

        if len(coverage['files']) > 1:
            yield ['---'] + [''] * (6 if branch_coverage else 4)

            s = coverage['summary']

            if branch_coverage:
                exec_b = s['covered_branches']
                miss_b = s['missing_branches']
                pct_b = 100*exec_b/(exec_b+miss_b) if (exec_b+miss_b) else 0

            yield ['(summary)', s['covered_lines']+s['missing_lines'], s['missing_lines'],
                   *([exec_b+miss_b, miss_b, round(pct_b)] if branch_coverage else []),
                   round(s['percent_covered']), '']



    print("", file=outfile)
    headers = ["File", "#lines", "#l.miss",
               *(["#br.", "#br.miss", "brCov%", "totCov%"] if branch_coverage else ["Cover%"]),
               "Missing"]
    maxcolwidths = [None] * (len(headers)-1) + [missing_width]
    print(tabulate(table(), headers=headers, maxcolwidths=maxcolwidths), file=outfile)


def add_summaries(cov: dict) -> None:
    """Adds (or updates) 'summary' entries in coverage information."""
    # global summary
    g_summary : dict = defaultdict(int)
    g_nom = g_den = 0

    if 'files' in cov:
        for f_cov in cov['files'].values():
            summary : dict = { # per-file summary
                'covered_lines': len(f_cov['executed_lines']),
                'missing_lines': len(f_cov['missing_lines']),
            }

            nom = summary['covered_lines']
            den = nom + summary['missing_lines']

            if 'executed_branches' in f_cov:
                summary.update({
                    'covered_branches': len(f_cov['executed_branches']),
                    'missing_branches': len(f_cov['missing_branches'])
                })

                nom += summary['covered_branches']
                den += summary['covered_branches'] + summary['missing_branches']

            summary['percent_covered'] = 100.0 if den == 0 else 100*nom/den
            f_cov['summary'] = summary

            for k in summary:
                g_summary[k] += summary[k]
            g_nom += nom
            g_den += den

    g_summary['percent_covered'] = 100.0 if g_den == 0 else 100*g_nom/g_den
    g_summary['percent_covered_display'] = str(int(round(g_summary['percent_covered'], 0)))
    cov['summary'] = g_summary


def merge_coverage(a: dict, b: dict) -> dict:
    """Merges coverage result 'b' into 'a'."""

    if a.get('meta', {}).get('software', None) != 'slipcover':
        raise SlipcoverError('Cannot merge coverage: only SlipCover format supported.')

    if a.get('meta', {}).get('show_contexts', False) or \
       b.get('meta', {}).get('show_contexts', False):
        raise SlipcoverError('Merging coverage with show_contexts=True unsupported')

    branch_coverage = a.get('meta', {}).get('branch_coverage', False)
    if branch_coverage and not b.get('meta', {}).get('branch_coverage', False):
        raise SlipcoverError('Cannot merge coverage: branch coverage missing')

    a_files = a['files']
    b_files = b['files']

    def both(f, field):
        return (a_files[f][field] if f in a_files else []) + b_files[f][field]

    for f in b_files:
        executed_lines = set(both(f, 'executed_lines'))
        missing_lines = set(both(f, 'missing_lines'))
        missing_lines -= executed_lines
        update = {
            'executed_lines': sorted(executed_lines),
            'missing_lines': sorted(missing_lines)
        }

        if branch_coverage:
            executed_branches = set(tuple(br) for br in both(f, 'executed_branches'))
            missing_branches = set(tuple(br) for br in both(f, 'missing_branches'))
            missing_branches -= executed_branches
            update.update({
                'executed_branches': sorted(list(br) for br in executed_branches),
                'missing_branches': sorted(list(br) for br in missing_branches)
            })

        a_files[f] = update

    add_summaries(a)
    return a


# The Slipcover class is now implemented in Rust (slipcover_core)
# All methods are available from the imported class above
