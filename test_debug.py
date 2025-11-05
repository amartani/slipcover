import inspect
from src.covers.branch import preinstrument
from src.covers.covers_core import analyze_branches_ts

source = """
if x == 0:
    x += 1
elif x == 1:
    x += 2
else:
    x += 3

x += 3
"""
source = inspect.cleandoc(source)

print("Source:")
for i, line in enumerate(source.splitlines(), 1):
    print(f"{i}: {line}")

print("\nBranch data from analyze_branches_ts:")
branch_data = analyze_branches_ts(source)
for branch_line, markers in sorted(branch_data.items()):
    print(f"Line {branch_line}: {markers}")
