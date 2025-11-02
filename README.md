# Covers: Near Zero-Overhead Python Code Coverage

Covers is a fast Python code coverage tool, originally based on [SlipCover](httpshttps://github.com/plasma-umass/slipcover).
This version has been re-written as a Rust / PyO3 extension for improved performance and maintainability.

[![license](https://img.shields.io/github/license/amartani/covers?color=blue)](LICENSE)
[![pypi](https://img.shields.io/pypi/v/covers?color=blue)](https://pypi.org/project/covers/)
![pyversions](https://img.shields.io/pypi/pyversions/covers?logo=python&logoColor=FBE072)
![tests](https://github.com/amartani/covers/workflows/tests/badge.svg)

## About Covers
Covers is a fast [code coverage](https://en.wikipedia.org/wiki/Code_coverage) tool.
It tracks a Python program as it runs and reports on the parts that executed and
those that didn't.
That can help guide your testing (showing code that isn't being tested), debugging,
[fuzzing](https://en.wikipedia.org/wiki/Fuzzing) or to find "dead" code.

Past code coverage tools can make programs significantly slower;
it is not uncommon for them to take twice as long to execute.
Covers aims to provide the same information with **near-zero overhead**, often
almost as fast as running the original Python program.

### How it works
Previous coverage tools like [Coverage.py](https://github.com/nedbat/coveragepy) rely on 
[Python's tracing facilities](https://docs.python.org/3/library/sys.html?highlight=settrace#sys.settrace),
which add significant overhead.
Instead, Covers uses the new
[`sys.monitoring`](https://docs.python.org/3.12/library/sys.monitoring.html) API
to collect coverage information with minimal performance impact. The core data collection
is implemented in Rust for maximum efficiency.

## Getting started
Covers is available from [PyPI](https://pypi.org/project/covers).
You can install it like any other Python module with
```console
pip3 install covers
```

You could then run your Python script with:
```console
python3 -m covers myscript.py
```

### Using it with a test harness
Covers can also execute a Python module, as in:
```console
python3 -m covers -m pytest -x -v
```
which starts `pytest`, passing it any options (`-x -v` in this example)
after the module name.
No plug-in is required for pytest.

## Platforms
Our GitHub workflows run the automated test suite on Linux, MacOS and Windows, but
really it should work anywhere where CPython does.

## Contributing
Covers is under active development; contributions are welcome!
Please also feel free to [create a new issue](https://github.com/amartani/covers/issues/new)
with any suggestions or issues you may encounter.
