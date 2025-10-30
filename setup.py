import setuptools
import sys
import os
import re
from pathlib import Path
import tomllib


def get_version():
    v = re.findall(r"^__version__ *= *\"([^\"]+)\"", Path("src/slipcover/version.py").read_text())[0]
    return v


def get_dev_build():
    # If we're testing packaging, build using a ".devN" suffix in the version number,
    # so that we can upload new files (as testpypi/pypi don't allow re-uploading files with
    # the same name as previously uploaded).
    # Numbering scheme: https://www.python.org/dev/peps/pep-0440
    return '.dev' + Path('dev-build.txt').read_text().strip() if Path('dev-build.txt').exists() else ''


def get_url():
    return tomllib.loads(Path("pyproject.toml").read_text())['project']['urls']['Repository']


def get_description():
    text = Path("README.md").read_text(encoding="utf-8")

    # Rewrite any relative paths to version-specific absolute paths,
    # so that they work from within PyPI
    sub = r'\1' + get_url() + "/blob/v" + get_version() + r'/\2'
    text = re.sub(r'(src=")((?!https?://))', sub, text)
    text = re.sub(r'(\[.*?\]\()((?!https?://))', sub, text)

    return text


def bdist_wheel_options():
    # For Python 3.12 onwards, we're a pure Python distribution
    options = {'python_tag': 'py312'}  # this requires 3.12+
    return options


setuptools.setup(
    version=get_version() + get_dev_build(),
    long_description=get_description(),
    long_description_content_type="text/markdown",
    options={'bdist_wheel': bdist_wheel_options()}
)
