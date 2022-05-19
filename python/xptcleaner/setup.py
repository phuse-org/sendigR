"""Setup script for xptcleaner"""

# Standard library imports
import pathlib

# Third party imports
from setuptools import setup
from setuptools import setup, find_namespace_packages, find_packages

# The directory containing this file
HERE = pathlib.Path(__file__).resolve().parent

# The text of the README file is used as a description
README = (HERE / "README.md").read_text()

# This call to setup() does all the work
setup(
    name="xptcleaner",
    version="1.0.0",
    description="CDISC SEND XPT files cleaner",
    long_description=README,
    long_description_content_type="text/markdown",
    url="https://github.com/phuse-org/sendigR/tree/main/python/XPTcleaner",
    author=["Brianna Paisley", "Wenxian Wang"],
    author_email=["paisley_brianna_meadow@lilly.com", "wenxian.wang@bms.com"],
    license="MIT",
    classifiers=[
        "License :: OSI Approved :: MIT License",
        "Programming Language :: Python",
        "Programming Language :: Python :: 3",
    ],
    packages=find_packages(include=['xptcleaner', 'xptcleaner.*', 
    'xptcleaner.cdisc_mapping.*','xptcleaner.cdisc_mapping.file_mapping.*', 
    'xptcleaner.cdisc_mapping.vocab_management.*', 
    'xptcleaner.cdisc_mapping.logger.*',
    'xptcleaner.cdisc_mapping.utils.*']),
    include_package_data=True,
    install_requires=["pandas", "pyreadstat"],
    entry_points={"console_scripts": ["xptceaner=xptceaner.__main__:main"]},
)
