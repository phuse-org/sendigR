"""Setup script for xptcleaner"""

# Standard library imports
import pathlib
import os

# Third party imports
from setuptools import setup
from setuptools import setup, find_namespace_packages, find_packages

# Utility function to read the README.md file.
# Used for the long_description.  It's nice, because now 1) we have a top level
# README.md file and 2) it's easier to type in the README.md file than to put a raw
# string in below ...
def read(fname):
    return open(os.path.join(os.path.dirname(__file__), fname)).read()

# The directory containing this file
HERE = pathlib.Path(__file__).resolve().parent

# The text of the README file is used as a description
README = (HERE / "README.md").read_text()

# This call to setup() does all the work
setup(
    name="xptcleaner",
    version="1.0.0",
    description="CDISC SEND xpt (SAS v5 Transport format) files cleaner",
    long_description=read('README.md'),
    long_description_content_type="text/markdown",
    url="https://github.com/phuse-org/sendigR/tree/main/python/xptcleaner",
    author="Cmsabbir Ahmed, Yousuf Ali, Susan Butler, Michael Denieu, William Houser, Brianna Paisley, Michael Rosentreter, Kevin Snyder, Wenxian Wang",
    author_email="cmsabbir.ahmed@fda.hhs.gov, md.ali@fda.hhs.gov, susan.butler@fda.hhs.gov, michael.denieu@labcorp.com, william.houser@bms.com, paisley_brianna_meadow@lilly.com, michael.rosentreter@bayer.com, kevin.snyder@fda.hhs.gov, wenxian.wang@bms.com",
    license=read("LICENSE"),
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