"""CDISC XPT files cleaner.
Import the modules to work with the XPTCleaner:
"""
import yaml

try:
    from importlib import resources
except ImportError:
    import importlib_resources as resources

from configparser import ConfigParser

# Supress warnings ....
import warnings
warnings.filterwarnings("ignore")

# Version of xptcleaner package
__version__ = "1.0.0"

# Read yaml config file
with resources.path("xptcleaner", "config.yaml") as path:
    f = open(str(path))
    CONFIG = yaml.load(f, Loader=yaml.FullLoader)
    print(CONFIG)
    f.close()
