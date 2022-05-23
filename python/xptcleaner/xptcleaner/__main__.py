"""Main entry point for the XPT Cleaner.
"""
# Standard library imports
import sys
from importlib import resources
# xptcleaner and module xptclean import
import xptcleaner
from xptcleaner import xptclean

def main() -> None:
    print("running gen_vacab")
    # if multiple files provided and the same Synonym exists in both files and map to different submission values,
    # the one in the second file will overwrite the mapping in the first file
    # so you want to pass the CDISC published file as the last file in the list
    #xptclean.gen_vocab(["C:/Project/src/R/sendigRPkg/SEND_Terminology_EXTENSIBLE.txt",
    #          "C:/Project/src/R/sendigRPkg/SEND Terminology_2021_12_17.txt"],
    #          "C:/Project/src/R/sendigRPkg/SEND3.1_test.json")

    #print("running standardize_file")
    #jsonfile = "C:/Project/src/R/sendigRPkg/SEND3.1_test.json"
    #rawXptFolder = "C:/BioCelerate/TDSStudies/43066/"
    #cleanXptFolder = "C:/BioCelerate/TDSStudiesClean/43066/"
    #xptclean.standardize_file(rawXptFolder, cleanXptFolder, jsonfile)

if __name__ == "__main__":
    main()
