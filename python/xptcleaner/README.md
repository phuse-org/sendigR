# xptcleaner

* xptcleaner package provides function for creating json file for vocabulary mappings. Keys for the mapping are synonyms and values are the CDISC Controlled Terminology Submission values. Vocabularies are read by column values from the tab-delimited files, published by CDISC CT team or user defined extensible term.

* xptcleaner package provides functions for Standardizing SEND xpt files using CDISC controlled terminologies.

## Installation
### Using pip

Probably the easiest way: from your conda, virtualenv or just base installation do:

```
pip install xptcleaner
```

If you are running on a machine without admin rights, and you want to install against your base installation you can do:

```
pip install xptcleaner --user
```
### Using source archive or using wheel file
You can choose to install xptcleaner using source archive or using wheel file.

* Using source archive:
Using the below shell command to install the xptcleaner package, assume that the source archive is under 'dist' sub folder.

```
$ py -m pip install ./dist/xptcleaner-1.0.0.tar.gz

```
* Using wheel:
Using the below shell command to install the xptcleaner package, assume that the wheel file is under 'dist' sub folder.

```
$ py -m pip install ./dist/xptcleaner-1.0.0-py3-none-any.whl

```
The following required python packages will be installed during the xptcleaner package installation:<br>

    * pandas
    
    * pyreadstat

## How to use
xptcleaner can be used from python script and from R script. 

### Use xptcleaner from python script

```python
# xptcleaner and module xptclean import
import xptcleaner
from xptcleaner import xptclean

#input CDISC and Extensible CT files.
infile1="C:/Project/src/R/sendigRPkg/SEND Terminology_2021_12_17.txt"
infile2="C:/Project/src/R/sendigRPkg/SEND_Terminology_EXTENSIBLE.txt"
#output JSON file
jsonfile="C:/Project/src/R/sendigRPkg/SENDct.json"

#Call the gen_vocab function with the input and output files
xptclean.gen_vocab([infile1,infile1],jsonfile)

#Call the standardize_file function to clean the xpt file
rawXptFolder = "C:/BioCelerate/TDSStudies/96298/"
cleanXptFolder = "C:/BioCelerate/TDSStudiesClean/96298/"
xptclean.standardize_file(rawXptFolder, cleanXptFolder, jsonfile)

```

### Use xptcleaner from R script

xptcleaner is integrated with sendigR package. refer to installation and usage on [sendigR](https://github.com/phuse-org/sendigR).
