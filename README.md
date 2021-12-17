[![MIT licensed](https://img.shields.io/badge/license-MIT-blue.svg)](https://github.com/phuse-org/phuse-scripts/blob/master/LICENSE.md) 

# BioCelerate Script Repository

The search scripts shared in this folder are intended to be used to query and collate information from 
SEND datasets which can then be utilized by the searcher for their own cross-study analysis.  Nothing 
in these scripts in intended to guide the analytic process and any interpretations of data found as a 
result of using these scripts are solely the responsibility of the user of the scripts and not the developers.

The scripts in this folder are subject to the MIT Open Source License:

MIT License

Copyright (c) 2019 PhUSEPermission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in
all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
THE SOFTWARE.


## Script overview

The scripts in this package have been designed and are maintined by [BioCelerate](https://transceleratebiopharmainc.com/biocelerate/) in collaboration with [phuse nonclinical topics working group](https://www.phusewiki.org/wiki/index.php?title=Nonclinical_Topics_Working_Group).

If you have questions or you're interested in contributing to the scripts, please contact any of the GitHub contributors listed on the right of this page.   


## Getting started

### Dependencies 

The scripts in this repository reply on the following external packages that need to be installed into the R programming environment:

* data.table
* tidyverse
* RSQLite 

### Configuration

Prior to using these scripts scripts, a configuration file named `sysParameters.R` needs to be created in the home directory of the scripts. In 
this file three variables should be named declaring the path of the folder containing the SEND study datasets, the folder in this repository called `metadata/` 
and the location of the SQLite database. This file should look like so:

sysParameters.R
```
library(tools)

# 
studyRoot <- "/PATH/TO/SEND/DATASETS"
metadataRoot <- file.path(file_path_as_absolute("."), 'metadata')

# SEND data base name and location
dbFullName <- file.path("/PATH/TO/SQLite", "send.db")

```

Alternatively, this file can be created by running the config.R script from the command line like so:

```
RScript.R config.R
```

or normally as a script within R Studio.

You will be prompted to provide the location of the SEND dataset folder as well as where you would like to store the SQLite database.

The SEND dataset folder structure should look like this:
<pre>
/PATH/TO/SEND/DATASETS  
+-- STUDYID1  
   |   +-- ts.xpt  
   |   +-- dm.xpt  
   |   +-- ex.xpt  
   |   +-- etc.  
+-- STUDYID2   
   |   +-- ts.xpt  
   |   +-- dm.xpt  
   |   +-- ex.xpt  
   |   +-- etc.  
+-- STUDYID3  
   |   +-- ts.xpt  
   |   +-- dm.xpt  
   |   +-- ex.xpt  
   |   +-- etc.  
</pre>
However, the subdirectories within `/PATH/TO/SEND/DATASETS`  can be titled anything and not necessarily named by their STUDYID.

### Creating the SQLite database.

The scripts contained within this repository rely on the creation of a SQLite database storing multiple study and domain records. 
Provided in the repository are two scripts that will create this database for you. 

`poolAllStudyDomains.R` will populate the database and assumes that the studies provided within  `/PATH/TO/SEND/DATASETS` contain minimal 
errors.  

`poolAllStudyDomainsStrict.R` will populate the database, but is strict and restrictive to include studies in the SQLIte database unless they meet
 the following criteria: 

1) contains a TS domain
2) contains a DM domain
3) contains an EX domain
4) has readable .xpt files for all domains in the study
5) containes domain variables only allowed in SEND IG 3.0 and 3.1
6) has one unique STUDYID across all domains submitted.


### Script overview.

The following R modules each contain functions for querying and manipulating the SQLite SEND database. Further documentation and usage can be found within each module. 

An example use case can be found in the script `useCaseQuestionMiFindings.R` and the accompanying data flow is outlined the following schematic. 


![Min Findings](useCaseQuestionMiFindings_flow.png "MI Findings")



### Script Directory 

| Script  | Description |
|---|---|
| useCaseQuestionMiFindings.R  |  Example use case script. |
| importSENDDomains.R  | Import a set of specified SEND domains from a SQLite database.  |
| poolAllStudyDomains.R  |  Create SQLite database. |
| poolAllStudyDomainsStrict.R  |  Create SQLite database.  |
| addFindingsAnimalAge.R  |  Add a variable with the age of animal at the time of finding to a set of rows extracted from a findings domain.  |
| animalListControl.R  | Extract a list of control animals for a given list of studies.  |
| extractTSParam.R  | Extract the value(s) of one or more TS parameters for a given list of studies. |
| filterAnimalsSex.R  | extracts set of animals of the specified sex from an input data sets with a set of animals.  |
| filterFindingsAnimalAge.R  | Extract a set of findings within a specified range of age of animals at the time of finding.  |
| filterFindingsPhase.R  | Extract a set of findings for a specified study phase.  |
| filterMIFindings.R  | Extract a set of MI findings for specified MISTRESC and/or MISPEC value(s).  |
| filterStudyAnimalRoute.R  | Filter studyids by animal route.  |
| filterStudyAnimalSpeciesStrain.R  | Extract studies and animals which fulfill a specified route.  |
| miscFunctions.R  | Miscellaneous functions.  |
| studyListDoseDuration.R  |  Generate a function GetStudyListDOSDUR to extract a list of SEND study ids which fulfills a specified dose duration period. |
| studyListSpeciesStrain.R  | Extract a list of SEND study ids which fulfills a specified species.  |
| studyListStudyDesign.R  | Extract a list of SEND study ids which fulfills a specified study design.  |
| studyListStudyStartDate.R  | Extract a list of SEND study ids the study start date a specified study design.  |
| subjDataExtract.R  | Extract all rows and columns from a specified finding domain for a specified list of animals. |



