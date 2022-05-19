[![MIT licensed](https://img.shields.io/badge/license-MIT-blue.svg)](https://github.com/phuse-org/phuse-scripts/blob/master/LICENSE.md) 

# sendigR
<a href="https://github.com/phuse-org/sendigR"><img src="man/figures/logo.png" alt="sendigR logo" align="right" /></a> 

## Overview
The purpose of the `sendigR` package is to extract data from a set of nonclinical studies stored in [CDISC SEND](https://www.cdisc.org/standards/foundational/send) format in a database to be used for cross study analysis.
It can either be done in a script by execution of a set of functions from the package to extract data for further processing or by execution an encapsulated R Shiny application.

The package supports currently these types of databases:

* [SQLite](https://www.sqlite.org/index.html)
  It's possible to access an existing SEND database or build a database with SEND data imported from SAS transport/xpt files, which then can be accessed.
* Oracle
  It's possible to access an already existing SEND database

This vignettes introduces how to use `sendigR` to:

* initiate the use of the functions.
* build and maintain a SQLite database of SEND data.
* filter and extract SEND data from a database.
* execute the R Shiny application to browse the SEND control data stored in the database.

Familiarity with the CDISC SEND data model and at least basic knowledge about animal studies is necessary to follow this vignette and to use the package.

# Getting started

## Installation 

```

# Get CRAN version
install.packages("sendigR")
# Or the development version from GitHub:
# install.packages("devtools")
devtools::install_github('phuse-org/sendigR')

```


## Package dependencies 
Before we are ready to use the functions in the package, we must ensure that a minimum of prerequisites are fulfilled.
Beside the required packages listed in the Imports section of [DESCRIPTION](../DESCRIPTION), one or more additional package(s) must be installed:

* If the SEND databases is stored in an Oracle database:
    * _ROracle_
* If a SEND database are to be build using this package's functions
    * _SASxport_
    * _sjlabelled_
* If the sendDashboard app is to be executed:
    * TO BE ADDED

## Database

If an existing SEND database is to be accessed, it must contain a set of tables and columns representing the domains and variables described in the SEND IG version [3.0](https://www.cdisc.org/standards/foundational/send/sendig-v3-0) or [3.1](https://www.cdisc.org/standards/foundational/send/sendig-v3-1) - or a union of the two versions.

It is required that all names and basic data types (i.e. numerical or character) are as described the the SEND IG.

## Study data to import

If study data are to be imported into a SQLite database, data for each study must be located in a separate folder as [SAS transport](https://documentation.sas.com/?cdcId=pgmsascdc&cdcVersion=9.4_3.5&docsetId=movefile&docsetTarget=n1xbwdre0giahfn11c99yjkpi2yb.htm&locale=en) files - also konw as XPT files.

Multiple studies may be located together in a folder hierarchy as described below.

## SEND controlled terminology

Several of the function in the packages uses CDISC SEND terminology code list to verify validity of the SEND data. The package includes the values for the relevant code lists extracted from the newest CT version at build time. If it is relevant to use another version - newer or older - this version must be downloaded in Excel format from [NCI](https://evs.nci.nih.gov/ftp1/CDISC/SEND/) or [CDISC library archives](https://www.cdisc.org/members-only/cdisc-library-archives) and saved in any folder which is accessible from the R script(s) using this package.

# Function overview.

The package contains 5 sets of functions covering different areas - each of these are described below, including detailed examples.

## Initiation and closure of SEND environment

Before any use of the package's function, a database must be opened and the database must be closed again as the final action.

| Function | Description |
|-|------------|
| [initEnvironment](../html/initEnvironment.html) |	Initialize the environment. It must be executed as the absolute first function before any other package functions. It opens an existing or create a new empty database and returns a handle to the database, which must be used in all following function calls. |
| [disconnectDB](../html/disconnectDB.html)       | Close the open database. It must be called as the absolute last function in a session. |   

The call of `initEnvironment` returns a handle with a reference to the open database and pointers to low level functions accessing the specific type of database. This handle must be given as the first input parameter in the execution of all other function in the package.
It is possible to open and access multiple databases - also of different types - in the same R session. Just execute `initEnvironment` for each relevant database and save the returned db handles in each distinct variable.

Examples in the following sections demonstrates the use of these functions in a complete work flow.

## Build a SQLite SEND database

It is possible to build an SQLite database with SEND data imported from a set of studies.

| Function | Description |
|-|------------|
| [dbCreateSchema](../html/dbCreateSchema.html)     | Create a SEND schema, i.e. all the tables to represent the domains documented in SEND IG, in an open and empty database|
| [dbCreateIndexes](../html/dbCreateIndexes.html)   | Create a set of indexes on the tables in a  SEND database to optimize performance of extraction of data|   
| [dbImportOneStudy](../html/dbImportOneStudy.html) | Import SEND study data in SAS xport format into a SEND database from a single study folder|   
| [dbImportStudies](../html/dbImportStudies.html)   | Import SEND study data in SAS xport format into a SEND database from a hierarchy study folders|   
| [dbDeleteStudies](../html/dbDeleteStudies.html)   | Delete one or more studies in SEND database|   

It should be possible to import data into an existing SQLite SEND database using `dbImportOneStudy` and `dbImportStudies`, even though the tables are not created by `dbCreateSchema`. 
It is possible to delete studies in any SQLite SEND database `dbDeleteStudies` - whether or not the tables are created by `dbCreateSchema`. 

To make it possible to import data for a set of studies with `dbImportStudies`, the input data should be saved in folder structure like this: 

/path/to/SEND/datasets  
+-- study01  
   &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;+-- ts.xpt  
   &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;+-- dm.xpt  
   &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;+-- ex.xpt  
   &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;+-- etc.  
+-- study02   
  &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;+-- ts.xpt  
  &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;+-- dm.xpt  
  &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;+-- ex.xpt  
  &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;+-- etc.  
+-- proj1234<br>
   &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;+-- study11<br>
   &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;+-- ts.xpt  
   &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;+-- dm.xpt  
   &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;+-- ex.xpt  
   &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;+-- etc.<br>
   &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;+-- study12<br>
   &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;+-- ts.xpt  
   &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;+-- dm.xpt  
   &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;+-- ex.xpt  
   &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;+-- etc.<br>
+-- etc.
## Extract and filter SEND data

These functions are divided into different groups:

* __Fetch rows from data__
A single function `genericQuery` gives the possibility to execute a SQL query against the database, i.e.it executes any user defined select statement and returns the selected rows. It is used internally by the functions described below, but can also be used directly to fetcg rows which ar e not possible with any of the specific extraction functions described below.  
* __Extract sets of studies__  
Each of these functions extracts, filter and return studies for different kinds of conditions.  
They can all be called with or without a set of studies given as input. In the former case, the extraction/filtering is based on the `TS` subset of input studies. In the latter case, the extraction/filtering is based on the full `TS` table.  
The naming convention of these function are `getStudies`_`TSPARMCD`_, where _`TSPARMCD`_ is the specific TSPARMCD value handled by the function - e.g. `STSTDTC`.
* __Extract full set of control animals__  
One function, `getControlSubj`, which extracts and return the set of control animals for a given input set of study id values.  
Only 'negative' control animals are included in the set of control animals - i.e. animals which are not treated with a compound but only vehicle.
* __Extract subsets of animals__  
Each of these functions take a set of animals as input, extracts and return a subset fulfilling a given condition - e.g. for species or route of administration.  
It is possible to execute each of these functions with no conditions for filtering - it will then just return the set of input rows with additional variables for the actual kind of attributes, e.g. sex or species/strain.  
The naming convention of these function are `getSubj`_`attributes`_ where _`attributes`_ is the specific attribute handled by the function - e.g. `Sex`.
* __Extract subject level data__  
One function, `getSubjData`, which extracts and return all rows for a given input set of animals from a specified subject level table - it can be any SEND table including `USUBJID` column.
* __Extract subsets of findings__  
Each of these functions take a set of finding rows as input, extracts and return a subset fulfilling a given condition - e.g. for a specific study phase.  
It is possible to execute each of these functions with no conditions for filtering - it will then just return the set of input rows with additional variables for the actual kind of attributes, e.g. the study phase.  
The naming convention of these function are `getFindings`_`attributes`_ where _`attributes`_ is the specific attribute handled by the function - e.g. `Phase`.

The functions rely on the _data.table_ package - and all input/output sets of data are data tables.

The complete list of functions:

| Function | Description |
|-|------------|
| [genericQuery](../html/genericQuery.html)                 | Execute database query and returns fetched rows |
| [getStudiesSDESIGN](../html/getStudiesSDESIGN.html)       | Extract a list of SEND studies with a specified study design |
| [getStudiesSTSTDTC](../html/getStudiesSTSTDTC.html)       | Extract a list of SEND studies with study start date within a specified interval|
| [getControlSubj](../html/getControlSubj.html)             | Extract a list of control animals for a list of studies|
| [getSubjRoute](../html/getSubjRoute.html)                 | Extract the set of animals of the specified route of administration - or just add actual route of administration for each animal.|
| [getSubjSpeciesStrain](../html/getSubjSpeciesStrain.html) | Extract the set of animals of the specified species and strain - or just add the species and strain for each animal.|
| [getSubjSex](../html/getSubjSex.html)                     | Extract the set of animals of the specified sex - or just add the sex of each animal.|
| [getSubjData](../html/getSubjData.html)                   | Extract data from a subject level domain.|
| [getFindingsPhase](../html/getFindingsPhase.html)         | Extract a set of findings for a specified study phase - or just add phase for each animal|
| [getFindingsSubjAge](../html/getFindingsSubjAge.html)     | Add the subject age at finding time - and optionally extract the set of findings within a specified range of ages|
| [gen_vocab](../html/gen_vocab.html)     | Create json file for vocabulary mappings. Keys are synonyms and values are the CDISC Controlled Terminology Submission values. Vocabularies are defined by column values from the tab-delimited files.|   
| [standardize_file](../html/standardize_file.html)     | Standardizes SEND xpt files using CDISC controlled terminologies.|   



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