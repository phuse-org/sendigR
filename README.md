[![MIT licensed](https://img.shields.io/badge/license-MIT-blue.svg)](https://github.com/phuse-org/phuse-scripts/blob/master/LICENSE.md) 

# sendigR
<a href="https://github.com/phuse-org/sendigR"><img src="man/figures/logo.png" alt="sendigR logo" align="right" /></a> 

The purpose of the `sendigR` package is to extract control data from a set of nonclinical studies stored in [CDISC SEND](https://www.cdisc.org/standards/foundational/send) format in a database to be used for cross study analysis.


`sendigR` includes:

* An optional Python package (xptclearner) to harmonize SEND datasets using SEND controlled terminology and extensible terminology;

* An R package to construct a relational database from any collection of SEND datasets;

* An R Shiny application with a graphical user interface for facilitating crosss study analysis.


This vignettes describes the `sendigR` Step by Step Installation, usage and license.


Familiarity with the CDISC SEND data model and at least basic knowledge about animal studies is necessary to follow this vignette and to use the package.


## Step by Step Installation

#### Installation of R and RStudio
R version 4.1.2 and above are used in developing and testing the `sendigR`.

[R and RStudio Installation](https://rstudio-education.github.io/hopr/starting.html)

#### Installation of Python and xptcleaner (optional)
Python version 3.9.6 and above are used in developing and testing the xptcleaner of `sendigR`. This is an optional component, you can skip this step if you will not use the xptcleaner to clean the xpt files. 

[Python installation](https://docs.python.org/3/using/index.html)

[xptcleaner Python package installation](https://phuse-org.github.io/sendigR/articles/Usingxptcleaner.html)

#### Installation of sendigR package

```

# Get CRAN version, this will be available soon
install.packages("sendigR")

# Or the development version from GitHub:
install.packages("devtools")
devtools::install_github('phuse-org/sendigR')

```

## sendigR Usage

#### [Using the sendigR xptcleaner](https://phuse-org.github.io/sendigR/articles/Usingxptcleaner.html)

#### [sendigR Overview and Functional Descriptions](https://phuse-org.github.io/sendigR/articles/Introduction.html)

#### [Running the sendigR R Shiny Application](https://phuse-org.github.io/sendigR/articles/SendDashboard.html)

#### An Example R script
An example R script [importStudies.R](https://github.com/phuse-org/sendigR/blob/main/importStudies.R) is provided for using functions provided by `sendigR`.

## sendigR License


The `sendigR` package including the optional Python xptcleaner is subject to the MIT Open Source License:

MIT License

Copyright (c) 2022 PhUSE and Biocelearate

Permission is hereby granted, free of charge, to any person obtaining a copy
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
