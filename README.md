
<!-- README.md is generated from README.Rmd. Please edit that file -->
<!-- badges: start -->
# 
[![MIT
licensed](https://img.shields.io/badge/license-MIT-blue.svg)](https://github.com/phuse-org/phuse-scripts/blob/master/LICENSE.md)
<!-- badges: end -->

# sendigR

<a href="https://github.com/phuse-org/sendigR"><img src="man/figures/logo.png" alt="sendigR logo" align="right" /></a>

The purpose of the `sendigR` package is to extract control data from a
set of nonclinical studies stored in [CDISC
SEND](https://www.cdisc.org/standards/foundational/send) format in a
database to be used for cross study analysis.

`sendigR` includes:

-   Functions to construct a relational database from any collection of
    SEND datasets;

-   functions to query control data from database;

-   An R Shiny application with a graphical user interface for
    facilitating cross-study analysis. [Demo App can be found here.](https://phuse-org.shinyapps.io/sendigR/)

-   An optional Python package (xptclearner) to harmonize SEND datasets
    using SEND controlled terminology and extensible terminology;

Familiarity with the CDISC SEND data model and at least basic knowledge
about animal studies is necessary to use the package.

## Step by Step Installation

#### Installation of R and RStudio

R version 4.1.2 and above are used in developing and testing the
`sendigR`.

[R and RStudio
Installation](https://rstudio-education.github.io/hopr/starting.html)

#### Installation of Python and xptcleaner (optional)

Python version 3.9.6 and above are used in developing and testing the
xptcleaner of `sendigR`. This is an optional component, you can skip
this step if you will not use the xptcleaner to clean the xpt files.

[Python installation](https://docs.python.org/3/using/index.html)

[xptcleaner Python package
installation](https://phuse-org.github.io/sendigR/articles/Usingxptcleaner.html)

#### Installation of sendigR package

``` r

# Get CRAN version
install.packages("sendigR")

# Or the development version from GitHub:
install.packages("devtools")
devtools::install_github('phuse-org/sendigR')
```

## sendigR Usage

#### [sendigR Overview](https://phuse-org.github.io/sendigR/articles/Introduction.html)

#### [sendigR R Shiny Application](https://phuse-org.github.io/sendigR/articles/SendDashboard.html)

#### [Using the sendigR xptcleaner](https://phuse-org.github.io/sendigR/articles/Usingxptcleaner.html)

<!-- badges: start -->

[![R-CMD-check](https://github.com/phuse-org/sendigR/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/phuse-org/sendigR/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

[![](https://cranlogs.r-pkg.org/badges/sendigR)](https://cran.r-project.org/package=sendigR)

[![](https://cranlogs.r-pkg.org/badges/grand-total/sendigR)](https://cran.r-project.org/package=sendigR)


# Instructions for testing `sendigR` `PostgreSQL` functionality with the `testthat` package

-   Sign into RStudio Workbench Prod and start a new session

-   Create a new R Project with version control:

    -   File-\>New Project-\>Version Control-\>Git
    -   Paste the forked `sendigR` repo url <https://git.fda.gov/FDA/CDER/ots/rapid/sendigr.git> into Repository URL
    -   Project directory name should autofill with `sendigr`
    -   Choose desired subdirectory
    -   In bottom left corner of popup window, change `R version 3.6.3` to `R version 4.1.0`
    -   Click Create Project in bottom right corner

-   Install `testthat` library by running `install.packages('testthat')` in the console

    -   Note `''` and `""` are interchangeable in R save for a few limited exceptions

-   Once `testthat` has finished installing, run `library(testthat)` to load it into your current session

-   Before you can start running the test scripts, a number of other scripts and functions need to be run

-   As you run these, you will likely get messages and/or errors saying a package isn't loaded or that "there is no package called `pkg_name`". The "there is no package..." error is from trying to load a package (via `library(pkg_name)`) when it isn't installed. If you get this error, run `install.packages('pkg_name')` in the console

    -   Note that package names *are* case sensitive

## Code to run before you can start running the test scripts

#### Keyboard shortcut for running all code in a script is `CTRL+ALT+R`

-   In the `~/data-raw/` folder, run `load_sysdata.R`
-   In the `~/R/` folder, run `dbFunctionsPostgreSQL.R` and then `initSENDFunctions.R`
-   Open `test001_init_functions.R` in `tests/testthat/` and edit `dbPath = '~/Kevin Snyder Project/sendigr-main/tests/data/dummy.db'` for your folder structure. Needs to be updated in four places in the script
-   Run whole script at once with `CTRL+ALT+R` or run each `test_that()` function with `CTRL+ENTER` while the cursor is on the `test_that()` line
-   Passing tests return `Test passed` and an emoji in the console. If a test fails, it returns an error message with where/why
-   The general testing process is that each source file needs to be run before the test file with the same/similar name


## sendigR License

The `sendigR` package including the optional Python xptcleaner is
subject to the MIT Open Source License:

MIT License

Copyright (c) 2022 PHUSE and BioCelerate

Permission is hereby granted, free of charge, to any person obtaining a
copy of this software and associated documentation files (the
“Software”), to deal in the Software without restriction, including
without limitation the rights to use, copy, modify, merge, publish,
distribute, sublicense, and/or sell copies of the Software, and to
permit persons to whom the Software is furnished to do so, subject to
the following conditions:

The above copyright notice and this permission notice shall be included
in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED “AS IS”, WITHOUT WARRANTY OF ANY KIND, EXPRESS
OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY
CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,
TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
SOFTWARE, ANY UPDATES TO THE SOFTWARE MADE BY CRAN, OR THE USE OR 
OTHER DEALINGS IN THE SOFTWARE OR ANY UPDATED VERSION.

Nothing in these scripts is intended to guide the analytic process and 
any interpretations of data found as a result of using these scripts are 
solely the responsibility of the user of the scripts and not the developers. All users are responsible for ensuring their own compliance with applicable laws, 
regulations, and agency guidance.
