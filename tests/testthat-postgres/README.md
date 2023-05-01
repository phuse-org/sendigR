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
