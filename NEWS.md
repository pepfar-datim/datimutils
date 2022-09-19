# datimutils 0.6.0

## New features
* Adds `loginToDATIMOAuth` function to assist with OAUTH2.0 authentication. 

# datimutils 0.5.4

## Bug fixes
* Fixes error conditions in getDataValueSets.


# datimutils 0.5.2

## Bug fixes
* Fixes is timeout error handling in API calls.


# datimutils 0.5.1

## New features
* Adds `verbose` parameter to all functions which returns entire response. Set to `FALSE` by default.
* Adds `quiet` parameter to all functions which determines whether URL is printed. Set to `TRUE` by default.

# datimutils 0.5.0

## New features
* Adds `getDataStoreKey` (DP-533)
* Adds `convertFYQToCQ` (DP-132)

# datimutils 0.4.0

## New features
* Adds `getDataValueSets` (DP-137).

## Minor updates and bug fixes
* Fixes bug in `callDATIMapi.R` to handle timeout errors (DP-763).
* Adds linting to CI checks and updates package so that all checks pass.
* Removes eight packages from `IMPORTS` that were not being called anywhere and moves two packages to `SUGGESTS` that were only used in vignettes.
* Refactors package to remove hard dependencies on `magrittr` (DP-729), `dplyr` (DP-728), and `purrr` (DP-730).
* Replaces usage of `stringr` with `stringi` (DP-732).

# datimutils 0.3.0

## New Features
* Adds `getSqlView`
* Adds `listSqlViews`

# datimutils 0.2.0

## Features

* add `getUserGroups`
* add `listMechs`
* add `getMyStreams`
* add `getMyUserType`
