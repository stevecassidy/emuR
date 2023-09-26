## Resurrecting from archive

This package was archived on 2023-08-31 because we weren’t able to fix the
issues in time. We believe that all issues are now addressed and would like to
get the package back to CRAN.

## Test environments

* local Arch Linux install, gcc, R 4.3.1
* CRAN win-builder, R-release
* R-hub builder:
  * Windows Server 2022, R-devel, 64 bit
  * Ubuntu Linux 20.04.1 LTS, R-release, GCC
  * Fedora Linux, R-devel, clang, gfortran

## R CMD check results

There were no WARNINGs and no ERRORs.

There were minor NOTEs:

- about archival on CRAN
- about a possibly misspelled word “SDMS” in DESCRIPTION, but that word is
  correct (and short for Speech Database Management System)

On the R-hub builder platform, there were additional NOTEs about:

- non-standard things in the check directory: `NULL`
- detritus in the temp directory: `lastMiKTeXException`

Both are probably due to bugs in the builder platform.

Only on R-hub Fedora, there was a segfault in readr::read_tsv, which suggests
that that test server has installed shared libraries incompatible with its
version of readr.

## Downstream dependencies

There are currently no downstream dependencies for this package.
