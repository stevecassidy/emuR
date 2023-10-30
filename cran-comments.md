## Bug in R-devel

See https://stat.ethz.ch/pipermail/r-package-devel/2023q4/009790.html

There was a bug in some of the recent versions of R-devel that caused a NOTE in
the HTML validation of this package’s manual. This bug has been (temporarily)
fixed in commit r85440. It would be great if you could process this package.

## Test environments

* local Arch Linux install, gcc, R 4.3.1
* CRAN win-builder, R-release
* R-hub builder:
  * Windows Server 2022, R-devel, 64 bit
  * Ubuntu Linux 20.04.1 LTS, R-release, GCC
  * Fedora Linux, R-devel, clang, gfortran

## R CMD check results

There were no WARNINGs and no ERRORs.

On CRAN win-builder and locally, there were no NOTEs.

On the R-hub builder platform, there were NOTEs about:

- non-standard things in the check directory: `NULL`
- detritus in the temp directory: `lastMiKTeXException`

Both are probably due to bugs in the builder platform.

## Downstream dependencies

There are currently no downstream dependencies for this package.
