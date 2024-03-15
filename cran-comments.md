## Test environments

* local Arch Linux install, gcc, R 4.3.3
* local Arch Linux install, gcc, R-devel r86036
* CRAN win-builder, R-devel
* R-hub builder:
  * Windows Server 2022, R-devel, 64 bit
  * Ubuntu Linux 20.04.1 LTS, R-release, GCC
  * Fedora Linux, R-devel, clang, gfortran

## R CMD check results

There were no WARNINGs and no ERRORs.

On some platforms, there were minor NOTEs about:

- non-standard things in the check directory: `NULL`
- detritus in the temp directory: `lastMiKTeXException`

Both are probably due to bugs in the respective builder platform.

## revdepcheck results

We checked 1 reverse dependencies, comparing R CMD check results across CRAN and dev versions of this package.

 * We saw 0 new problems
 * We failed to check 0 packages

