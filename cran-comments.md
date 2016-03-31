## Resubmission
This is a resubmission. In this version I have:

* R depends version bump to 3.2.0 (as requested by CRAN maintainer) + 3 minor bug fixes (see NEWS.md)

## Test environments
* local OS X install, R 3.3.0 Under development (unstable) (2016-03-02 r70268) -- "Unsuffered Consequences"
* ubuntu 12.04 (on travis-ci), R 3.2.3
* win-builder (devel and release)

## R CMD check results
There were no ERRORs or WARNINGs.

There was 1 NOTE:

* checking CRAN incoming feasibility ... NOTE
  Maintainer: 'Raphael Winkelmann <raphael@phonetik.uni-muenchen.de>'

  New submission

  Possibly mis-spelled words in DESCRIPTION:
    SDMS (10:37)

  @ New submission: This is our first submission of this package. However we would like to note that it is 
  a complete rewrite of an archived CRAN package called "emu": 
  https://cran.r-project.org/src/contrib/Archive/emu/. As
  almost the entire API as well as the backend and data structures have changed we have 
  opted for an update of the package name to "emuR".

  @ Possibly mis-spelled words: EMU_SDMS is an acronym for the EMU (S)peech (D)atabase (M)anagement (S)ystem


## Downstream dependencies
There are currently no downstream dependencies for this package.