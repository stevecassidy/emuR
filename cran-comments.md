## Resurrecting from archive

This package was archived on 2023-04-10 because we weren’t able to fix the
issues in time. We believe that all issues are now addressed and would like to
get the package back to CRAN.

## Change of maintainership

Maintainership of this package is being handed over from Raphael Winkelmann to
Markus Jochim. Raphael is still reachable at the below email address and he will
confirm the mutual intention.

* Raphael Winkelmann <raphael@phonetik.uni-muenchen.de>
* Markus Jochim <markusjochim@phonetik.uni-muenchen.de>

## Test environments

* local Arch Linux install, gcc, R 4.3.0
* R-hub builder:
  * Windows Server 2022, R-devel, 64 bit
  * Ubuntu Linux 20.04.1 LTS, R-release, GCC
  * Fedora Linux, R-devel, clang, gfortran

## R CMD check results

There were no WARNINGs and no ERRORs.

There were minor NOTEs:

- about archival on CRAN and the change of maintainership
- about a possibly misspelled word “SDMS” in DESCRIPTION, but that word is
  correct (and short for Speech Database Management System)

On the R-hub builder platform, there were additional NOTEs about:

- non-standard things in the check directory: `NULL`
- detritus in the temp directory: `lastMiKTeXException`

Both are probably due to bugs in the builder platform.

## Downstream dependencies

There are currently no downstream dependencies for this package.

## Comments from CRAN email from 2023-05-17 16:59, Victoria Wimmer

### TRUE and FALSE instead of T and F

I have gone through the entire code and replaced instances of T and F with TRUE
and FALSE, respectively.


### Add \value to .Rd files

> Please add \value to .Rd files regarding exported methods and explain the functions results in the documentation. Please write about the structure of the output (class) and also what the output means. (If a function does not return a value, please document that too, e.g. \value{No return value, called for side effects} or similar)
> Missing Rd-tags in up to 105 .Rd files, e.g.:
>      add_files.Rd: \value
>      AddListRemoveAttrDefLabelGroup.Rd: \value
>      AddListRemoveLabelGroup.Rd: \value
>      AddListRemoveLevelDefinitions.Rd: \value
>      AddListRemoveLinkDefinition.Rd: \value
>      AddListRemovePerspective.Rd: \value
>     ...


### Unexecutable code in man/

- man/AddListRemoveSsffTrackDefinition.Rd
- man/SetGetlevelCanvasesOrder.Rd

I have corrected these examples.


### Examples for unexported functions

> You have examples for unexported functions. Please either omit these examples or export these functions.
> Examples for unexported function
>   dct() in:
>      by.trackdata.Rd
>      convert_legacyEmuDB.Rd
>      dapply.Rd

This does not seem to be the case. `dct()` is an exported function, and the three
`.Rd` files mentioned contain no reference to `dct()`. Nor do they contain a
reference to any unexported function.


### Pre-generated comments

> Please omit the pre-generated comments in your .RD files. e.g.: man/classify.Rd

I don’t understand what you mean by “pre-generated.”


### Use of print() and cat()

> You write information messages to the console that cannot be easily suppressed. It is more R like to generate objects that can be used to extract the information a user is interested in, and then print() that object.
> Instead of print()/cat() rather use message()/warning()  or if(verbose)cat(..) (or maybe stop()) if you really have to write text to the console.
> (except for print, summary, interactive functions)

I have gone through the entire code looking for instances of `print()` and `cat()`.
I have replaced some instances with warning() or stop(). All remaining instances are either:

- part of a `print` or `summary` function themselves (e.g. `print.emuRsegs()`)
- part of interactive functions (everything in `R/emuR-server.R`)
- inside an `if(verbose)` block
- inside utils::capture.output()
- used as cat (..., file = ...), thus printing to a user-defined file


### No writing outside tempdir()

> Please ensure that your functions do not write by default or in your examples/vignettes/tests in the user's home filespace (including the package directory and getwd()). This is not allowed by CRAN policies.
> Please omit any default path in writing functions. In your examples/vignettes/tests you can write to tempdir().


### User’s options, par or working directory

> Please make sure that you do not change the user's options, par or working directory. If you really have to do so within functions, please ensure with an *immediate* call of on.exit() that the settings are reset when the function is exited. e.g.:
> ...
> oldpar <- par(no.readonly = TRUE)    # code line i
> on.exit(par(oldpar))            # code line i + 1
> ...
> par(mfrow=c(2,2))            # somewhere after
> ...
> If you're not familiar with the function, please check ?on.exit. This function makes it possible to restore options before exiting a function even if the function breaks. Therefore it needs to be called immediately after the option change within a function.
> 
> Please always make sure to reset to user's options(), working directory or par() after you changed it in examples and vignettes and demos.
> e.g.:
> oldpar <- par(mfrow = c(1,2))
> ...
> par(oldpar)

I have gone through the entire code looking for instances of `par()`, `options()` and `setwd()`.

- There is no instance of `setwd` in this package.
- For all writing instances of `par()` and `options()`, I have made sure to restore to the old values.
