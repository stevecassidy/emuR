## Change of maintainership

Maintainership of this package is being handed over from Raphael Winkelmann to
Markus Jochim. Raphael is still reachable at the below email address and he will
confirm the mutual intention.

* Raphael Winkelmann <raphael@phonetik.uni-muenchen.de>
* Markus Jochim <markusjochim@phonetik.uni-muenchen.de>

## Test environments

* local Arch Linux install, gcc, R 4.2.2
* local Windows 10 install, version 10.0.19045 Build 19045
* local Docker container rocker/r-devel, R Under development (unstable) (2023-03-19 r84006) -- "Unsuffered Consequences"
* R-hub builder:
  * Windows Server 2022, R-devel, 64 bit
  * Ubuntu Linux 20.04.1 LTS, R-release, GCC
  * Fedora Linux, R-devel, clang, gfortran

## R CMD check results

There were no WARNINGs and no ERRORs.

There were minor NOTEs about the change of maintainership and about LaTeX.

There was a NOTE about comparing class() to string. The comparison works in all mentioned cases.

## Downstream dependencies

There are currently no downstream dependencies for this package.
