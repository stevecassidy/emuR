# emuR - Main package of the EMU Speech Database Management System


[![Build Status](https://travis-ci.org/IPS-LMU/emuR.png)](https://travis-ci.org/IPS-LMU/emuR)
[![Coverage Status](https://coveralls.io/repos/IPS-LMU/emuR/badge.svg)](https://coveralls.io/r/IPS-LMU/emuR)
[![CRAN_Status_Badge](http://www.r-pkg.org/badges/version/emuR)](https://cran.r-project.org/package=emuR)

The emuR package provides the next iteration of the EMU Speech 
Database Management System with database management, data 
extraction, data preparation and data visualization facilities. 
It also contains a server that 
is intended to host databases in the emuDB format 
(see `vignette('emuDB_intro')`) to the EMU-webApp 
([http://ips-lmu.github.io/EMU-webApp/](http://ips-lmu.github.io/EMU-webApp/)). The querying of annotations is 
performed using EMU's own EQL2 (EMU Query Language Version 2).

This package is part of the next iteration of the EMU Speech Database Management System 
which aims to be as close to an all-in-one solution for generating, manipulating, querying, 
analyzing and managing speech databases as possible. 
For an overview of the system please visit 
this URL: [http://ips-lmu.github.io/EMU.html](http://ips-lmu.github.io/EMU.html).



## Installation

* install the current [CRAN release](https://cran.r-project.org/package=emuR):
```r
install.packages("emuR")
```

* Download & extract the package from GitHub. Then install it with the following command: 
```r
install.packages("path/to/emuR", repos = NULL, type="source")
```

* or install the latest development version from GitHub (**preferred method**):
```r
library(devtools)
install_github("IPS-LMU/emuR", build_vignettes = TRUE)
```

## Quick start


* for more information see the `An introduction to the emuR package` vignette: 
```r
vignette('emuR_intro')
```
