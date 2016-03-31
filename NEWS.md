# emuR 0.1.7

* R depends version bump to 3.2.0 (as requested by CRAN maintainer)
* updated testthat::expect\_less\_than to expect\_lt calls (due to deprecated warnings)
* Using new .keep_all = T parameter of dplyr 
* removed legacy version of EQL vignette (overlooked as inst/doc was in .gitignore)

# emuR 0.1.6

* skipping in-depth thorough tests on CRAN for query and autobuild SQL functions 

# emuR 0.1.5

* fixed problem of interm\_res\_tables already being present with queries that have multiple recursion depth on both sides
of either -> or ^ operand (e.g. query (ae ,  "[[[Phonetic = n -> Phonetic =z] -> Phonetic = S ] ^ [Text = friends -> Text = she]]")) 
* fixed bad URL in README.md
* added CITATION file


# emuR 0.1.3.9000

* renamed SQL tables & columns from camel case to underscore notation 
* variable SQL backend implementation

# emuR 0.1.2.9000

* multiple check fixes on various plattforms

# emuR 0.1.1.9000

* `serve` problem with internalVars bug fixed
* file locking problem that caused vignettes to fail under windows problem fixed

# emuR 0.1.0.9000

* massive refactor of all functions that used to refer to an emuDB by 
  name and optionally by its UUID. They now use the new emuDBhandle object
  that is now returned by the `load_emuDB()` function.
* `convert_XXX_to_emuDB()` functions renamed to `convert_XXX()`