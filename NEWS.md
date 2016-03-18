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