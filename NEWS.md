# emuR 0.1.0.9000

* massive refactor of all functions that used to refer to an emuDB by 
  name and optionally by its UUID. They now use the new emuDBhandle object
  that is now returned by the `load_emuDB()` function.
* `convert_XXX_to_emuDB()` functions renamed to `convert_XXX()`