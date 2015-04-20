# emuR 0.0.6

* switched from `emuDB` class object to DBI conform DB engine. This means all databases are now refered to simply by their name (or by their UUID if a name clash occurs).  

* functions now use `_` notation to avoid S3 naming conflicts (see section "Object names" http://r-pkgs.had.co.nz/style.html)

* `load_emuDB()` now simply returns name of database.

* all function now working on DBI (SQL table) representation of loaded DB (see `database.R``for table definitions). 
  