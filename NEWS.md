# emuR 0.2.1.9004

## new features / performance tweaks / improvements

* choosing explicit paths with intersecting hierarchies now possible
* remove levelDef & linkDef now implement force parameters
* new function convert_txtCollection converts plain text collections into
single-node emuDB
* new functions runBASwebservice_* that call various BAS webservices
from inside emuR
* NULLing out empty DFs on list_level/linkDefs for more consisten API

## bug fixes

* wrapped `readChar`s in `enc2utf8` to avoid encodings issues on windows
* updating label table correctly on add_attributeDefinition() (#138)

# emuR 0.2.1


## new features / performance tweaks / improvements

* added new `EMUwebAppConfig -> perspectives -> signalCanvases -> minMaxValLims` config option to emuDB vignette
* requery_hier + requery_seq now implement the same timeRefSegmentLevel parameter as query (#135)

## bug fixes

* fixed requery\_hier() bug of requery on same attribute definition
* fixed requery\_hier() bug of requery on same level but different attribute definition

# emuR 0.2.0

## new features / performance tweaks / improvements

* rewrite of query engine to not require links_ext table any more (== redundant links)
* calcTimes parameter added to query() / requery\_seq() / requery\_hier() to make calculating times optional (extreme performance boost if no times have to be calculated)
* rewrite of annotJSONcharToBundleAnnotDFs() for faster loads emuDBs containing large annotJSONs
* replaced tidyjson as annot.json parser with own solution at tidyjson didn't scale well on larger annotation files
* added verbose parameter to export\_TextGridCollection()
* improved pre-check of dir exists in export\_TextGridCollection()
* added new replace\_itemLabels function
* improved export\_TextGridCollection() doc
* improved replace_itemLables() speed
* implemented rename\_emuDB() (\#116)
* implemented duplicate\_level() (\#113)
* implemented linkDuplicates parameter in duplicate\_level()
* autobuild\_linkFromTimes() speed improvements
* FUNCQ queries (start(),end(), medial()) now additionally support TRUE & FALSE and T & F values (vs. 0 & 1) 
* added attrDefNames column to list\_levelDefinitions() output
* can now deal with read only emuDBs by copying the cache to tempdir() and making it writable for the user
* added start\_item\_seq\_idx and end\_item\_seq\_idx to emuRsegs object
* added start\_item\_seq\_idx and end\_item\_seq\_idx type values to all intermediate result tables
* added optional function to reduce hierarchical query results to left and right most children only (large performance gain on calcTimes = T)
* rewriting annot.json files now updates MD5 sums as well (avoids unnecessary reload on next load\_emuDB)
* rewriting annot.json files now writes all (including empty / missing) attributeDef. labels

## bug fixes

* fixed bad DBconfig gen. on add_perspective
* fixed list\_linkDefinitions() returning strings as factors
* fixed bad error message when passing in ITEM levels to autobuild\_linkFromTimes()
* fixed incorrect handling of DBconfig when writeToFS was set to FALSE (writeToFS is now called rewriteAllAnnots)

# emuR 0.1.9

## new features / performance tweaks / improvements

* also allowing "time = " in TextTiers
* "levels of type 'EVENT' are not allowed to be super levels (== parents) in a domination relationship" constraint enforced in add_linkDefinition
* added "MEDIAFILE\_SAMPLES" as constant name to access audio samples to get\_trackdata() function
* improved error message to include tgPath in create_DBconfigFromTextGrid function
* no integer return value returned by create_emuRdemoData() any more! It was implicitly returned from wrassp function call...
* improved the slow overlap checking function in the BPF parser (is now O(n) instead of O(n^2))
* fixed col naming problems for new (unreleased) RSQLite version
* added export_TextGridCollection() function
* improved doc for get_trackdata
* constant naming of EMU-SDMS vs EMU_SDMS in various files
* rewriting all annotation file on add\_levelDefinition, remove\_levelDefinition 

## bug fixes

* fixed problem in conversion to JSON with empty items array (object '{}' vs array '[]')
* fixed problem of keywords "number" | "time" | "xmin" | ... in labels causing TextGrid parser to fail
* fixed problem with to lax RegEx in TextGrid parser
* fixed validation problem with missing levels regarding types

# emuR 0.1.8

## new features / performance tweaks / improvements

* get_trackdata with onTheFly calculation now reuses AsspDataObj if the current utterance is the same as the previous (large performance gain especially on long audio files)
* checking if DBconfig exists for better error message if 'name' field is not set correctly in DBconfig
* setting PRAGMA temp_store = 2; for SQLite connections
* not extracting tables to R if no RegEx needed to create filtered_tmp tables (performance gain when querying large emuDBs)
* convert_BPFCollection can now assigns the same label to more than one item when unifying tiers
* newline at the end of load_emuDB if no redundant links are built
* queries using dominates operator '^' don't use linksExt table anymore -> large performances benefits 
* only using \_filtered\_tmp tables if RegEx patterns are used
* changed primary key on items table which leads to massive performance gains (deleting _emuDBcache.sqlite required)

## bug fixes

* fixed error handling of create_emuRtrackdata + added @export to roxygen doc
* invalid annotJSONs generated by import_mediaFiles fixed
* convert_TextGridCollection can now handle nested folders again
* invalid UUIDs in DBConfig produced by convert_BPFCollection. Also added additional unit test to detect this.
* list_bundles uses session argument again
* fixed "Expression tree is too large (maximum depth 1000)" error in get_trackdata with long emuRsegs lists

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

* multiple check fixes on various platforms

# emuR 0.1.1.9000

* `serve` problem with internalVars bug fixed
* file locking problem that caused vignettes to fail under windows problem fixed

# emuR 0.1.0.9000

* massive refactor of all functions that used to refer to an emuDB by 
  name and optionally by its UUID. They now use the new emuDBhandle object
  that is now returned by the `load_emuDB()` function.
* `convert_XXX_to_emuDB()` functions renamed to `convert_XXX()`