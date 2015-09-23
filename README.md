# lazyR

An R package for stashing objects in lazy load databases, analogous to lazy loaded packages. This uses the archivist package for a lot of the back end, but replaces the storing of rda objects in the database with rdx/rdb objects.

The basic work flow is:

- decide on a folder for the database, and set it using `setLazyDir()`
- save objects to a lazyR database using `lazySave(obj)`
- optionally give it tags while saving, `lazySave(obj, tags=c("maps", "USA"))`
- list the contents of the lazyR database with `lazyLs()`
- re-load objects via name or tag using `lazyLoad2(tag="maps")`
- remove objects via `lazyRm(lazyLs("maps"))` or `lazyRm("objName")`

Notes:

- Some tags are done automatically

    - a full set of classes, a result of a call to `is(obj)`, prefixed by `class:` in the tags
    - the original object name, prefixed with `objectName:`    
    
- Objects of class `Raster*` are special because of their "sometimes on disk" nature. 

    - If the object was "in memory", it will be saved as any other `R` object within an rdx/rdb file pair. 
    - If, on the other hand, it was being read from disk, then only the file location will be stored within the rdx/rdb pair. 
    - The original file will be used as part of the lazy loading. 
    - If the original file is a temporary file, then this will create a situation that is not easily lazy saved/loaded. It would be best to always write the file to a permanent location, then `lazySave` it.

- Objects are stored individually within an rdx/rdb file pair, with the file pair name being the md5Hash. The archivist package saves an file with .rda, but it will be a small file, just the object name hashed. So there are 3 files for each object.




