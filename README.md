# lazyR

An R package for stashing objects in lazy load databases, analogous to lazy loaded packages.
This uses the `archivist` package for a lot of the back end, but replaces the storing of `.rda` objects in the database with `.rdx`/`.rdb` objects.

## Using manual lazy saving

The basic work flow is:

1. decide on a folder for the database, and set it using `lazyDir()`

2. save objects to a lazyR database using `lazySave(obj)`

    - optionally give it tags while saving: `lazySave(obj, tags=c("maps", "USA"))`

3. list the contents of the lazyR database with `lazyLs()`
4. re-load objects via name or tag using `lazyLoad2(tag="maps")`
5. remove objects via `lazyRm("objName")` or `lazyRm(lazyLs("tagname"))`

## Using caching (automatic lazy saving)

An alternative workflow is to use the `%<%` (`assignCache`) operator, which will automatically cache the object with its assigned object name. Caching, like with the archivist package, will first do a hash of the arguments in the function, compare with the hashed value of the objects in the database, and return the cached object, if the hash values are the same.

Example:

1. decide on a folder for the database, and set it using

    lazyDir(tempdir(), create=TRUE)

2. assign objects via `%<%`. See help(cacheAssign)

    a %<% seq(1,10,1)

## Common things to use:

- `lazyLs()` will list all objects in the database
- `lazyLs(tagType="all")` will list the full `archivist` `data.frame` with columns: `md5Hash`, `tag`, `dateCreated`.
- others to come

## Connection with `archivist` package

The `lazyDir` argument in the `lazyR` package is exactly the same as a repository in archivist.
Thus, all archivist functions work with the `lazyR` package. Simply use lazyDir and RepoDir interchangeably.

Notes:

- Some tags are done automatically

    - a full set of classes, a result of a call to `is(obj)`, prefixed by `class:` in the tags
    - the original object name, prefixed with `objectName:`    
    
- Objects of class `Raster*` are special because of their "sometimes on disk" nature. 

    - If the object was in memory, it will be saved as any other `R` object within a `.rdb`/`.rdx` file pair. 
    - If, on the other hand, it was being read from disk, then only the file location will be stored within the `.rdb`/`.rdx` pair. 
    - The original file will be used as part of the lazy loading.
    - If the original file is a temporary file, be sure to use `lazySave(..., copyRasterFile=TRUE)` to save it. This is also required to ensure portability of a `lazyR` database.

- Objects are stored individually within an `.rdb`/`.rdx` file pair, with the file pair name being the md5Hash. The archivist package saves an file with `.rda`, but it will be a small file, just the object name hashed. So there are 3 files for each object.
