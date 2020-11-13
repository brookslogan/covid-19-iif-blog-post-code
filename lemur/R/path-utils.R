
get_repo_root_dirpath <- function(starting_dirpath=getwd()) {
    dirpath = starting_dirpath
    while (length(list.files(dirpath, "^\\.git$", all.files=TRUE)) != 1L) {
        parent.dirpath = dirname(dirpath)
        if (parent.dirpath == dirpath) {
            stop ('Did not find repo root (searching for .git file/directory within ancestors (according to `dirname`) of `getwd()`)')
        } else {
            dirpath <- parent.dirpath
        }
    }
    return (dirpath)
}
