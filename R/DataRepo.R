#' Manage a large data file repository
#'
#' This package helps manage a large (100+ TB) collection of data files. It only
#' works on a unix-like file system (i.e. not on Windows) as it uses unix file
#' system groups and permissions to manage data access. Access is not very
#' granular, with read permission granted by membership in a file system group,
#' the "read group" and write permission granted by membership in another group,
#' the "write-group". This is currently in a very preliminary state and under
#' active development. Don't count on API consistency across versions yet.
#'
#' See the [SRJ-TODO] vignette for an overview of how to use this package.
#'
"_PACKAGE"
