.onAttach <- function(libname, pkgname) {
  packageStartupMessage("For news about '", pkgname, "', please, see https://www.r4photobiology.info/")
  packageStartupMessage("For on-line documentation see https://docs.r4photobiology.info/", pkgname, "/")
}
