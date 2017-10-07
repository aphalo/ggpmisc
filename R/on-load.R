.onAttach <- function(libname, pkgname) {
  packageStartupMessage("For news about '", pkgname, "', please, see http://www.r4photobiology.info/")
  packageStartupMessage("For on-line documentation see http://docs.r4photobiology.info/", pkgname, "/")
}
