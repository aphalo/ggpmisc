.onAttach <- function(libname, pkgname) {
  packageStartupMessage("News about '", pkgname,
                        "' at https://www.r4photobiology.info/")
}
