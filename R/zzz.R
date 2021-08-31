checkLoad = function(pkg) {
  pkgs = .packages()
  if (! pkg %in% pkgs) {
    stime = paste0("[", as.character(Sys.time()), "]")
    nline = paste0("[", paste(rep(" ", nchar(stime) - 2), collapse = ""), "]")
    message(stime," Package ", pkg, " was not loaded yet!")
    message(nline, " Please do not load it after calling `library(rmmodeldata)` or")
    message(nline, " call functions by using the namespace `rmmodeldata::function`")
    message(nline, " (e.g. `rmmodeldata::cforest`) instead!")
  }
}

.onLoad = function(libname, pkgname) {
  checkLoad("partykit")
  invisible()
}
