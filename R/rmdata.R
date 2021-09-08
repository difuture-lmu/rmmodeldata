rmdata = function(mod, ...) {
  UseMethod("rmdata", mod)
}

rmdata.lm = function(mod) {
 out = mod
 out$model = NULL
 return(out)
}

rmdata.ctm = function(mod) {
  return(mod)
}
