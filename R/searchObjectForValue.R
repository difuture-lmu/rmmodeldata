#' Check if a layer is the last one in the object structure
#'
#' @param layer [`character(1L)`]
#' @examples
#' isFinalLayer("iris$Sepal.Length[[140]]")
#' isFinalLayer("iris$Sepal.Length")
#' @export
isFinalLayer = function(layer) {
  lvalue = try(eval(parse(text = layer)), silent = TRUE)
  if (inherits(lvalue, "try-error")) return(TRUE)
  if (length(lvalue) == 1) {
    lvaluenext = try(eval(parse(text = eval(parse(text = paste0("deparseLayer(", layer, ")[[1]]"))))), silent = TRUE)
    if (inherits(lvaluenext, "try-error")) return(TRUE)
    return(identical(lvalue, lvaluenext))
  } else {
    return(FALSE)
  }
}

#' Get the first level structure of an R object
#'
#' @param obj [`R object`] R object which is deparsed.
#' @examples
#' deparseLayer(iris$Sepal.Length)
#' @export
deparseLayer = function(obj) {
  oname  = deparse(substitute(obj))
  llayer = length(obj)
  lattr  = attributes(obj)
  if (is.null(lattr)) {
    out = paste0(oname, "[[", seq_along(obj), "]]")
  } else {
    if (is.null(lattr$names)) {
      out = paste0(oname, "[[", seq_along(obj), "]]")
    } else {
      lattr_tmp = lattr$names
      empty_names = lattr$names == ""
      if (any(empty_names)) lattr_tmp[empty_names] = which(empty_names)
      lattr_tmp[! empty_names] = paste0("'", lattr_tmp[! empty_names], "'")
      out = paste0(oname, "[[", lattr_tmp, "]]")
    }
    lattr_non = setdiff(names(lattr), "names")
    for (attrb in lattr_non) {
      out = c(out, paste0("attr(", oname, ", '", attrb, "')"))
    }
  }
  return(out)
}

#' Deparse the whole structure of an R object
#'
#' @param obj [`R object`] R object which is deparsed.
#' @param terminator [`function`] Function to check if a final layer is reached (default `terminator = isFinalLayer`).
#' @examples
#' obj = lm(Petal.Length ~ ., data = iris)
#' strcts = deparseStructure(obj)
#' @export
deparseStructure = function(obj, terminator = isFinalLayer) {
  oname  = deparse(substitute(obj))
  layers = eval(parse(text = paste0("deparseLayer(", oname, ")")))
  while (! all(vapply(layers, terminator, logical(1L)))) {
    layers_new = character()
    for (layer in layers) {
      if (terminator(layer))
        layers_new = c(layers_new, layer)
      else
        layers_new = c(layers_new, eval(parse(text = paste0("deparseLayer(", layer, ")"))))
    }
    layers = layers_new
  }
  return(layers)
}

#' Search for value in complex R objects
#'
#'
#' @param obj [`R object`] R object for which we search for `value`.
#' @param value [`numeric(1L)|character(1L)`] Value which which for which `obj` is searched through.
#' @param show_output [`logical(1L)`] Show log (default `show_output = FALSE`).
#' @examples
#' obj = lm(Petal.Length ~ ., data = iris)
#' lfound = searchObjectForValue(obj, iris$Sepal.Length[1])
#' # Get values found:
#' lapply(lfound$found, function(l) eval(parse(text = l)))
#' @export
searchObjectForValue = function(obj, value, show_output = FALSE) {
  checkmate::assertLogical(show_output, any.missing = FALSE, len = 1L)
  oname  = deparse(substitute(obj))
  strcts = eval(parse(text = paste0("deparseStructure(", oname, ")")))
  out = character()
  failures = character()
  counter = 1L
  for (strct in strcts) {
    if (show_output) message("[", Sys.time(), "] ", counter, "/", length(strcts), ": ", strct)
    svalue = try(eval(parse(text = strct)), silent = TRUE)
    if (inherits(svalue, "try-error")) {
      failures = c(failures, paste0("Could not check `", strct, "`."))
    } else {
      vcomp = try(svalue == value, silent = TRUE)
      if (inherits(vcomp, "try-error"))
        failures = c(failures, paste0("Could not check `", strct, "`."))
      else if (vcomp) out = c(out, strct)
    }
  }
  return(list(found = out, failures = failures))
}

#' Flatten R object
#'
#' @param obj [`R object`] R object which should be flatten.
#' @param patience [`integer(1L)`] How many iterations needs the length of the old and new flatten object to be equal to stop the algorithm?
#' @return [`list`] containing the flatten object.
#' @examples
#' obj = lm(Petal.Length ~ ., data = iris)
#' obj_flatten = flattenObject(obj)
#' @export
flattenObject = function(obj, patience = 10L) {
  mod_flatten = unlist(obj)
  counter  = 1
  pat0     = 0
  while (any(! (sapply(mod_flatten, class) %in% c("list", "integer", "numeric", "logical", "data.frame", "environment", "function")))) {
    lmod_old = length(mod_flatten)
    mod_flatten = lapply(mod_flatten, function(x) {
      # message(class(x))
      if (! inherits(x, c("environment", "function"))) {
        class(x) = "list"
        for (i in seq_along(x)) {
          if (! inherits(x[[i]], c("environment", "function"))) class(x[[i]]) = "list"
        }
      }
      return(unlist(x))
    })
    mod_flatten = unlist(mod_flatten)
    lmod_new    = length(mod_flatten)

    if (lmod_old == lmod_new) pat0 = pat0 + 1 else pat0 = 0
    if (pat0 == patience + 1) break
    message("Iter ", counter, "; Patience = ", pat0)
    counter = counter + 1
  }
  return(mod_flatten)
}

#' Find number in R object
#' @param obj [`R object`] R object which should be flatten.
#' @param number [`anything`] Value which which for which `obj` is searched through.
#' @return [`list`] Containing messages whether the object was found or not.
#' @export
findNumber = function(obj, number) {
  contains_number = lapply(obj, function(x) {
    names(x)
    e = try({ any(x == number) }, silent = TRUE)

    if (inherits(e, "try-error")) return("Failure")
    if (is.na(e)) return("Comparison gives NA")
    if (length(e) == 0) return("Comparison is empty")
    if (e) return("Found value") else return("No value found")
  })
  return(contains_number)
}
