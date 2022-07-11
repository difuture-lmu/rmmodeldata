#' Construct skeleton of a data set
#'
#' Representing the structure of a data set without including any observation of the data frame.
#' Numerical values are uniformly drawn between `min(x) - sm` and `max(x) + sm` where `sm` is
#' a "safety margin" to not get insights about the minimum and maximum of a feature. Categorical
#' features first contains the factor levels to ensure all levels in the skeleton. The remaining
#' observations are then filled by randomly sampling from the factor levels.
#'
#' @param dat [`data.frame()`] Data set for which the skeleton is calculated
#' @param n [`integer(1L)`] Number of observations of the skeleton. The default equals the maximal
#'   number of factor levels.
#' @param sm [`numeric(1L)`] Fraction of the range used as safety margin (default `sm = 0.01`).
#' @param handler [`function`] Function used as feedback if a feature cannot be
#'   processed. Default is `handler = warning` which prints a warning. The first argument of the
#'   handler is a string naming the feature which cannot be processed.
#' @param ... Additional arguments passed to `handler`.
#' @return Skeleton `data.frame`
#' @examples
#' constructDataSkeleton(iris, n = 10L)
#' @export
constructDataSkeleton = function(dat, n = NULL, sm = 0.01, handler = warning, ...) {
  checkmate::assertDataFrame(dat)
  checkmate::assertIntegerish(n, lower = 1, any.missing = FALSE, len = 1L, null.ok = TRUE)
  checkmate::assertNumeric(sm, lower = 0, any.missing = FALSE, len = 1L)

  nms = names(dat)
  maxfac = max(vapply(dat, function(x) {
    if (is.character(x) | is.factor(x))
      return(length(unique(x)))
    else
      return(0)
  }, numeric(1L)))
  if (is.null(n)) n = maxfac
  if (n < maxfac) {
    warning("n is smaller than maximal number of factor values. Setting n = max(factor.levels).")
    n = maxfac
  }

  maxfac = n
  skeleton = lapply(nms, function(nm) {
    not_handled = TRUE
    # Use `==` instead of `is.numeric` because classes, such as `Surv`, returns
    # `TRUE` for `is.numeric` but binary operations are failing.
    if ((class(dat[[nm]]) == "numeric") || (class(dat[[nm]]) == "integer")) {
      xmin = min(dat[[nm]], na.rm = TRUE)
      xmax = max(dat[[nm]], na.rm = TRUE)
      smm  = sm * (xmax - xmin)
      out  = runif(maxfac, xmin - smm, xmax + smm)
      not_handled = FALSE
    }
    if (is.character(dat[[nm]]) | is.factor(dat[[nm]])) {
      uvals = unique(dat[[nm]])
      add   = NULL
      if (length(uvals) < maxfac) add = sample(uvals, size = maxfac - length(uvals), TRUE)
      out = c(uvals, add)
      not_handled = FALSE
    }
    if (inherits(dat[[nm]], "Surv")) {
      out = sample(dat[[nm]], maxfac)
      not_handled = FALSE
    }
    if (not_handled) {
      msg = paste0("Was not able to process feature ", nm, "!")
      handler(msg, ...)
    }
    return(out)
  })
  names(skeleton) = nms
  return(do.call(data.frame, skeleton))
}
