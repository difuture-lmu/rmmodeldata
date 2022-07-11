#' @title Transformation Forests
#' See \code{?trtf::traforest}.
#' @param object (see \code{?trtf::traforest})
#' @param parm (see \code{?trtf::traforest})
#' @param reparm (see \code{?trtf::traforest})
#' @param update (see \code{?trtf::traforest})
#' @param min_update (see \code{?trtf::traforest})
#' @param mltargs (see \code{?trtf::traforest})
#' @param ... (see \code{?trtf::traforest})
#' @export
traforest <- function(object, parm = 1:length(coef(object)), reparm = NULL,
                      update = TRUE, min_update = length(coef(object)) * 2,
                      mltargs = list(maxit = 10000), ...) {

    message("[", Sys.time(), "] Using `rmmodeldata::traforest`")

    if (inherits(object, "mlt")) object <- object$model
    ### this is tricky because parm is only valid
    ### for this ctm object (not not for tram-like objects)
    mltargs$model <- object
    ### note: weights, offset, cluster etc. are evaluated here !!!
    args <- list(...)
    args$ytrafo <- trtf:::.ctmfit(object = object, parm = parm,
                           mltargs = mltargs, reparm = reparm,
                           min_update = min_update)
    args$update <- update
    ret <- do.call(rmmodeldata::cforest, args)

    ret$model <- rmdata(object)
    ret$mltargs <- mltargs
    ret$mltobj <- ret$trafo(model = TRUE, estfun = FALSE, object = TRUE)

    if ("object" %in% names(ret$mltobj)) {
      message("[", Sys.time(), "] Remove data `$mltobj$object$data`")
      ret$mltobj$object$data = NULL
    }
    class(ret) <- c("traforest.nodat", class(ret))
    ret
}

#' @title Transformation Forests
#' See \code{?trtf::predict.traforest}.
#' @param object (see \code{?trtf::predict.traforest})
#' @param newdata (see \code{?trtf::predict.traforest})
#' @param mnewdata (see \code{?trtf::predict.traforest})
#' @param K (see \code{?trtf::predict.traforest})
#' @param q (see \code{?trtf::predict.traforest})
#' @param type (see \code{?trtf::predict.traforest})
#' @param OOB (see \code{?trtf::predict.traforest})
#' @param simplify (see \code{?trtf::predict.traforest})
#' @param trace (see \code{?trtf::predict.traforest})
#' @param updatestart (see \code{?trtf::predict.traforest})
#' @param applyfun (see \code{?trtf::predict.traforest})
#' @param cores (see \code{?trtf::predict.traforest})
#' @param ... (see \code{?trtf::predict.traforest})
#' @export
predict.traforest.nodat <- function(object,  newdata, mnewdata = data.frame(1), K = 20, q = NULL,
    type = c("weights", "node", "coef", "trafo", "distribution", "survivor", "density",
             "logdensity", "hazard", "loghazard", "cumhazard", "quantile"),
    OOB = FALSE, simplify = FALSE, trace = FALSE, updatestart = FALSE,
    applyfun = NULL, cores = NULL, ...) {

    message("[", Sys.time(), "] Using `rmmodeldata::predict.traforest.nodat`")

    type <- match.arg(type)

    if (!missing(newdata) && !type == "node" && (!is.null(applyfun) || !is.null(cores))) {
        call <- match.call()
        if (is.null(applyfun)) {
            applyfun <- if(is.null(cores)) {
                lapply
            } else {
                function(X, FUN, ...)
                    parallel::mclapply(X, FUN, ..., mc.cores = cores)
            }
        }
        i <- 1:nrow(newdata)
        idx <- cut(i, breaks = (0:cores/cores) * nrow(newdata))
        idx <- tapply(i, idx, function(x) x, simplify = FALSE)
        call$applyfun <- call$cores <- NULL
        ret <- applyfun(idx, function(i) {
            predict(object = object, newdata = newdata[i,,drop = FALSE],
                    mnewdata = mnewdata, K = K, q = q, type = type,
                    OOB = OOB, simplify = simplify, trace = trace,
                    updatestart = updatestart, applyfun = NULL, cores = NULL, ...)
        })
        type <- match.arg(type)
        names(ret) <- NULL
        if (type == "weights") {
            ret <- do.call("cbind", ret)
        } else {
            ret <- do.call("c", ret)
        }
        return(ret)
    }

    tmp <- object
    class(tmp) <- class(tmp)[-1L]

    ptype <- type
    if (!(ptype %in% c("weights", "node"))) ptype <- "weights"
    if (missing(newdata)) {
        ret <- predict(tmp, OOB = OOB, type = ptype,
                       simplify = TRUE)
    } else {
        ret <- predict(tmp, newdata = newdata, type = ptype,
                       simplify = TRUE)
    }
    if (type %in% c("weights", "node")) return(ret)

    mod <- object$model
    if (is.null(q))
        q <- mkgrid(mod, n = K)[[mod$response]]
    mltmod <- object$mltobj
    mod <- mltmod$object
    thetastart <- coef(mod, fixed = FALSE)

    ans <- vector(mode = "list", length = ncol(ret))
    names(ans) <- colnames(ret)
    cf <- vector(mode = "list", length = ncol(ret))
    names(cf) <- colnames(ret)

    converged <- logical(ncol(ret))
    if (trace) pb <- txtProgressBar(style = 3)
    for (i in 1:ncol(ret)) {
        if (trace) setTxtProgressBar(pb, i / ncol(ret))
        w <- ret[,i]
        thetastart <- trtf:::.thetastart(mltmod$object, ret, i, updatestart, cf)
        ### try hard to estimate parameters; if may happen that parameters for
        ### a specific obs are not identified (out of range)
        umod <- try(object$trafo(subset = which(w > 0),
                                 weights = w, info = list(coef = thetastart),
                                 estfun = FALSE), silent = TRUE)
        converged[i] <- umod$converged
        if (inherits(umod, "try-error")) {
            cf[[i]] <- NA
            ans[[i]] <- NA
        } else {
            cf[[i]] <- umod$coef
            if (type != "coef") {
                coef(mod)[names(umod$coef)] <- umod$coef
                ans[[i]] <- predict(mod, q = q, newdata = mnewdata, type = type, ...)
            }
        }
    }
    if (!all(converged))
        warning("Parameter estimation did not converge for observations ",
                paste(which(!converged), sep = ", "))
    if (trace) close(pb)
    if (type == "coef") return(cf)
    return(ans)
}
