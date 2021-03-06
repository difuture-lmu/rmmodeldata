#' @title Conditional Random Forests
#' See \code{?partykit::cforest}.
#' @param formula (see \code{?partykit::cforest})
#' @param data (see \code{?partykit::cforest})
#' @param weights (see \code{?partykit::cforest})
#' @param subset (see \code{?partykit::cforest})
#' @param offset (see \code{?partykit::cforest})
#' @param cluster (see \code{?partykit::cforest})
#' @param strata (see \code{?partykit::cforest})
#' @param na.action (see \code{?partykit::cforest})
#' @param control (see \code{?partykit::cforest})
#' @param ytrafo (see \code{?partykit::cforest})
#' @param scores (see \code{?partykit::cforest})
#' @param ntree (see \code{?partykit::cforest})
#' @param perturb (see \code{?partykit::cforest})
#' @param mtry (see \code{?partykit::cforest})
#' @param applyfun (see \code{?partykit::cforest})
#' @param cores (see \code{?partykit::cforest})
#' @param trace (see \code{?partykit::cforest})
#' @param ... (see \code{?partykit::cforest})
#' @export
cforest <- function(
    formula,
    data,
    weights,
    subset,
    offset,
    cluster,
    strata,
    na.action = na.pass,
    control = partykit::ctree_control(
        teststat = "quad", testtype = "Univ", mincriterion = 0,
        saveinfo = FALSE, ...),
    ytrafo = NULL,
    scores = NULL,
    ntree = 500L,
    perturb = list(replace = FALSE, fraction = 0.632),
    mtry = ceiling(sqrt(nvar)),
    applyfun = NULL,
    cores = NULL,
    trace = FALSE,
    ...
) {
    message("[", Sys.time(), "] Using `rmmodeldata::cforest`")

    ### get the call and the calling environment for .urp_tree
    call <- match.call(expand.dots = FALSE)
    oweights <- NULL
    if (!missing(weights))
        oweights <- weights
    m <- match(c("formula", "data", "subset", "na.action", "offset", "cluster",
                 "scores", "ytrafo", "control"), names(call), 0L)
    ctreecall <- call[c(1L, m)]
    ctreecall$doFit <- FALSE
    if (!is.null(oweights))
        ctreecall$weights <- 1:NROW(oweights)
    ctreecall$control <- control ### put ... into ctree_control()
    ctreecall[[1L]] <- quote(partykit::ctree)
    tree <- eval(ctreecall, parent.frame())

    if (is.null(control$update))
        control$update <- is.function(ytrafo)

    d <- tree$d
    updatefun <- tree$update

    nvar <- sum(d$variables$z > 0)
    control$mtry <- mtry
    control$applyfun <- lapply

    strata <- d[["(strata)"]]
    if (!is.null(strata)) {
        if (!is.factor(strata)) stop("strata is not a single factor")
    }

    probw <- NULL
    iweights <- model.weights(model.frame(d))
    if (!is.null(oweights)) {
        if (is.matrix(oweights)) {
            weights <- oweights[iweights,,drop = FALSE]
        } else {
            weights <- oweights[iweights]
        }
    } else {
        weights <- NULL
    }
    rm(oweights)
    rm(iweights)
    N <- nrow(model.frame(d))
    rw <- NULL
    if (!is.null(weights)) {
        if (is.matrix(weights)) {
            if (ncol(weights) == ntree && nrow(weights) == N) {
                rw <- unclass(as.data.frame(weights))
                rw <- lapply(rw, function(w)
                    rep(1:length(w), w))
                weights <- integer(0)
            } else {
                stop(sQuote("weights"), "argument incorrect")
            }
        } else {
            probw <- weights / sum(weights)
        }
    } else {
        weights <- integer(0)
    }

    idx <- partykit:::.start_subset(d)

    frctn <- pmin(1, sum(perturb$fraction))

    if (is.null(rw)) {
        ### for honesty testing purposes only
        if (frctn == 1) {
            rw <- lapply(1:ntree, function(b) idx)
        } else {
        if (is.null(strata)) {
            size <- N
            if (!perturb$replace) size <- floor(size * frctn)
            rw <- replicate(ntree,
                            sample(idx, size = size,
                                   replace = perturb$replace, prob = probw[idx]),
                            simplify = FALSE)
        } else {
            frac <- if (!perturb$replace) frctn else 1
            rw <- replicate(ntree, function()
                  do.call("c", tapply(idx, strata[idx],
                          function(i)
                              sample(i, size = length(i) * frac,
                                     replace = perturb$replace, prob = probw[i]))))
        }
        }
    }

    ### honesty: fraction = c(p1, p2) with p1 + p2 <= 1
    ### p1 is the fraction of samples used for tree induction
    ### p2 is the fraction used for honest predictions (nearest neighbor
    ### weights)
    ### works for subsampling only
    if (!perturb$replace && length(perturb$fraction) == 2L) {
        frctn <- perturb$fraction[2L]
        if (is.null(strata)) {
            size <- N
            if (!perturb$replace) size <- floor(size * frctn)
            hn <- lapply(1:ntree, function(b)
                            sample(rw[[b]], size = size,
                                   replace = perturb$replace, prob = probw[rw[[b]]]))
        } else {
            frac <- if (!perturb$replace) frctn else 1
            hn <- lapply(1:ntree, function(b)
                  do.call("c", tapply(rw[[b]], strata[rw[[b]]],
                          function(i)
                              sample(i, size = length(i) * frac,
                                     replace = perturb$replace, prob = probw[i]))))
        }
        rw <- lapply(1:ntree, function(b) rw[[b]][!(rw[[b]] %in% hn[[b]])])
        tmp <- hn
        hn <- rw
        rw <- tmp
    } else {
        hn <- NULL
    }

    ## apply infrastructure for determining split points
    ## use RNGkind("L'Ecuyer-CMRG") to make this reproducible
    if (is.null(applyfun)) {
        applyfun <- if(is.null(cores)) {
            lapply
        } else {
            function(X, FUN, ...)
                parallel::mclapply(X, FUN, ..., mc.set.seed = TRUE, mc.cores = cores)
        }
    }

    trafo <- updatefun(sort(rw[[1]]), integer(0), control, doFit = FALSE)
    if (trace) pb <- txtProgressBar(style = 3)
    forest <- applyfun(1:ntree, function(b) {
        if (trace) setTxtProgressBar(pb, b/ntree)
        ret <- updatefun(sort(rw[[b]]), integer(0), control)
        ### honesty: prune-off empty nodes
        if (!is.null(hn)) {
            nid <- partykit::nodeids(ret$nodes, terminal = TRUE)
            nd <- unique(partykit::fitted_node(ret$nodes, data = d$data, obs = hn[[b]]))
            prn <- nid[!nid %in% nd]
            if (length(prn) > 0)
                ret <- list(nodes = partykit::nodeprune(ret$nodes, ids = prn), trafo = ret$trafo)
        }
        # trafo <<- ret$trafo
        ret$nodes
    })
    if (trace) close(pb)

    fitted <- data.frame(idx = 1:N)
    mf <- model.frame(d)
    fitted[[2]] <- mf[, d$variables$y, drop = TRUE]
    names(fitted)[2] <- "(response)"
    if (length(weights) > 0)
        fitted[["(weights)"]] <- weights

    ### turn subsets in weights (maybe we can avoid this?)
    rw <- lapply(rw, function(x) as.integer(tabulate(x, nbins = length(idx))))

    control$applyfun <- applyfun

    ret <- partykit:::constparties(nodes = forest, data = mf, weights = rw,
                        fitted = fitted, terms = d$terms$all,
                        info = list(call = match.call(), control = control))
    if (!is.null(hn))
        ret$honest_weights <- lapply(hn, function(x)
            as.integer(tabulate(x, nbins = length(idx))))

    ret$trafo <- trafo
    ret$predictf <- d$terms$z
    class(ret) <- c("cforest.nodat", class(ret))

    #message("[", Sys.time(), "] Precompute fdata")
    ret$fdata = lapply(ret$nodes, partykit::fitted_node, data = ret$data)

    ret$data_skeleton = constructDataSkeleton(ret$data)

    ## Remove data from the cforest object:
    ret$data = NULL
    message("[", Sys.time(), "] Remove data `$data`")
    ret$info$model$model = NULL
    message("[", Sys.time(), "] Remove data `$info$model$model`")
    ret$info$call$data = NULL
    message("[", Sys.time(), "] Remove data `$info$call$data`")
    ret$fitted = NULL
    message("[", Sys.time(), "] Remove data `$fitted`")

    return(ret)
}

#' @title Conditional Random Forests
#' See \code{?partykit::cforest}.
#' @param object (see \code{?partykit::predict.cforest})
#' @param newdata (see \code{?partykit::predict.cforest})
#' @param type (see \code{?partykit::predict.cforest})
#' @param OOB (see \code{?partykit::predict.cforest})
#' @param FUN (see \code{?partykit::predict.cforest})
#' @param simplify (see \code{?partykit::predict.cforest})
#' @param scale (see \code{?partykit::predict.cforest})
#' @param ... (see \code{?partykit::predict.cforest})
#' @export
predict.cforest.nodat <- function(object, newdata = NULL, type = c("response", "prob", "weights", "node"),
                            OOB = FALSE, FUN = NULL, simplify = TRUE, scale = TRUE, ...) {

    message("[", Sys.time(), "] Using `rmmodeldata::predict.cforest.nodat`")

    #responses <- object$fitted[["(response)"]]
    responses <- object$data_skeleton[[all.vars(object$terms)[1]]]
    forest <- object$nodes

    if ("data_skeleton" %in% names(object))
      cl = "object$data_skeleton"
    else
      cl = "object$data"

    # message("[", as.character(Sys.time()), "] Use ", cl, "")
    nd <- eval(parse(text = cl))

    vmatch <- 1:ncol(nd)
    if (!is.null(newdata)) {
      NOnewdata <- TRUE
        factors <- which(sapply(nd, is.factor))
        xlev <- lapply(factors, function(x) levels(nd[[x]]))
        names(xlev) <- names(nd)[factors]
        xlev <- xlev[attr(object$predictf, "term.labels")]
        nd <- model.frame(object$predictf, ### all variables W/O response
                          data = newdata, na.action = na.pass, xlev = xlev)
        OOB <- FALSE
        vmatch <- match(names(object$data), names(nd))
        NOnewdata <- FALSE
    }
    nam <- rownames(nd)

    type <- match.arg(type)

    ### return terminal node ids for data or newdata
    if (type == "node")
        return(lapply(forest, partykit::fitted_node, data = nd, vmatch = vmatch, ...))

    ### extract weights: use honest weights when available
    if (is.null(object$honest_weights)) {
        rw <- object$weights
    } else {
        rw <- object$honest_weights
        OOB <- FALSE
    }
    w <- 0L

    applyfun <- lapply
    if (!is.null(object$info))
        applyfun <- object$info$control$applyfun

    fdata = object$fdata
    #fdata <- lapply(forest, fitted_node, data = object$data, ...)
    if (NOnewdata && OOB) {
        fnewdata <- list()
    } else {
        fnewdata <- lapply(forest, partykit::fitted_node, data = nd, vmatch = vmatch, ...)
    }
    w <- partykit:::.rfweights(fdata, fnewdata, rw, scale)

#    for (b in 1:length(forest)) {
#        ids <- nodeids(forest[[b]], terminal = TRUE)
#        fnewdata <- fitted_node(forest[[b]], nd, vmatch = vmatch, ...)
#        fdata <- fitted_node(forest[[b]], object$data, ...)
#        tw <- rw[[b]]
#        pw <- sapply(ids, function(i) tw * (fdata == i))
#        ret <- pw[, match(fnewdata, ids), drop = FALSE]
#        ### obs which are in-bag for this tree don't contribute
#        if (OOB) ret[,tw > 0] <- 0
#        w <- w + ret
#    }
#
#    #w <- Reduce("+", bw)
#    if (!is.matrix(w)) w <- matrix(w, ncol = 1)

    if (type == "weights") {
        ret <- w
        colnames(ret) <- nam
        rownames(ret) <- rownames(responses)
        return(ret)
    }

    pfun <- function(response) {

        if (is.null(FUN)) {

            rtype <- class(response)[1]
            if (rtype == "ordered") rtype <- "factor"
            if (rtype == "integer") rtype <- "numeric"

            FUN <- switch(rtype,
                "Surv" = if (type == "response") partykit:::.pred_Surv_response else partykit:::.pred_Surv,
                "factor" = if (type == "response") partykit:::.pred_factor_response else partykit:::.pred_factor,
                "numeric" = if (type == "response") partykit:::.pred_numeric_response else partykit:::.pred_ecdf)
        }

        ret <- vector(mode = "list", length = ncol(w))
        for (j in 1:ncol(w))
            ret[[j]] <- FUN(response, w[,j])
        ret <- as.array(ret)
        dim(ret) <- NULL
        names(ret) <- nam

        if (simplify)
            ret <- partykit:::.simplify_pred(ret, names(ret), names(ret))
        ret
    }
    if (!is.data.frame(responses)) {
        ret <- pfun(responses)
    } else {
        ret <- lapply(responses, pfun)
        if (all(sapply(ret, is.atomic)))
            ret <- as.data.frame(ret)
        names(ret) <- colnames(responses)
    }
    ret
}
