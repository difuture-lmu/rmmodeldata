devtools::install()

library(MASS)
library(dplyr)
library(partykit)
library(rpart)
library(party)
# library(ICcforest)
library(survival)
library(caret)
library(tram)
library(trtf)
library(ALassoSurvIC)
library(intcensROC)
library(icenReg)

source(here::here("temp/helper.R"))

set.seed(729)

n <- 100
simdat <- sim_multi(n)
nom <- paste0("b", 1:10)
#betas
b <- rep(0, 10)
b[c(2, 9)] <- 0.5
names(b) <- nom
sim_df1 <- sim_1()

n <- 100
simdat <- sim_multi(n)
nom <- paste0("b", 1:10)
#betas
b <- rep(0, 10)
b[c(1, 2, 9)] <- 0.5
names(b) <- nom
sim_df2 <- sim_1()

n <- 100
simdat <- sim_multi(n)
nom <- paste0("b", 1:10)
#betas
b <- rep(0, 10)
b[c(2, 9)] <- 0.5
b[1] <- 0.25
names(b) <- nom
sim_df3 <- sim_1()

sim_df <- rbind(sim_df1, sim_df2, sim_df3)
sim_df$trt <- as.factor(rep(0:2, each = n))

k <- 5

sets <- createFolds(sim_df$trt, k = k)

#for(i in 1) {

  #create partitions for cross-validation
  i <- 1
  traindat <- sim_df[-sets[[i]], -c(1,4)]
  testdat <- sim_df[sets[[i]], -c(1,4)]

  #convert test data to evaluation format
  testdat_val <- testdat
  testdat_val$left <- testdat$left
  testdat_val$right <- testdat$right
  testdat_val$left[testdat$right == Inf] <- 0
  testdat_val$right[testdat_val$right == Inf] <- testdat$left[testdat$right == Inf]
  testdat_val$right[testdat_val$right == 0] <- 0.001

  # Create Models
  ## Cox Distributional Survival Forests
  ### Predictive
  coxmod_pred <- Coxph(Surv(left, right, type = "interval2") ~ trt, data = traindat)

  set.seed(31415)
  myforest_cox_pred <- trtf::traforest(coxmod_pred, formula =
                                   Surv(left, right, type = "interval2") | trt  ~ .,
                                 data = traindat, ntree = 50)
  ## Data in myforest_cox_pred$mltobj$object$data

 preds_forauc <- diag(do.call(rbind,
                       predict(myforest_cox_pred, newdata = testdat_val, mnewdata = testdat_val,
                               type = "survivor", q = 104)))

  set.seed(31415)
  myforest_cox_pred_rmd <- rmmodeldata::traforest(coxmod_pred, formula =
                                   Surv(left, right, type = "interval2") | trt  ~ .,
                                 data = traindat, ntree = 50)

  preds_forauc_rmd <- diag(do.call(rbind,
                       predict(myforest_cox_pred_rmd, newdata = testdat_val, mnewdata = testdat_val,
                               type = "survivor", q = 104)))

  cbind(preds_forauc, preds_forauc_rmd)
  all.equal(preds_forauc, preds_forauc_rmd)

  # - Modify outcome in predict (respone ind data skeleton)
  # - Summarize what we have done so far and summarize possible issues (e.g. data in environments etc.)
  # - Write Prof. Hothorn.


  ### CHECK FOR DATA IN THE MODELS:
  ### ===========================================================

  ## Flatten model
  ## -------------------------------------------------------

  #mod_flatten = unlist(myforest_cox_pred_rmd)
  mod_flatten = unlist(myforest_cox_pred)
  counter  = 1
  patience = 10
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

  ## Inspect classes:
  ## -------------------------------------------------------

  classes    = sapply(mod_flatten, class)
  classes_ul = unlist(classes)
  table(classes_ul)

  ## Suspicious elements:
  tmp = mod_flatten[sapply(classes, length) > 1]
  length(tmp)
  table(sapply(tmp, class))

  mod_flatten[sapply(classes, length) > 1]

  ## Check if element contains a specific number:
  ## -------------------------------------------------------
  number = traindat[1, 4]

  contains_number = lapply(mod_flatten, function(x) {
    names(x)
    e = try({ any(x == number) }, silent = TRUE)

    if (inherits(e, "try-error")) return("Failure")
    if (is.na(e)) return("Comparison gives NA")
    if (length(e) == 0) return("Comparison is empty")
    if (e) return("Found value") else return("No value found")
  })

  table(unlist(contains_number))
  names(contains_number)[grepl("Found", unlist(contains_number))]
  names(contains_number)[grepl("NA", unlist(contains_number))]
  names(contains_number)[grepl("empty", unlist(contains_number))]


  ## Compare both objects:
  ## -------------------------------------------------------

  mod_fl     = flattenObject(myforest_cox_pred)
  mod_fl_rmd = flattenObject(myforest_cox_pred_rmd)

  number = traindat[1, 10]

  cnb     = findNumber(mod_fl, number)
  cnb_rmd = findNumber(mod_fl_rmd, number)

  table(unlist(cnb))
  table(unlist(cnb_rmd))

  names(cnb)[grepl("Found", unlist(cnb))]
  names(cnb)[grepl("NA", unlist(cnb))]
  names(cnb)[grepl("empty", unlist(cnb))]


  #vsearch = searchObjectForValue(myforest_cox_pred_rmd, traindat[1,4])
  #vsearch$found


