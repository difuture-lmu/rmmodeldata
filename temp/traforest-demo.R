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

  vsearch = searchObjectForValue(myforest_cox_pred_rmd, traindat[1,4])
  vsearch$found

  myforest_cox_pred_rmd$mltobj$object$data = NULL
  preds_forauc_rmd <- diag(do.call(rbind,
                       predict(myforest_cox_pred_rmd, newdata = testdat_val, mnewdata = testdat_val,
                               type = "survivor", q = 104)))

  cbind(preds_forauc, preds_forauc_rmd)
  all.equal(preds_forauc, preds_forauc_rmd)


#### REST OF THE CODE COMES HERE
#### WAS DELETED TO KEEP THE FILE AS SHORT AND SIMPLE AS POSSIBLE



