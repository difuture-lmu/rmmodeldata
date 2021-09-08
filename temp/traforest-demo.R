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


sim_multi <- function(n, p = 10){
  m <- rep(0, p)
  cvr <- NULL

  for(i in 1:p){
    for(j in 1:p){
      cvr <- c(cvr, 0.5^(abs(i-j)))
    }
  }

  s <- matrix(data = cvr, nrow = p)
  dat <- mvrnorm(n, mu = m, Sigma = s)

  return(dat)
}

sim_1 <- function(m=1800, dat = simdat, beta = b, lambda = 0.002, gamma = 1.3) {
  n <- nrow(simdat)
  # Function arguments:
  # n - No. of random variables to generate
  simtime <- event(n, dat, beta, lambda, gamma) #simulate 1 event time per patient (Weibull)

  df_int <- interval(n, m, simtime) #create multiple rows for each patient to become intervals

  sim_tbl <- censor(df_int) %>% #generate censoring time-points of intervals
    filter(delta != 9) %>% #delete intervals after event
    filter(max(right)==right) #keep only the last interval

  ungroup(sim_tbl)

  sim_tbl$left <- with(sim_tbl, ifelse (delta == 0, right, left))
  sim_tbl$right <- with(sim_tbl, ifelse (delta == 0, Inf, right))
  df <- data.frame(round(select(sim_tbl, id, left, right, simtime)), dat)
  return(df)
}

event <- function(n, dat, beta, lambda, gamma) {
  # generate uniform random numbers
  u <- runif(n) #default min=0, max=1
  t <- (-log(u) / (lambda * exp(eta(dat, beta))))^(1 / gamma)
  # compute event times from u
  return(t)
}

eta <- function(dat, beta) {
  # log incidence rate
  return(dat%*%beta)
}

interval <- function(n, m, simtime, mu = 2) {

  #create ids for patients
  id <- seq.int(1, by = 1, length.out = n)

  y <- rnbinom(n = m, size = 2, mu = mu) #modeling the image process (not intervals)
  y <- ifelse(y == 0, 1, y)
  y <- y[y != 1] - 1 #eliminate 0 or 1 images & decrease by 1 for #of intervals

  n_int <- sample(y, n)

  df_bas <- data.frame(id = id,
                       simtime = simtime,
                       n_int = n_int)

  df <- df_bas[rep(seq_len(nrow(df_bas)), df_bas$n_int), 1:2]

  return(df)
}

censor <- function(x) {
  x$right <- runif(length(x[,1]), min = 0, max = 150)

  x <- x[order(x$id, x$right),] %>%
    as.tbl() %>%
    group_by(id) %>%
    mutate(
      left = lag(right),
      left = ifelse(is.na(left), 0, left),
      delta = ifelse(simtime < left, 9, #mark intervals after event
                     ifelse(simtime > right,
                            0, 1)
      )
    )
  return(x)
}

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


# Compute predictions (SCORE) from forests - for interval-censored data
pred_trft <- function(obs, mdl, ind_left = 1, ind_right = 2) {
  x <- unlist(predict(mdl, newdata = obs, mnewdata = obs,
             type = "survivor", q = 52/2)) -
  unlist(predict(mdl, newdata = obs, mnewdata = obs,
          type = "survivor", q = 52*2))
  return(x)
  }

k <- 5

sets <- createFolds(sim_df$trt, k = k)

for(i in 1) {
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
  myforest_cox_pred <- traforest(coxmod_pred, formula =
                                   Surv(left, right, type = "interval2") | trt  ~ .,
                                 data = traindat, ntree = 50)


  preds_forauc <- diag(do.call(rbind,
                       predict(myforest_cox_pred, newdata = testdat_val, mnewdata = testdat_val,
                               type = "survivor", q = 104)))

  myforest_cox_pred$data[,] <- NA
  myforest_cox_pred$info$call$data[,] <- NA

  preds_forauc_test <- diag(do.call(rbind,
                       predict(myforest_cox_pred, newdata = testdat_val, mnewdata = testdat_val,
                               type = "survivor", q = 104)))

  ### Progonstic (Virtual Twins like)
  coxmod_prog <- Coxph(Surv(left, right, type = "interval2") ~ 1, data = traindat)
  myforest_cox_prog <- traforest(coxmod_prog, formula =
                                   Surv(left, right, type = "interval2")  ~ .,
                                 data = traindat, ntree = 50)

  ## Lasso
  #Using the model matrix of preceding models to make it work with continuous input.
  #Interactions are automaticaly included this way.
  helpdat <- model.matrix(Coxph(Surv(left, right, type = "interval2") ~ (.)*trt,
                                data = traindat))
  mylasso <- alacoxIC(lowerIC = traindat$left, upperIC = traindat$right,
                      X = helpdat, normalize.X = FALSE)

  #PREDICTIONS
  ##forests
  predsintrvl_cox_pred <- NULL
  predsintrvl_cox_prog <- NULL
  predsintrvl_wei_pred <- NULL
  predsintrvl_wei_prog <- NULL
  #there might be an "apply()" solution but I kept receiving errors so I comforted myself with the good-old for loop.
  for (i in 1:length(testdat_val[,1])) {
   # Cox Forest predictive
    pintrvl_cox_pred <- pred_trft(mdl = myforest_cox_pred,
                        obs = testdat_val[i,])
    predsintrvl_cox_pred <- c(predsintrvl_cox_pred, pintrvl_cox_pred)

    # Cox Forest prognostic
    # pintrvl_cox_prog <- pred_trft(mdl = myforest_cox_prog,
    #                     obs = testdat_val[i,])
    # predsintrvl_cox_prog <- c(predsintrvl_cox_prog, pintrvl_cox_prog)
  }

    test <- predsintrvl_cox_pred

  # Lasso
  helpdat_tr <- model.matrix(Coxph(Surv(left, right, type = "interval2") ~ (.)*trt,
                                data = testdat))



  preds_lp_las <-  helpdat_tr %*% mylasso$b

  bas <- baseline(mylasso)

  surv_fit_pre <- with(bas, data.frame(lower.set, upper.set,
                                       s = exp(-bas$clambda)))

  surv_fit_pre2 <- rbind(c(0, surv_fit_pre[1,1], 1.0), surv_fit_pre)

  preds_cumhzrd_lasso <- c(0, bas$clambda)%*%t(exp(preds_lp_las))

  surv_fit_pre <- data.frame(bas.lower.set = c(0, bas$lower.set),
                             bas.upper.set = c(bas$lower.set[1], bas$upper.set),
                             lag_upper.set =
                               c(lag(c(bas$lower.set[1], bas$upper.set),
                                     default = 0)),
                             exp(-preds_cumhzrd_lasso))

  n.times <- surv_fit_pre[,2]-surv_fit_pre[,3]
  sum(n.times)

  s_all_pre <- surv_fit_pre[rep(seq_len(nrow(surv_fit_pre)), n.times),]

  s_all_pre$bas.lower.set <- 0:148
  s_all_pre$bas.upper.set <- 1:149

  preds_surv_lasso <- t(s_all_pre[,-c(1:3)])

  colnames(preds_surv_lasso) <- paste0("w", 1:149)

  predsintrvl_lasso <- NULL

  #there might be an "apply()" solution but I comforted myself with the good-old for loop.

  x<-0

  #check the left & right predictions well!!!

  for (i in 1:length(testdat_val[,1])) {
    pintrvl_lasso <- preds_surv_lasso[i, 52/2] -
      preds_surv_lasso[i, 52*2]
    predsintrvl_lasso <- c(predsintrvl_lasso, pintrvl_lasso)
  }
}

