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

# Compute predictions (SCORE) from forests - for interval-censored data
pred_trft <- function(obs, mdl, ind_left = 1, ind_right = 2) {
  x <- unlist(predict(mdl, newdata = obs, mnewdata = obs,
             type = "survivor", q = 52/2)) -
  unlist(predict(mdl, newdata = obs, mnewdata = obs,
          type = "survivor", q = 52*2))
  return(x)
  }


