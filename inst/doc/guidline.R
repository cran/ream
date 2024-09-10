## ---- eval=FALSE--------------------------------------------------------------
#  devtools::install_github("RaphaelHartmann/ream")  # installing from GitHub

## ---- eval=TRUE---------------------------------------------------------------
library(ream)

## ---- eval=TRUE, echo=FALSE---------------------------------------------------
set.seed(12345)

## ---- eval=TRUE---------------------------------------------------------------
?rDMC # check the help file
(samp <- rDMC(n = 10, phi = c(0.3, 0.5, -1, 0.2, 0.05, 2.5, 3, 1, 0.5, 0, 0, 1), dt = 1e-4))

## ---- eval=TRUE---------------------------------------------------------------
?dDMC # check the help file
(PDF <- dDMC(rt = samp$rt, resp = samp$resp, phi = c(0.3, 0.5, -1, 0.2, 0.05, 2.5, 3, 1, 0.5, 0, 0, 1), x_res = "default", t_res = "default"))
(CDF <- dDMC(rt = samp$rt, resp = samp$resp, phi = c(0.3, 0.5, -1, 0.2, 0.05, 2.5, 3, 1, 0.5, 0, 0, 1), x_res = "default", t_res = "default"))

## ---- eval=TRUE---------------------------------------------------------------
N <- 100
con <- rDMC(n = N, phi = c(0.3, 0.5, 1, 0.2, 0.05, 2.5, 3, 1, 0.5, 0, 0, 1), dt = 1e-05)
incon <- rDMC(n = N, phi = c(0.3, 0.5, -1, 0.2, 0.05, 2.5, 3, 1, 0.5, 0, 0, 1), dt = 1e-05)
data <- data.frame(congruency = rep(1:2, each = N), rt = c(con$rt, incon$rt), resp = c(con$resp, incon$resp))

## ---- eval=TRUE---------------------------------------------------------------
deviation <- function(pars, data) {
  ind_con <- which(data$congruency==1)
  ind_incon <- which(data$congruency==2)
  ls_con <- dDMC(rt = data$rt[ind_con], resp = data$resp[ind_con], phi = c(pars[1:2], 1, pars[3:6], 1, 0.5, 0, 0, 1), x_res = "higher", t_res = "higher")$sum_log_pdf
  ls_incon <- dDMC(rt = data$rt[ind_incon], resp = data$resp[ind_incon], phi = c(pars[1:2], -1, pars[3:6], 1, 0.5, 0, 0, 1), x_res = "higher", t_res = "higher")$sum_log_pdf
  return(-2*(ls_con+ls_incon))
}

## ---- eval=TRUE---------------------------------------------------------------
set.seed(3210)
(start_pars <- c(runif(1, .2, .6), runif(1, .3, .7), runif(1, .1, .6), runif(1, 0, .1), runif(1, 1.5, 4.5), runif(1, 0, 5)))
optim(par = start_pars, fn = deviation, method = "L-BFGS-B", lower = c(.2, .1, .1, 0.001, 1, 0.001), upper = c(.6, .9, .6, .1, 5, 5), data = data, )

