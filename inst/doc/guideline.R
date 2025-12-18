## ----eval=FALSE---------------------------------------------------------------
# devtools::install_github("RaphaelHartmann/ream")  # installing from GitHub

## ----eval=TRUE----------------------------------------------------------------
library(ream)

## ----eval=TRUE, echo=FALSE----------------------------------------------------
set.seed(12345)

## ----eval=TRUE----------------------------------------------------------------
?rDMC # check the help file
(samp <- rDMC(n = 10, phi = c(0.3, 0.5, -1, 0.2, 0.05, 2.5, 3, 1, 0.5, 0, 0, 1), dt = 1e-4))

## ----eval=TRUE----------------------------------------------------------------
?dDMC # check the help file
(PDF <- dDMC(rt = samp$rt, resp = samp$resp, phi = c(0.3, 0.5, -1, 0.2, 0.05, 2.5, 3, 1, 0.5, 0, 0, 1), x_res = "default", t_res = "default"))
(CDF <- dDMC(rt = samp$rt, resp = samp$resp, phi = c(0.3, 0.5, -1, 0.2, 0.05, 2.5, 3, 1, 0.5, 0, 0, 1), x_res = "default", t_res = "default"))

## ----eval=TRUE----------------------------------------------------------------
N <- 100
con <- rDMC(n = N, phi = c(0.3, 0.5, 1, 0.2, 0.05, 2.5, 3, 1, 0.5, 0, 0, 1), dt = 1e-05)
incon <- rDMC(n = N, phi = c(0.3, 0.5, -1, 0.2, 0.05, 2.5, 3, 1, 0.5, 0, 0, 1), dt = 1e-05)
data <- data.frame(congruency = rep(1:2, each = N), rt = c(con$rt, incon$rt), resp = c(con$resp, incon$resp))

## ----eval=TRUE----------------------------------------------------------------
deviation <- function(pars, data) {
  ind_con <- which(data$congruency==1)
  ind_incon <- which(data$congruency==2)
  ls_con <- dDMC(rt = data$rt[ind_con], resp = data$resp[ind_con], phi = c(pars[1:2], 1, pars[3:6], 1, 0.5, 0, 0, 1), x_res = "higher", t_res = "higher")$sum_log_pdf
  ls_incon <- dDMC(rt = data$rt[ind_incon], resp = data$resp[ind_incon], phi = c(pars[1:2], -1, pars[3:6], 1, 0.5, 0, 0, 1), x_res = "higher", t_res = "higher")$sum_log_pdf
  return(-2*(ls_con+ls_incon))
}

## ----eval=TRUE----------------------------------------------------------------
set.seed(3210)
(start_pars <- c(runif(1, .2, .6), runif(1, .3, .7), runif(1, .1, .6), runif(1, 0, .1), runif(1, 1.5, 4.5), runif(1, 0, 5)))
optim(par = start_pars, fn = deviation, method = "L-BFGS-B", lower = c(.2, .1, .1, 0.001, 1, 0.001), upper = c(.6, .9, .6, .1, 5, 5), data = data)

## -----------------------------------------------------------------------------
# /* method for the drift rate */
drift <- function(phi, x, t)  {
  mu = phi[3];
  l = phi[4];
  v = mu - l*x;
  return(v);
}

# /* method for the diffusion rate */
diffusion <- function(phi, x, t)  {
    return(phi[5]);
}

# /* method for the upper threshold */
upper_threshold <- function(phi, t)  {
    return(phi[6]);
}

# /* method for the lower threshold */
lower_threshold <- function(phi, t)  {
    return(-phi[6]);
}

# /* method for the contamination strength */
contamination_strength <- function(phi)  {
    return(phi[7]);
}

# /* method for the contamination probability distribution */
contamination_probability <- function(phi, t)  {
    gl = phi[8];
    gu = phi[9];
    pg = 0.0;
    if ((t >= gl) && (t <= gu)) {
      pg = 1.0/(gu - gl);
    }
    return(pg);
}

## -----------------------------------------------------------------------------
.Call("register_callbacks_tx", "drift", drift)
.Call("register_callbacks_tx", "diffusion", diffusion)
.Call("register_callbacks_tx", "upper_threshold", upper_threshold)
.Call("register_callbacks_tx", "lower_threshold", lower_threshold)
.Call("register_callbacks_tx", "contamination_strength", contamination_strength)
.Call("register_callbacks_tx", "contamination_probability", contamination_probability)

## ----echo=TRUE----------------------------------------------------------------
phi <- c(0.3, 0.5, 1.0, 0.5, 1.0, 0.5, 0.0, 0.0, 1.0)

set.seed(123)
dCSTM_TX(2, resp = "upper", phi = phi, x_res = "high", t_res = "high")
set.seed(123)
dLIM(2, resp = "upper", phi = phi, x_res = "high", t_res = "high")

# unregister the custom function
.Call("unregister_callbacks_tx")

## ----eval=FALSE---------------------------------------------------------------
# Rcpp::sourceCpp(code =
# "
#   #include <Rcpp.h>
#   #include <math.h>
#   using namespace Rcpp;
# 
#   /* method for the drift rate */
#   // [[Rcpp::export]]
#   double drift_cpp(NumericVector phi, double x, double t) {
#     double mu = phi[2];
#     double l = phi[3];
#     double v = mu - l*x;
#     return v;
#   }
# 
#   /* method for the diffusion rate */
#   // [[Rcpp::export]]
#   double diffusion_cpp(NumericVector phi, double x, double t)  {
#     return phi[4];
#   }
# 
#   /* method for the upper threshold */
#   // [[Rcpp::export]]
#   double upper_threshold_cpp(NumericVector phi, double t)  {
#     return phi[5];
#   }
# 
#   /* method for the lower threshold */
#   // [[Rcpp::export]]
#   double lower_threshold_cpp(NumericVector phi, double t)  {
#     return -phi[5];
#   }
# 
#   /* method for the contamination strength */
#   // [[Rcpp::export]]
#   double contamination_strength_cpp(NumericVector phi)  {
#     return phi[6];
#   }
# 
#   /* method for the contamination probability distribution */
#   // [[Rcpp::export]]
#   double contamination_probability_cpp(NumericVector phi, double t)  {
#     double gl = phi[7];
#     double gu = phi[8];
#     double pg = 0.0;
#     if ((t >= gl) && (t <= gu)) {
#       pg = 1.0/(gu - gl);
#     }
#     return pg;
#   }
# ")

## ----eval=FALSE---------------------------------------------------------------
# system("R CMD SHLIB my_lim.cpp")
# 
# # under Windows
# dyn.load("my_lim.dll")
# 
# # under Linux/MacOS
# dyn.load("my_lim.so")
# 
# # check if you find my_lim.dll or my_lim.so
# tail(getLoadedDLLs(), 1)

## ----echo=FALSE---------------------------------------------------------------
cppfile <- system.file("extdata", "my_lim.cpp", package = "ream")
tmp_dir <- normalizePath(tempdir(), winslash = "/", mustWork = TRUE)
cppfile_cpy <- paste0(tmp_dir, "/my_lim.cpp")

# copy source code to temp folder
invisible(file.copy(from = cppfile, to = cppfile_cpy, overwrite = TRUE))

# prepare dll or so file
dll <- file.path(tmp_dir, paste0("my_lim", .Platform$dynlib.ext))

# generate dll file
cmd <- paste(shQuote(file.path(R.home("bin"), "R")),
             "CMD SHLIB", shQuote(cppfile_cpy),
             "-o", shQuote(dll))
system(cmd)

# Load the plugin dynamic library
dyn.load(dll)

dllinfo <- tail(getLoadedDLLs(), 1)
dllinfo$my_lim$path <- "my_lim.so"
dllinfo

## -----------------------------------------------------------------------------
# Get the memory address of the custom C function
ptr.drift <- getNativeSymbolInfo("drift", PACKAGE = "my_lim")$address
ptr.diff <- getNativeSymbolInfo("diffusion", PACKAGE = "my_lim")$address
ptr.u_thr <- getNativeSymbolInfo("upper_threshold", PACKAGE = "my_lim")$address
ptr.l_thr <- getNativeSymbolInfo("lower_threshold", PACKAGE = "my_lim")$address
ptr.cont_str <- getNativeSymbolInfo("contamination_strength", PACKAGE = "my_lim")$address
ptr.cont_prob <- getNativeSymbolInfo("contamination_probability", PACKAGE = "my_lim")$address

# Register the function pointer with your package
.Call("register_callbacks_tx", "drift", ptr.drift)
.Call("register_callbacks_tx", "diffusion", ptr.diff)
.Call("register_callbacks_tx", "upper_threshold", ptr.u_thr)
.Call("register_callbacks_tx", "lower_threshold", ptr.l_thr)
.Call("register_callbacks_tx", "contamination_strength", ptr.cont_str)
.Call("register_callbacks_tx", "contamination_probability", ptr.cont_prob)

## -----------------------------------------------------------------------------
phi <- c(0.3, 0.5, 1.0, 0.5, 1.0, 0.5, 0.0, 0.0, 1.0)

set.seed(123)
dCSTM_TX(2, resp = "upper", phi = phi, x_res = "high", t_res = "high")
set.seed(123)
dLIM(2, resp = "upper", phi = phi, x_res = "high", t_res = "high")

# un-register the custom function
.Call("unregister_callbacks_tx")

## ----eval=FALSE---------------------------------------------------------------
# # under Windows
# dyn.unload("my_lim.dll")
# 
# # under Linux/MacOS
# dyn.unload("my_lim.so")

## ----echo=FALSE---------------------------------------------------------------
# un-load the dynamic linked library
dyn.unload(dll)

