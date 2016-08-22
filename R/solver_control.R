solver.control <- function(nthreads, max_iter, evaluation, solver = TDAP.control())
{
  max_threads = detectCores()
  if (nthreads >= max_threads) {
    warning("nthreads is greater than the max number of threads.\n  so nthreads will be set as ", max_threads, "!\n\n", immediate. = TRUE)
    nthreads = max_threads
  }

  stopifnot(evaluation %in% c("AUC", "ACC", "LL", "RMSE", "MAE"))


  list(nthreads = nthreads, max_iter = max_iter, evaluation = evaluation, solver = solver)
}


MCMC.control.default <- list(
  alpha_0 = 1.0,
  gamma_0 = 1.0,
  beta_0  = 1.0,
  mu_0    = 0.0,
  alpha    = 1.0,
  w0_mean_0 = 1.0
)

MCMC.control <- function(...)
{
  controls <- control_assign(MCMC.control.default, list(...))

  if (controls$is.default) {
    message("Use default MCMC solver.\n\n")
  }

  controls$contr
}


ALS.control.default <- list(
  alpha_0 = 1.0,
  gamma_0 = 1.0,
  beta_0  = 1.0,
  mu_0    = 0.0,
  alpha    = 1.0,
  w0_mean_0 = 1.0
)

ALS.control <- function(...)
{
  controls <- control_assign(ALS.control.default, list(...))

  if (controls$is.default) {
    message("Use default ALS solver.\n\n")
  }

  controls$contr
}


SGD.control.default <- list(
  learn_rate = 0.01,
  l1_penalty = FALSE,
  random_step = 1L
)

SGD.control <- function(...)
{
  controls <- control_assign(SGD.control.default, list(...))

  if (controls$is.default) {
    message("Use default SGD solver.\n\n")
  }

  controls$contr
}

FTRL.control.default <- list(
  L1.w = 0.5,
  L1.v = 1.0,
  L2.w = 0.1,
  L2.v = 0.5,
  alpha.w = 0.1,
  alpha.v = 0.1,
  beta.w = 1.0,
  beta.w = 1.0
)

FTRL.control <- function(...)
{
  controls <- control_assign(FTRL.control.default, list(...))

  if (controls$is.default) {
    message("Use default FTRL solver. \n\n")
  }

  controls$contr
}

TDAP.control.default <- list(
  L1.w = 0.5,
  L1.v = 1.0,
  L2.w = 0.1,
  L2.v = 0.5,
  gamma = 1e-4,
  alpha.w = 0.1,
  alpha.v = 0.1,
  random_step = 1L
)

TDAP.control <- function(...)
{
  controls <- control_assign(TDAP.control.default, list(...))

  if (controls$is.default) {
    message("Use default TDAP solver. \n\n")
  }

  controls$contr
}
