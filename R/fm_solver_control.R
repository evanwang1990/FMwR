##' @title Auxiliary for controlling optimization routines
##'
##' @description
##' Auxiliary functions for optimization routines
##'
##' @usage
##' solver.control(max_iter = 10000, nthreads = 1, convergence = 1e-4, evaluate.metric = "LL", solver = TDAP.solver())
##'
##' @param max_iter integer giving maximal number of iterations, 25 for MCMC/ALS and 10000 for other optimization routines by default
##'
##' @param nthreads integer, number of threads to speed up computing, \strong{openmp} should be supported
##'
##' @param solver function to set parameters of optimization routine
##'
##' @details
##' SGD/FTRL/TDAP updates model every single example, while MCMC/ALS needs to sweeps through all the data for each update.
##' So the maximal number of iterations of MCMC/ALS is much smaller, which is 25 by default.
##'
##' By now, only MCMC and ALS support parallel computing. And if openmp is not supported, nthreads makes no sense.
##'
##' \strong{solver} is a function to set parameters of optimization routine further, including \link{ALS.solver}
##' \link{MCMC.solver} \link{SGD.solver} \link{TDAP.solver} \link{FTRL.solver}

solver.control <- function(max_iter = 10000, nthreads = 1, solver = TDAP.solver())
{
  solver_ <- deparse(substitute(solver))
  if (grepl("MCMC|ALS", solver_) && max_iter > 100) {
    warning("the maximum number of iteratorions for MCMC/ALS solver is 100, so max_iter will be set to 100")
    max_iter <- min(max_iter, 100)
  }

  max_threads = parallel::detectCores()
  if (nthreads >= max_threads) {
    warning("nthreads is greater than the max number of threads.\n  so nthreads will be set as ", max_threads, "!\n\n", immediate. = TRUE)
    nthreads = max_threads
  }

  res <- list(nthreads = nthreads, max_iter = max_iter, solver = solver)
  class(res) <- "solver.control"
  res
}


MCMC.solver.default <- list(
  alpha_0   = 1.0,
  gamma_0   = 1.0,
  beta_0    = 1.0,
  mu_0      = 0.0,
  alpha     = 1.0,
  w0_mean_0 = 1.0
)


##' @title MCMC solver
##' @param alpha_0
##' @param gamma_0
##' @param beta_0
##' @param mu_0
##' @param alpha
##' @param w0_mean_0
##' @family solver
MCMC.solver <- function(...)
{
  controls <- control_assign(MCMC.solver.default, list(...))

  attr(controls$contr, "solver") <- "MCMC"

  controls$contr
}


ALS.solver.default <- list(
  alpha_0   = 1.0,
  gamma_0   = 1.0,
  beta_0    = 1.0,
  mu_0      = 0.0,
  alpha     = 1.0,
  w0_mean_0 = 1.0
)

##' @title ALS solver
##' @param alpha_0
##' @param gamma_0
##' @param beta_0
##' @param mu_0
##' @param alpha
##' @param w0_mean_0
##' @family solver
ALS.solver <- function(...)
{
  controls <- control_assign(ALS.solver.default, list(...))

  attr(controls$contr, "solver") <- "ALS"

  controls$contr
}


SGD.solver.default <- list(
  learn_rate  = 0.01,
  random_step = 1L
)

##' @title SGD solver
##' @param learn_rate
##' @param random_step
##' @family solver
SGD.solver <- function(...)
{
  controls <- control_assign(SGD.solver.default, list(...))

  attr(controls$contr, "solver") <- "SGD"

  controls$contr
}

FTRL.solver.default <- list(
  alpha_w     = 0.1,
  alpha_v     = 0.1,
  beta_w      = 1.0,
  beta_v      = 1.0,
  random_step = 1L
)


##' @title FTRL solver
##' @param alpha_w
##' @param alpha_v
##' @param beta_w
##' @param beta_v
##' @param random_step
##' @family solver
FTRL.solver <- function(...)
{
  controls <- control_assign(FTRL.solver.default, list(...))

  attr(controls$contr, "solver") <- "FTRL"

  controls$contr
}

TDAP.solver.default <- list(
  gamma       = 1e-4,
  alpha_w     = 0.1,
  alpha_v     = 0.1,
  random_step = 1L
)


##' @title TDAP solver
##' @param gamma
##' @param alpha_w
##' @param alpha_v
##' @param random_step
##' @family solver
TDAP.solver <- function(...)
{
  controls <- control_assign(TDAP.solver.default, list(...))

  attr(controls$contr, "solver") <- "TDAP"

  controls$contr
}
