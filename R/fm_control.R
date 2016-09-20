model.control <- function(task = "CLASSIFICATION", solver = "TDAP", ...)
{
  stopifnot(task %in% c("CLASSIFICATION", "REGRESSION"));
  stopifnot(solver %in% c("MCMC", "ALS", "SGD", "FTRL", "TDAP"));
  controls <- control_assign(model.control.default, list(...))

  if (controls$is.default) {
    message("Use default model settings.\n")
  }

  res <- list(task = task, solver = solver, nthreads = 1, hyper.params = controls$contr)
  class(res) <- "model.control"
  res
}

model.control.default <- list(
  # w0
  keep.w0       = TRUE,
  L2.w0         = 0.0,
  # w1
  keep.w1       = TRUE,
  L1.w1         = 0.0,
  L2.w1         = 0.0,
  # v
  factor.number = 2L,
  v.init_mean   = 0.0,
  v.init_stdev  = 0.01,
  L1.v          = 0.0,
  L2.v          = 0.0
)
