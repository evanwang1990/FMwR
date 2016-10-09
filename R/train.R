fm <- function(formula, data, na.action = na.omit, normalize = TRUE, max_threads = 1, control)
{
  if (!is.data.frame(data)) {
    stop("data must be a data.frame")
  }
  mc <- match.call()

  # controls
  control_default <- list(
    model      = model.control(),
    solver     = solver.control(max_iter = max(10000, length(nrow(data)))), #TODO 一直显示use default TDAP solver...
    validation = validation.control())
  if (!missing(control)) {
    ca <- sapply(control, function(x) { grepl("*.control", class(x)) })
    if (!all(ca)) {
      stop("control list is wrong")
    }
    ca_names <- sapply(control, function(x) { strsplit(class(x), '[.]')[[1]][1] })
    for (ci in 1:length(ca_names)) {
      control_default[[ca_names[ci]]] <- control[[ci]]
    }
  }
  control_default$model$nthreads      <- max_threads
  control_default$solver$nthreads     <- max_threads
  control_default$validation$max_iter <- control_default$solver$max_iter

  # data
  transpose <- ifelse(control_default$model$solver %in% c("MCMC", "ALS"), TRUE, FALSE)
  m <- match(c("formula", "data", "na.action", "normalize", "scales", "max_threads"), names(mc), 0L)
  m <- mc[c(1L, m)]
  m[[1]] <- quote(deal_data)
  m$transpose <- transpose # update
  dt <- eval(m)
  xx <<- dt
  # Y
  Y <- dt[["Y"]]
  if (control_default$model$task == "CLASSIFICATION") {
    if (!is.factor(Y)) {
      Y <- factor(Y)
    }
    if (length(levels(Y)) != 2) {
      stop("target should have 2 levels")
    }
    Y <- ifelse(Y == levels(Y)[1], -1, 1)
  }

  # fit <- FM(data_ = dt[["X"]], target = Y, fm_controls = control_default$model, solver_controls = control_default$solver, validation_controls = control_default$validation)
  fit <- FM(data_ = dt[c("X", "X_t")], target = Y, fm_controls = control_default$model, solver_controls = control_default$solver, validation_controls = control_default$validation)

  fit$Scaled <- dt[["scales"]]
  fit$Model$model.vars <- dt[["dimnames"]]
  fit$Model$formula <- formula
  if (control_default$validation$step_size > 0) {
    attr(fit$Validation, "class") <- control_default$solver$evaluate.method
  }
  fit$Model <- fit$Model[match(c("formula", "model.control", "model.vars", "model.params"), names(fit$Model))]

  fit
}
