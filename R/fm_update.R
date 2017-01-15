#' @title Update Factorization Machines Model
#'
#' @description
#'
#' @param object a FM object, created by \link{fm.train}
#'
#' @param data a fm.matrix object, created by \link{fm.matrix}
#'
#' @param normalize whether to normalize data
#'
#' @param control list of congtrols, \link{model.control}, \link{solver.control}, \link{track.control}
#'
#' @usage fm.update(object, data, normalize = TRUE, control = NULL)
fm.update <- function(...) {
  UseMethod("fm.update")
}

fm.update.FM <- function(object, data, normalize = TRUE, control = NULL) {
  # data
  if (class(data) != "fm.matrix") {
    stop("The input must be a fm.matrix object")
  }
  if (is.null(data$labels)) {
    stop("there are no labels in data")
  }

  f1 <- setdiff(object$Scales$model.vars, attr(data$features, "feature_names"))
  if (length(f1) > 0) {
    stop(paste0(paste0(f1, collapse = ","), " are not in data"))
  }
  f2 <- setdiff(attr(data$features, "feature_names"), object$Scales$model.vars)
  if (length(f1) > 0) {
    stop(paste0(paste0(f2, collapse = ","), " are not in the previously saved model"))
  }
  if (!identical(object$Scales$model.vars, attr(data$features, "feature_names"))) {
    stop("the orders of features in data is different from those in the previously saved model")
  }

  # normalize
  if (!is.logical(normalize) && !is.integer(normalize)) {
    stop("normalize should be a logical value or an integer vector")
  }
  is_model_normalized <- !is.null(object$Scales$mean)
  if (is.logical(normalize)) {
    if (normalize) {
      if (!is_model_normalized) {
        warning("all the features are not normalized in the previously saved model")
        choice <- c("yes", "no", "stop")
        decision <- readline("do you want to normalize all the features? [yes/no/stop]")
        decision <- choice[pmatch(decision, choice, 3)]
        if (decision == "yes") {
          normalize <- 1:ncol(data)
        } else if (decision == "no") {
          normalize <- -1
        } else {
          stop("stop updating model")
        }
      }
      message("follow the normalization settings in the previously saved model")
      normalize <- which(fm_fit$Scales$mean != 0 & fm_fit$Scales$std != 1)
    } else {
      if (is_model_normalized)
        warning("some features have been normalized in previously saved model, but those in data will not")
      normalize <- -1
    }
  } else {
    if (any(normalize < 1 || normalize > ncol(data))) {
      stop("the columns to be normalized is out of range")
    }
    if (!is_model_normalized) {
      warning("all the features are not normalized in the previously saved model")
      choice <- c("yes", "no", "stop")
      decision <- readline("do you still want to normalize the selected features? [yes/no/stop]")
      decision <- choice[pmatch(decision, choice, 3)]
      if (decision == "no") {
        normalize <- -1
      } else if (decision == "stop"){
        stop("stop updating model")
      }
    } else if (!identical(which(object$Scales$mean != 0 | object$Scales$std != 1), normalize)) {
      stop("the selected features to normalize are different from those in previously saved model")
    }
  }

  # control
  control_default <- list(
    model = attr(object$Model, "model.control"),
    solver= attr(object$Model, "solver.control"),
    track = attr(object$Model, "track.control"))
  control_default$solver$max_iter <- max(10000, 2*nrow(data))
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
  control_default$model$nthreads      <- fm.get_threads()
  control_default$solver$nthreads     <- fm.get_threads()
  control_default$track$max_iter <- control_default$solver$max_iter
  if (attr(control_default$solver$solver, "solver") %in% c("MCMC", "ALS") && control_default$track$step_size > 1) {
    warning("the step_size will be set to 1 for MCMC/ALS solver")
    control_default$track$step_size <- 1
  }

  if (attr(object$Model, "model.control")$task != control_default$model$task) {
    stop(paste0("current task is ",control_default$model$task, " while the previously saved model's task is ", attr(object$Model, "model.control")$task))
  }
  if (control_default$model$task == "CLASSIFICATION") {
    unique_target <- unique(data$labels)
    if (length(unique_target) != 2) {
      stop("target should have two levels")
    }
    if (identical(sort(unique_target), c(0, 1))) {
      data$labels <- ifelse(data$labels < 1, -1, 1)
    } else if (!identical(sort(unique_target), c(-1, 1))) {
      stop("target should be c(0, 1) or c(-1, 1)")
    }
  }

  fit <- FM(data, normalize-1, fm_controls = control_default$model, solver_controls = control_default$solver, track_controls = control_default$track, object)
  if (attr(object$Model, "track.control")$step_size > 0 && control_default$track$step_size > 0) {
    trace_idxes <- object$Trace$trace[[1]]
    trace <- c(object$Trace$trace, fit$Trace$trace[-1])
    trace[[1]] <- c(trace_idxes, fit$Trace$trace[[1]] + trace_idxes[length(trace_idxes)])
    evaluation_of_train <- c(object$Trace$evaluation.train, fit$Trace$evaluation.train)
    fit$Trace <- list(
      trace = trace,
      evaluation.train = evaluation_of_train)
  }
  fit
}
