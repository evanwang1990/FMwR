fm.train <- function(...) {
  UseMethod("fm.train")
}

#' @title Fitting Factorization Machine Models
#'
#' @description
#'
#' @examples
#'
#' # 1. classification
#'
#' library(FMwR)
#' data("airquality")
#' airquality <- airquality[complete.cases(airquality),]
#' xairquality <- airquality[sample(1:nrow(airquality), 1000, TRUE),]
#' airquality$Ozone <- ifelse(airquality$Ozone > 60, 1, -1)
#' idx <- sample(1:nrow(airquality), ceiling(nrow(airquality)*0.6))
#'
#' train <- fm.matrix(airquality[idx, 2:6], airquality[idx, 1])
#' test <- fm.matrix(airquality[-idx, 2:6], airquality[-idx, 1])
#'
#' fm_fit <- fm.train(train, control = list(track.control(step_size = 300000),
#'                                          solver.control(solver = TDAP.solver(random_step = 10, gamma = 1e-5), max_iter = 1000000),
#'                                          model.control(L2.w1 = 0.1)))
#' # or
#' # fm_fit <- fm.train(train, control = list(track.control(step_size = 1),
#' #                                          solver.control(solver = MCMC.solver(), max_iter = 20)))
#'
#' fm_track <- fm.track(fm_fit, train, test, evaluate.metric = "ACC")
#' plot(fm_track)
#' fm_pred <- predict(fm_fit, test)
#' table(test$labels, fm_pred > 0.5)
#'
#'
#' # 2. regression
#'
#' data("airquality")
#' airquality <- airquality[complete.cases(airquality),]
#' airquality <- airquality[sample(1:nrow(airquality), 1000, TRUE),]
#' idx <- sample(1:nrow(airquality), ceiling(nrow(airquality)*0.6))
#'
#' train <- fm.matrix(airquality[idx, 2:6], airquality[idx, 1])
#' test <- fm.matrix(airquality[-idx, 2:6], airquality[-idx, 1])
#'
#' fm_fit <- fm.train(train, normalize = T, control = list(track.control(step_size = 10000, evaluate.metric = "RMSE", convergence = 0),
#'                                                         solver.control(solver = SGD.solver(random_step = 10, learn_rate = 0.0001), max_iter = 1000000),
#'                                                         model.control(task = "RE", factor.number = 3, L2.v = 0.5)))
#'
#' # or
#' # fm_fit <- fm.train(train, normalize = T, control = list(track.control(step_size = 1, evaluate.metric = "RMSE"),
#' #                                                         solver.control(solver = ALS.solver(), max_iter = 20),
#'                                                           model.control(task = "RE", factor.number = 2)))
#' fm_track <- fm.track(fm_fit, train, test, evaluate.metric = "MAE")
#' plot(fm_track)
#' fm_pred <- predict(fm_fit, test)
#'
#'
#'
fm.train.fm.matrix <- function(data, normalize = TRUE, max_threads = 1, control)
{
  if (!is.logical(normalize) && !is.integer(normalize)) {
    stop("normalize should be a logical value or an integer vector")
  }
  if (is.logical(normalize)) {
    if (normalize)
      normalize <- 1:ncol(data)
    else
      normalize <- -1
  } else {
    if (any(normalize < 1 || normalize > ncol(data))) {
      stop("the columns to be normalized is out of range")
    }
  }

  # controls
  control_default <- list(
    model      = model.control(),
    solver     = solver.control(max_iter = max(10000, 2*nrow(data))),
    track      = track.control())
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
  control_default$track$max_iter <- control_default$solver$max_iter
  if (attr(control_default$solver$solver, "solver") %in% c("MCMC", "ALS") && control_default$track$step_size > 1) {
    warning("the step_size will be set to 1 for MCMC/ALS solver")
    control_default$track$step_size <- 1
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

  fit <- FM(data, normalize-1, fm_controls = control_default$model, solver_controls = control_default$solver, track_controls = control_default$track, list())

  fit
}
