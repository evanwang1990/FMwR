fm.track <- function(object, ...) {
  UseMethod("fm.track")
}


fm.track.FM <- function(object, data = NULL, newdata = NULL, data.normalize = TRUE, newdata.normalize = TRUE, evaluate.metric = c("LL", "AUC", "ACC", "RMSE", "MAE"), max_threads = 1L)
{
  if (is.null(object$Trace)) {
    stop("no Trace in fm object")
  }
  task <- attr(object$Model, "model.control")$task
  evaluate.metric <- match.arg(evaluate.metric)
  if ((task == "CLASSIFICATION" && evaluate.metric %in% c("RMSE", "MAE")) || (task == "REGRESSION" && evaluate.metric %in% c("LL", "AUC", "ACC"))) {
    stop("evaluate.metric is error")
  }
  new_metric <- ifelse(attr(object$Model, "track.control")$evaluate.metric == evaluate.metric, FALSE, TRUE)

  normalize <- ifelse(is.null(object$Scales$mean), FALSE, TRUE)


  if (new_metric) {
    if (is.null(data) || missing(data)) stop("data is missing")
    if (class(data) != "fm.matrix") stop("data is not a fm.matrix object")
    if (task == "CLASSIFICATION") {
      unique_target <- unique(data$labels)
      if (length(unique_target) != 2) {
        stop("data's target should have two levels")
      }
      if (identical(sort(unique_target), c(0, 1))) {
        data$labels <- ifelse(data$labels < 1, -1, 1)
      } else if (!identical(sort(unique_target), c(-1, 1))) {
        stop("data's target should be c(0, 1) or c(-1, 1)")
      }
    }
    val1 <- FMTrack(data, object, normalize, evaluate.metric, max_threads)
  } else {
    val1 <- object$Trace$evaluation.train
  }

  val2 <- NULL
  if (is.null(newdata) || missing(newdata)) stop("newdata is missing")
  if (class(newdata) != "fm.matrix") stop("newdata is not a fm.matrix object")
  if (task == "CLASSIFICATION") {
    unique_target <- unique(newdata$labels)
    if (length(unique_target) != 2) {
      stop("newdata's target should have two levels")
    }
    if (identical(sort(unique_target), c(0, 1))) {
      newdata$labels <- ifelse(newdata$labels < 1, -1, 1)
    } else if (!identical(sort(unique_target), c(-1, 1))) {
      stop("newdata's target should be c(0, 1) or c(-1, 1)")
    }
  }
  val2 <- FMTrack(newdata, object, normalize, evaluate.metric, max_threads)

  res <- list(
    iter = object$Trace$trace[[1]],
    trace.train = val1,
    trace.test = val2
    )

  attr(res, "class") <- c(class(res), "FMTrace")
  attr(res, "evaluate.metric") <- evaluate.metric
  res
}

plot.FMTrace <- function(object)
{
  data <- data.frame(
    iter = object$iter,
    trace = object$trace.train,
    type = "trace.train"
    )
  if (!is.null(object$trace.test)) {
    data <- rbind(data, data.frame(
      iter = object$iter,
      trace = object$trace.test,
      type = "trace.test"
      ))
  }

  evaluate.metric <- switch(
    attr(object, "evaluate.metric"),
    LL = "Loglikehood",
    ACC = "Accurancy",
    AUC = "AUC",
    MAE = "MAE",
    RMSE = "RMSE"
    )
  gg <- suggest_package("ggplot2")
  if (gg) {
    ggplot(data = data, aes(x = iter)) +
           geom_point(aes(y = trace, color = as.factor(type))) +
           labs(list(title = "Plot of Trace", x = "number of iterations", y = evaluate.metric, color = NULL)) +
           theme(legend.position = "bottom")
  } else {
   plot(data$iter, data$trace, col = ifelse(data$type == "trace.train", "blue", "red"), type = 'p', pch = 20,
        xlab = "number of iterations", ylab = evaluate.metric, main = "Plot of Trace", sub = "blue:data red:newdata")
  }
}
