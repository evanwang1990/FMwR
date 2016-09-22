validate <- function(object, ...) {
  UseMethod("validate")
}


validate.FM.c <- function(object, data = NULL, newdata = NULL, evaluate.method = c("LL", "AUC", "ACC"), max_threads = 1L)
{
  if (is.null(object$Validation)) {
    stop("no validation in fm object")
  }
  evaluate.method <- match.arg(evaluate.method)
  new_method <- ifelse(class(object$Validation) == evaluate.method, FALSE, TRUE)
  if (new_method && (is.null(data) || missing(data))) {
    stop("data is missing")
  }

  formula <- object$Model$formula
  scales <- object$Scaled
  normalize <- ifelse(is.null(scales), FALSE, TRUE)

  val1 <- object$Validation$evaluation.train
  if (!is.null(data)) {
    if (!is.data.frame(data)) stop("data is not a data.frame")
    if (new_method) {
      dt1 <- eval(as.call(list(quote(deal_data), formula, data, na.omit, normalize, scales, FALSE, max_threads)))
      if (is.null(dt1[["Y"]])) {
        stop("no target variable in data")
      }
      Y <- dt1[["Y"]]
      if (!is.factor(Y)) {
        Y <- factor(Y)
      }
      if (length(levels(Y)) != 2) {
        stop("target should have 2 levels")
      }
      Y <- ifelse(Y == levels(Y)[1], -1, 1)
      val1 <- FMValidate(dt1[["X"]], Y, object$Model, object$Validation$trace, evaluate.method, max_threads)
    }
  }

  val2 <- NULL
  if (!is.null(newdata)) {
    if (!is.data.frame(newdata)) stop("newdata is not a data.frame")
    dt2 <- eval(as.call(list(quote(deal_data), formula, newdata, na.omit, normalize, scales, FALSE, max_threads)))
    if (is.null(dt2[["Y"]])) {
      stop("no target variable in newdata")
    }
    Y2 <- dt2[["Y"]]
    if (!is.factor(Y2)) {
      Y2 <- factor(Y2)
    }
    if (length(levels(Y2)) != 2) {
      stop("target should have 2 levels")
    }
    Y2 <- ifelse(Y2 == levels(Y2)[1], -1, 1)
    val2 <- FMValidate(dt2[["X"]], Y2, object$Model, object$Validation$trace, evaluate.method, max_threads)
  }

  res <- list(
    iter = object$Validation$trace[[1]],
    trace.train = val1,
    trace.valid = val2
    )

  attr(res, "class") <- c(class(res), "FM.c.v")
  attr(res, "evaluate.method") <- evaluate.method
  res
}

plot.FM.c.v <- function(object)
{
  data <- data.frame(
    iter = object$iter,
    trace = object$trace.train,
    type = "trace.train"
    )
  if (!is.null(object$trace.valid)) {
    data <- rbind(data, data.frame(
      iter = object$iter,
      trace = object$trace.valid,
      type = "trace.valid"
      ))
  }

  evaluate.method <- switch(
    attr(object, "evaluate.method"),
    LL = "loglikehood",
    ACC = "accurancy",
    AUC = "auc"
    )
  gg <- suggest_package("ggplot2")
  if (gg) {
    res <- ggplot(data = data, aes(x = iter)) +
             geom_point(aes(y = trace, color = as.factor(type))) +
             labs(list(title = "Validation Plot", x = "number of iterations", y = evaluate.method, color = NULL)) +
             theme(legend.position = "bottom")
  } else {
    res <- plot(data$iter, data$trace, col = ifelse(data$type == "trace.train", "blue", "red"), type = 'p', pch = 20,
                xlab = "number of iterations", ylab = "evaluate.method", main = "Validation Plot")
  }

  res
}
