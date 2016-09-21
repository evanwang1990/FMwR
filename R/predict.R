predict.FM.c <- function (object, newdata = NULL, max_threads = 1)
{
  if (is.null(newdata)) {
    stop("newdata is null!")
  }
  if (!is.data.frame(newdata)) {
    stop("newdata must be a data.frame")
  }

  formula <- object$Model$formula
  scales <- object$Scaled
  normalize <- ifelse(is.null(scales), FALSE, TRUE)
  dt <- eval(as.call(list(quote(deal_data), formula, newdata, na.omit, normalize, scales, TRUE, max_threads)))
  
  pred <- FMPredict(dt[["X"]], object$Model, max_threads)
  if (!is.null(dt[["na.rows"]]))
    pred <- NApredict(pred, dt[["na.rows"]])

  pred
}
