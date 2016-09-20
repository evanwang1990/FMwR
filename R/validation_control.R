validation.control <- function(step_size = -1, evaluate.method = "LL")
{
  stopifnot(evaluate.method %in% c("AUC", "ACC", "LL", "RMSE", "MAE"))
  res <- list(max_iter = 1, step_size = step_size, evaluate.method = evaluate.method)
  class(res) <- "validation.control"
  res
}
