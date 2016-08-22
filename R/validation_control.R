validation.control <- function(max_iter, step_size = -1, evaluation = "LL")
{
  stopifnot(evaluation %in% c("AUC", "ACC", "LL", "RMSE", "MAE"))
  list(max_iter = max_iter, step_size = step_size, evaluation = evaluation)
}
