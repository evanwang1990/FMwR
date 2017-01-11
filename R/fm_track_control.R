##' @title Auxiliary for controlling tracking routines
##'
##' @param step_size record parameters every \strong{step_size} iteration
##'
##' @param convergence,evaluate.metric control how to early stop
##'
##' @details
##' Early stopping is controlled by \strong{evaludate.metric} and \strong{convergence}. If the difference of the value between two interations, which is
##' calculated by \code{evaluate.metric}, is smaller than \code{convergence} for three times in a row, iteration will stop early.
##' \code{evaluate.metric} includes:
##' \itemize{
##' \item "LL":loglikehood
##' \item "AUC": auc
##' \item "ACC": accurancy
##' \item "RMSE": root mean square err
##' \item "MAE": mean absolute error
##' }
##' "LL","AUC","ACC" are for CLASSIFICATION task. And "RMSE","MAE" are for REGRESSION task.
##'
track.control <- function(step_size = -1, evaluate.metric = "LL", convergence = 1e-4)
{
  stopifnot(evaluate.metric %in% c("AUC", "ACC", "LL", "RMSE", "MAE"))
  res <- list(max_iter = 1, step_size = step_size, evaluate.metric = evaluate.metric, convergence = convergence)
  class(res) <- "track.control"
  res
}
