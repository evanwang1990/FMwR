#' @title Set Number of Threads
#'
#' @description set the maximum number of available threads for parallel execution
#'
#' @param max.threads an integer, number of threads to speed up computing, \strong{openmp} should be supported.
#'
#' @usage fm.set_threads(max.threads = 1)
fm.set_threads <- function(max.threads = 1)
{
  actual.max.threads <- parallel::detectCores()
  if (max.threads > actual.max.threads) {
    warning(paste0("max number of available threads is ", actual.max.threads, " which is less than max.threads\nmax.threads will be set as ", actual.max.threads))
    max.threads <- actual.max.threads
  }
  options("FM.threads" = max.threads)
}


fm.get_threads <- function()
{
  max_threads <- getOption("FM.threads")
  if (is.null(max_threads)) {
    max_threads <- 1
  }
  max_threads
}
