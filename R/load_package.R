suggest_package <- function(package_name)
{
  res <- 1
  if (!exists(package_name)) {
    res <- tryCatch(do.call(library, list(package = package_name)), error = function(e) 0)
  }
  if (!identical(res, 0))
    res <- 1
  res
}
