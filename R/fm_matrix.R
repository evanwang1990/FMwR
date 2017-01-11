fm.matrix <- function(...)
{
  UseMethod("fm.matrix")
}

fm.matrix.dgCMatrix <- function(data, labels = NULL, feature_names = NULL)
{
  # labels
  if (!is.null(labels)) {
    stopifnot(is.numeric(labels))
    stopifnot(length(labels) == nrow(data))
    stopifnot(!any(is.na(labels)))
  }

  # feature_names
  if (is.null(feature_names)) {
    feature_names <- colnames(data)
  } else {
    stopifnot(ncol(data) == length(feature_names))
  }
  if (is.null(feature_names)) {
    stop("there's no feature_names")
  }

  data_t <- Matrix::t(data)
  features <- list(
    value = data_t@x,
    col_idx = data_t@i,
    row_size = diff(data_t@p),
    dim = rev(data_t@Dim),
    size = length(data_t@i)
    )
  attr(features, "feature_names") <- feature_names
  attr(features, "transposed") <- FALSE

  res <- list(
    features = features,
    labels = labels
    )
   attr(res, "class") <- "fm.matrix"

   res
}

fm.matrix.matrix <- function(data, labels = NULL, feature_names = NULL)
{
  if (!is.null(labels)) {
    stopifnot(is.numeric(labels))
    stopifnot(nrow(data) == length(labels))
    stopifnot(!any(is.na(labels)))
  }

  if (is.null(feature_names) && is.null(colnames(data))) {
    stop("there's no feature_names")
  }
  if (!is.null(feature_names) && length(feature_names) != nrow(data)) {
    stop("the length of feature_names should equal to the rows of data!")
  }

  data <- Matrix::Matrix(data, sparse = TRUE)
  fm.matrix.dgCMatrix(data, labels, feature_names)
}

fm.matrix.data.frame <- function(data, labels = NULL, feature_names = NULL)
{
  if (!is.null(labels)) {
    stopifnot(is.numeric(labels))
    stopifnot(nrow(data) == length(labels))
    stopifnot(!any(is.na(labels)))
  }

  if (is.null(feature_names) && is.null(names(data))) {
    stop("there's no feature_names")
  }
  if (!is.null(feature_names) && length(feature_names) != nrow(data)) {
    stop("the length of feature_names should equal to the rows of data!")
  }

  col_classes <- sapply(data, class)
  if (any(col_classes == "character") || any(col_classes == "factor")) {
    stop("all the elements in data.frame should be numeric")
  }
  data <- Matrix::Matrix(as.matrix(data), sparse = TRUE)
  fm.matrix.dgCMatrix(data, labels, feature_names)
}

dim.fm.matrix <- function(data) {
  data$features$dim
}

names.fm.matrix <- function(data) {
  attr(data$features, "feature_names")
}
