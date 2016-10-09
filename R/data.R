deal_data <- function(formula, data, na.action, normalize, scales, delete.y = FALSE, transpose = FALSE, max_threads = 1)
{
  if (class(formula) == "character")
    formula <<- as.formula(formula)
  if (length(formula) != 3) {
    stop("formula is not correct")
  }

  mc <- match.call()

  params <- match(c("formula", "data", "na.action"), names(mc), 0L)
  mc <- mc[c(1L, params)]
  mc$drop.unused.levels <- TRUE
  mc[[1]] <- quote(stats::model.frame)
  mf <- eval(mc)

  # Y
  Y <- model.response(mf)

  # X
  mt <- terms(mf)
  if (delete.y) {
    mt <- delete.response(mt)
  }
  if (normalize) {
    tl <- attr(mt, "term.labels")
    illegal <- grepl(pattern = "[/+/^/*/-///(/)]", tl[!grepl("^(as.){0,}factor*+", tl, perl = TRUE)])
    if (any(illegal)) {
      stop("the formula does not support arithmetic expressions or '+:^' operators when normalize = TRUE")
    }
    dc <- attr(mt, "dataClasses")
    if (length(tl) == length(dc) - 1) {
      dc <- dc[-1]
    } else {
      dc <- dc[names(dc) %in% tl]
    }

    norm_vars <- names(dc[dc %in% c("numeric", "integer")])
    if ("data.table" %in% class(data)) {
      new_matrix <- as.matrix(data[,norm_vars,with = FALSE])
    } else {
      new_matrix <- as.matrix(data[norm_vars])
    }

    if (missing(scales)) {
      scales_ <- normalize(matrix = new_matrix, nthreads = max_threads)
      scales_$xlevels <- .getXlevels(mt, mf)
    } else {
      normalize1(matrix = new_matrix, scales = scales_, nthreads = max_threads)
    }

    dt_flag <- suggest_package("data.table")
    col_names <- names(data)
    if (dt_flag) {
      setDT(data)
      data <- copy(data)
      for (norm_var in norm_vars) {
        set(x = data, j = match(norm_var, col_names), value = new_matrix[,norm_var])
      }
    } else {
      for (norm_var in norm_vars) {
        data[norm_var] <- new_matrix[,norm_var]
      }
    }
  } else {
    if (missing(scales)) {
      scales_ <- NULL
    } else {
      scales_ <- scales
    }
  }

  attr(mt, "intercept") <- 0
  if (missing(scales)) {
    X <- MatrixModels::model.Matrix(mt, data, sparse = TRUE, xlev = NULL)
  } else {
    X <- MatrixModels::model.Matrix(mt, data, sparse = TRUE, xlev = scales_$xlevels)
  }

  dimnames <- X@Dimnames[[2]]
  X_ <- Smatrix(X, FALSE)
  if (transpose) {
    X_t <- Smatrix(X, TRUE)
  } else {
    X_t <- NULL
  }

  list(X = X_, X_t = X_t, Y = Y, scales = scales_, dimnames = dimnames, na.rows = attr(mf, "na.action"))
}
