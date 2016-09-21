deal_data <- function(formula, data, na.action, normalize, scales, delete.y = FALSE, max_threads = 1)
{
  if (class(formula) == "character")
    formula <<- as.formula(formula)
  if (length(formula) != 3) {
    stop("formula is not correct")
  }

  mc <- match.call()
  if (delete.y) {
    mc$formula[[2]] <- NULL
  }

  params <- match(c("formula", "data", "na.action"), names(mc), 0L)
  mc <- mc[c(1L, params)]
  mc$drop.unused.levels <- TRUE
  mc[[1]] <- quote(stats::model.frame)
  mf <- eval(mc)

  # Y
  Y <- model.response(mf)

  # X
  mt <- terms(mf)
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
      scales <- normalize(matrix = new_matrix, nthreads = max_threads)
    } else {
      normalize1(matrix = new_matrix, scales = scales, nthreads = max_threads)
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
  }
  if (!exists("scales")) {
    scales <- NULL
  }

  attr(mt, "intercept") <- 0
  X <- MatrixModels::model.Matrix(mt, data, sparse = TRUE)
  dimnames <- X@Dimnames[[2]]
  X <- Smatrix(X, FALSE)

  list(X = X, Y = Y, scales = scales, dimnames = dimnames, na.rows = attr(mf, "na.action"))
}
