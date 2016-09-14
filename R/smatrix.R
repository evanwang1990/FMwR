Smatrix <- function(dgCMatrix, transpose = FALSE)
{
  stopifnot(class(dgCMatrix) == "dsparseModelMatrix")
  dgCMatrix <- as(dgCMatrix, "dgCMatrix")
  if (!transpose) dgCMatrix <- t(dgCMatrix)
  smatrix <- list(
    value = dgCMatrix@x,
    col_idx = dgCMatrix@i,
    row_size = diff(dgCMatrix@p),
    dim = rev(dgCMatrix@Dim),
    size = length(dgCMatrix@i)
  )
  attr(smatrix, "class") <- "SMatrix"
  attr(smatrix, "transposed") <- transpose
  smatrix
}
