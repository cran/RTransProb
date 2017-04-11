

cm.matrix.counts<-function (mtrix)
{
  # tests if the given matrix mtrix is a migration matrix. So the dimensions of the migration matrix should
  # be at least 2 times 2 and the row and column dimensions must be equal. Further the values in the migration
  # matrix should be between 0 and 1. And the sum of each row should be 1.

  if (!is.matrix(mtrix))
    stop("mtrix is not a matrix")
  if (dim(mtrix)[1] < 2 || dim(mtrix)[2] < 2 || dim(mtrix)[1] != dim(mtrix)[2])
    stop("incorrect dimension of migration matrix")
  if (length(mtrix[mtrix < 0]) != 0)
    stop("negative value in input data")

}

