#' @name skmean
#' @docType package
#' @param
#' @return
#' @export
#' @examples
skmean <- function(X, k)
{
  bin <- X
  n <- ncol(bin)
  it <- -1
  centroid <- dplyr::sample_n(bin, k)
  centr_clstr <- centroid
  repeat{
    dis <-
      data.frame(apply(
        centroid, 1,
        FUN = function(x) {
          apply(
            bin, 1,
            FUN = function(y) {
              sqrt(sum((y - x) ^ 2))
            }
          )
        }
      ))

    clstr <- apply(
      dis, 1,
      FUN = function(x) {
        which.min(x)
      }
    )

    for (ki in 1 : k) {
      for (j in 1 : n) {
        centr_clstr[ki, j] <-  mean(as.matrix(bin[clstr == ki,][j]))
      }
    }

    if (sum(centroid != centr_clstr) == 0) {
      break
    } else {
      centroid <- centr_clstr
    }
  }
  return(list('Clstr'=clstr, 'Cntr'=centr_clstr))
}
