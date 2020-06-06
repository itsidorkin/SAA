#' @name predict_skmean
#' @docType package
#' @param
#' @return
#' @export
#' @examples
predict_skmean <- function(x, centroid) {
  if (is.data.frame(x)) {
    bin <- x
  } else {
    bin <- t(data.frame(x))
  }

  z <- 1
  if (nrow(bin) == 1 || is.vector(bin)) {
    z <- 2
  }
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
    dis, z,
    FUN = function(x) {
      which.min(x)
    }
  )
  return(clstr)
}
