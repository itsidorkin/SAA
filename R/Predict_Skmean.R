#' @name predict_skmean
#' @docType package
#' @param
#' @return
#' @export
#' @examples
predict_skmean <- function(X, centroid)
{
  if (is.data.frame(X)) {
    bin <- X
  } else {
    bin <- t(data.frame(X))
  }
  #bin <- t(data.frame(c(-86,-90,-67,-88,-86,-84,-79), c(-86,-90,-97,-88,-89,-84,-79)))
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
