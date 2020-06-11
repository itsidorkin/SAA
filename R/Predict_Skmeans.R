#' @name predict_skmeans
#' @return
#' @export
predict_skmeans <- function(x, centroid) {
  if (is.data.frame(x)) {
    bin <- data.frame(scale(x))
  } else {
    bin <- t(data.frame(scale(x)))
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
