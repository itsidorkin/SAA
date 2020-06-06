#' @name dbscan
#' @docType package
#' @param
#' @return
#' @export
#' @examples

dbscan <- function(X, e, minpts)
{
  bin <- X

  bin2 <- data.frame(bin,
                     clstr = NA,
                     tmp.ctg = FALSE,
                     sosedi = FALSE)

  kl <- 0

  dis <- as.matrix(dist(bin))

  for (k in 1:nrow(bin2)) {
    if (is.na(bin2[k, 'clstr'])) {
      bin2['sosedi'] <- FALSE
      for (kk in 1:nrow(bin2)) {
        if (dis[kk, k] < e) {
          bin2[kk, 'sosedi'] <- TRUE
        }
      }

      if (sum(bin2[, 'sosedi']) < minpts) {
        bin2[k, 'clstr'] <- 'noise'
      } else{
        kl <- kl + 1
        bin2[k, 'clstr'] <- kl
        for (kk in 1:nrow(bin2)) {
          if (isTRUE(bin2[kk, "sosedi"]) &&
              (is.na(bin2[kk, "clstr"]))) {
            bin2[kk, "tmp.ctg"] <- TRUE
          }
        }

        while (TRUE) {
          for (kk in 1:nrow(bin2)) {
            if (is.na(bin2[kk, "clstr"]) &&
                (isTRUE(bin2[kk, "tmp.ctg"]))) {
              bin2['sosedi'] <- FALSE
              for (kkk in 1:nrow(bin2)) {
                if ((dis[kkk, kk] < e) &&
                    (is.na(bin2[kkk, 'clstr']))) {
                  bin2[kkk, 'sosedi'] <- TRUE
                }
              }
              if (sum(dis[, kk] < e) >= minpts) {
                bin2[kk, 'clstr'] <- kl
                bin2[, "tmp.ctg"] <-
                  bin2[, "tmp.ctg"] | apply(
                    bin2["sosedi"],
                    1,
                    FUN = function(x) {
                      if (isTRUE(x)) {
                        TRUE
                      } else{
                        FALSE
                      }
                    }
                  )
              }
              bin2[kk, "tmp.ctg"] <- FALSE
            }
          }
          if (sum(bin2[, "tmp.ctg"]) == 0) {
            break()
          }
        }

      }
    }
  }

  bin5 <- bin2

  for (k in 1:nrow(bin5)) {
    if (bin5[k, 'clstr'] != 'noise') {
      bin5[k, 'tmp.ctg'] <- TRUE
    }
  }

  for (k in 1:nrow(bin5)) {
    bin5['sosedi'] <- FALSE
    cl <- 'noise'
    if (bin5[k, 'clstr'] == 'noise') {
      for (kk in 1:nrow(bin5)) {
        if (dis[kk, k] < e) {
          bin5[kk, 'sosedi'] <- TRUE
          if ((bin5[kk, 'clstr'] != 'noise') &&
              (isTRUE(bin5[kk, 'tmp.ctg']))) {
            cl <- bin5[kk, 'clstr']
          }
        }
      }
      if ((sum(bin5[, 'sosedi']) > 1) &&
          (cl != 'noise')) {
        bin5[k, 'clstr'] <- cl
      }
    }
  }

  bin3 <- bin2
  bin4 <- bin5
  for (z in 1:nrow(bin3)) {
    if (bin3[z, 'clstr'] == 'noise') {
      bin3[z, 'clstr'] <- NA
    }
    if (bin4[z, 'clstr'] != 'noise') {
      bin4[z, 'clstr'] <- NA
    }
    if (bin5[z, 'clstr'] == 'noise') {
      bin5[z, 'clstr'] <- NA
    }
  }
  bin3 <- na.omit(bin3)
  bin4 <- na.omit(bin4)
  bin5 <- na.omit(bin5)
  if (nrow(bin4) == 0) {
    bin4[1, 1] <- NA
    bin4[2, 1] <- NA
  }
  if (nrow(bin3) == 0) {
    bin3[1, 1] <- NA
    bin3[2, 1] <- NA
  }
  return(list('bin3'=bin3, 'bin4'=bin4, 'bin5'=bin5))
}
