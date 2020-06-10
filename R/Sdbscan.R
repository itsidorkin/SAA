#" @name sdbscan
#" @docType package
#" @param
#" @return
#" @examples
#" @export
sdbscan <- function(x, e, minpts) {
  #bin <- data.frame(scale(x))
  bin <- data.frame(x)
  bin2 <- data.frame(bin,
                     clstr = NA,
                     tmp.ctg = FALSE,
                     sosedi = FALSE)

  kl <- 0
  dis <- as.matrix(dist(bin))
  for (k in seq_len(nrow(bin2))) {
    if (is.na(bin2[k, "clstr"])) {
      bin2["sosedi"] <- FALSE
      for (kk in seq_len(nrow(bin2))) {
        if (dis[kk, k] < e) {
          bin2[kk, "sosedi"] <- TRUE
        }
      }

      if (sum(bin2[, "sosedi"]) < minpts) {
        bin2[k, "clstr"] <- "noise"
      } else{
        kl <- kl + 1
        bin2[k, "clstr"] <- kl
        for (kk in seq_len(nrow(bin2))) {
          if (isTRUE(bin2[kk, "sosedi"]) &&
              (is.na(bin2[kk, "clstr"]))) {
            bin2[kk, "tmp.ctg"] <- TRUE
          }
        }

        while (TRUE) {
          for (kk in seq_len(nrow(bin2))) {
            if (is.na(bin2[kk, "clstr"]) &&
                (isTRUE(bin2[kk, "tmp.ctg"]))) {
              bin2["sosedi"] <- FALSE
              for (kkk in seq_len(nrow(bin2))) {
                if ((dis[kkk, kk] < e) &&
                    (is.na(bin2[kkk, "clstr"]))) {
                  bin2[kkk, "sosedi"] <- TRUE
                }
              }
              if (sum(dis[, kk] < e) >= minpts) {
                bin2[kk, "clstr"] <- kl
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
            break
          }
        }

      }
    }
  }

  bin2_fix <- bin2

  for (k in seq_len(nrow(bin2_fix))) {
    if (bin2_fix[k, "clstr"] != "noise") {
      bin2_fix[k, "tmp.ctg"] <- TRUE
    }
  }

  for (k in seq_len(nrow(bin2_fix))) {
    bin2_fix["sosedi"] <- FALSE
    cl <- "noise"
    if (bin2_fix[k, "clstr"] == "noise") {
      for (kk in seq_len(nrow(bin2_fix))) {
        if (dis[kk, k] < e) {
          bin2_fix[kk, "sosedi"] <- TRUE
          if ((bin2_fix[kk, "clstr"] != "noise") &&
              (isTRUE(bin2_fix[kk, "tmp.ctg"]))) {
            cl <- bin2_fix[kk, "clstr"]
          }
        }
      }
      if ((sum(bin2_fix[, "sosedi"]) > 1) &&
          (cl != "noise")) {
        bin2_fix[k, "clstr"] <- cl
      }
    }
  }
  
  result_scale <- bin2_fix[-c(ncol(bin2_fix),(ncol(bin2_fix) - 1))]
  result <- cbind(x, 'clstr' = factor(bin2_fix$clstr))
  
  wout_border <- bin2[bin2$clstr != 'noise',]
  with_border <- bin2_fix[bin2_fix$clstr != 'noise',]
  noise <- bin2_fix[bin2_fix$clstr == 'noise',]
  
  result$clstr <- factor(result$clstr)
  result_scale$clstr <- factor(result_scale$clstr)
  wout_border$clstr <- factor(wout_border$clstr)
  with_border$clstr <- factor(with_border$clstr)
  noise$clstr <- factor(noise$clstr)
  
  wout_border <- wout_border[-c(ncol(wout_border),(ncol(wout_border) - 1))]
  with_border <- with_border[-c(ncol(with_border),(ncol(with_border) - 1))]
  noise <- noise[-c(ncol(noise),(ncol(noise) - 1))]
  
  # with_border <- bin2_fix[bin2_fix$clstr != 'noise',]
  # noise <- bin2_fix[bin2_fix$clstr == 'noise',]
  # tempbin <- cbind(x, 'clstr' = bin2$clstr)
  # wout_border <- tempbin[tempbin$clstr != 'noise',]
  # tempbin <- cbind(x, 'clstr' = factor(bin2_fix$clstr)
  # with_border <- tempbin[tempbin$clstr != 'noise',]
  # noise <- tempbin[tempbin$clstr == 'noise',]
  
  if (nrow(noise) == 0) {
    noise[1, ] <- NaN
  }
  if (nrow(wout_border) == 0) {
    wout_border[1, ] <- NaN
  }
  if (nrow(with_border) == 0) {
    with_border[1, ] <- NaN
  }
  
  return(list("result" = result, "graphics"=list("circle" = wout_border, "encircle" = with_border, "noise" = noise, "result" = result_scale)))
}
