#' @name sdbscan_v2
#' @return
#' @export
sdbscan_v2 <- function(x, e, minpts) {
  bin <- x
  row.names(bin) <- c(1:nrow(bin))
  bin2 <- data.frame(bin, clstr = NA)
  kl <- 0
  dis <- as.matrix(dist(bin))
  for (k in 1:nrow(bin2)) {
    if (is.na(bin2[k, "clstr"])) {
      a <- bin2[dis[, k] <= e,]
      
      if (nrow(a) >= minpts) { #!!
        kl <- kl + 1
        bin2[k, "clstr"] <- kl
        
        a <- a[-k,]
        while (nrow(a) == 0) {
          kk <- as.numeric(rownames(a))[1]
          b <- bin2[dis[, kk] <= e,]
          
          if (nrow(b) >= minpts) { #!!
            b <- b[is.na(b[, "clstr"]),]
            a <-  unique(rbind(a,b))
          } 
          bin2[kk, "clstr"] <- kl
          a <- a[-1,]
        } 
      } else {
        bin2[k, "clstr"] <- 0
      }
    }
  }
  bin2$clstr <- factor(bin2$clstr)
  return("result" = bin2)
}
