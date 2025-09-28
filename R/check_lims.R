check_lims <- function(lims) {
  # if min of two together is x, max of the third is 1-x
  alim[2] <- min(alim[2], 1 - blim[1] - clim[1])
  blim[2] <- min(blim[2], 1 - alim[1] - clim[1])
  clim[2] <- min(clim[2], 1 - alim[1] - blim[1])
  rbind(a = alim, b = blim, c = clim)
}
