
is.wholenumber <- function(x, tol = .Machine$double.eps^0.5)  abs(x - round(x)) < tol

genStrataLam <- function(Lam, strata = 8, rows = 4, cols = 2, plot = TRUE){
  if(cols*rows != strata) {stop("Number of rows and columns do not total number of strata")}
  xdim = Lam$dim[2]
  ydim = Lam$dim[1]
  if(!is.wholenumber(xdim/cols)) {stop("x dimension is not divisible by number of columns")}
  if(!is.wholenumber(ydim/rows)) {stop("y dimension is not divisible by number of rows")}
  # added in below claus to make more flexible if we say only want rows not columns or vice versa
  if(cols > 1){cutx <- split(1:xdim,cut(1:xdim, cols))}else{cutx <- list(c(1:xdim))}
  if(rows > 1){cuty <- split(1:ydim,cut(1:ydim, rows))}else{cuty <- list(c(1:ydim))}
  stratxy <- expand.grid(1:xdim, 1:ydim)
  names(stratxy) <- c("x","y")
  k = 1
  for (j in 1:rows){
    for (i in 1:cols){
      stratxy$stratum[stratxy$x %in% cutx[[i]] & stratxy$y %in% cuty[[j]]] <- k
      k <- k+1
    }
  }
  rfv <- log(Lam$v)
  rfv <- as.vector(rfv)
  dat <- data.frame(x = stratxy$x, y = stratxy$y, sim1 = rfv,stratum =stratxy$stratum)
  if(plot == TRUE){plot(dat$y ~ dat$x, col = dat$stratum)}
  return(dat)
}