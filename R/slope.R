slope_lm <- function(y, x){
    # TODO: add tests for slopew
    if (!is.matrix(y)) y <- as.matrix(y)
    n <- nrow(y)

    if (missing(x)) x <- 1:n
    I_bad <- which(!is.finite(y)) # NA or Inf
    if (length(I_bad) > 0) {
        y <- y[-I_bad,]
        x <- x[-I_bad, ]
    }
    slope <- qr.solve(cbind(x*0+1, x) , y)[2, ] # only return slp
    slope
    # set_names(slope, "slope")
}
