#' plsreg1_adj
#'
#' @param ... other params will be ignored
#'
#' @examples
#' \dontrun{
#' plsreg1_adj(X, Y, comps = 2, autoVars = TRUE, nminVar = 2, minVIP = 0.8)
#' }
#' @export
plsreg1_adj <- function(X, Y, comps = 2,
    autoVars = TRUE, nminVar = 2, minVIP = 0.8,
    include.fitted = FALSE,
    slope = slope_lm,
    verbose = FALSE,
    seed = 1,
    ...)
{
    varnames <- varnames0 <- colnames(X)
    # select_model
    m_pls  <- plsdepot::plsreg1(X[, varnames, drop = FALSE], Y, comps = comps, crosval = TRUE, seed = seed)
    r_init <- PLS_performIndex(m_pls, varnames0, include.fitted, slope = slope)
    r_last <- r_init

    PRESS_prev <- last_row(m_pls$Q2)["PRESS"]
    if (verbose) print(last_row(m_pls$Q2))
    if (autoVars) {
        repeat({
            VIP <- last_row(m_pls$VIP)
            Q2  <- last_row(m_pls$Q2)

            if (length(VIP) >= (nminVar + 1) && any(VIP <= minVIP)) {
                I_rm <- which.min(VIP)
                varnames_temp <- varnames[-I_rm]
                m_pls <- plsdepot::plsreg1(X[, varnames_temp, drop = FALSE], Y,
                    comps = comps, crosval = TRUE, seed = seed)

                PRESS_new = last_row(m_pls$Q2)["PRESS"]
                if (PRESS_new > PRESS_prev) break
                
                PRESS_prev = PRESS_new
                if (verbose) print(PRESS_prev)
                varnames = varnames_temp

                r_last <- PLS_performIndex(m_pls, varnames0, include.fitted, slope = slope) # I_nona
            } else {
                break
            }
        })
        # only init and the last kept
        # browser()
        list(init = r_init, last = r_last) # r is improved
        # purrr::transpose(r) %>% map(~do.call(rbind, .) %>%
        #     data.frame() %>% set_rownames(NULL) %>%
        #     cbind(type = c("init", "last"), .))
    } else {
        list(init = r_init) # r is improved
    }
}

fill_missingVar <- function(x, varnames0){
    n <- length(varnames0)
    ans <- rep(NA_real_, n) %>% set_names(varnames0)
    I <- match(names(x), varnames0)
    ans[I] <- x
    ans
}

#' @export
PLS_performIndex <- function(obj, varnames0, include.fitted = FALSE, slope = slope_lm, ...){
    Xx <- obj$INPUT$Xx
    Yy <- obj$INPUT$Yy
    mu.x <- attributes(Xx)$"scaled:center" #%>% as.matrix(ncol = 1)
    sd.x <- attributes(Xx)$"scaled:scale"  #%>% as.matrix(ncol = 1)
    mu.y <- attributes(Yy)$"scaled:center"
    sd.y <- attributes(Yy)$"scaled:scale"

    reg.coefs <- obj$reg.coefs[-1] # rm intercept
    std.coefs <- obj$std.coefs

    n <- nrow(Xx)
    ypred2 <- rep(NA_real_, n)
    ypred  <- obj$y.pred
    ind <- match(names(ypred), rownames(Yy))
    ypred2[ind] <- ypred

    # slope need * nyear = delta  (linear regression used here)
    # Note that: Xx and Yy has been scaled.
    slope2 <- function(x) slope(x)[1]

    # fatel error
    attribute_change <- c(
        slope2(Yy)*sd.y, # EOS
        fill_missingVar(apply(Xx, 2, slope2)*sd.x*reg.coefs, varnames0))

    scale = list(x.mu = mu.x, x.sd = sd.x, y.mu = mu.y, y.sd = sd.y)
    ans <- list(
        reg.coefs = reg.coefs,
        std.coefs = std.coefs,
        VIP = last_row(obj$VIP),
        attribute_change = attribute_change,
        Q2  = last_row(obj$Q2),
        scale = scale)

    if (include.fitted) {
        ans$ypred <- ypred
    }
    ans[1:3] %<>% lapply(fill_missingVar, varnames0)
    ans
}

# #' attribute_change
# #'
# #' @param obj plsr object, list(`SOS`, `NonSOS`, I_grid)
# #' @param l_trend list(trend, pvalue), trend is a data.frame
# #'
# #' @return data.table
# #'
# #' @export
# attribute_change <- function(obj, l_trend, nyear = 1, model = "SOS") {
#     plsr   = obj[[model]]
#     I_grid = obj$I
#     # if (is.null(I_grid)) I_grid <- 1:nrow(slope_Xx)

#     I_y = 6 # EOS
#     slope_Xx  <- l_trend$trend[, -6]
#     slope_Yy  <- l_trend$trend[, 6]
#     # x.sd      <- plsr$scale$x.sd
#     # y.sd      <- plsr$scale$y.sd

#     reg.coefs <- plsr$reg.coefs %>% fill_df_null(I_grid) # fill missing grid
#     # cbind(slope_Yy * y.sd, slope_Xx * x.sd * reg.coefs) * nyear
#     ans <- cbind(slope_Yy, slope_Xx * reg.coefs) * nyear # last return
#     d   <- cbind(I = 1:nrow(ans), ans) %>% data.table()
#     d
# }
