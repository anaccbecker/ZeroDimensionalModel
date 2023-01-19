randomizationTest <- function(x, groups, n = 9999,
                              FUN = mean, alternative = "two.sided",
                              plot = FALSE){
    n <- n - 1
    new.data <- na.omit(droplevels(data.frame(x = x, group = groups)))
    fun.apply <- tapply(new.data[["x"]], new.data[["group"]],
                        FUN)
    if (sum(is.na(fun.apply)) == 0) {
        obs <- diff(fun.apply)
        if (obs != 0) {
            theoretical <- replicate(n, {
                diff(tapply(new.data[["x"]],
                            sample(new.data[["group"]]),
                            FUN))
            })
            final <- c(theoretical, obs)
            if (alternative == "two.sided") {
                abs <- ifelse(obs < 0, -obs, obs)
                p.value <- (sum(final <= (-abs)) +
                                sum(final >= abs))/length(final)
                if (plot) {
                    plot(density(final))
                    abline(v = -abs, col = 2, lty = 2)
                    abline(v = abs, col = 2, lty = 2)
                    legend("topright",
                           legend = sprintf("p-valor = %f", p.value))
                }
            } else if (alternative == "one.sided") {
                if (obs < 0) {
                    p.value <- sum(final <= obs)/length(final)
                } else {
                    p.value <- sum(final >= obs)/length(final)
                }
                if (plot) {
                    plot(density(final))
                    abline(v = obs, col = 2, lty = 2)
                    legend("topright",
                           legend = sprintf("p-valor = %f", p.value))
                }
            } else {
                stop("Value of alternative hypothesis not is valid")
            }
            return(p.value)
        } else {
            return(1)
        }
    } else {
        return(NA)
    }
}
