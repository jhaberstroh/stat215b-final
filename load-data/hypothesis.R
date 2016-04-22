library(ggplot2)

N <- 10000

p <- matrix(NA, ncol=3, nrow=N)
p <- data.frame(p)
colnames(p) <- c("truth", "O","R")


for (i in seq(1,N))
{
    if (runif(1) < .3)
    {
        p$truth <- 1
        p$O[i] <- runif(1, max=.05)
        p$R[i] <- runif(1, max=.05)
    }
    else
    {
        p$truth <- 0
        p$O[i] <- min(runif(1, max=1.0))
        p$R[i] <- runif(1, max=1.0)
    }
}


goodvals <- which(p$O < .05)
out.loc     <- "output/pval_hypothesis_hist.pdf"
pdf(out.loc)
qplot(p$R[goodvals] / p$O[goodvals]) +
    scale_x_log10(limits=c(.01, 100))
dev.off()
