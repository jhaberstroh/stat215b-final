library(ggplot2)

N <- 10000

p <- matrix(NA, ncol=3, nrow=N)
p <- data.frame(p)
colnames(p) <- c("truth", "O","R")


for (i in seq(1,100))
{
    if (runif(1) < .45)
    {
        p$truth <- 1
        p$O[i] <- runif(1, max=.05)
        p$R[i] <- runif(1, max=.05)
    }
    else
    {
        p$truth <- 0
        p$O[i] <- min(runif(60, max=1.0))
        p$R[i] <- runif(1, max=1.0)
    }
}

goodvals <- which(p$O < .05)



out.loc     <- "output/pval_hypothesis_hist.pdf"
pdf(out.loc)
qplot(p$R[goodvals] / p$O[goodvals]) +
    scale_x_log10(limits=c(.00001, 10000))
dev.off()
print(length(which(p$R[goodvals] < .05)) / length(goodvals))

kde <- density( log10(p$R[goodvals] / p$O[goodvals]) , from=-5, to=5)
out.loc     <- "output/pval_hypothesis_kde.pdf"
pdf(out.loc)
qplot(kde$x, kde$y) + xlim(-5, 5)
dev.off()
print(length(which(p$R[goodvals] < .05)) / length(goodvals))
