library(ggplot2)
library(reshape2)
library(plyr)
library(devtools)
library(optparse)
source_url('https://raw.githubusercontent.com/FredHasselman/toolboxR/master/C-3PR.R')

option_list <- list(
    make_option(c("-s", "--seed"),  action="store", type="integer", default=90210)
)
opt <- parse_args(OptionParser(option_list=option_list))


violin_plot.lv2 <- function(df)
{
    df$grpN <- as.numeric(df$grp)
    probs   <- seq(0,1,.25)

    # VQP PANEL A: p-value -------------------------------------------------

    # Get p-value quantiles and frequencies from data
    qtiles <- ldply(unique(df$grpN),function(gr) quantile(round(df$p.value[df$grpN==gr],digits=4),probs,na.rm=T,type=3))
    freqs  <- ldply(unique(df$grpN),function(gr) table(cut(df$p.value[df$grpN==gr],breaks=qtiles[gr,],na.rm=T,include.lowest=T,right=T)))
    labels <- sapply(unique(df$grpN),function(gr)levels(cut(round(df$p.value[df$grpN==gr],digits=4), breaks = qtiles[gr,],na.rm=T,include.lowest=T,right=T)))

    # Check the Quantile bins!
    ori           <-cbind(freq=as.numeric(t(freqs[1,])))
    rownames(ori) <- labels[,1]
    ori
    
    rep           <-cbind(freq=as.numeric(t(freqs[2,])))
    rownames(rep) <- labels[,2]
    rep
    
    # Get regular violinplot using package ggplot2
    g.pv <- ggplot(df,aes(x=grp,y=p.value)) + geom_violin(aes(group=grp),scale="width",color="grey30",fill="grey30",trim=T,adjust=.7)
    # Cut at quantiles using vioQtile() in C-3PR
    g.pv0 <- vioQtile(g.pv,qtiles,probs)

    # Get ggplot2 themes predefined in C-3PR
    mytheme <- gg.theme("clean")

    # Garnish 
    g.pv1 <- g.pv0 + geom_hline(aes(yintercept=.05),linetype=2) +
      ggtitle("A") + xlab("") + ylab("p-value") +
      mytheme
    # View
    g.pv1
}




N <- 1000
N.select <- 100 

p <- matrix(NA, ncol=4, nrow=N)
p <- data.frame(p)
colnames(p) <- c("sample", "truth", "O","R")

set.seed(opt$seed)

mu.var <- 1.9
mu.exp.var <- .45
frac.real <- 0.4
pval.hack <- 60

for (i in seq(1,N))
{
    p$sample[i] <- i
    rando <- runif(1)
    mu.i <- 0
    if (rando < frac.real)
    {
        p$truth <- 1
        # if the effect is real, it needs to have a
        # mean greater than the experimental threshhold
        # of one-sigma
        while (abs(mu.i) < mu.exp.var/2)
        {
            mu.i <- rnorm(1, 0, mu.var)
        }
        mu.O <- rnorm(1, mu.i, mu.exp.var)
        mu.R <- rnorm(1, mu.i, mu.exp.var)
        p$O[i] <- 10^(-mu.O^2)
        p$R[i] <- 10^(-mu.R^2)
    }
    else
    {
        p$truth <- 0
        p$O[i] <- min(runif(pval.hack, max=1.0))
        p$R[i] <- runif(1, max=1.0)
    }
}

goodvals <- which(p$O < .05)
goodvals <- goodvals[seq(1,N.select)]



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



p.melt <- melt(p[goodvals,], id.vars=c("sample", "truth"), 
                variable.name="grp", value.name="p.value")
# head(p)
head(p.melt)
goodvals <- which(p$O < .05)
out.loc     <- "output/p-rep_hypothesis_violin.pdf"
pdf(out.loc)
violin_plot.lv2(p.melt)
dev.off()


