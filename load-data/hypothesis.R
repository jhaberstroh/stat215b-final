library(ggplot2)
library(reshape2)
library(plyr)
library(devtools)
library(optparse)
source_url('https://raw.githubusercontent.com/FredHasselman/toolboxR/master/C-3PR.R')


option_list <- list(
    make_option(c("-s", "--seed"),  action="store", type="integer", default=90210),
    make_option(c("-S", "--save"),  action="store", default=""),
    make_option(c("-H", "--hack"),  action="store", type="integer", default=1),
    make_option(c("-n", "--hacknull"),  action="store", type="integer", default=60),
    make_option(c("-c", "--cutoff"),  action="store", type="double", default=0.0),
    make_option(c("-p", "--distexp"),  action="store", type="double", default=1.0),
    make_option(c("-f", "--nullfrac"),  action="store", type="double", default=0.0),
    make_option(c("-m", "--effmean"),  action="store", type="double", default=1.0),
    make_option(c("-w", "--effwidth"),  action="store", type="double", default=1.9)
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

for (i in seq(1,N))
{
    p$sample[i] <- i
    rando <- runif(1)
    z.mu <- 0
    if (rando > opt$nullfrac)
    {
        z.O <- 0
        z.R <- 0
        while ((abs(z.O) < 1.6) | (abs(z.mu) < opt$cutoff))
        {
            z.mu <- abs(rnorm(1,    opt$effmean, opt$effwidth))**opt$distexp
            z.O  <- max(rnorm(opt$hack, z.mu, 1.0))
            z.R  <-            rnorm(1, z.mu, 1.0)
        }
        # Can extend to use p$truth = z.mu > .25 or something like that
        p$truth <- 1
        p$O[i] <- 1 - pnorm(abs(z.O))
        p$R[i] <- 1 - pnorm(abs(z.R))
    }
    else
    {
        p$truth <- 0
        p$O[i] <- min(runif(opt$hacknull, max=1.0))
        p$R[i] <- runif(1, max=1.0)
    }
}

goodvals <- which(p$O < .05)
goodvals <- goodvals[seq(1,N.select)]
if (length(goodvals) < N.select)
{
    print("Did not get enough good values, killing program")
    exit()
}


out.loc     <- paste0("output/", opt$save, "pval_hypothesis_hist.pdf")
pdf(out.loc)
qplot(p$R[goodvals] / p$O[goodvals]) +
    scale_x_log10(limits=c(.00001, 10000))
dev.off()
print(length(which(p$R[goodvals] < .05)) / length(goodvals))

kde <- density( log10(p$R[goodvals] / p$O[goodvals]) , from=-5, to=5)
out.loc     <- paste0("output/", opt$save, "pval_hypothesis_kde.pdf")
pdf(out.loc)
qplot(kde$x, kde$y) + xlim(-5, 5)
dev.off()



p.melt <- melt(p[goodvals,], id.vars=c("sample", "truth"), 
                variable.name="grp", value.name="p.value")
# head(p)
head(p.melt)
goodvals <- which(p$O < .05)
out.loc     <- paste0("output/", opt$save, "p-rep_hypothesis_kde.pdf")
pdf(out.loc)
violin_plot.lv2(p.melt)
dev.off()


