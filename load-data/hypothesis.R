library(ggplot2)
library(reshape2)

N <- 400

p <- matrix(NA, ncol=4, nrow=N)
p <- data.frame(p)
colnames(p) <- c("sample", "truth", "O","R")

set.seed(90210)

for (i in seq(1,100))
{
    p$sample[i] <- i
    if (runif(1) < .4)
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





goodvals <- which(p$O < .05)
p.melt <- melt(p[goodvals,], id.vars=c("sample", "truth"))
# head(p)
# head(p.melt)
goodvals <- which(p$O < .05)
out.loc     <- "output/p-rep_hypothesis_violin.pdf"
pdf(out.loc)
ggplot(p.melt, aes(variable, value)) + 
    geom_violin() + 
    geom_boxplot(width=.05)
dev.off()









# ############
# # FIGURE 1
# # VIOLIN QUANTILE PLOTS (VQP) -----------------------------------------------
# ############
# 
# # Restructure the data to "long" format: Study type will be a factor
# df <- dplyr::select(RPPdata,starts_with("T."))
# df <- data.frame(EffectSize=as.numeric(c(df$T.r.O,df$T.r.R)),p.value=as.numeric(c(df$T.pval.USE.O,df$T.pval.USE.R)),grp=factor(c(rep("Original Studies",times=length(df$T.r.O)),rep("Replications",times=length(df$T.r.R)))))
# 
# # Create some variables for plotting
# df$grpN <- as.numeric(df$grp)
# probs   <- seq(0,1,.25)
# 
# # VQP PANEL A: p-value -------------------------------------------------
# 
# # Get p-value quantiles and frequencies from data
# qtiles <- ldply(unique(df$grpN),function(gr) quantile(round(df$p.value[df$grpN==gr],digits=4),probs,na.rm=T,type=3))
# freqs  <- ldply(unique(df$grpN),function(gr) table(cut(df$p.value[df$grpN==gr],breaks=qtiles[gr,],na.rm=T,include.lowest=T,right=T)))
# labels <- sapply(unique(df$grpN),function(gr)levels(cut(round(df$p.value[df$grpN==gr],digits=4), breaks = qtiles[gr,],na.rm=T,include.lowest=T,right=T)))
# 
# # Check the Quantile bins!
# ori           <-cbind(freq=as.numeric(t(freqs[1,])))
# rownames(ori) <- labels[,1]
# ori
# 
# rep           <-cbind(freq=as.numeric(t(freqs[2,])))
# rownames(rep) <- labels[,2]
# rep
# 
# # Get regular violinplot using package ggplot2
# g.pv <- ggplot(df,aes(x=grp,y=p.value)) + geom_violin(aes(group=grp),scale="width",color="grey30",fill="grey30",trim=T,adjust=.7)
# # Cut at quantiles using vioQtile() in C-3PR
# g.pv0 <- vioQtile(g.pv,qtiles,probs)
# # Garnish 
# g.pv1 <- g.pv0 + geom_hline(aes(yintercept=.05),linetype=2) +
#   ggtitle("A") + xlab("") + ylab("p-value") +
#   mytheme
# # View
# g.pv1



