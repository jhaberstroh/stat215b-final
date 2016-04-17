library(ggplot2)
library(optparse)

option_list <- list(
    make_option(c("-x", "--xspeed"), action="store", type="double", default=1.0)
)
opt <- parse_args(OptionParser(option_list=option_list))

sprint <- function(output, t = 2.0){
    print(output)
    Sys.sleep(t * opt$xspeed)
}

# 1. Read the raw data
MASTER <- read.csv("data/osf/rpp_data.csv")[1:167, ]

# 2. Find which columns have entries for O and R
columns <- colnames(MASTER)
strip  <- function (t) substr(t, 1, nchar(t)-2) 
newcols <- sapply(columns, strip, simplify="array", USE.NAMES=FALSE)
dup_cols <- newcols[duplicated(newcols)]
sprint("Welcome to the OSF data set!", 1.0)
print("Here are columns with Original (O) AND Replication (R)!")
sprint("Apppend \'O.\' or \'R.\' to access each, respectively.", 2.5)
print(dup_cols)


# !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
# For User: Use this section to make simple plots
# !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
# 3. Make scatterplots of O and R datasets to compare
out.loc     <- "output/pval_USE_scatter.pdf"
column.name <- "T_pval_USE.."
pdf(out.loc)
sprint(sprintf("Creating scatterplot of [%s] comparison in %s...", column.name, out.loc) , 2.5)
qplot(MASTER[,paste0(column.name, "O.")], MASTER[, paste0(column.name, "R.")]) +
    xlab("Original")                                                           +
    ylab("Replication")                                                        +
    ggtitle(paste("Column:", column.name))
print("Done.")
dev.off()


column.name <- "T_N.."
out.loc     <- paste0("output/", column.name, "_scatter.pdf")
pdf(out.loc)
sprint(sprintf("Creating scatterplot of [%s] comparison in %s...", column.name, out.loc) , 2.5)
qplot(MASTER[,paste0(column.name, "O.")], MASTER[, paste0(column.name, "R.")]) +
    xlab("Original")                                                           +
    ylab("Replication")                                                        +
    scale_x_log10()                                                            +
    scale_y_log10()                                                            +
    ggtitle(paste("Column:", column.name))
print("Done.")
dev.off()
