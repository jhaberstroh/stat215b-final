#!/bin/bash
SRCDIR=$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )
cd $SRCDIR/..

# Rscript $SRCDIR/simple-eda.R -x .002
seed=90210
Rscript $SRCDIR/hypothesis.R -s $seed
while [ $? -eq 1 ]; do
    seed=$((seed+1))
    Rscript $SRCDIR/hypothesis.R -s $seed
done
# if [ $(uname) == "Linux" ]; then
#     evince output/pval_scatter.pdf
# fi
