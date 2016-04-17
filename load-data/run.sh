#!/bin/bash
set -o errexit 
SRCDIR=$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )
cd $SRCDIR/..

Rscript $SRCDIR/simple-eda.R -x .2
# if [ $(uname) == "Linux" ]; then
#     evince output/pval_scatter.pdf
# fi