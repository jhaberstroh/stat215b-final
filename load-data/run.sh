#!/bin/bash
SRCDIR=$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )
cd $SRCDIR/..

# Rscript $SRCDIR/simple-eda.R -x .002
# Pretty good values
# args="-f 0.0 -H 5 -m 0.5 -w 1.0 -p 1.0"


# seed=90210
# args="-f 1.0 -n 1 -S null_"
# Rscript $SRCDIR/hypothesis.R -s $seed $args
# while [ $? -eq 1 ] && [ $seed -lt 90220 ]; do
#     seed=$((seed+1))
#     Rscript $SRCDIR/hypothesis.R -s $seed $args
# done


# seed=90210
# args="-f 1.0 -n 80 -S nullhax_"
# Rscript $SRCDIR/hypothesis.R -s $seed $args
# while [ $? -eq 1 ] && [ $seed -lt 90220 ]; do
#     seed=$((seed+1))
#     Rscript $SRCDIR/hypothesis.R -s $seed $args
# done


seed=90210
args="-f 0.0 -H 1 -m 0.0 -w 1.0 -c .5 -p 1.0 -S real_"
Rscript $SRCDIR/hypothesis.R -s $seed $args
while [ $? -eq 1 ] && [ $seed -lt 90220 ]; do
    seed=$((seed+1))
    Rscript $SRCDIR/hypothesis.R -s $seed $args
done
