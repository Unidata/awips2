#!/bin/sh
###############################################################################
# DR 19718, 01/25/2017  lshi  Merge changes from DCS 19188 into product.cfg file 
#                             at sites rather than overwriting
################################################################################

cd /awips/adapt/NWRWAVES

# backup  
cp product.cfg product.cfg.`date "+%m%d%Y"`

# Get Baseline TCV items
grep -E "^SS\.[AW]" product.cfg.BASE > append.tmp_

# Reserve site comments and add one for this one
grep "^\s*#" product.cfg > comments.tmp_
date=`date "+%m/%d/%y"`
echo "# Updated $date, DR-19718, TCV support." >> comments.tmp_
sed  "/^#/d" product.cfg > product.tmp_
cp comments.tmp_ product.cfg

# Delete old TCV items from the site version
sed  "/^SS\.[AW]/d" product.tmp_  > product.cfg.tmp_

#Update site product.cfg with new TCV items from baseline version
cat append.tmp_ >> product.cfg.tmp_
sort product.cfg.tmp_ >> product.cfg

rm -f *.tmp_  


