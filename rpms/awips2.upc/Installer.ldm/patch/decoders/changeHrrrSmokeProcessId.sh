#!/bin/bash

#
# Make sure all std out from this script also goes to the log file
#
exec 2>>/awips2/ldm/logs/hrrrOverrideProcessId.log 1>&2

#
# Store the original productid but prepend with a distribution-matching
# prefix of ZZHH00. This new product-id will be used in the pqinsert below
#
productId=ZZHH00.$1

#
# Completely read std in and store in temp and unique file name. If you
# do not do this, LDM hangs! That is to say, the entire file needs to be
# read through end-of-file and cat is the easiest way to do that.
#
cat > /tmp/temp.grib2.$$

#
# Here's the wgrib2 command to change the process-id (from 83) to 254
#
/awips2/tools/bin/wgrib2 -match "^[0-9]*(:|\.1)"  /tmp/temp.grib2.$$ -set background_process_id 0 -set analysis_or_forecast_process_id 254 -GRIB /tmp/out.grib2.$$

#
# Insert the patched grib file back into the LDM queue.
#
pqinsert -vl /awips2/ldm/logs/hrrrOverrideProcessId.log -p $productId -f FSL2 /tmp/out.grib2.$$

#
# Clean up temporary files
#
rm -f /tmp/temp.grib2.$$
rm -f /tmp/out.grib2.$$

#
# leave happy
#
exit 0
