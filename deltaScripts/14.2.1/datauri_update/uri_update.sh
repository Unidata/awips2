#!/bin/bash

# This script updates all tables in A2 that should contain a space or / but instead have an underscore.
# uri_update.py takes the name of the plugin to update then 1-n sequences of uri index to check, 
# character to look for at that index and character to replace it with.  Optional last argument is
# an hdf5 path layout for modifying the datauris in hdf5.  If no need to modify hdf5, this can be
# left blank.  This was done in support of Redmine DR 2333

DIR=`dirname $0`

# acars will replace _ with empty string to remove extra chars
python $DIR/uri_update.py acars 2 '_' ''

# acars is special as it also needs to update the column that has extra spaces in it
PSQL="/awips2/psql/bin/psql"
${PSQL} -U awips -d metadata -c "UPDATE acars SET tailnumber = replace(tailnumber, ' ', '')"

# bufrua needs to replace _ with space
python $DIR/uri_update.py bufrua 4 '_' ' '

# intlsigmet needs to replace _ with space
python $DIR/uri_update.py intlsigmet 3 '_' ' '

# satellite needs to replace _ with space at index 4 and 5
python $DIR/uri_update.py satellite 4 '_' ' ' 5 '_' ' ' '[sectorid]/[physicalelement]/'

# svrwx needs to replace _ with encoded %2F as the field actually contains a '/' in it
python $DIR/uri_update.py svrwx 3 '_' '%2F'

# vaa needs to rplace _ with space at index 2 and _ with encoded '/' at index 6
python $DIR/uri_update.py vaa 2 '_' ' ' 6 '_' '%2F'


