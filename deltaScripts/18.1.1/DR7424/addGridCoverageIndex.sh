#!/bin/bash

# #7424 - This script adds an index to the grid_info table's location column to 
#         allow for faster lookup and deletion of large numbers of grid_coverage. 
#
# NOTE: This script will error if the index already exists, but this will not 
#       negatively impact the system.

# run the update
echo "DR #7427 - Adding index to grid_info.location..."
/awips2/psql/bin/psql -U awipsadmin -d metadata -c "CREATE INDEX grid_info_location_id_index ON grid_info USING btree (location_id);"