#!/bin/bash
# This script updates xxxid entries in the stdtextproducts table.
# It takes xxxid entries that are 2 characters and pads them to 3
# by adding a trailing space.
#
# This needs to be performed with build 11.9.0-1 where all xxxid
# entries that are inserted and queried for must be 3 characters.
#
# Set up
# 1) Perform this task on the machine with the edex database
# 2) create a directory where you have read write permissions
# 3) cd to the directory
# 4) Copy this file (update_stdtextproducts.sh) to this directory
# 5) do: chmod +x update_stdtextproducts.sh
# 6) ./update_stdtextproducts.sh

export DEFAULT_HOST=${DEFAULT_HOST:-localhost}
psql=/awips2/psql/bin/psql
dq='"'
selCmd="select distinct xxxid from stdtextproducts where length(xxxid) = 2 ;"

rm -f ./tmp.sql
${psql} -h ${DEFAULT_HOST} -U awips -d fxatext  -c "${selCmd}" | \
sed -n  -e '/^ \(..\)$/s//UPDATE stdtextproducts set xxxid="\1 " where xxxid="\1";/p'  \
   -e '/^ \(.\)$/s//UPDATE stdtextproducts set xxxid="\1  " where xxxid="\1";/p'  \
   -e '/^$/s//UPDATE stdtextproducts set xxxid="   " where length(xxxid) = 0 ;/p' | \
sed -e"s/$dq/'/g"  > ./tmp.sql

cat ./tmp.sql

echo ""
echo "Press Enter to perform the updates Ctrl-C to quit."
read done

${psql} -h ${DEFAULT_HOST} -U awips -d fxatext  < ./tmp.sql
rm -f ./tmp.sql
