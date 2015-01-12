#! /bin/sh
# This script updates afosid entries in the afos_to_awips table.
# It takes afosid entries that are 7 or 8 characters and pads them to 9.
#
# This needs to be performed with build 11.9.0-7 where all afosid
# entries that are inserted and queried for must be 9 characters and
# wmottaaii must be 6 and wmoccc must be 4.
#
# Alternate method. If you have the afos2awips.txt used to first generate the
# afos_to_awips table it can be copied to /awips2/edex/data/ndm and when
# ingested the table will correct.
#
# Set up
# 1) Perform this task on the machine with the edex database
# 2) create a directory where you have read write permissions 
# 3) cd to the directory
# 4) Copy this file (update_afos_to_awips.sh) to this directory
# 5) do: chmod +x update_afos_to_awips.sh
# 6) ./update_afos_to_awips.sh

export DEFAULT_HOST=${DEFAULT_HOST:-localhost}
psql=/awips2/psql/bin/psql
dq='"'

rm -f ./tmp.sql
touch ./tmp.sql

selCmd="select distinct afosid from afos_to_awips where length(afosid) = 8 or length(afosid) = 7 or length(afosid) = 6 ;"
${psql} -h ${DEFAULT_HOST} -U awips -d fxatext  -c "${selCmd}" | \
sed -n  -e '/^ \([^ ][^ ][^ ][^ ][^ ][^ ][^ ][^ ]\) *$/s//UPDATE afos_to_awips set afosid="\1 " where afosid="\1";/p'  \
        -e '/^ \([^ ][^ ][^ ][^ ][^ ][^ ][^ ]\) *$/s//UPDATE afos_to_awips set afosid="\1  " where afosid="\1";/p'  \
        -e '/^ \([^ ][^ ][^ ][^ ][^ ][^ ]\) *$/s//UPDATE afos_to_awips set afosid="\1   " where afosid="\1";/p'  | \
sed -e"s/$dq/'/g"  >> ./tmp.sql

cat ./tmp.sql

echo ""
echo "Press Enter to perform the updates Ctrl-C to quit."
read done

${psql} -h ${DEFAULT_HOST} -U awips -d fxatext  < ./tmp.sql
