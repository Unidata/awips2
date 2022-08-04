#!/bin/bash
# DCS #21048 - This script:
#            1. Check stdtextproducts and practicestdtextproducts tables for duplicates
#            2. Alters the multi-column unique constraint of the stdtextproducts
#            and practicestdtextproducts tables to exclude the wmoid and cccid columns

psql="a2dbauth /awips2/psql/bin/psql"

echo INFO: Checking for duplicates in stdtextproducts table

ndups=$(${psql} -U awipsadmin -d fxatext -c "SELECT count(*) FROM (SELECT *, count(*) OVER (PARTITION BY datacrc, hdrtime, nnnid, site, xxxid) AS count FROM stdtextproducts) tableWithCount WHERE tableWithCount.count > 1;" | sed '3q;d' | sed "s/ //g")

if [ "$ndups" = "0" ]; then
  echo INFO: Found no duplicates
else
  echo INFO: Found $ndups duplicates: starting to delete duplicates
  result=$(${psql} -U awipsadmin -d fxatext -c "CREATE TABLE stdtextproducts_dups2remove (LIKE stdtextproducts);")
  result=$(${psql} -U awipsadmin -d fxatext -c "INSERT INTO stdtextproducts_dups2remove (cccid,datacrc,hdrtime,nnnid,site,wmoid,xxxid,bbbid,inserttime,product,reftime) SELECT cccid,datacrc,hdrtime,nnnid,site,wmoid,xxxid,bbbid,inserttime,product,reftime FROM (SELECT *, ROW_NUMBER() OVER( PARTITION BY datacrc, hdrtime, nnnid, site, xxxid ORDER BY inserttime) AS row_num FROM stdtextproducts) t WHERE t.row_num > 1;" | sed '1q;d' | cut -c 9-)
  echo INFO: Inserted $result duplicates into duplicates table
   result=$(${psql} -U awipsadmin -d fxatext -c "DELETE FROM stdtextproducts WHERE (cccid,datacrc,hdrtime,nnnid,site,wmoid,xxxid,inserttime,product) IN (SELECT cccid,datacrc,hdrtime,nnnid,site,wmoid,xxxid,inserttime,product from stdtextproducts_dups2remove);" | sed '1q;d' | cut -c 7-)
  echo INFO: Deleted $result duplicates from stdtextproduct
   result=$(${psql} -U awipsadmin -d fxatext -c "SELECT count(*) FROM (SELECT *, count(*) OVER (PARTITION BY datacrc, hdrtime, nnnid, site, xxxid) AS count FROM stdtextproducts) tableWithCount WHERE tableWithCount.count > 1;" | sed '3q;d' | sed "s/ //g")
  echo INFO: Found $result duplicates after duplicate deletion
   result=$(${psql} -U awipsadmin -d fxatext -c "DROP TABLE stdtextproducts_dups2remove;")
fi

echo INFO: Altering table stdtextproducts to drop wmoid and cccc from primary key

result=$(${psql} --user=awipsadmin --db=fxatext -Atc "
        begin transaction;
        alter table stdtextproducts drop constraint if exists stdtextproducts_pkey;
        alter table stdtextproducts add constraint stdtextproducts_pkey primary key (nnnid, xxxid, site, hdrtime, datacrc);
        commit transaction;
        ")

if [[ "${result}" == "COMMIT" ]]; then
    echo INFO: Completed altering stdtextproducts_pkey
else
    echo WARNING: "${result}"
fi

echo INFO: Checking for duplicates in practicestdtextproducts table

ndups=$(${psql} -U awipsadmin -d fxatext -c "select count(*) from (SELECT inserttime FROM (SELECT *, count(*) OVER (PARTITION BY datacrc, hdrtime, nnnid, site, xxxid) AS count FROM practicestdtextproducts) tableWithCount WHERE tableWithCount.count > 1) as foo;" | sed '3q;d' | sed "s/ //g")

if [ "$ndups" = "0" ]; then
  echo INFO: Found no duplicates
else
  result=$(${psql} -U awipsadmin -d fxatext -c "CREATE TABLE practicestdtextproducts_dups2remove (LIKE practicestdtextproducts);")
  result=$(${psql} -U awipsadmin -d fxatext -c "INSERT INTO practicestdtextproducts_dups2remove (cccid,datacrc,hdrtime,nnnid,site,wmoid,xxxid,bbbid,inserttime,product,reftime) SELECT cccid,datacrc,hdrtime,nnnid,site,wmoid,xxxid,bbbid,inserttime,product,reftime FROM (SELECT *, ROW_NUMBER() OVER( PARTITION BY datacrc, hdrtime, nnnid, site, xxxid ORDER BY inserttime) AS row_num FROM practicestdtextproducts) t WHERE t.row_num > 1;" | sed '1q;d' | cut -c 9-)
  echo INFO: Inserted $result duplicates into duplicates table
   result=$(${psql} -U awipsadmin -d fxatext -c "DELETE FROM practicestdtextproducts WHERE (cccid,datacrc,hdrtime,nnnid,site,wmoid,xxxid,inserttime,product) IN (SELECT cccid,datacrc,hdrtime,nnnid,site,wmoid,xxxid,inserttime,product from practicestdtextproducts_dups2remove);" | sed '1q;d' | cut -c 7-)
  echo INFO: Deleted $result duplicates from practicestdtextproduct
   result=$(${psql} -U awipsadmin -d fxatext -c "SELECT count(*) FROM (SELECT *, count(*) OVER (PARTITION BY datacrc, hdrtime, nnnid, site, xxxid) AS count FROM practicestdtextproducts) tableWithCount WHERE tableWithCount.count > 1;" | sed '3q;d' | sed "s/ //g")
  echo INFO: Found $result duplicates after duplicate deletion
   result=$(${psql} -U awipsadmin -d fxatext -c "DROP TABLE practicestdtextproducts_dups2remove;") 
fi

echo INFO: Altering table practicestdtextproducts to drop wmoid and cccc from primary key

result=$(${psql} --user=awipsadmin --db=fxatext -Atc "
        begin transaction;
        alter table practicestdtextproducts drop constraint if exists practicestdtextproducts_pkey;
        alter table practicestdtextproducts add constraint practicestdtextproducts_pkey primary key (nnnid, xxxid, site, hdrtime, datacrc);
        commit transaction;
        ")

if [[ "${result}" == "COMMIT" ]]; then
    echo INFO: Completed altering practicestdtextproducts_pkey
else
    echo WARNING: "${result}"
fi
