#Update postgis extensions to 2.3.3

for db in metadata maps ncep; do
echo Updating postgis extensions for ${db} database
/awips2/psql/bin/psql -U awipsadmin -d ${db} -c "ALTER EXTENSION postgis UPDATE TO '2.3.3'";
/awips2/psql/bin/psql -U awipsadmin -d ${db} -c "ALTER EXTENSION postgis_topology UPDATE TO '2.3.3'";
done

echo Done
