#! /bin/bash
echo "DR #7836: dropping replication tables"

/awips2/psql/bin/psql -U awipsadmin -d metadata -c "delete from ebxml.registryobject where id = 'RegistryAvailability';"
/awips2/psql/bin/psql -U awipsadmin -d metadata -c "drop table registry_replication_site_events;"
/awips2/psql/bin/psql -U awipsadmin -d metadata -c "drop table registry_replication_events;"
/awips2/psql/bin/psql -U awipsadmin -d metadata -c "drop sequence replicationevent_seq;"

echo "DR #7836: Successfully dropped replication tables"
