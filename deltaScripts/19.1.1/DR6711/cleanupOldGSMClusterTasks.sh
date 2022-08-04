#! /bin/bash
#
# removes obsolete cluster_task entries used by the previous implementation of
# Radar General Status Message notification code
#
/awips2/psql/bin/psql -U awips -d metadata -c "DELETE FROM cluster_task WHERE name IN ('rdaStatus', 'rpgStatus','rdaOperabilityStatus','rpgOperabilityStatus','rpgNarrowbandStatus');"