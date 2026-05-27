#!/bin/bash

/awips2/psql/bin/psql -U awips -d ncep -c "UPDATE stns.cities SET station_id='UTQIAGVI',  name='UTQIAGVIK'  WHERE station_number='25711' and name='BARROW';"
/awips2/psql/bin/psql -U awips -d hmdb -c "UPDATE sta_agency_codes SET agency_sta_name='UTQIAGVIK (BARROW)' WHERE station_id=23210 and agency_sta_name='BARROW';"
/awips2/psql/bin/psql -U awips -d hmdb -c "UPDATE station_location SET station_name='UTQIAGVIK (BARROW)' WHERE station_id=23210 and station_name='BARROW';"
