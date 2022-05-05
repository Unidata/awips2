#!/bin/bash

/awips2/psql/bin/psql --user=awipsadmin --db=metadata -ac \
    "insert into awips.radar_spatial
     (rda_id,rpg_id_dec,immutablex,wfo_id,eqp_elv,name,lon,lat,elevmeter,the_geom)
     values ('FNAW','4014','4014.0000','KEY','556.0000','Guantanamo Bay','-75.15900','19.90000','169.46880',ST_GeometryFromText('POINT(-75.159 19.900)',4326) )
     on conflict do nothing;"
