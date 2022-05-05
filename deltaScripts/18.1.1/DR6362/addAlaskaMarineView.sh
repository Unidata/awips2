#!/bin/sh
# DR #6362 - This script creates the alaska_marine view
SIMPLEVS=("0.064" "0.016" "0.004" "0.001")
#
# Ensure simplification levels exist for both marinezones and offshore
#
for LEV in "${SIMPLEVS[@]}" ; do
    echo "    Creating simplified geometry level $LEV ..."
    SUFFIX=${LEV/./_}
    /awips2/psql/bin/psql -d maps -U awipsadmin -q -c "
    DO \$\$
        BEGIN
            BEGIN
                PERFORM AddGeometryColumn('mapdata','marinezones','the_geom_${SUFFIX}','4326',(SELECT type FROM public.geometry_columns WHERE f_table_schema='mapdata' and f_table_name='marinezones' and f_geometry_column='the_geom'),2);
                UPDATE mapdata.marinezones SET the_geom_${SUFFIX}=ST_Segmentize(ST_Multi(ST_SimplifyPreserveTopology(the_geom,${LEV})),0.1);
                CREATE INDEX marinezones_the_geom_${SUFFIX}_gist ON mapdata.marinezones USING gist(the_geom_${SUFFIX});
              EXCEPTION
                  WHEN duplicate_column THEN
            END;
            BEGIN
                PERFORM AddGeometryColumn('mapdata','offshore','the_geom_${SUFFIX}','4326',(SELECT type FROM public.geometry_columns WHERE f_table_schema='mapdata' and f_table_name='offshore' and f_geometry_column='the_geom'),2);
                UPDATE mapdata.offshore SET the_geom_${SUFFIX}=ST_Segmentize(ST_Multi(ST_SimplifyPreserveTopology(the_geom,${LEV})),0.1);
                CREATE INDEX offshore_the_geom_${SUFFIX}_gist ON mapdata.offshore USING gist(the_geom_${SUFFIX});
              EXCEPTION
                  WHEN duplicate_column THEN
            END;
        END;
    \$\$"
done
#
# Create the alaska_marine view
#
/awips2/psql/bin/psql -d maps -U awipsadmin -q -c "
        DROP VIEW IF EXISTS mapdata.alaska_marine;
        CREATE OR REPLACE VIEW mapdata.alaska_marine AS
        SELECT CAST(ROW_NUMBER() OVER(ORDER BY id) AS INT) GID, * FROM (
            SELECT id, wfo, name, lat, lon, 
                the_geom, the_geom_0, the_geom_0_064, the_geom_0_016, the_geom_0_004, the_geom_0_001 
            FROM mapdata.marinezones WHERE wfo LIKE '%AFC%' or wfo LIKE '%AFG%' or wfo LIKE '%AJK%' 
            UNION 
            SELECT id, wfo, name, lat, lon, 
                the_geom, the_geom_0, the_geom_0_064, the_geom_0_016, the_geom_0_004, the_geom_0_001 
            FROM mapdata.offshore WHERE wfo LIKE '%AFC%' or wfo LIKE '%AFG%' or wfo LIKE '%AJK%'
        ) a;
"
