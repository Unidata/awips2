#!/bin/bash

psql=/awips2/psql/bin/psql
databases=$(${psql} --db metadata -U awipsadmin -Atc "
    select datname
    from pg_database
    where datistemplate = false
    and datname not in ('awips', 'postgres');
    ")
required=(metadata maps ncep)

new_ver="2.2"
new_ver_long="2.2.2"
scripts_dir="/awips2/postgresql/share/contrib/postgis-${new_ver}"

postgis_exists() {
    ${psql} --db "$1" -U awipsadmin -Atc "
        select exists(select 1 from pg_proc where proname = 'postgis_version');
        "
}

raster_exists() {
    "${psql}" --db "$1" -U awipsadmin -Atc "
    select exists(
        select postgis_full_version
        from (select postgis_full_version()) t
        where postgis_full_version ~ 'RASTER'
        );
        "
}

topology_exists() {
    "${psql}" --db "$1" -U awipsadmin -Atc "
    select exists(
        select postgis_full_version
        from (select postgis_full_version()) t
        where postgis_full_version ~ 'TOPOLOGY'
        );
        "
}

get_postgis_ver () {
    ${psql} --db "$1" -U awipsadmin -Atc "
        select substring(extversion from 1 for 3) s from pg_extension where extname = 'postgis';
        "
}

alter_extensions () {
    ${psql} --db "$1" -U awipsadmin -Atc "
        begin transaction;
        alter extension postgis update to '${new_ver_long}';
        alter extension postgis_topology update to '${new_ver_long}';
        commit;
        "
}

create_postgis () {
    ${psql} --db "$1" -U awipsadmin -Atc "
        begin transaction;
        create extension postgis;
        create extension postgis_topology;
        commit;
        "
}

create_postgis_from_unpackaged () {
    ${psql} --db "$1" -U awipsadmin -Atc "
        begin transaction;
        create extension postgis from unpackaged;
        create extension postgis_topology from unpackaged;
        commit;
        "
}

get_legacy_error_count () {
    ${psql} --db "$1" -U awipsadmin -Atc "
        select count(*) c from pg_proc where probin like '%postgis%' and probin not like '%${new_ver}%';
        "
}

create_legacy () {
    echo "INFO: Creating legacy functions for $1"
    ${psql} --db "$1" -U awipsadmin -f "${scripts_dir}"/uninstall_legacy.sql >/dev/null 2>&1
    ${psql} --db "$1" -U awipsadmin -f "${scripts_dir}"/legacy.sql 2>&1 | grep -Ev "CREATE (FUNCTION|AGGREGATE)"
    echo "INFO: Done creating legacy functions for "$1""
    error_count=$(get_legacy_error_count "$1")
    if [[ "${error_count}" -ne 0 ]]; then
        echo ERROR: Found "${error_count}" remaining references to postgis-2.0 in "$1". Check the PostgreSQL logs for details.
    fi
}

alternate_upgrade() {
    ${psql} --db "$1" -U awipsadmin -f "${scripts_dir}"/postgis_upgrade.sql >/dev/null
    if [[ "$(raster_exists "$1")" == "t" ]]; then
        echo "INFO: Upgrading raster"
        ${psql} --db "$1" -U awipsadmin -f "${scripts_dir}"/rtpostgis_upgrade.sql >/dev/null
        echo "INFO: Done upgrading raster"
    else
        echo "INFO: raster is not installed. Installing"
        ${psql} --db "$1" -U awipsadmin -f "${scripts_dir}"/rtpostgis.sql >/dev/null
        echo "INFO: Done installing raster"
    fi
    if [[ "$(topology_exists "$1")" == "t" ]]; then
        echo "INFO: Upgrading topology"
        ${psql} --db "$1" -U awipsadmin -f "${scripts_dir}"/topology_upgrade.sql >/dev/null
        echo "INFO: Done upgrading topology"
    else
        echo "INFO: topology is not installed. Installing"
        ${psql} --db "$1" -U awipsadmin -f "${scripts_dir}"/topology.sql >/dev/null
        echo "INFO: Done installing topology"
    fi
}


echo INFO: Starting PostGIS upgrade.
if [[ -z "${databases}" ]]; then
    echo "ERROR: Failed to get a list of databases. Make sure postgres is running and pg_hba.conf is allowing connections."
    exit 1
fi

echo INFO: Found databases: ${databases}
found=false
for required_db in "${required[@]}"; do
    found=false
    for db in ${databases}; do
        if [[ "${db}" == "${required_db}" ]]; then
            found=true
        fi
    done
    if [[ "${found}" != "true" ]]; then
        echo WARN: Required database "${required_db}" was not found! Continuing without it...
    fi
done

for db in ${databases} ; do
    echo -n "  ${db}... "
    has_extension=$(${psql} --db ${db} -U awipsadmin -Atc "
        select 'y' from pg_extension where extname like 'postgis%' limit 1;
        ")
    if [[ "$?" -ne 0 ]]; then
        echo "ERROR: Failed to query the ${db} database. Make sure postgres is running and pg_hba.conf is allowing connections."
        continue
    fi
    if [[ "${has_extension}" == "y" ]]; then
        ver=$(get_postgis_ver "${db}")
        echo "found PostGIS version ${ver}"
        if [[ "${ver}" != "${new_ver}" ]]; then
            echo "Upgrading ${db}"
            alter_extensions "${db}"
            ver=$(get_postgis_ver "${db}")
            if [[ "${ver}" != "${new_ver}" ]]; then
                echo "ERROR: 'alter extension' failed for ${db}. Check the PostgreSQL logs for details"
            fi
        fi
        create_legacy "${db}"
    else
        echo -n "no PostGIS extension found. Checking for PostGIS installed via script..."
        if [[ "$(postgis_exists "${db}")" == "t" ]]; then
            echo "found."
            echo "INFO: PostGIS on ${db} was installed via script. Using alternate upgrade procedure"
            alternate_upgrade "${db}"
            echo "INFO: Creating extensions for ${db}"
            create_postgis_from_unpackaged "${db}"
            echo "INFO: Done creating extensions for ${db}"
            if [[ "${ver}" != "${new_ver}" ]]; then
                echo "ERROR: 'create extension' failed for ${db}. Check the PostgreSQL logs for details"
            fi
            create_legacy "${db}"
        else
            echo "not found."
            for required_db in "${required[@]}"; do
                if [[ "${db}" == "${required_db}" ]]; then
                    echo "WARN: "${db}" should have PostGIS but doesn't. Installing it now."
                    create_postgis "${db}"
                    ver=$(get_postgis_ver ${db})
                    if [[ "${ver}" != "${new_ver}" ]]; then
                        echo "ERROR: 'create extension' failed for ${db}. Check the PostgreSQL logs for details"
                    fi
                    create_legacy "${db}"
                fi
            done
        fi
    fi
done
echo INFO: Finished PostGIS upgrade.
