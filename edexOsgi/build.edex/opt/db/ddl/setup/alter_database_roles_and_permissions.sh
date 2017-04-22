#!/bin/bash
# alters all database objects to be owned by awips admin. Gives CRUD roles to pg_user and awips accounts.

PSQL='/awips2/psql/bin/psql'
adminUser='awips'
users=('awips' 'pguser')


# first arg is database, second arg is schema, third is field type, rest is the query to execute
scanAndUpdate() {
local database=$1
local schema=$2
local fieldType=$3
shift 3
local query="$@"

local i=0
local stmt
local rows=$(${PSQL} -d $database -U ${adminUser} -Aqtc "${query}")
for row in ${rows}; do
    let "i=i+1"
    stmt[$i]="ALTER $fieldType \"${schema}\".$row OWNER to ${adminUser};"
done

if [ ${i} -gt 0 ]; then
   echo -e "\t\tUpdating $i ${fieldType}s"
   ${PSQL} -U ${adminUser} -d $database -q << EOF
BEGIN TRANSACTION;
${stmt[@]}
COMMIT TRANSACTION;
EOF
else
   echo -e "\t\tNo ${fieldType}s to update"
fi

}

# first arg is database, second arg is schema
grantForSchema() {
local database=$1
local schema=$2

echo -e "\tUpdating privileges on schema $schema"
${PSQL} -U ${adminUser} -d $database -qc "ALTER SCHEMA \"${schema}\" owner to ${adminUser};"

# Update ownership of all tables to admin account
scanAndUpdate $database $schema table "select quote_ident(tablename) from pg_tables where schemaname = '${schema}' and tableowner != '${adminUser}';"

# Update ownership of all views to admin account
scanAndUpdate $database $schema view "select quote_ident(c.relname) from pg_catalog.pg_class c left JOIN pg_catalog.pg_namespace n on n.oid = c.relnamespace where c.relkind = 'v'::\"char\" and n.nspname = '${schema}' and pg_get_userbyid(c.relowner) != '${adminUser}'"

# Update ownership of all sequences to admin account
scanAndUpdate $database $schema sequence "select quote_ident(c.relname) from pg_catalog.pg_class c left JOIN pg_catalog.pg_namespace n on n.oid = c.relnamespace where c.relkind = 'S'::\"char\" and n.nspname = '${schema}' and pg_get_userbyid(c.relowner) != '${adminUser}';"

# Update ownership of all functions to admin account
scanAndUpdate $database $schema function "SELECT  quote_ident(p.proname) || '(' || pg_catalog.pg_get_function_identity_arguments(p.oid) || ')' AS func_def FROM pg_catalog.pg_proc p JOIN pg_catalog.pg_namespace n ON n.oid = p.pronamespace WHERE n.nspname = '${schema}' and pg_get_userbyid(p.proowner) != '${adminUser}';"

for user in ${users[@]}; do
    echo -e "\t\tGranting CRUD privileges to $user"
   ${PSQL} -U ${adminUser} -d $database -q << EOF
BEGIN TRANSACTION;
GRANT USAGE ON SCHEMA "${schema}" TO $user;
GRANT SELECT, INSERT, UPDATE, DELETE, TRIGGER, TRUNCATE ON ALL TABLES IN SCHEMA "$schema" TO $user;
GRANT ALL ON ALL SEQUENCES IN SCHEMA "${schema}" TO $user;
GRANT ALL ON ALL FUNCTIONS IN SCHEMA "${schema}" TO $user;
ALTER DEFAULT PRIVILEGES IN SCHEMA "${schema}" GRANT SELECT, INSERT, UPDATE, DELETE, TRIGGER, TRUNCATE ON TABLES TO $user;
ALTER DEFAULT PRIVILEGES IN SCHEMA "${schema}" GRANT ALL ON SEQUENCES TO $user;
ALTER DEFAULT PRIVILEGES IN SCHEMA "${schema}" GRANT ALL ON FUNCTIONS TO $user;
ALTER DEFAULT PRIVILEGES IN SCHEMA "${schema}" GRANT ALL ON TYPES TO $user;
COMMIT TRANSACTION;
EOF
done
}

grantForDatabase() {
local database=$1

echo
echo "Updating privileges on database $database"

# Change ownership to ${adminUser}
local i=0
local stmt[0]="ALTER DATABASE \"${database}\" OWNER to ${adminUser};"
for user in ${users[@]}; do
    let "i=i+1"
    stmt[$i]="GRANT CONNECT, TEMPORARY ON DATABASE \"${database}\" TO $user;"
done

${PSQL} -U ${adminUser} -d $database -q << EOF
BEGIN TRANSACTION;
${stmt[@]}
COMMIT TRANSACTION;
EOF

schemas=$(${PSQL} -d $database -U ${adminUser} -qtAc "select nspname from pg_namespace where nspname not like 'pg_%' and  nspname not in ('information_schema')")

for schema in $schemas; do
    grantForSchema $database $schema
done
}

# set field separator to line feed only
IFS=$'\n'

${PSQL} -d metadata -U ${adminUser} -qc "ALTER DATABASE metadata SET search_path = awips, public, topology;"

databases=$(${PSQL} -d metadata -U ${adminUser} -Aqtc "
    select datname
    from pg_database
    where datistemplate = false
    and datname not in ('awips', 'postgres')
    ")

for database in ${databases}; do
    grantForDatabase $database
done

