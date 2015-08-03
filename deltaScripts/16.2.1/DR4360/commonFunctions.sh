# Common functions intended to be source by other scripts.

# takes two args: table, new constraint name
# will find table's 'unamed' constraint and if only one exists will rename it.
#
function renameConstraint {
	cmd="select constraint_name from information_schema.table_constraints
where table_catalog = 'metadata' and table_schema='awips' and constraint_type='UNIQUE' and table_name='$1'; "
	constraint_name=(`${PSQL} -U awips -d metadata -t -c "$cmd"`)
	
	if [ $? -ne 0 ] ; then
		echo "ERROR: The update failed to obtain 'unnamed' unique constraint on table $1"
	fi
	
	cnLen=${#constraint_name[@]}
	if [ ${cnLen} -eq 0 ] ; then
		echo "WARNING: no UNIQUE constraint found for table $1"
	elif [ ${cnLen} -gt 1 ] ; then
		echo "ERROR: The update failed. More then one unnamed UNIQUE constraint found for table ${1} (${constraint_name[@]})"
	elif [ ${constraint_name} == ${2} ] ; then
		echo "INFO: No constraint rename on table ${table} performed; ${2} already exists."
	else
		echo "INFO: On $1 renaming constraint: ${constraint_name} to ${2}"
		${PSQL} -U awips -d metadata -c "ALTER TABLE ${1} RENAME CONSTRAINT ${constraint_name} TO ${2} ;"
		if [ $? -ne 0 ] ; then
			echo "ERROR: The update failed on table $1 to rename unique constraint ${constraint_name} to $2."
		fi
	fi
}

# takes two args: table column
# Add not null constraint to table's column
function updateNotNullCol {
	${PSQL} -U awips -d metadata -c "ALTER TABLE $1 ALTER COLUMN $2 SET NOT NULL ;"
	if [ $? -ne 0 ] ; then
		echo "ERROR: Failed on table $1 to add not null constraint on column $2."
		echo "FATAL: The update failed."
		exit 1
	else
		echo "INFO: Added non null constraint to column $2 on table $1."
	fi
}

# takes one args: table
# return 0 status if the table exists.
function tableExists {
${PSQL} -U awips -d metadata -q \
	-c "SELECT count(*) from information_schema.tables where table_schema='awips' and table_name='${1}' ; " \
	2>1 | grep -q '^\s*1$'
}
