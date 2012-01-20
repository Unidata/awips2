#!/bin/bash

export DELTA_BUILD="11.4"
export DELTA_ID="updateA2_pilDelta_purge"
export DELTA_DESC="remove old-style subscription tables"

export DELTA_RUN_USER="awips"

function runUpdate()
{
	#find pgsql
	local PSQL_INSTALL=`rpm -q --queryformat '%{INSTALLPREFIX}\n' awips2-psql`

	local PSQL="${PSQL_INSTALL}/bin/psql -U awips -d metadata -c"

	################################################################
	# Remove the static table.
	################################################################
	${PSQL} "DROP TABLE IF EXISTS subscription.static;"
	RC="$?"
	if [ ! "${RC}" = "0" ]; then
		return 1
	fi
	################################################################
	# Remove the replacements table.
	################################################################
	${PSQL} "DROP TABLE IF EXISTS subscription.replacements;"
	RC="$?"
	if [ ! "${RC}" = "0" ]; then
		return 1
	fi
	################################################################
	# Remove the subscriptions table.
	################################################################
	${PSQL} "DROP TABLE IF EXISTS subscription.subscriptions;"
	RC="$?"
	if [ ! "${RC}" = "0" ]; then
		return 1
	fi
	
	return 0
}
