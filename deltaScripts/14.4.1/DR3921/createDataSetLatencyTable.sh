#!/bin/bash
##
# This software was developed and / or modified by Raytheon Company,
# pursuant to Contract DG133W-05-CQ-1067 with the US Government.
# 
# U.S. EXPORT CONTROLLED TECHNICAL DATA
# This software product contains export-restricted data whose
# export/transfer/disclosure is restricted by U.S. law. Dissemination
# to non-U.S. persons whether in the United States or abroad requires
# an export license or other authorization.
# 
# Contractor Name:        Raytheon Company
# Contractor Address:     6825 Pine Street, Suite 340
#                         Mail Stop B8
#                         Omaha, NE 68106
#                         402.291.0100
# 
# See the AWIPS II Master Rights File ("Master Rights File.pdf") for
# further licensing information.
##

# Issue: #3921 Delta script for #3550 dataset latency
#
# This script will add table dd_data_set_latency to the metadata.AWIPS schema.
# This script will also create an Identifier Sequence (data_set_latency_seq sequence) for the new table.
#
PSQL="/awips2/psql/bin/psql"

if [ ! -f ${PSQL} ];
then
	echo "ERROR: The PSQL executable does not exist - ${PSQL}."
	echo "FATAL: Update Failed!"
	exit 1
fi

CREATE_DD_DATA_SET_LATENCY_TABLE_SQL="CREATE TABLE dd_data_set_latency ( \
basereferencetime timestamp without time zone, \
datasetname character varying(255) NOT NULL, \
extendedlatency integer, \
provider character varying(255) NOT NULL, \
identifier bigint NOT NULL, \
CONSTRAINT dd_data_set_latency_pkey PRIMARY KEY (identifier), \
CONSTRAINT dd_data_set_latency_key UNIQUE (datasetname, provider) \
) WITH ( OIDS=FALSE ); \
ALTER TABLE dd_data_set_latency OWNER TO awips;"

CREATE_DATA_SET_LATENCY_SEQ_SQL="CREATE SEQUENCE data_set_latency_seq \
  INCREMENT 1 \
  MINVALUE 1 \
  MAXVALUE 9223372036854775807 \
  START 240 \
  CACHE 1; \
ALTER TABLE data_set_latency_seq \
  OWNER TO awips;" 

function createDataSetLatencyTable
{
	echo "INFO: Adding dd_data_set_latency table"

	${PSQL} -U awips -d metadata -a -c "${CREATE_DD_DATA_SET_LATENCY_TABLE_SQL}"
	if [ $? -ne 0 ];
	then
		echo "FATAL: Add Failed!"
		exit 1	
	fi
	echo "INFO: Completed Adding dd_data_set_latency table"

	echo "INFO: Adding data_set_latency_seq sequence"
	${PSQL} -U awips -d metadata -a -c "${CREATE_DATA_SET_LATENCY_SEQ_SQL}"
	if [ $? -ne 0 ]; 
	then
		echo "FATAL: Add Failed!"
		exit 1
	fi
	echo "INFO: Completed Adding data_set_latency_seq sequence"
	echo "INFO: Table and Sequence add complete"
}

function dropDataSetLatencyTable
{
	echo "INFO: Dropping data_set_latency_seq sequence"
	${PSQL} -U awips -d metadata -a -c "DROP SEQUENCE data_set_latency_seq;"
	if [ $? -ne 0 ];
	then
		echo "FATAL: Drop Failed!"
	fi
	echo "INFO: Dropping dd_data_set_latency table"
	${PSQL} -U awips -d metadata -a -c "DROP TABLE dd_data_set_latency;"
	if [ $? -ne 0 ];
	then
		echo "FATAL: Drop Failed!"
	fi
	echo "INFO: Table and Sequence drop complete"
}


dropDataSetLatencyTable
createDataSetLatencyTable

exit 0


