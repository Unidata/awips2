#!/bin/bash
# This script will add create tables for the grid plugin
#
# This needs to be performed with build ????
#

PSQL="/awips2/psql/bin/psql"
GRID_COMMAND="CREATE TABLE grid
(
  id integer NOT NULL,
  forecasttime integer,
  reftime timestamp without time zone,
  utilityflags character varying(255),
  rangeend timestamp without time zone,
  rangestart timestamp without time zone,
  datauri character varying(255),
  inserttime timestamp without time zone,
  info_id integer,
  CONSTRAINT grid_pkey PRIMARY KEY (id),
  CONSTRAINT fk308b46a3c100e9 FOREIGN KEY (info_id)
      REFERENCES grid_info (id) MATCH SIMPLE
      ON UPDATE NO ACTION ON DELETE NO ACTION,
  CONSTRAINT grid_datauri_key UNIQUE (datauri)
)
WITH (OIDS=FALSE);
ALTER TABLE grid OWNER TO awips;

CREATE INDEX "gridDatasetReftime_idx"
  ON grid
  USING btree
  (info_id, reftime, forecasttime);

CREATE INDEX gridpersistableplugindataobjectdatauri_idx
  ON grid
  USING btree
  (datauri);

CREATE INDEX gridpersistableplugindataobjectfcsttimeindex
  ON grid
  USING btree
  (forecasttime);

CREATE INDEX gridpersistableplugindataobjectinserttimeindex
  ON grid
  USING btree
  (inserttime);

CREATE INDEX gridpersistableplugindataobjectreftimeindex
  ON grid
  USING btree
  (reftime);
"

INFO_SEQ_COMMAND="CREATE SEQUENCE gridinfo_seq
  INCREMENT 1
  MINVALUE 1
  MAXVALUE 9223372036854775807
  START 91
  CACHE 1;
ALTER TABLE gridinfo_seq OWNER TO awips;"

INFO_COMMAND="CREATE TABLE grid_info
(
  id integer NOT NULL,
  datasetid character varying(255),
  ensembleid character varying(255),
  secondaryid character varying(255),
  level_id bigint,
  location_id integer,
  parameter_abbreviation character varying(255),
  CONSTRAINT grid_info_pkey PRIMARY KEY (id),
  CONSTRAINT fk4c4dae072d36f480 FOREIGN KEY (level_id)
      REFERENCES "level" (id) MATCH SIMPLE
      ON UPDATE NO ACTION ON DELETE NO ACTION,
  CONSTRAINT fk4c4dae0746b2bf12 FOREIGN KEY (location_id)
      REFERENCES gridcoverage (id) MATCH SIMPLE
      ON UPDATE NO ACTION ON DELETE NO ACTION,
  CONSTRAINT fk4c4dae076765a9e7 FOREIGN KEY (parameter_abbreviation)
      REFERENCES parameter (abbreviation) MATCH SIMPLE
      ON UPDATE NO ACTION ON DELETE NO ACTION
)
WITH (OIDS=FALSE);
ALTER TABLE grid_info OWNER TO awips;

CREATE INDEX "gridinfoNameParamLevel_idx"
  ON grid_info
  USING btree
  (datasetid, parameter_abbreviation, level_id);

CREATE INDEX "gridinfoSecondryId_idx"
  ON grid_info
  USING btree
  (secondaryid);"

PARAM_COMMAND="CREATE TABLE parameter
(
  abbreviation character varying(255) NOT NULL,
  "name" character varying(255) NOT NULL,
  unit character varying(255),
  CONSTRAINT parameter_pkey PRIMARY KEY (abbreviation)
)
WITH (OIDS=FALSE);
ALTER TABLE parameter OWNER TO awips;"

SQL_COMMAND_REGISTER="insert into plugin_info (name, database, initialized, tablename) VALUES('grid', 'metadata', TRUE, 'grid'), ('parameter', 'metadata', TRUE, 'parameter');"


if [ ! -f ${PSQL} ]; then
echo "ERROR: The PSQL executable does not exist - ${PSQL}."
echo "FATAL: Update Failed!"
exit 1
fi

echo ""
echo "Press Enter to perform the updates Ctrl-C to quit."
read done

${PSQL} -U awips -d metadata -c "${PARAM_COMMAND}"
if [ $? -ne 0 ]; then
echo "FATAL: Update Failed!"
exit 1
fi

${PSQL} -U awips -d metadata -c "${INFO_SEQ_COMMAND}"
if [ $? -ne 0 ]; then
echo "FATAL: Update Failed!"
exit 1
fi


${PSQL} -U awips -d metadata -c "${INFO_COMMAND}"
if [ $? -ne 0 ]; then
echo "FATAL: Update Failed!"
exit 1
fi

${PSQL} -U awips -d metadata -c "${GRID_COMMAND}"
if [ $? -ne 0 ]; then
echo "FATAL: Update Failed!"
exit 1
fi

${PSQL} -U awips -d metadata -c "${SQL_COMMAND_REGISTER}"
if [ $? -ne 0 ]; then
echo "FATAL: Update Failed!"
exit 1
fi

echo "INFO: The update was successfully applied."

exit 0