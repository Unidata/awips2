/**
 * This software was developed and / or modified by NOAA/NWS/OCP/ASDT
 *
 * Create climate database 
 */
\set ON_ERROR_STOP 1
/*
 * Connect to unrelated DB; new one does not exist and cannot be connected to the DB to be copied.
 */
\connect metadata
/*
 * Create tablespace.
 */
CREATE TABLESPACE climate OWNER awipsadmin LOCATION '/awips2/database/tablespaces/climate';
COMMENT ON TABLESPACE climate IS 'Climate Database tablespace';
/*
 * Copy hmdb.
 */
CREATE DATABASE climate WITH TEMPLATE hmdb;

/*
 * Connect to new DB.
 */
\connect climate

/*
 * Create new tables.
 */
CREATE TABLE cpg_session
(
  cpg_session_id character varying(50) NOT NULL,
  run_type integer NOT NULL,
  prod_type integer NOT NULL,
  state integer NOT NULL,
  status integer,
  status_desc character varying(1024),
  global_config bytea,
  prod_setting bytea,
  report_data bytea,
  prod_data bytea,
  start_at timestamp without time zone NOT NULL,
  last_updated timestamp without time zone NOT NULL,
  pending_expire timestamp without time zone,
  CONSTRAINT cpg_session_pkey PRIMARY KEY (cpg_session_id)
)
WITH (
  OIDS=FALSE
);
ALTER TABLE cpg_session
  OWNER TO awipsadmin;
GRANT ALL ON TABLE cpg_session TO awipsadmin;
GRANT SELECT, UPDATE, INSERT, TRUNCATE, DELETE, TRIGGER ON TABLE cpg_session TO awips;
GRANT SELECT, UPDATE, INSERT, TRUNCATE, DELETE, TRIGGER ON TABLE cpg_session TO pguser;

CREATE TABLE sent_prod_record
(
  prod_id character varying(16) NOT NULL,
  period_type character varying(16) NULL,
  prod_type character varying(4) NOT NULL,
  file_name character varying(50) NULL,
  prod_text TEXT NOT NULL,
  send_time timestamp without time zone NOT NULL,
  user_id character varying(32) NULL,
  CONSTRAINT sent_prod_record_pkey PRIMARY KEY (prod_id, send_time)
)
WITH (
  OIDS=FALSE
);
ALTER TABLE sent_prod_record
  OWNER TO awipsadmin;
GRANT ALL ON TABLE sent_prod_record TO awipsadmin;
GRANT SELECT, UPDATE, INSERT, TRUNCATE, DELETE, TRIGGER ON TABLE sent_prod_record TO awips;
GRANT SELECT, UPDATE, INSERT, TRUNCATE, DELETE, TRIGGER ON TABLE sent_prod_record TO pguser;