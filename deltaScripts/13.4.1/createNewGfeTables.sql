-- called by normalizeGfe.sh to create new GFE tables
DROP TABLE IF EXISTS gfe_locks CASCADE;
DROP TABLE IF EXISTS gfe_parmid CASCADE;
DROP TABLE IF EXISTS gfe_dbid CASCADE;
DROP SEQUENCE IF EXISTS gfe_lock_seq;
DROP SEQUENCE IF EXISTS gfe_parmid_seq;
DROP SEQUENCE IF EXISTS gfe_dbid_seq;
DROP SEQUENCE IF EXISTS gfe_history_seq;

CREATE TABLE gfe_dbid
(
  id integer NOT NULL,
  dbtype character varying(15),
  format character varying(255) NOT NULL,
  modelname character varying(64) NOT NULL,
  modeltime character varying(13) NOT NULL,
  siteid character varying(4) NOT NULL,
  CONSTRAINT gfe_dbid_pkey PRIMARY KEY (id),
  CONSTRAINT gfe_dbid_siteid_modelname_modeltime_dbtype_key UNIQUE (siteid, modelname, modeltime, dbtype)
)
WITH (
  OIDS=FALSE
);

ALTER TABLE gfe_dbid
  OWNER TO awips;

CREATE TABLE gfe_parmid
(
  id integer NOT NULL,
  parmlevel character varying(8),
  parmname character varying(100),
  dbid_id integer NOT NULL,
  CONSTRAINT gfe_parmid_pkey PRIMARY KEY (id),
  CONSTRAINT fkbec2950012156549 FOREIGN KEY (dbid_id)
      REFERENCES gfe_dbid (id) MATCH SIMPLE
      ON UPDATE NO ACTION ON DELETE CASCADE,
  CONSTRAINT gfe_parmid_dbid_id_parmname_parmlevel_key UNIQUE (dbid_id, parmname, parmlevel)
)
WITH (
  OIDS=FALSE
);
ALTER TABLE gfe_parmid
  OWNER TO awips;

CREATE TABLE gfe_locks
(
  id integer NOT NULL,
  endtime timestamp without time zone NOT NULL,
  starttime timestamp without time zone NOT NULL,
  wsid character varying(255) NOT NULL,
  parmid_id integer NOT NULL,
  CONSTRAINT gfe_locks_pkey PRIMARY KEY (id),
  CONSTRAINT fk92582e8f7bab05cc FOREIGN KEY (parmid_id)
      REFERENCES gfe_parmid (id) MATCH SIMPLE
      ON UPDATE NO ACTION ON DELETE CASCADE,
  CONSTRAINT gfe_locks_parmid_id_starttime_endtime_key UNIQUE (parmid_id, starttime, endtime)
)
WITH (
  OIDS=FALSE
);
ALTER TABLE gfe_locks
  OWNER TO awips;


CREATE SEQUENCE gfe_lock_seq
  INCREMENT 1
  MINVALUE 1
  MAXVALUE 9223372036854775807
  START 1
  CACHE 1;
ALTER TABLE gfe_lock_seq
  OWNER TO awips;

CREATE SEQUENCE gfe_history_seq
  INCREMENT 1
  MINVALUE 1
  MAXVALUE 9223372036854775807
  START 1
  CACHE 1;
ALTER TABLE gfe_history_seq
  OWNER TO awips;

ALTER TABLE gfe ADD COLUMN parmId_id integer;
