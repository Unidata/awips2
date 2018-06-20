DROP TABLE IF EXISTS gfe_spatial;

-- Sequence: gfe_gridlocation_seq

-- DROP SEQUENCE gfe_gridlocation_seq;

CREATE SEQUENCE gfe_gridlocation_seq
  INCREMENT 1
  MINVALUE 1
  MAXVALUE 9223372036854775807
  START 1
  CACHE 1;
ALTER TABLE gfe_gridlocation_seq
  OWNER TO awips;
  
-- Table: gfe_gridlocation

-- DROP TABLE gfe_gridlocation;

CREATE TABLE gfe_gridlocation
(
  id integer NOT NULL,
  extent bytea NOT NULL,
  nx integer NOT NULL,
  ny integer NOT NULL,
  origin bytea NOT NULL,
  gridpointll bytea NOT NULL,
  gridpointur bytea NOT NULL,
  latintersect double precision NOT NULL,
  latlonll bytea NOT NULL,
  latlonorigin bytea NOT NULL,
  latlonur bytea NOT NULL,
  loncenter double precision NOT NULL,
  lonorigin double precision NOT NULL,
  projectionid character varying(32) NOT NULL,
  projectiontype character varying(20) NOT NULL,
  stdparallelone double precision NOT NULL,
  stdparalleltwo double precision NOT NULL,
  siteid character varying(8) NOT NULL,
  timezone character varying(32) NOT NULL,
  dbid_id integer NOT NULL,
  CONSTRAINT gfe_gridlocation_pkey PRIMARY KEY (id),
  CONSTRAINT fk22b8153412156549 FOREIGN KEY (dbid_id)
      REFERENCES gfe_dbid (id) MATCH SIMPLE
      ON UPDATE NO ACTION ON DELETE CASCADE,
  CONSTRAINT gfe_gridlocation_dbid_id_key UNIQUE (dbid_id)
)
WITH (
  OIDS=FALSE
);
ALTER TABLE gfe_gridlocation
  OWNER TO awips;

-- Sequence: gfe_parminfo_seq

-- DROP SEQUENCE gfe_parminfo_seq;

CREATE SEQUENCE gfe_parminfo_seq
  INCREMENT 1
  MINVALUE 1
  MAXVALUE 9223372036854775807
  START 1
  CACHE 1;
ALTER TABLE gfe_parminfo_seq
  OWNER TO awips;
  
-- Table: gfe_parminfo

-- DROP TABLE gfe_parminfo;

CREATE TABLE gfe_parminfo
(
  id integer NOT NULL,
  datamultiplier real NOT NULL,
  dataoffset real NOT NULL,
  datatype character varying(8) NOT NULL,
  descriptivename character varying(64) NOT NULL,
  gridtype character varying(8) NOT NULL,
  maxvalue real NOT NULL,
  minvalue real NOT NULL,
  "precision" integer NOT NULL,
  rateparm boolean NOT NULL,
  duration integer NOT NULL,
  repeatinterval integer NOT NULL,
  starttime integer NOT NULL,
  valid boolean NOT NULL,
  timeindependentparm boolean NOT NULL,
  unitstring character varying(64) NOT NULL,
  storagetype character varying(8) NOT NULL,
  gridloc_id integer NOT NULL,
  parmid_id integer NOT NULL,
  CONSTRAINT gfe_parminfo_pkey PRIMARY KEY (id),
  CONSTRAINT fk1871875338803a4d FOREIGN KEY (gridloc_id)
      REFERENCES gfe_gridlocation (id) MATCH SIMPLE
      ON UPDATE NO ACTION ON DELETE CASCADE,
  CONSTRAINT fk187187537bab05cc FOREIGN KEY (parmid_id)
      REFERENCES gfe_parmid (id) MATCH SIMPLE
      ON UPDATE NO ACTION ON DELETE CASCADE,
  CONSTRAINT gfe_parminfo_parmid_id_key UNIQUE (parmid_id)
)
WITH (
  OIDS=FALSE
);
ALTER TABLE gfe_parminfo
  OWNER TO awips;
  