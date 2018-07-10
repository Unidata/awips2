/**
 * This software was developed and / or modified by Raytheon Company,
 * pursuant to Contract DG133W-05-CQ-1067 with the US Government.
 * 
 * U.S. EXPORT CONTROLLED TECHNICAL DATA
 * This software product contains export-restricted data whose
 * export/transfer/disclosure is restricted by U.S. law. Dissemination
 * to non-U.S. persons whether in the United States or abroad requires
 * an export license or other authorization.
 * 
 * Contractor Name:        Raytheon Company
 * Contractor Address:     6825 Pine Street, Suite 340
 *                         Mail Stop B8
 *                         Omaha, NE 68106
 *                         402.291.0100
 * 
 * See the AWIPS II Master Rights File ("Master Rights File.pdf") for
 * further licensing information.
 **/
\set ON_ERROR_STOP 1
DROP DATABASE IF EXISTS maps;
DROP TABLESPACE IF EXISTS maps;
CREATE TABLESPACE maps owner awipsadmin location '%{tablespace_dir}%/maps';
CREATE DATABASE maps OWNER awipsadmin TABLESPACE maps;

\connect maps

BEGIN TRANSACTION;
GRANT CONNECT, TEMPORARY ON DATABASE maps TO awips;
CREATE SCHEMA mapdata AUTHORIZATION awipsadmin;

GRANT USAGE ON SCHEMA mapdata to awips;
ALTER DEFAULT PRIVILEGES IN SCHEMA mapdata GRANT SELECT, INSERT, UPDATE, DELETE, TRIGGER, TRUNCATE ON TABLES TO awips;
ALTER DEFAULT PRIVILEGES IN SCHEMA mapdata GRANT ALL ON SEQUENCES TO awips;
ALTER DEFAULT PRIVILEGES IN SCHEMA mapdata GRANT ALL ON FUNCTIONS TO awips;
ALTER DEFAULT PRIVILEGES IN SCHEMA mapdata GRANT ALL ON TYPES TO awips;

ALTER DEFAULT PRIVILEGES IN SCHEMA public GRANT SELECT, INSERT, UPDATE, DELETE, TRIGGER, TRUNCATE ON TABLES TO awips;
ALTER DEFAULT PRIVILEGES IN SCHEMA public GRANT ALL ON SEQUENCES TO awips;
ALTER DEFAULT PRIVILEGES IN SCHEMA public GRANT ALL ON FUNCTIONS TO awips;
ALTER DEFAULT PRIVILEGES IN SCHEMA public GRANT ALL ON TYPES TO awips;

CREATE SCHEMA topology AUTHORIZATION awipsadmin;
GRANT USAGE ON SCHEMA topology to awips;

ALTER DEFAULT PRIVILEGES IN SCHEMA topology GRANT SELECT, INSERT, UPDATE, DELETE, TRIGGER, TRUNCATE ON TABLES TO awips;
ALTER DEFAULT PRIVILEGES IN SCHEMA topology GRANT ALL ON SEQUENCES TO awips;
ALTER DEFAULT PRIVILEGES IN SCHEMA topology GRANT ALL ON FUNCTIONS TO awips;
ALTER DEFAULT PRIVILEGES IN SCHEMA topology GRANT ALL ON TYPES TO awips;

CREATE TABLE mapdata.map_version (
  table_name varchar(256) not null, 
  filename varchar(256) not null,
  import_time timestamp without time zone NOT NULL DEFAULT now(),
  CONSTRAINT map_version_pkey PRIMARY KEY (table_name)
/***
   failed attempt to make the corresponding row in map_version get dropped 
   if the referenced table is dropped 
  CONSTRAINT table_name_cascade FOREIGN KEY (table_name)
      REFERENCES information_schema.tables (table_name) MATCH SIMPLE
      ON UPDATE NO ACTION ON DELETE CASCADE
***/
);

COMMIT TRANSACTION;
