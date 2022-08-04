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
--This is the inital setup SQL to create the AWIPS metadata database.
--This script creates the AWIPS user, tablespace, database and objects 
--accessed by the AWIPS Data Layer.  Use psql to execute this script as the postgres user.

--Create the awips user and modify passwords
ALTER USER %{databaseUsername} with password 'postgres' login superuser createdb createrole;
CREATE USER postgres superuser;

-- connect as postgres user to allow rename of primary user to awipsadmin
\c postgres postgres

ALTER USER %{databaseUsername} rename to awipsadmin;
ALTER USER awipsadmin with password 'awips';

\c postgres awipsadmin

CREATE USER awips with password 'awips' login; 

--Create the metadata tablespace
CREATE TABLESPACE metadata owner awipsadmin location '%{tablespace_dir}/metadata';

--Create the database
CREATE DATABASE metadata OWNER awipsadmin TABLESPACE metadata;

--Switch to the metadata database
\c metadata;

--Create the AWIPS schema
GRANT CONNECT, TEMPORARY ON DATABASE metadata TO awips;
CREATE SCHEMA awips AUTHORIZATION awipsadmin;

-- Grant privileges to awips
GRANT USAGE ON SCHEMA awips to awips; -- Don't grant create
ALTER DEFAULT PRIVILEGES IN SCHEMA awips GRANT SELECT, INSERT, UPDATE, DELETE, TRIGGER, TRUNCATE ON TABLES TO awips; -- Don't grant references
ALTER DEFAULT PRIVILEGES IN SCHEMA awips GRANT ALL ON SEQUENCES TO awips;
ALTER DEFAULT PRIVILEGES IN SCHEMA awips GRANT ALL ON FUNCTIONS TO awips;
ALTER DEFAULT PRIVILEGES IN SCHEMA awips GRANT ALL ON TYPES TO awips;

ALTER DATABASE metadata SET search_path = awips, public, topology;

COMMENT ON ROLE awipsadmin IS 'Primary AWIPS admin user';
COMMENT ON ROLE awips IS 'Primary AWIPS user';
COMMENT ON DATABASE metadata IS 'AWIPS Metadata Database';
COMMENT ON TABLESPACE metadata IS 'AWIPS Metadata Database Tablespace';
COMMENT ON SCHEMA awips IS 'AWIPS Schema';

CREATE TABLESPACE pgdata_ihfs OWNER awipsadmin LOCATION '%{tablespace_dir}/pgdata_ihfs';
COMMENT ON TABLESPACE pgdata_ihfs IS 'IHFS Database tablespace';

