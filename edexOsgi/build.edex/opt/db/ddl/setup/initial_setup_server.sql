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
ALTER USER %{databaseUsername} with password 'postgres';
CREATE ROLE awips with password 'awips' login superuser createdb createrole;
ALTER USER awips with password 'awips';

--Create the metadata tablespace
CREATE TABLESPACE metadata owner awips location '%{database_files_home}/metadata';

--Create the database
CREATE DATABASE metadata OWNER awips TABLESPACE metadata;

--Switch to the metadata database
\c metadata;

--Create the AWIPS schema
CREATE SCHEMA awips AUTHORIZATION awips;

COMMENT ON ROLE awips IS 'Primary AWIPS user';
COMMENT ON DATABASE metadata IS 'AWIPS Metadata Database';
COMMENT ON TABLESPACE metadata IS 'AWIPS Metadata Database Tablespace';
COMMENT ON SCHEMA awips IS 'AWIPS Schema';

CREATE TABLESPACE pgdata_ihfs OWNER awips LOCATION '%{database_files_home}/pgdata_ihfs';
COMMENT ON TABLESPACE pgdata_ihfs IS 'IHFS Database tablespace';
