/**
 * This software was developed and / or modified by NOAA/NWS/OCP/ASDT
 *
 * Create climate tablespace and database 
 * 
 */
\set ON_ERROR_STOP 1
DROP DATABASE IF EXISTS climate;
DROP TABLESPACE IF EXISTS climate;
CREATE TABLESPACE climate OWNER awipsadmin LOCATION '%{tablespace_dir}%/climate';
COMMENT ON TABLESPACE climate IS 'Climate Database tablespace';
CREATE DATABASE climate OWNER awipsadmin TABLESPACE climate;
