--
-- PostgreSQL database dump 
--
--
-- Name: SCHEMA public; Type: COMMENT; Schema: -; Owner: postgres
--
-------------------------------------------------
-- create ncgrib tables
-- ---------------------------------------------
-- Create ncgrib_vcrdgrib1 table
DROP TABLE IF EXISTS ncgrib.vcrdgrib1 CASCADE;
CREATE TABLE ncgrib.vcrdgrib1(
pkey  SERIAL PRIMARY KEY,
        id varchar(8)      NOT NULL,
        name varchar(48) NOT NULL,
        units varchar(16),
        GNAM varchar(10) NOT NULL,
        scale smallint
);

