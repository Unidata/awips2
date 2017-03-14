--
-- PostgreSQL database dump 
--
--
-- Name: SCHEMA public; Type: COMMENT; Schema: -; Owner: postgres
--
--------------------------------------------------------
--create config.clo tables
DROP TABLE IF EXISTS config.clo CASCADE;
CREATE TABLE config.clo (
	pkey SERIAL PRIMARY KEY,
	alias_name varchar(20),
	file_type int,
	table_name varchar(32)
);
