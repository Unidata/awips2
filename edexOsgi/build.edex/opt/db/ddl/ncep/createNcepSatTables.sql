--
-- PostgreSQL database dump 
--
--
-- Name: SCHEMA public; Type: COMMENT; Schema: -; Owner: postgres
--

--------------------------------------------------------
-- create sat.channel tables
DROP TABLE IF EXISTS sat.channel CASCADE;
CREATE TABLE sat.channel (
	PKEY SERIAL PRIMARY KEY,
        CHANNEL_STRINGS varchar(32),
        NUM int
);

-- create sat.channel tables
DROP TABLE IF EXISTS sat.imgcoeffs CASCADE;
CREATE TABLE sat.imgcoeffs (
	pkey SERIAL PRIMARY KEY,
	satellite_id varchar(8),
	satellite_num smallint,
	channel smallint,
	det smallint,
	scal_coef_m double precision,
	scal_coef_b double precision,
	side int,
	conv_coef_n double precision,
	conv_coef_a double precision,
	conv_coef_b double precision,
	conv_coef_g double precision
);
