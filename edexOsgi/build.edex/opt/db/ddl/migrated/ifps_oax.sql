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
drop database ifps;

create database ifps with encoding='SQL_ASCII';

\c ifps;
create user postgres superuser;
create user pguser nosuperuser;

SET client_encoding = 'SQL_ASCII';
SET check_function_bodies = false;

SET SESSION AUTHORIZATION 'postgres';

--
-- TOC entry 4 (OID 2200)
-- Name: public; Type: ACL; Schema: -; Owner: postgres
--

REVOKE ALL ON SCHEMA public FROM PUBLIC;
GRANT ALL ON SCHEMA public TO PUBLIC;


SET SESSION AUTHORIZATION 'pguser';

SET search_path = public, pg_catalog;

--
-- TOC entry 5 (OID 1701748)
-- Name: geography_directry; Type: TABLE; Schema: public; Owner: pguser
--

CREATE TABLE geography_directry (
    geo_id character varying(10) NOT NULL,
    icao_id character varying(10),
    zcs_id character varying(1) NOT NULL,
    zncode character varying(5),
    name character varying(255),
    abbrev character varying(20),
    geo_region character varying(80),
    timezone character varying(15),
    elevation smallint,
    sheltered character varying(1),
    pcid character varying(20),
    pzid character varying(20),
    pfzid character varying(20),
    pmzid character varying(20),
    lat_cntr real,
    lon_cntr real,
    snowlev_upper smallint DEFAULT 0 NOT NULL,
    snowlev_lower smallint DEFAULT 0 NOT NULL
);


--
-- TOC entry 6 (OID 1701748)
-- Name: geography_directry; Type: ACL; Schema: public; Owner: pguser
--

REVOKE ALL ON TABLE geography_directry FROM PUBLIC;
GRANT ALL ON TABLE geography_directry TO PUBLIC;


SET SESSION AUTHORIZATION 'pguser';

--
-- TOC entry 7 (OID 1701754)
-- Name: county_directory; Type: VIEW; Schema: public; Owner: pguser
--

CREATE VIEW county_directory AS
    SELECT x0.geo_id, x0.zncode, x0.name, x0.abbrev, x0.geo_region, x0.timezone, x0.lat_cntr, x0.lon_cntr FROM geography_directry x0 WHERE ((x0.zcs_id)::text = 'C'::text);


--
-- TOC entry 8 (OID 1701754)
-- Name: county_directory; Type: ACL; Schema: public; Owner: pguser
--

REVOKE ALL ON TABLE county_directory FROM PUBLIC;
GRANT ALL ON TABLE county_directory TO PUBLIC;


SET SESSION AUTHORIZATION 'pguser';

--
-- TOC entry 9 (OID 1701755)
-- Name: forecasters; Type: TABLE; Schema: public; Owner: pguser
--

CREATE TABLE forecasters (
    number smallint NOT NULL,
    name character varying(41) NOT NULL
);


--
-- TOC entry 10 (OID 1701755)
-- Name: forecasters; Type: ACL; Schema: public; Owner: pguser
--

REVOKE ALL ON TABLE forecasters FROM PUBLIC;
GRANT ALL ON TABLE forecasters TO PUBLIC;


SET SESSION AUTHORIZATION 'pguser';

--
-- TOC entry 11 (OID 1701759)
-- Name: fwx_directory; Type: VIEW; Schema: public; Owner: pguser
--

CREATE VIEW fwx_directory AS
    SELECT x0.geo_id, x0.zncode, x0.name, x0.abbrev, x0.geo_region, x0.timezone, x0.elevation, x0.lat_cntr, x0.lon_cntr FROM geography_directry x0 WHERE ((x0.zcs_id)::text = 'F'::text);


--
-- TOC entry 12 (OID 1701762)
-- Name: zone_directory; Type: VIEW; Schema: public; Owner: pguser
--

CREATE VIEW zone_directory AS
    SELECT x0.geo_id, x0.zncode, x0.name, x0.abbrev, x0.geo_region, x0.timezone, x0.lat_cntr, x0.lon_cntr, x0.snowlev_upper, x0.snowlev_lower FROM geography_directry x0 WHERE ((x0.zcs_id)::text = 'Z'::text);


--
-- TOC entry 13 (OID 1701762)
-- Name: zone_directory; Type: ACL; Schema: public; Owner: pguser
--

REVOKE ALL ON TABLE zone_directory FROM PUBLIC;
GRANT ALL ON TABLE zone_directory TO PUBLIC;


SET SESSION AUTHORIZATION 'pguser';

--
-- TOC entry 14 (OID 1701763)
-- Name: geo_groups_descr; Type: TABLE; Schema: public; Owner: pguser
--

CREATE TABLE geo_groups_descr (
    geo_list character varying(20) NOT NULL,
    zcs_id character varying(1),
    geo_descr character varying(80)
);


--
-- TOC entry 15 (OID 1701763)
-- Name: geo_groups_descr; Type: ACL; Schema: public; Owner: pguser
--

REVOKE ALL ON TABLE geo_groups_descr FROM PUBLIC;
GRANT ALL ON TABLE geo_groups_descr TO PUBLIC;


SET SESSION AUTHORIZATION 'pguser';

--
-- TOC entry 16 (OID 1701765)
-- Name: geography_groups; Type: TABLE; Schema: public; Owner: pguser
--

CREATE TABLE geography_groups (
    geo_list character varying(20),
    wfo character varying(10),
    geo_id character varying(10),
    orderby smallint
);


--
-- TOC entry 17 (OID 1701765)
-- Name: geography_groups; Type: ACL; Schema: public; Owner: pguser
--

REVOKE ALL ON TABLE geography_groups FROM PUBLIC;
GRANT ALL ON TABLE geography_groups TO PUBLIC;


SET SESSION AUTHORIZATION 'pguser';

--
-- TOC entry 18 (OID 1701767)
-- Name: intro_phrases; Type: TABLE; Schema: public; Owner: pguser
--

CREATE TABLE intro_phrases (
    "type" character varying(1) NOT NULL,
    product character varying(10) NOT NULL,
    phrase character varying(160),
    format character varying(10) DEFAULT 'geo'::character varying NOT NULL
);


--
-- TOC entry 19 (OID 1701767)
-- Name: intro_phrases; Type: ACL; Schema: public; Owner: pguser
--

REVOKE ALL ON TABLE intro_phrases FROM PUBLIC;
GRANT ALL ON TABLE intro_phrases TO PUBLIC;


SET SESSION AUTHORIZATION 'pguser';

--
-- TOC entry 20 (OID 1701770)
-- Name: lookup; Type: TABLE; Schema: public; Owner: pguser
--

CREATE TABLE lookup (
    search character varying(60) NOT NULL,
    "replace" character varying(60) NOT NULL,
    "before" character varying(60) NOT NULL,
    "after" character varying(60) NOT NULL
);


--
-- TOC entry 21 (OID 1701770)
-- Name: lookup; Type: ACL; Schema: public; Owner: pguser
--

REVOKE ALL ON TABLE lookup FROM PUBLIC;
GRANT ALL ON TABLE lookup TO PUBLIC;


SET SESSION AUTHORIZATION 'pguser';

--
-- TOC entry 22 (OID 1701774)
-- Name: marine_stn_drctry; Type: VIEW; Schema: public; Owner: pguser
--

CREATE VIEW marine_stn_drctry AS
    SELECT x0.geo_id, x0.name, x0.geo_region, x0.timezone, x0.elevation, x0.pmzid, x0.lat_cntr, x0.lon_cntr FROM geography_directry x0 WHERE (((x0.zcs_id)::text = 'S'::text) AND (x0.pmzid IS NOT NULL));


--
-- TOC entry 23 (OID 1701774)
-- Name: marine_stn_drctry; Type: ACL; Schema: public; Owner: pguser
--

REVOKE ALL ON TABLE marine_stn_drctry FROM PUBLIC;
GRANT ALL ON TABLE marine_stn_drctry TO PUBLIC;


SET SESSION AUTHORIZATION 'pguser';

--
-- TOC entry 24 (OID 1701777)
-- Name: marine_zone_drctry; Type: VIEW; Schema: public; Owner: pguser
--

CREATE VIEW marine_zone_drctry AS
    SELECT x0.geo_id, x0.zncode, x0.name, x0.geo_region, x0.timezone, x0.sheltered, x0.lat_cntr, x0.lon_cntr FROM geography_directry x0 WHERE ((x0.zcs_id)::text = 'M'::text);


--
-- TOC entry 25 (OID 1701777)
-- Name: marine_zone_drctry; Type: ACL; Schema: public; Owner: pguser
--

REVOKE ALL ON TABLE marine_zone_drctry FROM PUBLIC;
GRANT ALL ON TABLE marine_zone_drctry TO PUBLIC;


SET SESSION AUTHORIZATION 'pguser';

--
-- TOC entry 26 (OID 1701778)
-- Name: nwr_products; Type: TABLE; Schema: public; Owner: pguser
--

CREATE TABLE nwr_products (
    pil character varying(9),
    fmt character varying(5),
    eff character varying(6),
    per character varying(8),
    act character varying(2),
    del character varying(2),
    con character varying(2),
    "int" character varying(2),
    ton character varying(2),
    ugc character varying(255),
    exp character varying(6),
    stt character varying(4),
    ett character varying(4),
    cre character varying(10),
    mrd character varying(95)
);


--
-- TOC entry 27 (OID 1701778)
-- Name: nwr_products; Type: ACL; Schema: public; Owner: pguser
--

REVOKE ALL ON TABLE nwr_products FROM PUBLIC;
GRANT ALL ON TABLE nwr_products TO PUBLIC;


SET SESSION AUTHORIZATION 'pguser';

--
-- TOC entry 28 (OID 1701780)
-- Name: plot_points; Type: TABLE; Schema: public; Owner: pguser
--

CREATE TABLE plot_points (
    geo_id character varying(10) NOT NULL,
    orderby smallint NOT NULL,
    lat real,
    lon real,
    avn_lat real,
    avn_lon real
);


--
-- TOC entry 29 (OID 1701780)
-- Name: plot_points; Type: ACL; Schema: public; Owner: pguser
--

REVOKE ALL ON TABLE plot_points FROM PUBLIC;
GRANT ALL ON TABLE plot_points TO PUBLIC;


SET SESSION AUTHORIZATION 'pguser';

--
-- TOC entry 30 (OID 1701787)
-- Name: tower_directory; Type: VIEW; Schema: public; Owner: pguser
--

CREATE VIEW tower_directory AS
    SELECT x0.geo_id, x0.zncode, x0.name, x0.timezone, x0.elevation, x0.pzid, x0.lat_cntr, x0.lon_cntr FROM geography_directry x0 WHERE ((x0.zcs_id)::text = 'T'::text);


--
-- TOC entry 31 (OID 1701787)
-- Name: tower_directory; Type: ACL; Schema: public; Owner: pguser
--

REVOKE ALL ON TABLE tower_directory FROM PUBLIC;
GRANT ALL ON TABLE tower_directory TO PUBLIC;


SET SESSION AUTHORIZATION 'pguser';

--
-- TOC entry 32 (OID 1701793)
-- Name: mexwx_data; Type: TABLE; Schema: public; Owner: pguser
--

CREATE TABLE mexwx_data (
    model_name character(6),
    netcdf_in character varying(50),
    netcdf_out character varying(50),
    mexwx_els character varying(10),
    cube_order smallint,
    convscale_type smallint,
    s2g_maptype smallint,
    grid_name character varying(50),
    output_res smallint,
    disc_expct smallint,
    vmin double precision,
    vmax double precision
);


--
-- TOC entry 33 (OID 1701795)
-- Name: cntrl_const; Type: TABLE; Schema: public; Owner: pguser
--

CREATE TABLE cntrl_const (
    prod_id character(20),
    item character(20),
    member smallint,
    use character(1),
    "cycle" smallint,
    fore_adj character(1),
    value character(20),
    wfo character(4)
);


--
-- TOC entry 34 (OID 13867587)
-- Name: const_descr; Type: TABLE; Schema: public; Owner: pguser
--

CREATE TABLE const_descr (
    item character varying(21) NOT NULL,
    member smallint NOT NULL,
    description character varying(255),
    valid_values character varying(100)
);


--
-- TOC entry 35 (OID 13867587)
-- Name: const_descr; Type: ACL; Schema: public; Owner: pguser
--

REVOKE ALL ON TABLE const_descr FROM PUBLIC;
GRANT ALL ON TABLE const_descr TO PUBLIC;


SET SESSION AUTHORIZATION 'pguser';

--
-- TOC entry 36 (OID 13867591)
-- Name: grid2parms; Type: TABLE; Schema: public; Owner: pguser
--

CREATE TABLE grid2parms (
    grid_name character varying(20) NOT NULL,
    grid_type character varying(1) NOT NULL,
    slider_element character varying(1) NOT NULL,
    parm_name character varying(20) NOT NULL,
    factor real DEFAULT 1.00000000 NOT NULL,
    protected character varying(1) DEFAULT 'y'::character varying NOT NULL
);


--
-- TOC entry 37 (OID 13867591)
-- Name: grid2parms; Type: ACL; Schema: public; Owner: pguser
--

REVOKE ALL ON TABLE grid2parms FROM PUBLIC;
GRANT SELECT ON TABLE grid2parms TO PUBLIC;


SET SESSION AUTHORIZATION 'pguser';

--
-- TOC entry 38 (OID 13867598)
-- Name: grid_attributes; Type: TABLE; Schema: public; Owner: pguser
--

CREATE TABLE grid_attributes (
    element character varying(10) NOT NULL,
    grid_name character varying(20) NOT NULL,
    full_name character varying(30),
    rowspace smallint
);


--
-- TOC entry 39 (OID 13867598)
-- Name: grid_attributes; Type: ACL; Schema: public; Owner: pguser
--

REVOKE ALL ON TABLE grid_attributes FROM PUBLIC;
GRANT SELECT ON TABLE grid_attributes TO PUBLIC;


SET SESSION AUTHORIZATION 'pguser';

--
-- TOC entry 40 (OID 13867603)
-- Name: loose_cntrl_const; Type: VIEW; Schema: public; Owner: pguser
--

CREATE VIEW loose_cntrl_const AS
    SELECT x0.prod_id, x0.item, x0.member, x0.use, x0."cycle", x0.fore_adj, x0.value, x0.wfo FROM cntrl_const x0 WHERE ((((((((((((((((x0.item <> 'element_name'::bpchar) AND (x0.item <> 'element_selected'::bpchar)) AND (x0.item <> 'element_start_time'::bpchar)) AND (x0.item <> 'element_stop_time'::bpchar)) AND (x0.item <> 'element_time_step'::bpchar)) AND (x0.item <> 'element_min_time'::bpchar)) AND (x0.item <> 'element_max_time'::bpchar)) AND (x0.item <> 'precip_detail'::bpchar)) AND (x0.item <> 'precip_style'::bpchar)) AND (x0.item <> 'snow_detail'::bpchar)) AND (x0.item <> 'cloud_detail'::bpchar)) AND (x0.item <> 'temp_detail'::bpchar)) AND (x0.item <> 'wind_detail'::bpchar)) AND (x0.item <> 'wind_gust'::bpchar)) AND (x0.item <> 'wave_detail'::bpchar)) AND (x0.item <> 'vsby_detail'::bpchar));


--
-- TOC entry 41 (OID 13867603)
-- Name: loose_cntrl_const; Type: ACL; Schema: public; Owner: pguser
--

REVOKE ALL ON TABLE loose_cntrl_const FROM PUBLIC;
GRANT ALL ON TABLE loose_cntrl_const TO PUBLIC;


SET SESSION AUTHORIZATION 'pguser';

--
-- TOC entry 42 (OID 13868018)
-- Name: station_directory; Type: VIEW; Schema: public; Owner: pguser
--

CREATE VIEW station_directory AS
    SELECT x0.geo_id, x0.icao_id, x0.name, x0.geo_region, x0.timezone, x0.elevation, x0.pcid, x0.pzid, x0.pfzid, x0.pmzid, x0.lat_cntr, x0.lon_cntr FROM geography_directry x0 WHERE ((x0.zcs_id)::text = 'S'::text);


--
-- TOC entry 43 (OID 13868019)
-- Name: maps; Type: TABLE; Schema: public; Owner: pguser
--

CREATE TABLE maps (
    map_name character varying(20) NOT NULL,
    geo_list character varying(20),
    grid_factor real NOT NULL,
    map_length integer NOT NULL,
    map_data oid
);


--
-- TOC entry 44 (OID 13867589)
-- Name: cd1index; Type: INDEX; Schema: public; Owner: pguser
--

CREATE INDEX cd1index ON const_descr USING btree (item);


--
-- TOC entry 45 (OID 13867590)
-- Name: cd2index; Type: INDEX; Schema: public; Owner: pguser
--

CREATE UNIQUE INDEX cd2index ON const_descr USING btree (item, member);


--
-- TOC entry 47 (OID 13867595)
-- Name: u103_11; Type: INDEX; Schema: public; Owner: pguser
--

CREATE UNIQUE INDEX u103_11 ON grid2parms USING btree (grid_name, parm_name);


--
-- TOC entry 48 (OID 13867600)
-- Name: u_grid_attributes; Type: INDEX; Schema: public; Owner: pguser
--

CREATE UNIQUE INDEX u_grid_attributes ON grid_attributes USING btree (element);


--
-- TOC entry 46 (OID 13867596)
-- Name: grid2parms_grid_name_key; Type: CONSTRAINT; Schema: public; Owner: pguser
--

ALTER TABLE ONLY grid2parms
    ADD CONSTRAINT grid2parms_grid_name_key UNIQUE (grid_name, parm_name);


SET SESSION AUTHORIZATION 'postgres';

--
-- TOC entry 3 (OID 2200)
-- Name: SCHEMA public; Type: COMMENT; Schema: -; Owner: postgres
--

COMMENT ON SCHEMA public IS 'Standard public schema';


