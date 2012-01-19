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
-- drop database dc_ob7oax;

-- create database dc_ob7oax with encoding='SQL_ASCII';

\c dc_ob7oax;
-- create user postgres superuser;
-- create user pguser nosuperuser;

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
-- TOC entry 5 (OID 16939)
-- Name: dammaster; Type: TABLE; Schema: public; Owner: pguser
--

CREATE TABLE dammaster (
    nidid character varying(10) NOT NULL,
    dam_name character varying(65),
    county character varying(30),
    river character varying(30),
    downstream_hazard character varying(11),
    max_storage double precision,
    hsa character varying(3),
    rfc character varying(5),
    latitude_dam double precision,
    longitude_dam double precision
);


--
-- TOC entry 6 (OID 16939)
-- Name: dammaster; Type: ACL; Schema: public; Owner: pguser
--

REVOKE ALL ON TABLE dammaster FROM PUBLIC;
GRANT INSERT,SELECT,UPDATE,DELETE ON TABLE dammaster TO PUBLIC;


SET SESSION AUTHORIZATION 'pguser';

--
-- TOC entry 7 (OID 16941)
-- Name: damfeatures; Type: TABLE; Schema: public; Owner: pguser
--

CREATE TABLE damfeatures (
    nidid character varying(10) NOT NULL,
    other_dam_name character varying(65),
    dam_former_name character varying(50),
    stateid character varying(20),
    section_t_r character varying(30),
    owner_name character varying(50),
    owner_type character varying(14),
    dam_designer character varying(65),
    private_on_federal character varying(6),
    dam_type character varying(6),
    core character varying(6),
    foundation character varying(6),
    purposes character varying(8),
    year_completed character varying(20),
    year_modified character varying(20),
    emerg_action_plan character varying(3),
    inspection_date character varying(20),
    inspection_freq character varying(20),
    st_reg_dam character varying(6),
    st_reg_agency character varying(30),
    spillway_type character varying(6),
    spillway_width double precision,
    outlet_gates character varying(6),
    volume_dam double precision,
    number_locks double precision,
    length_locks double precision,
    width_locks double precision,
    fed_funding character varying(20),
    fed_design character varying(20),
    fed_construction character varying(20),
    fed_regulatory character varying(20),
    fed_inspection character varying(20),
    fed_operation character varying(20),
    fed_other character varying(20),
    fed_owner character varying(20),
    source_agency character varying(60),
    drainage_area double precision,
    topo_map character varying(22),
    return_flow_region integer,
    dam_length double precision,
    dam_height double precision,
    structural_height double precision,
    hydraulic_height double precision,
    nid_height double precision,
    max_discharge double precision,
    normal_storage double precision,
    nid_storage double precision,
    surface_area double precision,
    elev double precision,
    prebreak_avail character varying(1),
    comments character varying(30),
    updated timestamp without time zone
);


--
-- TOC entry 8 (OID 16941)
-- Name: damfeatures; Type: ACL; Schema: public; Owner: pguser
--

REVOKE ALL ON TABLE damfeatures FROM PUBLIC;
GRANT INSERT,SELECT,UPDATE,DELETE ON TABLE damfeatures TO PUBLIC;


SET SESSION AUTHORIZATION 'pguser';

--
-- TOC entry 9 (OID 16943)
-- Name: damreservoir; Type: TABLE; Schema: public; Owner: pguser
--

CREATE TABLE damreservoir (
    nidid character varying(10) NOT NULL,
    "type" character varying(1) NOT NULL,
    elevation double precision NOT NULL,
    stordis double precision,
    surface double precision,
    updated timestamp without time zone
);


--
-- TOC entry 10 (OID 16943)
-- Name: damreservoir; Type: ACL; Schema: public; Owner: pguser
--

REVOKE ALL ON TABLE damreservoir FROM PUBLIC;
GRANT INSERT,SELECT,UPDATE,DELETE ON TABLE damreservoir TO PUBLIC;


SET SESSION AUTHORIZATION 'pguser';

--
-- TOC entry 11 (OID 16945)
-- Name: downstream; Type: TABLE; Schema: public; Owner: pguser
--

CREATE TABLE downstream (
    nidid character varying(10) NOT NULL,
    down_name character varying(25) NOT NULL,
    longitude double precision,
    latitude double precision,
    elevation double precision,
    distance_from_dam double precision,
    flood_flow double precision,
    flood_depth double precision,
    flood_width double precision,
    mann_oc double precision,
    comments character varying(30),
    xsec_best_type character varying(2),
    updated timestamp without time zone
);


--
-- TOC entry 12 (OID 16945)
-- Name: downstream; Type: ACL; Schema: public; Owner: pguser
--

REVOKE ALL ON TABLE downstream FROM PUBLIC;
GRANT INSERT,SELECT,UPDATE,DELETE ON TABLE downstream TO PUBLIC;


SET SESSION AUTHORIZATION 'pguser';

--
-- TOC entry 13 (OID 16947)
-- Name: sdbin; Type: TABLE; Schema: public; Owner: pguser
--

CREATE TABLE sdbin (
    nidid character varying(10) NOT NULL,
    src character varying(3) NOT NULL,
    scenario character varying(2) NOT NULL,
    hde double precision,
    bme double precision,
    vol double precision,
    sa double precision,
    tfm double precision,
    qo double precision,
    bw double precision,
    idam integer,
    comments character varying(30),
    updated timestamp without time zone
);


--
-- TOC entry 14 (OID 16947)
-- Name: sdbin; Type: ACL; Schema: public; Owner: pguser
--

REVOKE ALL ON TABLE sdbin FROM PUBLIC;
GRANT INSERT,SELECT,UPDATE,DELETE ON TABLE sdbin TO PUBLIC;


SET SESSION AUTHORIZATION 'pguser';

--
-- TOC entry 15 (OID 16949)
-- Name: sdbout; Type: TABLE; Schema: public; Owner: pguser
--

CREATE TABLE sdbout (
    nidid character varying(10) NOT NULL,
    src character varying(3) NOT NULL,
    scenario character varying(2) NOT NULL,
    down_name character varying(25) NOT NULL,
    slope double precision,
    max_flow double precision,
    max_depth double precision,
    time_max_depth double precision,
    time_flood double precision,
    time_deflood double precision,
    comments character varying(30),
    updated timestamp without time zone
);


--
-- TOC entry 16 (OID 16949)
-- Name: sdbout; Type: ACL; Schema: public; Owner: pguser
--

REVOKE ALL ON TABLE sdbout FROM PUBLIC;
GRANT INSERT,SELECT,UPDATE,DELETE ON TABLE sdbout TO PUBLIC;


SET SESSION AUTHORIZATION 'pguser';

--
-- TOC entry 17 (OID 16951)
-- Name: sectionpair; Type: TABLE; Schema: public; Owner: pguser
--

CREATE TABLE sectionpair (
    nidid character varying(10) NOT NULL,
    down_name character varying(25) NOT NULL,
    pair_num integer NOT NULL,
    xsec_type character varying(2) NOT NULL,
    elev double precision,
    tw double precision,
    mann_n double precision,
    inactive_width double precision,
    updated timestamp without time zone
);


--
-- TOC entry 18 (OID 16951)
-- Name: sectionpair; Type: ACL; Schema: public; Owner: pguser
--

REVOKE ALL ON TABLE sectionpair FROM PUBLIC;
GRANT INSERT,SELECT,UPDATE,DELETE ON TABLE sectionpair TO PUBLIC;


SET SESSION AUTHORIZATION 'pguser';

--
-- TOC entry 22 (OID 81049)
-- Name: river_idx; Type: INDEX; Schema: public; Owner: pguser
--

CREATE INDEX river_idx ON dammaster USING btree (river);


--
-- TOC entry 19 (OID 81050)
-- Name: county_idx; Type: INDEX; Schema: public; Owner: pguser
--

CREATE INDEX county_idx ON dammaster USING btree (county);


--
-- TOC entry 20 (OID 81051)
-- Name: dam_name_idx; Type: INDEX; Schema: public; Owner: pguser
--

CREATE INDEX dam_name_idx ON dammaster USING btree (dam_name);


--
-- TOC entry 21 (OID 81052)
-- Name: dammasterpk; Type: CONSTRAINT; Schema: public; Owner: pguser
--

ALTER TABLE ONLY dammaster
    ADD CONSTRAINT dammasterpk PRIMARY KEY (nidid);


--
-- TOC entry 23 (OID 81054)
-- Name: damfeaturespk; Type: CONSTRAINT; Schema: public; Owner: pguser
--

ALTER TABLE ONLY damfeatures
    ADD CONSTRAINT damfeaturespk PRIMARY KEY (nidid);


--
-- TOC entry 24 (OID 81056)
-- Name: damreservoirpk; Type: CONSTRAINT; Schema: public; Owner: pguser
--

ALTER TABLE ONLY damreservoir
    ADD CONSTRAINT damreservoirpk PRIMARY KEY (nidid);


--
-- TOC entry 25 (OID 81058)
-- Name: downstreampk; Type: CONSTRAINT; Schema: public; Owner: pguser
--

ALTER TABLE ONLY downstream
    ADD CONSTRAINT downstreampk PRIMARY KEY (nidid, down_name);


--
-- TOC entry 26 (OID 81060)
-- Name: sdbinpk; Type: CONSTRAINT; Schema: public; Owner: pguser
--

ALTER TABLE ONLY sdbin
    ADD CONSTRAINT sdbinpk PRIMARY KEY (nidid, src, scenario);


--
-- TOC entry 27 (OID 81062)
-- Name: location_pk; Type: CONSTRAINT; Schema: public; Owner: pguser
--

ALTER TABLE ONLY sdbout
    ADD CONSTRAINT location_pk PRIMARY KEY (nidid, src, scenario, down_name);


--
-- TOC entry 28 (OID 81064)
-- Name: sectionpairpk; Type: CONSTRAINT; Schema: public; Owner: pguser
--

ALTER TABLE ONLY sectionpair
    ADD CONSTRAINT sectionpairpk PRIMARY KEY (nidid, down_name, pair_num, xsec_type);


--
-- TOC entry 36 (OID 81066)
-- Name: featuresmasterfk; Type: FK CONSTRAINT; Schema: public; Owner: pguser
--

ALTER TABLE ONLY damfeatures
    ADD CONSTRAINT featuresmasterfk FOREIGN KEY (nidid) REFERENCES dammaster(nidid) MATCH FULL;


--
-- TOC entry 37 (OID 81070)
-- Name: reservoirmasterfk; Type: FK CONSTRAINT; Schema: public; Owner: pguser
--

ALTER TABLE ONLY damreservoir
    ADD CONSTRAINT reservoirmasterfk FOREIGN KEY (nidid) REFERENCES dammaster(nidid) MATCH FULL;


--
-- TOC entry 38 (OID 81074)
-- Name: downmasterfk; Type: FK CONSTRAINT; Schema: public; Owner: pguser
--

ALTER TABLE ONLY downstream
    ADD CONSTRAINT downmasterfk FOREIGN KEY (nidid) REFERENCES dammaster(nidid) MATCH FULL;


--
-- TOC entry 39 (OID 81078)
-- Name: inmasterfk; Type: FK CONSTRAINT; Schema: public; Owner: pguser
--

ALTER TABLE ONLY sdbin
    ADD CONSTRAINT inmasterfk FOREIGN KEY (nidid) REFERENCES dammaster(nidid) MATCH FULL;


--
-- TOC entry 40 (OID 81082)
-- Name: outdownfk; Type: FK CONSTRAINT; Schema: public; Owner: pguser
--

ALTER TABLE ONLY sdbout
    ADD CONSTRAINT outdownfk FOREIGN KEY (nidid, down_name) REFERENCES downstream(nidid, down_name) MATCH FULL;


--
-- TOC entry 41 (OID 81086)
-- Name: outinfk; Type: FK CONSTRAINT; Schema: public; Owner: pguser
--

ALTER TABLE ONLY sdbout
    ADD CONSTRAINT outinfk FOREIGN KEY (nidid, src, scenario) REFERENCES sdbin(nidid, src, scenario) MATCH FULL;


--
-- TOC entry 42 (OID 81090)
-- Name: pairdownfk; Type: FK CONSTRAINT; Schema: public; Owner: pguser
--

ALTER TABLE ONLY sectionpair
    ADD CONSTRAINT pairdownfk FOREIGN KEY (nidid, down_name) REFERENCES downstream(nidid, down_name) MATCH FULL;


SET SESSION AUTHORIZATION 'postgres';

--
-- TOC entry 3 (OID 2200)
-- Name: SCHEMA public; Type: COMMENT; Schema: -; Owner: postgres
--

COMMENT ON SCHEMA public IS 'Standard public schema';


