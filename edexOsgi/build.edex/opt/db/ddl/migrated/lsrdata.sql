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
drop database lsrdata;

create database lsrdata with encoding='SQL_ASCII';

\c lsrdata;
create user postgres superuser;
create user pguser nosuperuser;

--
-- PostgreSQL database dump
--

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
-- TOC entry 5 (OID 1708004)
-- Name: event_report; Type: TABLE; Schema: public; Owner: pguser
--

CREATE TABLE event_report (
    event_rpt_id_ serial NOT NULL,
    event_datetime_ timestamp without time zone NOT NULL,
    update_datetime_ timestamp without time zone NOT NULL,
    spotter_id_ character varying(15) NOT NULL,
    observer_type_ character varying(30) NOT NULL,
    city_ character varying(25) NOT NULL,
    distance_ smallint NOT NULL,
    direction_ character varying(5) NOT NULL,
    county_ character varying(28) NOT NULL,
    state_id_ character(2) NOT NULL,
    latitude_ double precision NOT NULL,
    longitude_ double precision NOT NULL,
    event_type_ character varying(23) NOT NULL,
    severity_ double precision NOT NULL,
    units_ character(4) NOT NULL,
    accuracy_ character(1) NOT NULL,
    fatalities_ smallint NOT NULL,
    injuries_ smallint NOT NULL,
    comment_text_ text,
    received_by_ character(20) NOT NULL,
    practice_ character(1) NOT NULL,
    transmitted_ character(1) NOT NULL,
    edit_after_trans_ character(1) NOT NULL,
    event_idx_ integer NOT NULL
);


--
-- TOC entry 6 (OID 1708004)
-- Name: event_report; Type: ACL; Schema: public; Owner: pguser
--

REVOKE ALL ON TABLE event_report FROM PUBLIC;
GRANT ALL ON TABLE event_report TO PUBLIC;


SET SESSION AUTHORIZATION 'pguser';

--
-- TOC entry 7 (OID 1708012)
-- Name: ls_report; Type: TABLE; Schema: public; Owner: pguser
--

CREATE TABLE ls_report (
    lsr_id_ serial NOT NULL,
    report_time_ timestamp without time zone NOT NULL,
    practice_ character(1) NOT NULL,
    transmitted_ character(1) NOT NULL,
    event_count_ smallint NOT NULL,
    free_text_ text
);


--
-- TOC entry 8 (OID 1708012)
-- Name: ls_report; Type: ACL; Schema: public; Owner: pguser
--

REVOKE ALL ON TABLE ls_report FROM PUBLIC;
GRANT ALL ON TABLE ls_report TO PUBLIC;


SET SESSION AUTHORIZATION 'pguser';

--
-- TOC entry 9 (OID 1708018)
-- Name: lsr_to_event; Type: TABLE; Schema: public; Owner: pguser
--

CREATE TABLE lsr_to_event (
    lsr_id_ integer NOT NULL,
    event_rpt_id_ integer NOT NULL
);


--
-- TOC entry 10 (OID 1708018)
-- Name: lsr_to_event; Type: ACL; Schema: public; Owner: pguser
--

REVOKE ALL ON TABLE lsr_to_event FROM PUBLIC;
GRANT ALL ON TABLE lsr_to_event TO PUBLIC;


SET SESSION AUTHORIZATION 'pguser';

--
-- TOC entry 13 (OID 1709040)
-- Name: event_report_pkey; Type: CONSTRAINT; Schema: public; Owner: pguser
--

ALTER TABLE ONLY event_report
    ADD CONSTRAINT event_report_pkey PRIMARY KEY (event_rpt_id_);


--
-- TOC entry 14 (OID 1709042)
-- Name: ls_report_pkey; Type: CONSTRAINT; Schema: public; Owner: pguser
--

ALTER TABLE ONLY ls_report
    ADD CONSTRAINT ls_report_pkey PRIMARY KEY (lsr_id_);


--
-- TOC entry 15 (OID 1709044)
-- Name: lsr_to_event_pkey; Type: CONSTRAINT; Schema: public; Owner: pguser
--

ALTER TABLE ONLY lsr_to_event
    ADD CONSTRAINT lsr_to_event_pkey PRIMARY KEY (lsr_id_, event_rpt_id_);


--
-- TOC entry 19 (OID 1709046)
-- Name: $1; Type: FK CONSTRAINT; Schema: public; Owner: pguser
--

ALTER TABLE ONLY lsr_to_event
    ADD CONSTRAINT "$1" FOREIGN KEY (lsr_id_) REFERENCES ls_report(lsr_id_);


--
-- TOC entry 20 (OID 1709050)
-- Name: $2; Type: FK CONSTRAINT; Schema: public; Owner: pguser
--

ALTER TABLE ONLY lsr_to_event
    ADD CONSTRAINT "$2" FOREIGN KEY (event_rpt_id_) REFERENCES event_report(event_rpt_id_);


--
-- TOC entry 11 (OID 1708002)
-- Name: event_report_event_rpt_id__seq; Type: SEQUENCE SET; Schema: public; Owner: pguser
--

SELECT pg_catalog.setval('event_report_event_rpt_id__seq', 3072, true);


--
-- TOC entry 12 (OID 1708010)
-- Name: ls_report_lsr_id__seq; Type: SEQUENCE SET; Schema: public; Owner: pguser
--

SELECT pg_catalog.setval('ls_report_lsr_id__seq', 2317, true);


SET SESSION AUTHORIZATION 'postgres';

--
-- TOC entry 3 (OID 2200)
-- Name: SCHEMA public; Type: COMMENT; Schema: -; Owner: postgres
--

COMMENT ON SCHEMA public IS 'Standard public schema';


