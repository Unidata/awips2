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
\connect hd_ob83krf

SET search_path = public, pg_catalog;

SET default_tablespace = '';

SET default_with_oids = false;

--
-- TOC entry 1788 (class 1259 OID 18900)
-- Dependencies: 6
-- Name: adjustfactor; Type: TABLE; Schema: public; Owner: awips; Tablespace: 
--

CREATE TABLE adjustfactor (
    lid character varying(8) NOT NULL,
    pe character varying(2) NOT NULL,
    dur smallint NOT NULL,
    ts character varying(2) NOT NULL,
    extremum character varying(1) NOT NULL,
    divisor double precision,
    base double precision,
    multiplier double precision,
    adder double precision
);


ALTER TABLE public.adjustfactor OWNER TO awips;

--
-- TOC entry 1789 (class 1259 OID 18903)
-- Dependencies: 6
-- Name: admin; Type: TABLE; Schema: public; Owner: awips; Tablespace: 
--

CREATE TABLE "admin" (
    focalpoint character varying(24),
    ofc character varying(20),
    phone character varying(12),
    region character varying(20),
    regno character varying(1),
    cd404 character varying(8),
    tenyr date,
    oneyr date,
    hsa character varying(5) NOT NULL,
    hsa_num smallint,
    hb_password character varying(8)
);


ALTER TABLE public."admin" OWNER TO awips;

--
-- TOC entry 1790 (class 1259 OID 18906)
-- Dependencies: 6
-- Name: locextagency; Type: TABLE; Schema: public; Owner: awips; Tablespace: 
--

CREATE TABLE locextagency (
    lid character varying(8) NOT NULL,
    agency_code character varying(8) NOT NULL,
    office character varying(20) NOT NULL
);


ALTER TABLE public.locextagency OWNER TO awips;

--
-- TOC entry 1791 (class 1259 OID 18909)
-- Dependencies: 2071 6
-- Name: agencyofficeunique; Type: VIEW; Schema: public; Owner: awips
--

CREATE VIEW agencyofficeunique AS
    SELECT locextagency.agency_code, locextagency.office FROM locextagency GROUP BY locextagency.agency_code, locextagency.office;


ALTER TABLE public.agencyofficeunique OWNER TO awips;

--
-- TOC entry 1792 (class 1259 OID 18913)
-- Dependencies: 6
-- Name: agricultural; Type: TABLE; Schema: public; Owner: awips; Tablespace: 
--

CREATE TABLE agricultural (
    lid character varying(8) NOT NULL,
    pe character varying(2) NOT NULL,
    dur smallint NOT NULL,
    ts character varying(2) NOT NULL,
    extremum character varying(1) NOT NULL,
    obstime timestamp without time zone NOT NULL,
    value double precision,
    shef_qual_code character varying(1),
    quality_code integer,
    revision smallint,
    product_id character varying(10),
    producttime timestamp without time zone,
    postingtime timestamp without time zone
);


ALTER TABLE public.agricultural OWNER TO awips;

--
-- TOC entry 1793 (class 1259 OID 18916)
-- Dependencies: 6
-- Name: alertalarmval; Type: TABLE; Schema: public; Owner: awips; Tablespace: 
--

CREATE TABLE alertalarmval (
    lid character varying(8) NOT NULL,
    pe character varying(2) NOT NULL,
    dur smallint NOT NULL,
    ts character varying(2) NOT NULL,
    extremum character varying(1) NOT NULL,
    probability real NOT NULL,
    validtime timestamp without time zone NOT NULL,
    basistime timestamp without time zone NOT NULL,
    value double precision,
    suppl_value double precision,
    shef_qual_code character varying(1),
    quality_code integer,
    revision smallint,
    product_id character varying(10),
    producttime timestamp without time zone,
    postingtime timestamp without time zone,
    action_time timestamp without time zone,
    aa_categ character varying(6) NOT NULL,
    aa_check character varying(6) NOT NULL
);


ALTER TABLE public.alertalarmval OWNER TO awips;

--
-- TOC entry 1794 (class 1259 OID 18919)
-- Dependencies: 6
-- Name: arealfcst; Type: TABLE; Schema: public; Owner: awips; Tablespace: 
--

CREATE TABLE arealfcst (
    lid character varying(8) NOT NULL,
    pe character varying(2) NOT NULL,
    dur smallint NOT NULL,
    ts character varying(2) NOT NULL,
    extremum character varying(1) NOT NULL,
    probability real NOT NULL,
    validtime timestamp without time zone NOT NULL,
    basistime timestamp without time zone NOT NULL,
    value double precision,
    shef_qual_code character varying(1),
    quality_code integer,
    revision smallint,
    product_id character varying(10),
    producttime timestamp without time zone,
    postingtime timestamp without time zone
);


ALTER TABLE public.arealfcst OWNER TO awips;

--
-- TOC entry 1795 (class 1259 OID 18922)
-- Dependencies: 6
-- Name: arealobs; Type: TABLE; Schema: public; Owner: awips; Tablespace: 
--

CREATE TABLE arealobs (
    lid character varying(8) NOT NULL,
    pe character varying(2) NOT NULL,
    dur smallint NOT NULL,
    ts character varying(2) NOT NULL,
    extremum character varying(1) NOT NULL,
    obstime timestamp without time zone NOT NULL,
    value double precision,
    shef_qual_code character varying(1),
    quality_code integer,
    revision smallint,
    product_id character varying(10),
    producttime timestamp without time zone,
    postingtime timestamp without time zone
);


ALTER TABLE public.arealobs OWNER TO awips;

--
-- TOC entry 1796 (class 1259 OID 18925)
-- Dependencies: 6
-- Name: benchmark; Type: TABLE; Schema: public; Owner: awips; Tablespace: 
--

CREATE TABLE benchmark (
    lid character varying(8) NOT NULL,
    bnum character varying(6) NOT NULL,
    elev double precision,
    remark character varying(255)
);


ALTER TABLE public.benchmark OWNER TO awips;

--
-- TOC entry 1797 (class 1259 OID 18928)
-- Dependencies: 6
-- Name: city; Type: TABLE; Schema: public; Owner: awips; Tablespace: 
--

CREATE TABLE city (
    name character varying(20) NOT NULL,
    state character varying(2),
    lat double precision,
    lon double precision,
    disp_precedence integer,
    population integer
);


ALTER TABLE public.city OWNER TO awips;

--
-- TOC entry 1798 (class 1259 OID 18931)
-- Dependencies: 6
-- Name: colorname; Type: TABLE; Schema: public; Owner: awips; Tablespace: 
--

CREATE TABLE colorname (
    color_name character varying(25) NOT NULL
);


ALTER TABLE public.colorname OWNER TO awips;

--
-- TOC entry 1799 (class 1259 OID 18934)
-- Dependencies: 6
-- Name: coloroverlay; Type: TABLE; Schema: public; Owner: awips; Tablespace: 
--

CREATE TABLE coloroverlay (
    userid character varying(32) NOT NULL,
    application_name character varying(20) NOT NULL,
    overlay_type character varying(20) NOT NULL,
    color_name character varying(25) NOT NULL
);


ALTER TABLE public.coloroverlay OWNER TO awips;

--
-- TOC entry 1800 (class 1259 OID 18937)
-- Dependencies: 6
-- Name: colorvalue; Type: TABLE; Schema: public; Owner: awips; Tablespace: 
--

CREATE TABLE colorvalue (
    userid character varying(32) NOT NULL,
    application_name character varying(20) NOT NULL,
    color_use_name character varying(15) NOT NULL,
    duration integer NOT NULL,
    threshold_value double precision NOT NULL,
    threshold_unit character varying(1) NOT NULL,
    color_name character varying(25) NOT NULL
);


ALTER TABLE public.colorvalue OWNER TO awips;

--
-- TOC entry 1801 (class 1259 OID 18940)
-- Dependencies: 6
-- Name: commentvalue; Type: TABLE; Schema: public; Owner: awips; Tablespace: 
--

CREATE TABLE commentvalue (
    lid character varying(8) NOT NULL,
    pe character varying(2) NOT NULL,
    dur smallint NOT NULL,
    ts character varying(2) NOT NULL,
    extremum character varying(1) NOT NULL,
    probability real NOT NULL,
    validtime timestamp without time zone NOT NULL,
    basistime timestamp without time zone NOT NULL,
    value double precision,
    shef_qual_code character varying(1),
    revision smallint,
    product_id character varying(10),
    producttime timestamp without time zone,
    postingtime timestamp without time zone,
    shef_comment character varying(80)
);


ALTER TABLE public.commentvalue OWNER TO awips;

--
-- TOC entry 1802 (class 1259 OID 18943)
-- Dependencies: 6
-- Name: contacts; Type: TABLE; Schema: public; Owner: awips; Tablespace: 
--

CREATE TABLE contacts (
    lid character varying(8) NOT NULL,
    contact character varying(28) NOT NULL,
    phone character varying(18),
    email character varying(60),
    remark character varying(255),
    priority integer
);


ALTER TABLE public.contacts OWNER TO awips;

--
-- TOC entry 1803 (class 1259 OID 18946)
-- Dependencies: 6
-- Name: contingencyvalue; Type: TABLE; Schema: public; Owner: awips; Tablespace: 
--

CREATE TABLE contingencyvalue (
    lid character varying(8) NOT NULL,
    pe character varying(2) NOT NULL,
    dur smallint NOT NULL,
    ts character varying(2) NOT NULL,
    extremum character varying(1) NOT NULL,
    probability real NOT NULL,
    validtime timestamp without time zone NOT NULL,
    basistime timestamp without time zone NOT NULL,
    value double precision,
    shef_qual_code character varying(1),
    quality_code integer,
    revision smallint,
    product_id character varying(10),
    producttime timestamp without time zone,
    postingtime timestamp without time zone
);


ALTER TABLE public.contingencyvalue OWNER TO awips;

--
-- TOC entry 1804 (class 1259 OID 18949)
-- Dependencies: 6
-- Name: coopcomms; Type: TABLE; Schema: public; Owner: awips; Tablespace: 
--

CREATE TABLE coopcomms (
    comm character varying(10) NOT NULL
);


ALTER TABLE public.coopcomms OWNER TO awips;

--
-- TOC entry 1805 (class 1259 OID 18952)
-- Dependencies: 6
-- Name: cooprecip; Type: TABLE; Schema: public; Owner: awips; Tablespace: 
--

CREATE TABLE cooprecip (
    recip character varying(15) NOT NULL
);


ALTER TABLE public.cooprecip OWNER TO awips;

--
-- TOC entry 1806 (class 1259 OID 18955)
-- Dependencies: 6
-- Name: coopspons; Type: TABLE; Schema: public; Owner: awips; Tablespace: 
--

CREATE TABLE coopspons (
    spons character varying(7) NOT NULL
);


ALTER TABLE public.coopspons OWNER TO awips;

--
-- TOC entry 1807 (class 1259 OID 18958)
-- Dependencies: 6
-- Name: counties; Type: TABLE; Schema: public; Owner: awips; Tablespace: 
--

CREATE TABLE counties (
    county character varying(20) NOT NULL,
    state character varying(2) NOT NULL,
    countynum character varying(4),
    wfo character varying(3) NOT NULL,
    primary_back character varying(3) NOT NULL,
    secondary_back character varying(3) NOT NULL
);


ALTER TABLE public.counties OWNER TO awips;

--
-- TOC entry 1808 (class 1259 OID 18961)
-- Dependencies: 6
-- Name: countynum; Type: TABLE; Schema: public; Owner: awips; Tablespace: 
--

CREATE TABLE countynum (
    lid character varying(8) NOT NULL,
    state character varying(2) NOT NULL,
    county character varying(20) NOT NULL
);


ALTER TABLE public.countynum OWNER TO awips;

--
-- TOC entry 1809 (class 1259 OID 18964)
-- Dependencies: 2072 6
-- Name: countyinfo; Type: VIEW; Schema: public; Owner: awips
--

CREATE VIEW countyinfo AS
    SELECT cn.lid, cn.state, cn.county, c.countynum FROM countynum cn, counties c WHERE (((cn.state)::text = (c.state)::text) AND ((cn.county)::text = (c.county)::text));


ALTER TABLE public.countyinfo OWNER TO awips;

--
-- TOC entry 1810 (class 1259 OID 18968)
-- Dependencies: 6
-- Name: countytransmit; Type: TABLE; Schema: public; Owner: awips; Tablespace: 
--

CREATE TABLE countytransmit (
    call_sign character varying(6) NOT NULL,
    county character varying(20) NOT NULL,
    state character varying(2) NOT NULL
);


ALTER TABLE public.countytransmit OWNER TO awips;

--
-- TOC entry 1811 (class 1259 OID 18971)
-- Dependencies: 6
-- Name: crest; Type: TABLE; Schema: public; Owner: awips; Tablespace: 
--

CREATE TABLE crest (
    lid character varying(8) NOT NULL,
    datcrst date NOT NULL,
    cremark character varying(80),
    hw character varying(1),
    jam character varying(1),
    olddatum character varying(1),
    q integer,
    stage double precision,
    suppress character varying(1),
    timcrst character varying(5) NOT NULL,
    prelim character varying(1)
);


ALTER TABLE public.crest OWNER TO awips;

--
-- TOC entry 1812 (class 1259 OID 18974)
-- Dependencies: 6
-- Name: curpc; Type: TABLE; Schema: public; Owner: awips; Tablespace: 
--

CREATE TABLE curpc (
    lid character varying(8) NOT NULL,
    pe character varying(2) NOT NULL,
    dur smallint NOT NULL,
    ts character varying(2) NOT NULL,
    extremum character varying(1) NOT NULL,
    obstime timestamp without time zone NOT NULL,
    value double precision,
    shef_qual_code character varying(1),
    quality_code integer,
    revision smallint,
    product_id character varying(10),
    producttime timestamp without time zone,
    postingtime timestamp without time zone
);


ALTER TABLE public.curpc OWNER TO awips;

--
-- TOC entry 1813 (class 1259 OID 18977)
-- Dependencies: 6
-- Name: curpp; Type: TABLE; Schema: public; Owner: awips; Tablespace: 
--

CREATE TABLE curpp (
    lid character varying(8) NOT NULL,
    pe character varying(2) NOT NULL,
    dur smallint NOT NULL,
    ts character varying(2) NOT NULL,
    extremum character varying(1) NOT NULL,
    obstime timestamp without time zone NOT NULL,
    value double precision,
    shef_qual_code character varying(1),
    quality_code integer,
    revision smallint,
    product_id character varying(10),
    producttime timestamp without time zone,
    postingtime timestamp without time zone
);


ALTER TABLE public.curpp OWNER TO awips;

--
-- TOC entry 1814 (class 1259 OID 18980)
-- Dependencies: 6
-- Name: dailypp; Type: TABLE; Schema: public; Owner: awips; Tablespace: 
--

CREATE TABLE dailypp (
    lid character varying(8) NOT NULL,
    ts character varying(2) NOT NULL,
    obstime timestamp without time zone NOT NULL,
    value double precision,
    qc character varying(1),
    postingtime timestamp without time zone
);


ALTER TABLE public.dailypp OWNER TO awips;

--
-- TOC entry 1815 (class 1259 OID 18983)
-- Dependencies: 6
-- Name: damtypes; Type: TABLE; Schema: public; Owner: awips; Tablespace: 
--

CREATE TABLE damtypes (
    "type" character varying(10) NOT NULL
);


ALTER TABLE public.damtypes OWNER TO awips;

--
-- TOC entry 1816 (class 1259 OID 18986)
-- Dependencies: 6
-- Name: datalimits; Type: TABLE; Schema: public; Owner: awips; Tablespace: 
--

CREATE TABLE datalimits (
    pe character varying(2) NOT NULL,
    dur smallint NOT NULL,
    monthdaystart character varying(5) NOT NULL,
    monthdayend character varying(5) NOT NULL,
    gross_range_min double precision,
    gross_range_max double precision,
    reason_range_min double precision,
    reason_range_max double precision,
    roc_max double precision,
    alert_upper_limit double precision,
    alert_roc_limit double precision,
    alarm_upper_limit double precision,
    alarm_roc_limit double precision,
    alert_lower_limit double precision,
    alarm_lower_limit double precision,
    alert_diff_limit double precision,
    alarm_diff_limit double precision
);


ALTER TABLE public.datalimits OWNER TO awips;

--
-- TOC entry 1817 (class 1259 OID 18989)
-- Dependencies: 6
-- Name: datum; Type: TABLE; Schema: public; Owner: awips; Tablespace: 
--

CREATE TABLE datum (
    lid character varying(8) NOT NULL,
    ddate date NOT NULL,
    elev double precision
);


ALTER TABLE public.datum OWNER TO awips;

--
-- TOC entry 1818 (class 1259 OID 18992)
-- Dependencies: 6
-- Name: dcp; Type: TABLE; Schema: public; Owner: awips; Tablespace: 
--

CREATE TABLE dcp (
    lid character varying(8) NOT NULL,
    criteria character varying(50),
    "owner" character varying(10),
    goes character varying(8),
    rptfreq character varying(4),
    rptime character varying(8),
    "notify" character varying(1)
);


ALTER TABLE public.dcp OWNER TO awips;

--
-- TOC entry 1819 (class 1259 OID 18995)
-- Dependencies: 6
-- Name: dcpowner; Type: TABLE; Schema: public; Owner: awips; Tablespace: 
--

CREATE TABLE dcpowner (
    "owner" character varying(10) NOT NULL
);


ALTER TABLE public.dcpowner OWNER TO awips;

--
-- TOC entry 1820 (class 1259 OID 18998)
-- Dependencies: 6
-- Name: definingissuecriteria; Type: TABLE; Schema: public; Owner: awips; Tablespace: 
--

CREATE TABLE definingissuecriteria (
    def_issue_crit character varying(50) NOT NULL
);


ALTER TABLE public.definingissuecriteria OWNER TO awips;

--
-- TOC entry 1821 (class 1259 OID 19001)
-- Dependencies: 6
-- Name: descrip; Type: TABLE; Schema: public; Owner: awips; Tablespace: 
--

CREATE TABLE descrip (
    lid character varying(8) NOT NULL,
    bed character varying(60),
    divert character varying(60),
    remark character varying(255),
    ice character varying(160),
    proximity character varying(6) NOT NULL,
    reach character varying(80),
    res character varying(255),
    topo character varying(255)
);


ALTER TABLE public.descrip OWNER TO awips;

--
-- TOC entry 1822 (class 1259 OID 19004)
-- Dependencies: 6
-- Name: dhradapt; Type: TABLE; Schema: public; Owner: awips; Tablespace: 
--

CREATE TABLE dhradapt (
    radid character varying(3) NOT NULL,
    obstime timestamp without time zone NOT NULL,
    min_reflth real,
    max_reflth real,
    ref_tltest real,
    rng_tltin real,
    rng_tltout real,
    max_birng real,
    min_birng real,
    min_echoar real,
    min_awrefl real,
    max_pctred real,
    mlt_zrcoef real,
    pwr_zrcoef real,
    min_zrefl real,
    max_zrefl real,
    max_stmspd real,
    max_timdif real,
    min_artcon real,
    tim_p1cont real,
    tim_p2cont real,
    max_ecarch real,
    rng_cutoff real,
    rng_e1coef real,
    rng_e2coef real,
    rng_e3coef real,
    min_prate real,
    max_prate real,
    tim_restrt real,
    max_timint real,
    min_timprd real,
    thr_hlyout real,
    end_timgag real,
    max_prdval real,
    max_hlyval real,
    tim_biest real,
    thr_nosets real,
    res_bias real,
    longest_lag real,
    bias_applied character varying(1)
);


ALTER TABLE public.dhradapt OWNER TO awips;

--
-- TOC entry 1823 (class 1259 OID 19007)
-- Dependencies: 6
-- Name: dhrradar; Type: TABLE; Schema: public; Owner: awips; Tablespace: 
--

CREATE TABLE dhrradar (
    radid character varying(3) NOT NULL,
    obstime timestamp without time zone NOT NULL,
    volcovpat smallint,
    opermode smallint,
    dbzmin real,
    dbzinc real,
    dbzcnt real,
    j_date smallint,
    j_time smallint,
    mean_field_bias smallint,
    sample_size smallint,
    grid_filename character varying(20)
);


ALTER TABLE public.dhrradar OWNER TO awips;

--
-- TOC entry 1824 (class 1259 OID 19010)
-- Dependencies: 6
-- Name: discharge; Type: TABLE; Schema: public; Owner: awips; Tablespace: 
--

CREATE TABLE discharge (
    lid character varying(8) NOT NULL,
    pe character varying(2) NOT NULL,
    dur smallint NOT NULL,
    ts character varying(2) NOT NULL,
    extremum character varying(1) NOT NULL,
    obstime timestamp without time zone NOT NULL,
    value double precision,
    shef_qual_code character varying(1),
    quality_code integer,
    revision smallint,
    product_id character varying(10),
    producttime timestamp without time zone,
    postingtime timestamp without time zone
);


ALTER TABLE public.discharge OWNER TO awips;

--
-- TOC entry 1825 (class 1259 OID 19013)
-- Dependencies: 6
-- Name: dpaadapt; Type: TABLE; Schema: public; Owner: awips; Tablespace: 
--

CREATE TABLE dpaadapt (
    radid character varying(3) NOT NULL,
    obstime timestamp without time zone NOT NULL,
    min_reflth real,
    max_reflth real,
    ref_tltest real,
    rng_tltin real,
    rng_tltout real,
    max_birng real,
    min_birng real,
    min_echoar real,
    min_awrefl real,
    max_pctred real,
    mlt_zrcoef real,
    pwr_zrcoef real,
    min_zrefl real,
    max_zrefl real,
    beam_width real,
    blockage_thresh real,
    clutter_thresh real,
    weight_thresh real,
    hybrid_scan_thresh real,
    low_reflect_thresh real,
    detect_reflect_thr real,
    detect_area_thresh real,
    detect_time_thresh real,
    exclusion_zones real,
    max_stmspd real,
    max_timdif real,
    min_artcon real,
    tim_p1cont real,
    tim_p2cont real,
    max_ecarch real,
    rng_cutoff real,
    rng_e1coef real,
    rng_e2coef real,
    rng_e3coef real,
    min_prate real,
    max_prate real,
    tim_restrt real,
    max_timint real,
    min_timprd real,
    thr_hlyout real,
    end_timgag real,
    max_prdval real,
    max_hlyval real,
    tim_biest real,
    thr_nosets real,
    res_bias real,
    longest_lag real,
    bias_applied character varying(1)
);


ALTER TABLE public.dpaadapt OWNER TO awips;

--
-- TOC entry 1826 (class 1259 OID 19016)
-- Dependencies: 6
-- Name: dparadar; Type: TABLE; Schema: public; Owner: awips; Tablespace: 
--

CREATE TABLE dparadar (
    radid character varying(3) NOT NULL,
    obstime timestamp without time zone NOT NULL,
    minoff smallint,
    maxvalh real,
    maxvald real,
    s1_bias_value real,
    producttime timestamp without time zone,
    nisolbin smallint,
    noutint smallint,
    noutrep smallint,
    areared real,
    biscanr real,
    block_bins_reject integer,
    clutter_bins_rej integer,
    bins_smoothed integer,
    scan_bins_filled real,
    high_elev_angle real,
    scan_rain_area real,
    nbadscan smallint,
    nhourout smallint,
    volcovpat smallint,
    opermode smallint,
    missper character varying(1),
    supplmess smallint,
    grid_filename character varying(20)
);


ALTER TABLE public.dparadar OWNER TO awips;

--
-- TOC entry 1827 (class 1259 OID 19019)
-- Dependencies: 6
-- Name: dspadapt; Type: TABLE; Schema: public; Owner: awips; Tablespace: 
--

CREATE TABLE dspadapt (
    radid character varying(3) NOT NULL,
    obstime timestamp without time zone NOT NULL,
    min_reflth real,
    max_reflth real,
    ref_tltest real,
    rng_tltin real,
    rng_tltout real,
    max_birng real,
    min_birng real,
    min_echoar real,
    min_awrefl real,
    max_pctred real,
    mlt_zrcoef real,
    pwr_zrcoef real,
    min_zrefl real,
    max_zrefl real,
    max_stmspd real,
    max_timdif real,
    min_artcon real,
    tim_p1cont real,
    tim_p2cont real,
    max_ecarch real,
    rng_cutoff real,
    rng_e1coef real,
    rng_e2coef real,
    rng_e3coef real,
    min_prate real,
    max_prate real,
    tim_restrt real,
    max_timint real,
    min_timprd real,
    thr_hlyout real,
    end_timgag real,
    max_prdval real,
    max_hlyval real,
    tim_biest real,
    thr_nosets real,
    res_bias real,
    longest_lag real,
    bias_applied character varying(1)
);


ALTER TABLE public.dspadapt OWNER TO awips;

--
-- TOC entry 1828 (class 1259 OID 19022)
-- Dependencies: 6
-- Name: dspradar; Type: TABLE; Schema: public; Owner: awips; Tablespace: 
--

CREATE TABLE dspradar (
    radid character varying(3) NOT NULL,
    obstime timestamp without time zone NOT NULL,
    volcovpat smallint,
    opermode smallint,
    minval real,
    maxval real,
    num_data_lev real,
    scale_factor real,
    begin_time timestamp without time zone,
    end_time timestamp without time zone,
    j_beg_date smallint,
    j_beg_time smallint,
    j_end_date smallint,
    j_end_time smallint,
    mean_field_bias smallint,
    sample_size smallint,
    grid_filename character varying(20)
);


ALTER TABLE public.dspradar OWNER TO awips;

--
-- TOC entry 1829 (class 1259 OID 19025)
-- Dependencies: 6
-- Name: eligzon; Type: TABLE; Schema: public; Owner: awips; Tablespace: 
--

CREATE TABLE eligzon (
    state character varying(2) NOT NULL,
    zonenum character varying(3) NOT NULL,
    descr character varying(20)
);


ALTER TABLE public.eligzon OWNER TO awips;

--
-- TOC entry 1830 (class 1259 OID 19028)
-- Dependencies: 6
-- Name: evaporation; Type: TABLE; Schema: public; Owner: awips; Tablespace: 
--

CREATE TABLE evaporation (
    lid character varying(8) NOT NULL,
    pe character varying(2) NOT NULL,
    dur smallint NOT NULL,
    ts character varying(2) NOT NULL,
    extremum character varying(1) NOT NULL,
    obstime timestamp without time zone NOT NULL,
    value double precision,
    shef_qual_code character varying(1),
    quality_code integer,
    revision smallint,
    product_id character varying(10),
    producttime timestamp without time zone,
    postingtime timestamp without time zone
);


ALTER TABLE public.evaporation OWNER TO awips;

--
-- TOC entry 1831 (class 1259 OID 19031)
-- Dependencies: 6
-- Name: fcstdischarge; Type: TABLE; Schema: public; Owner: awips; Tablespace: 
--

CREATE TABLE fcstdischarge (
    lid character varying(8) NOT NULL,
    pe character varying(2) NOT NULL,
    dur smallint NOT NULL,
    ts character varying(2) NOT NULL,
    extremum character varying(1) NOT NULL,
    probability real NOT NULL,
    validtime timestamp without time zone NOT NULL,
    basistime timestamp without time zone NOT NULL,
    value double precision,
    shef_qual_code character varying(1),
    quality_code integer,
    revision smallint,
    product_id character varying(10),
    producttime timestamp without time zone,
    postingtime timestamp without time zone
);


ALTER TABLE public.fcstdischarge OWNER TO awips;

--
-- TOC entry 1832 (class 1259 OID 19034)
-- Dependencies: 6
-- Name: fcstgenmethod; Type: TABLE; Schema: public; Owner: awips; Tablespace: 
--

CREATE TABLE fcstgenmethod (
    fcst_gen_method character varying(30) NOT NULL
);


ALTER TABLE public.fcstgenmethod OWNER TO awips;

--
-- TOC entry 1833 (class 1259 OID 19037)
-- Dependencies: 6
-- Name: fcstheight; Type: TABLE; Schema: public; Owner: awips; Tablespace: 
--

CREATE TABLE fcstheight (
    lid character varying(8) NOT NULL,
    pe character varying(2) NOT NULL,
    dur smallint NOT NULL,
    ts character varying(2) NOT NULL,
    extremum character varying(1) NOT NULL,
    probability real NOT NULL,
    validtime timestamp without time zone NOT NULL,
    basistime timestamp without time zone NOT NULL,
    value double precision,
    shef_qual_code character varying(1),
    quality_code integer,
    revision smallint,
    product_id character varying(10),
    producttime timestamp without time zone,
    postingtime timestamp without time zone
);


ALTER TABLE public.fcstheight OWNER TO awips;

--
-- TOC entry 1834 (class 1259 OID 19040)
-- Dependencies: 6
-- Name: fcsthorizon; Type: TABLE; Schema: public; Owner: awips; Tablespace: 
--

CREATE TABLE fcsthorizon (
    fcst_horizon character varying(30) NOT NULL
);


ALTER TABLE public.fcsthorizon OWNER TO awips;

--
-- TOC entry 1835 (class 1259 OID 19043)
-- Dependencies: 6
-- Name: fcstother; Type: TABLE; Schema: public; Owner: awips; Tablespace: 
--

CREATE TABLE fcstother (
    lid character varying(8) NOT NULL,
    pe character varying(2) NOT NULL,
    dur smallint NOT NULL,
    ts character varying(2) NOT NULL,
    extremum character varying(1) NOT NULL,
    probability real NOT NULL,
    validtime timestamp without time zone NOT NULL,
    basistime timestamp without time zone NOT NULL,
    value double precision,
    shef_qual_code character varying(1),
    quality_code integer,
    revision smallint,
    product_id character varying(10),
    producttime timestamp without time zone,
    postingtime timestamp without time zone
);


ALTER TABLE public.fcstother OWNER TO awips;

--
-- TOC entry 1836 (class 1259 OID 19046)
-- Dependencies: 6
-- Name: fcstprecip; Type: TABLE; Schema: public; Owner: awips; Tablespace: 
--

CREATE TABLE fcstprecip (
    lid character varying(8) NOT NULL,
    pe character varying(2) NOT NULL,
    dur smallint NOT NULL,
    ts character varying(2) NOT NULL,
    extremum character varying(1) NOT NULL,
    probability real NOT NULL,
    validtime timestamp without time zone NOT NULL,
    basistime timestamp without time zone NOT NULL,
    value double precision,
    shef_qual_code character varying(1),
    quality_code integer,
    revision smallint,
    product_id character varying(10),
    producttime timestamp without time zone,
    postingtime timestamp without time zone
);


ALTER TABLE public.fcstprecip OWNER TO awips;

--
-- TOC entry 1837 (class 1259 OID 19049)
-- Dependencies: 6
-- Name: fcstptdeterm; Type: TABLE; Schema: public; Owner: awips; Tablespace: 
--

CREATE TABLE fcstptdeterm (
    lid character varying(8) NOT NULL,
    snow_method character varying(30) NOT NULL,
    hydrol_method character varying(30) NOT NULL,
    reservoir_model character varying(30) NOT NULL,
    upstream_seg character varying(8) NOT NULL,
    hydraul_method character varying(30) NOT NULL,
    def_issue_crit character varying(50) NOT NULL,
    hours_qpf smallint NOT NULL,
    frequpd_normal character varying(30),
    frequpd_flood character varying(30),
    frequpd_drought character varying(30),
    fcst_horizon character varying(30),
    hours_qtf smallint,
    hours_qzf smallint,
    num_elev_zones smallint,
    consumptive_use character varying(1),
    channel_loss character varying(1),
    fcst_gen_method character varying(30),
    impl_date date,
    web_date date
);


ALTER TABLE public.fcstptdeterm OWNER TO awips;

--
-- TOC entry 1838 (class 1259 OID 19052)
-- Dependencies: 6
-- Name: fcstptesp; Type: TABLE; Schema: public; Owner: awips; Tablespace: 
--

CREATE TABLE fcstptesp (
    lid character varying(8) NOT NULL,
    snow_method character varying(30) NOT NULL,
    hydrol_method character varying(30) NOT NULL,
    reservoir_model character varying(30) NOT NULL,
    upstream_seg character varying(8) NOT NULL,
    hydraul_method character varying(30) NOT NULL,
    flowtype character varying(40) NOT NULL,
    fcsttype character varying(20) NOT NULL,
    frequpd_normal character varying(30),
    frequpd_flood character varying(30),
    frequpd_drought character varying(30),
    fcst_horizon character varying(30),
    nummonclim smallint,
    numdayhyd smallint,
    num_elev_zones smallint,
    consumptive_use character varying(1),
    channel_loss character varying(1),
    post_processor character varying(30),
    impl_date date,
    external_date date,
    web_date date
);


ALTER TABLE public.fcstptesp OWNER TO awips;

--
-- TOC entry 1839 (class 1259 OID 19055)
-- Dependencies: 6
-- Name: fcstptservice; Type: TABLE; Schema: public; Owner: awips; Tablespace: 
--

CREATE TABLE fcstptservice (
    lid character varying(8) NOT NULL,
    flood_thres double precision,
    exceed_prob smallint,
    service_type character varying(20),
    anal_start_date date,
    anal_end_date date,
    impl_date date,
    web_date date
);


ALTER TABLE public.fcstptservice OWNER TO awips;

--
-- TOC entry 1840 (class 1259 OID 19058)
-- Dependencies: 6
-- Name: fcstptwatsup; Type: TABLE; Schema: public; Owner: awips; Tablespace: 
--

CREATE TABLE fcstptwatsup (
    lid character varying(8) NOT NULL,
    watsup_method character varying(50) NOT NULL,
    watsup_coord_agency character varying(64) NOT NULL,
    frequpd_normal character varying(30) NOT NULL,
    period_req character varying(30) NOT NULL,
    watsup_crit character varying(30) NOT NULL,
    watsup_resp_agency character varying(64),
    customer_desc character varying(80),
    impl_date date,
    web_date date
);


ALTER TABLE public.fcstptwatsup OWNER TO awips;

--
-- TOC entry 1841 (class 1259 OID 19061)
-- Dependencies: 6
-- Name: fcsttemperature; Type: TABLE; Schema: public; Owner: awips; Tablespace: 
--

CREATE TABLE fcsttemperature (
    lid character varying(8) NOT NULL,
    pe character varying(2) NOT NULL,
    dur smallint NOT NULL,
    ts character varying(2) NOT NULL,
    extremum character varying(1) NOT NULL,
    probability real NOT NULL,
    validtime timestamp without time zone NOT NULL,
    basistime timestamp without time zone NOT NULL,
    value double precision,
    shef_qual_code character varying(1),
    quality_code integer,
    revision smallint,
    product_id character varying(10),
    producttime timestamp without time zone,
    postingtime timestamp without time zone
);


ALTER TABLE public.fcsttemperature OWNER TO awips;

--
-- TOC entry 1842 (class 1259 OID 19064)
-- Dependencies: 6
-- Name: fcsttype; Type: TABLE; Schema: public; Owner: awips; Tablespace: 
--

CREATE TABLE fcsttype (
    fcsttype character varying(20) NOT NULL
);


ALTER TABLE public.fcsttype OWNER TO awips;

--
-- TOC entry 1843 (class 1259 OID 19067)
-- Dependencies: 6
-- Name: fishcount; Type: TABLE; Schema: public; Owner: awips; Tablespace: 
--

CREATE TABLE fishcount (
    lid character varying(8) NOT NULL,
    pe character varying(2) NOT NULL,
    dur smallint NOT NULL,
    ts character varying(2) NOT NULL,
    extremum character varying(1) NOT NULL,
    obstime timestamp without time zone NOT NULL,
    value double precision,
    shef_qual_code character varying(1),
    quality_code integer,
    revision smallint,
    product_id character varying(10),
    producttime timestamp without time zone,
    postingtime timestamp without time zone
);


ALTER TABLE public.fishcount OWNER TO awips;

--
-- TOC entry 1844 (class 1259 OID 19070)
-- Dependencies: 6
-- Name: flood; Type: TABLE; Schema: public; Owner: awips; Tablespace: 
--

CREATE TABLE flood (
    lid character varying(8) NOT NULL,
    stage double precision NOT NULL,
    damage character varying(512),
    dispstmt character varying(60)
);


ALTER TABLE public.flood OWNER TO awips;

--
-- TOC entry 1845 (class 1259 OID 19073)
-- Dependencies: 6
-- Name: floodcat; Type: TABLE; Schema: public; Owner: awips; Tablespace: 
--

CREATE TABLE floodcat (
    lid character varying(8) NOT NULL,
    minor_stage double precision,
    moderate_stage double precision,
    major_stage double precision,
    minor_flow double precision,
    moderate_flow double precision,
    major_flow double precision
);


ALTER TABLE public.floodcat OWNER TO awips;

--
-- TOC entry 1846 (class 1259 OID 19076)
-- Dependencies: 6
-- Name: floodstmt; Type: TABLE; Schema: public; Owner: awips; Tablespace: 
--

CREATE TABLE floodstmt (
    lid character varying(8) NOT NULL,
    impact_value double precision NOT NULL,
    "statement" character varying(512),
    rf character varying(1) NOT NULL,
    datestart character varying(5) NOT NULL,
    dateend character varying(5) NOT NULL,
    impact_pe character varying(2)
);


ALTER TABLE public.floodstmt OWNER TO awips;

--
-- TOC entry 1847 (class 1259 OID 19079)
-- Dependencies: 6
-- Name: floodts; Type: TABLE; Schema: public; Owner: awips; Tablespace: 
--

CREATE TABLE floodts (
    lid character varying(8) NOT NULL,
    obstime timestamp without time zone NOT NULL,
    flood_event_id integer,
    value double precision
);


ALTER TABLE public.floodts OWNER TO awips;

--
-- TOC entry 1848 (class 1259 OID 19082)
-- Dependencies: 6
-- Name: flowtype; Type: TABLE; Schema: public; Owner: awips; Tablespace: 
--

CREATE TABLE flowtype (
    flowtype character varying(40) NOT NULL
);


ALTER TABLE public.flowtype OWNER TO awips;

--
-- TOC entry 1849 (class 1259 OID 19085)
-- Dependencies: 6
-- Name: location; Type: TABLE; Schema: public; Owner: awips; Tablespace: 
--

CREATE TABLE "location" (
    lid character varying(8) NOT NULL,
    county character varying(20) NOT NULL,
    coe character varying(3),
    cpm character varying(3),
    detail character varying(10),
    elev double precision,
    hdatum character varying(9),
    hsa character varying(3) NOT NULL,
    hu character varying(8),
    lat double precision,
    lon double precision,
    lremark character varying(255),
    lrevise date,
    name character varying(50),
    network character varying(3) NOT NULL,
    rb character varying(30),
    rfc character varying(5) NOT NULL,
    sbd date,
    sn character varying(10),
    state character varying(2) NOT NULL,
    waro character varying(3),
    wfo character varying(3) NOT NULL,
    wsfo character varying(3),
    "type" character varying(4),
    des character varying(30),
    det character varying(30),
    post integer,
    stntype character varying(4),
    tzone character varying(8) NOT NULL
);


ALTER TABLE public."location" OWNER TO awips;

--
-- TOC entry 1850 (class 1259 OID 19088)
-- Dependencies: 6
-- Name: riverstat; Type: TABLE; Schema: public; Owner: awips; Tablespace: 
--

CREATE TABLE riverstat (
    lid character varying(8) NOT NULL,
    primary_pe character varying(2),
    bf double precision,
    cb double precision,
    da double precision,
    response_time double precision,
    threshold_runoff double precision,
    fq double precision,
    fs double precision,
    gsno character varying(10),
    "level" character varying(20),
    mile double precision,
    pool double precision,
    por character varying(30),
    rated character varying(20),
    lat double precision,
    lon double precision,
    remark character varying(255),
    rrevise date,
    rsource character varying(20),
    stream character varying(32),
    tide character varying(8),
    backwater character varying(8),
    vdatum character varying(20),
    action_flow double precision,
    wstg double precision,
    zd double precision,
    ratedat date,
    usgs_ratenum character varying(5),
    uhgdur integer,
    use_latest_fcst character varying(1)
);


ALTER TABLE public.riverstat OWNER TO awips;

--
-- TOC entry 1851 (class 1259 OID 19091)
-- Dependencies: 6
-- Name: rpffcstpoint; Type: TABLE; Schema: public; Owner: awips; Tablespace: 
--

CREATE TABLE rpffcstpoint (
    lid character varying(8) NOT NULL,
    group_id character varying(8) NOT NULL,
    ordinal integer,
    chg_threshold double precision,
    rec_type character varying(3),
    primary_back character varying(3) NOT NULL,
    secondary_back character varying(3) NOT NULL,
    backhrs integer,
    forwardhrs integer,
    adjustendhrs double precision
);


ALTER TABLE public.rpffcstpoint OWNER TO awips;

--
-- TOC entry 1852 (class 1259 OID 19094)
-- Dependencies: 2073 6
-- Name: fpinfo; Type: VIEW; Schema: public; Owner: awips
--

CREATE VIEW fpinfo AS
    SELECT l.lid, l.name, l.county, l.state, l.hsa, fp.primary_back, fp.secondary_back, r.stream, r.bf, r.wstg, r.fs, r.fq, r.action_flow, r.primary_pe AS pe, r.use_latest_fcst, d.proximity, d.reach, fp.group_id, fp.ordinal, fp.chg_threshold, fp.rec_type, fp.backhrs, fp.forwardhrs, fp.adjustendhrs, f.minor_stage, f.moderate_stage, f.major_stage, f.minor_flow, f.moderate_flow, f.major_flow FROM ((location l LEFT JOIN floodcat f ON (((l.lid)::text = (f.lid)::text))) LEFT JOIN descrip d ON (((l.lid)::text = (d.lid)::text))), riverstat r, rpffcstpoint fp WHERE (((((l.type)::text !~~ '%I%'::text) OR (l.type IS NULL)) AND ((l.lid)::text = (r.lid)::text)) AND ((l.lid)::text = (fp.lid)::text));


ALTER TABLE public.fpinfo OWNER TO awips;

--
-- TOC entry 1853 (class 1259 OID 19099)
-- Dependencies: 6
-- Name: fpprevprod; Type: TABLE; Schema: public; Owner: awips; Tablespace: 
--

CREATE TABLE fpprevprod (
    lid character varying(8) NOT NULL,
    product_id character varying(10),
    prod_categ character varying(3),
    producttime timestamp without time zone NOT NULL,
    office_id character varying(5),
    obsvalue double precision,
    obstime timestamp without time zone,
    max_fcstvalue double precision,
    validtime timestamp without time zone,
    basistime timestamp without time zone
);


ALTER TABLE public.fpprevprod OWNER TO awips;

--
-- TOC entry 1854 (class 1259 OID 19102)
-- Dependencies: 6
-- Name: fpprevprodpractice; Type: TABLE; Schema: public; Owner: awips; Tablespace: 
--

CREATE TABLE fpprevprodpractice (
    lid character varying(8) NOT NULL,
    product_id character varying(10),
    prod_categ character varying(3),
    producttime timestamp without time zone NOT NULL,
    office_id character varying(5),
    obsvalue double precision,
    obstime timestamp without time zone,
    max_fcstvalue double precision,
    validtime timestamp without time zone,
    basistime timestamp without time zone
);


ALTER TABLE public.fpprevprodpractice OWNER TO awips;

--
-- TOC entry 1855 (class 1259 OID 19105)
-- Dependencies: 6
-- Name: frequencyupdate; Type: TABLE; Schema: public; Owner: awips; Tablespace: 
--

CREATE TABLE frequencyupdate (
    frequency_update character varying(30) NOT NULL
);


ALTER TABLE public.frequencyupdate OWNER TO awips;

--
-- TOC entry 1856 (class 1259 OID 19108)
-- Dependencies: 6
-- Name: gage; Type: TABLE; Schema: public; Owner: awips; Tablespace: 
--

CREATE TABLE gage (
    lid character varying(8) NOT NULL,
    gbegin date NOT NULL,
    "type" character varying(10) NOT NULL,
    gend date,
    remark character varying(255),
    maint character varying(10),
    "owner" character varying(10)
);


ALTER TABLE public.gage OWNER TO awips;

--
-- TOC entry 1857 (class 1259 OID 19111)
-- Dependencies: 6
-- Name: gagemaint; Type: TABLE; Schema: public; Owner: awips; Tablespace: 
--

CREATE TABLE gagemaint (
    maint character varying(10) NOT NULL
);


ALTER TABLE public.gagemaint OWNER TO awips;

--
-- TOC entry 1858 (class 1259 OID 19114)
-- Dependencies: 6
-- Name: gageowner; Type: TABLE; Schema: public; Owner: awips; Tablespace: 
--

CREATE TABLE gageowner (
    "owner" character varying(10) NOT NULL
);


ALTER TABLE public.gageowner OWNER TO awips;

--
-- TOC entry 1859 (class 1259 OID 19117)
-- Dependencies: 6
-- Name: gagetype; Type: TABLE; Schema: public; Owner: awips; Tablespace: 
--

CREATE TABLE gagetype (
    "type" character varying(10) NOT NULL
);


ALTER TABLE public.gagetype OWNER TO awips;

--
-- TOC entry 1860 (class 1259 OID 19120)
-- Dependencies: 6
-- Name: gatedam; Type: TABLE; Schema: public; Owner: awips; Tablespace: 
--

CREATE TABLE gatedam (
    lid character varying(8) NOT NULL,
    pe character varying(2) NOT NULL,
    dur smallint NOT NULL,
    ts character varying(2) NOT NULL,
    extremum character varying(1) NOT NULL,
    obstime timestamp without time zone NOT NULL,
    value double precision,
    shef_qual_code character varying(1),
    quality_code integer,
    revision smallint,
    product_id character varying(10),
    producttime timestamp without time zone,
    postingtime timestamp without time zone
);


ALTER TABLE public.gatedam OWNER TO awips;

--
-- TOC entry 1861 (class 1259 OID 19123)
-- Dependencies: 6
-- Name: geoarea; Type: TABLE; Schema: public; Owner: awips; Tablespace: 
--

CREATE TABLE geoarea (
    area_id character varying(8) NOT NULL,
    name character varying(40),
    boundary_type character varying(6),
    interior_lat double precision,
    interior_lon double precision
);


ALTER TABLE public.geoarea OWNER TO awips;

--
-- TOC entry 1862 (class 1259 OID 19126)
-- Dependencies: 6
-- Name: ground; Type: TABLE; Schema: public; Owner: awips; Tablespace: 
--

CREATE TABLE ground (
    lid character varying(8) NOT NULL,
    pe character varying(2) NOT NULL,
    dur smallint NOT NULL,
    ts character varying(2) NOT NULL,
    extremum character varying(1) NOT NULL,
    obstime timestamp without time zone NOT NULL,
    value double precision,
    shef_qual_code character varying(1),
    quality_code integer,
    revision smallint,
    product_id character varying(10),
    producttime timestamp without time zone,
    postingtime timestamp without time zone
);


ALTER TABLE public.ground OWNER TO awips;

--
-- TOC entry 1863 (class 1259 OID 19129)
-- Dependencies: 6
-- Name: height; Type: TABLE; Schema: public; Owner: awips; Tablespace: 
--

CREATE TABLE height (
    lid character varying(8) NOT NULL,
    pe character varying(2) NOT NULL,
    dur smallint NOT NULL,
    ts character varying(2) NOT NULL,
    extremum character varying(1) NOT NULL,
    obstime timestamp without time zone NOT NULL,
    value double precision,
    shef_qual_code character varying(1),
    quality_code integer,
    revision smallint,
    product_id character varying(10),
    producttime timestamp without time zone,
    postingtime timestamp without time zone
);


ALTER TABLE public.height OWNER TO awips;

--
-- TOC entry 1864 (class 1259 OID 19132)
-- Dependencies: 6
-- Name: hgstation; Type: TABLE; Schema: public; Owner: awips; Tablespace: 
--

CREATE TABLE hgstation (
    lid character varying(8) NOT NULL,
    pe character varying(2) NOT NULL,
    ts character varying(2) NOT NULL,
    fcstts character varying(2)
);


ALTER TABLE public.hgstation OWNER TO awips;

--
-- TOC entry 1865 (class 1259 OID 19135)
-- Dependencies: 6
-- Name: hourlypc; Type: TABLE; Schema: public; Owner: awips; Tablespace: 
--

CREATE TABLE hourlypc (
    lid character varying(8) NOT NULL,
    ts character varying(2) NOT NULL,
    obsdate date NOT NULL,
    minute_offset character varying(24),
    hourly_qc character varying(24),
    hour1 smallint,
    hour2 smallint,
    hour3 smallint,
    hour4 smallint,
    hour5 smallint,
    hour6 smallint,
    hour7 smallint,
    hour8 smallint,
    hour9 smallint,
    hour10 smallint,
    hour11 smallint,
    hour12 smallint,
    hour13 smallint,
    hour14 smallint,
    hour15 smallint,
    hour16 smallint,
    hour17 smallint,
    hour18 smallint,
    hour19 smallint,
    hour20 smallint,
    hour21 smallint,
    hour22 smallint,
    hour23 smallint,
    hour24 smallint
);


ALTER TABLE public.hourlypc OWNER TO awips;

--
-- TOC entry 1866 (class 1259 OID 19138)
-- Dependencies: 6
-- Name: hourlypp; Type: TABLE; Schema: public; Owner: awips; Tablespace: 
--

CREATE TABLE hourlypp (
    lid character varying(8) NOT NULL,
    ts character varying(2) NOT NULL,
    obsdate date NOT NULL,
    minute_offset character varying(24),
    hourly_qc character varying(24),
    hour1 smallint,
    hour2 smallint,
    hour3 smallint,
    hour4 smallint,
    hour5 smallint,
    hour6 smallint,
    hour7 smallint,
    hour8 smallint,
    hour9 smallint,
    hour10 smallint,
    hour11 smallint,
    hour12 smallint,
    hour13 smallint,
    hour14 smallint,
    hour15 smallint,
    hour16 smallint,
    hour17 smallint,
    hour18 smallint,
    hour19 smallint,
    hour20 smallint,
    hour21 smallint,
    hour22 smallint,
    hour23 smallint,
    hour24 smallint,
    sixhr06 smallint,
    sixhr12 smallint,
    sixhr18 smallint,
    sixhr24 smallint,
    sixhrqc character varying(4),
    sixhroffset character varying(4)
);


ALTER TABLE public.hourlypp OWNER TO awips;

--
-- TOC entry 1867 (class 1259 OID 19141)
-- Dependencies: 6
-- Name: hsa; Type: TABLE; Schema: public; Owner: awips; Tablespace: 
--

CREATE TABLE hsa (
    hsa character varying(3) NOT NULL
);


ALTER TABLE public.hsa OWNER TO awips;

--
-- TOC entry 1868 (class 1259 OID 19144)
-- Dependencies: 6
-- Name: stnclass; Type: TABLE; Schema: public; Owner: awips; Tablespace: 
--

CREATE TABLE stnclass (
    lid character varying(8) NOT NULL,
    disp_class character varying(10),
    dcp character varying(1),
    observer character varying(1),
    telem_type character varying(10)
);


ALTER TABLE public.stnclass OWNER TO awips;

--
-- TOC entry 1869 (class 1259 OID 19147)
-- Dependencies: 2074 6
-- Name: hvstation; Type: VIEW; Schema: public; Owner: awips
--

CREATE VIEW hvstation AS
    SELECT l.lid, l.name, l.lat, l.lon, r.stream AS stream_name, r.primary_pe, r.fs AS flood_stage, r.fq AS flood_flow, r.wstg AS action_stage, r.action_flow, s.disp_class, s.dcp AS is_dcp, s.observer AS is_observer, s.telem_type FROM (location l LEFT JOIN riverstat r ON (((l.lid)::text = (r.lid)::text))), stnclass s WHERE ((l.lid)::text = (s.lid)::text);


ALTER TABLE public.hvstation OWNER TO awips;

--
-- TOC entry 1870 (class 1259 OID 19152)
-- Dependencies: 2075 6
-- Name: hwstages; Type: VIEW; Schema: public; Owner: awips
--

CREATE VIEW hwstages AS
    SELECT r.lid, r.fs, r.wstg, max(c.stage) AS ms FROM riverstat r, crest c WHERE ((r.lid)::text = (c.lid)::text) GROUP BY r.lid, r.fs, r.wstg;


ALTER TABLE public.hwstages OWNER TO awips;

--
-- TOC entry 1871 (class 1259 OID 19156)
-- Dependencies: 6
-- Name: hydrologicmethod; Type: TABLE; Schema: public; Owner: awips; Tablespace: 
--

CREATE TABLE hydrologicmethod (
    hydrol_method character varying(30) NOT NULL
);


ALTER TABLE public.hydrologicmethod OWNER TO awips;

--
-- TOC entry 1872 (class 1259 OID 19159)
-- Dependencies: 6
-- Name: ice; Type: TABLE; Schema: public; Owner: awips; Tablespace: 
--

CREATE TABLE ice (
    lid character varying(8) NOT NULL,
    pe character varying(2) NOT NULL,
    dur smallint NOT NULL,
    ts character varying(2) NOT NULL,
    extremum character varying(1) NOT NULL,
    obstime timestamp without time zone NOT NULL,
    value double precision,
    shef_qual_code character varying(1),
    quality_code integer,
    revision smallint,
    product_id character varying(10),
    producttime timestamp without time zone,
    postingtime timestamp without time zone
);


ALTER TABLE public.ice OWNER TO awips;

--
-- TOC entry 1873 (class 1259 OID 19162)
-- Dependencies: 6
-- Name: ingestfilter; Type: TABLE; Schema: public; Owner: awips; Tablespace: 
--

CREATE TABLE ingestfilter (
    lid character varying(8) NOT NULL,
    pe character varying(2) NOT NULL,
    dur smallint NOT NULL,
    ts character varying(2) NOT NULL,
    extremum character varying(1) NOT NULL,
    ts_rank smallint,
    ingest character varying(1),
    ofs_input character varying(1),
    stg2_input character varying(1)
);


ALTER TABLE public.ingestfilter OWNER TO awips;

--
-- TOC entry 1874 (class 1259 OID 19165)
-- Dependencies: 6
-- Name: lake; Type: TABLE; Schema: public; Owner: awips; Tablespace: 
--

CREATE TABLE lake (
    lid character varying(8) NOT NULL,
    pe character varying(2) NOT NULL,
    dur smallint NOT NULL,
    ts character varying(2) NOT NULL,
    extremum character varying(1) NOT NULL,
    obstime timestamp without time zone NOT NULL,
    value double precision,
    shef_qual_code character varying(1),
    quality_code integer,
    revision smallint,
    product_id character varying(10),
    producttime timestamp without time zone,
    postingtime timestamp without time zone
);


ALTER TABLE public.lake OWNER TO awips;

--
-- TOC entry 1875 (class 1259 OID 19168)
-- Dependencies: 6
-- Name: latestobsvalue; Type: TABLE; Schema: public; Owner: awips; Tablespace: 
--

CREATE TABLE latestobsvalue (
    lid character varying(8) NOT NULL,
    pe character varying(2) NOT NULL,
    dur smallint NOT NULL,
    ts character varying(2) NOT NULL,
    extremum character varying(1) NOT NULL,
    obstime timestamp without time zone,
    value double precision,
    revision smallint,
    shef_qual_code character varying(1),
    quality_code integer,
    product_id character varying(10),
    producttime timestamp without time zone,
    postingtime timestamp without time zone
);


ALTER TABLE public.latestobsvalue OWNER TO awips;

--
-- TOC entry 1876 (class 1259 OID 19171)
-- Dependencies: 6
-- Name: lightning; Type: TABLE; Schema: public; Owner: awips; Tablespace: 
--

CREATE TABLE lightning (
    x_hgrid smallint NOT NULL,
    y_hgrid smallint NOT NULL,
    obstime timestamp without time zone NOT NULL,
    no_of_strike smallint
);


ALTER TABLE public.lightning OWNER TO awips;

--
-- TOC entry 1877 (class 1259 OID 19174)
-- Dependencies: 6
-- Name: linesegs; Type: TABLE; Schema: public; Owner: awips; Tablespace: 
--

CREATE TABLE linesegs (
    area_id character varying(8) NOT NULL,
    hrap_row integer NOT NULL,
    hrap_beg_col integer NOT NULL,
    hrap_end_col integer,
    area double precision
);


ALTER TABLE public.linesegs OWNER TO awips;

--
-- TOC entry 1878 (class 1259 OID 19177)
-- Dependencies: 6
-- Name: locarea; Type: TABLE; Schema: public; Owner: awips; Tablespace: 
--

CREATE TABLE locarea (
    lid character varying(8) NOT NULL,
    area character varying(80)
);


ALTER TABLE public.locarea OWNER TO awips;

--
-- TOC entry 1879 (class 1259 OID 19180)
-- Dependencies: 2076 6
-- Name: locclass; Type: VIEW; Schema: public; Owner: awips
--

CREATE VIEW locclass AS
    SELECT l.lid, l.name, l.lat, l.lon, l.wfo, l.hsa, l.post, s.disp_class, s.dcp AS is_dcp, s.observer AS is_observer, s.telem_type FROM (location l LEFT JOIN stnclass s ON (((l.lid)::text = (s.lid)::text)));


ALTER TABLE public.locclass OWNER TO awips;

--
-- TOC entry 1880 (class 1259 OID 19184)
-- Dependencies: 6
-- Name: locdatalimits; Type: TABLE; Schema: public; Owner: awips; Tablespace: 
--

CREATE TABLE locdatalimits (
    lid character varying(8) NOT NULL,
    pe character varying(2) NOT NULL,
    dur smallint NOT NULL,
    monthdaystart character varying(5) NOT NULL,
    monthdayend character varying(5) NOT NULL,
    gross_range_min double precision,
    gross_range_max double precision,
    reason_range_min double precision,
    reason_range_max double precision,
    roc_max double precision,
    alert_upper_limit double precision,
    alert_roc_limit double precision,
    alarm_upper_limit double precision,
    alarm_roc_limit double precision,
    alert_lower_limit double precision,
    alarm_lower_limit double precision,
    alert_diff_limit double precision,
    alarm_diff_limit double precision
);


ALTER TABLE public.locdatalimits OWNER TO awips;

--
-- TOC entry 1881 (class 1259 OID 19187)
-- Dependencies: 6
-- Name: locimage; Type: TABLE; Schema: public; Owner: awips; Tablespace: 
--

CREATE TABLE locimage (
    lid character varying(8) NOT NULL,
    imageid character varying(10) NOT NULL,
    title character varying(30),
    descr character varying(80),
    format character varying(10),
    url_internal character varying(120),
    url_external character varying(120)
);


ALTER TABLE public.locimage OWNER TO awips;

--
-- TOC entry 1882 (class 1259 OID 19190)
-- Dependencies: 2077 6
-- Name: locpdc; Type: VIEW; Schema: public; Owner: awips
--

CREATE VIEW locpdc AS
    SELECT l.lid, l.name, l.lat, l.lon, l.hsa, l.post, l.elev, r.primary_pe, r.fs, r.fq, s.disp_class, s.dcp AS is_dcp, s.observer AS is_observer, s.telem_type FROM (location l LEFT JOIN riverstat r ON (((l.lid)::text = (r.lid)::text))), stnclass s WHERE ((l.lid)::text = (s.lid)::text);


ALTER TABLE public.locpdc OWNER TO awips;

--
-- TOC entry 1883 (class 1259 OID 19195)
-- Dependencies: 2078 6
-- Name: locrivermon; Type: VIEW; Schema: public; Owner: awips
--

CREATE VIEW locrivermon AS
    SELECT l.lid, l.name, l.county, l.state, l.hsa, r.stream, r.bf AS bankfull, r.wstg AS action_stage, r.fs AS flood_stage, r.fq AS flood_flow, r.action_flow, r.primary_pe, d.proximity, d.reach, r.mile, f.minor_stage AS minor, f.moderate_stage AS moderate, f.major_stage AS major FROM ((location l LEFT JOIN floodcat f ON (((l.lid)::text = (f.lid)::text))) LEFT JOIN descrip d ON (((l.lid)::text = (d.lid)::text))), riverstat r WHERE ((((l.type)::text !~~ '%I%'::text) OR (l.type IS NULL)) AND ((l.lid)::text = (r.lid)::text));


ALTER TABLE public.locrivermon OWNER TO awips;

--
-- TOC entry 1884 (class 1259 OID 19200)
-- Dependencies: 6
-- Name: nwrtransmitter; Type: TABLE; Schema: public; Owner: awips; Tablespace: 
--

CREATE TABLE nwrtransmitter (
    call_sign character varying(6) NOT NULL,
    wfo character varying(3),
    city character varying(20),
    county character varying(20),
    state character varying(2),
    coverage_area character varying(25),
    lat double precision,
    lon double precision,
    transmit_freq double precision,
    transmit_power integer,
    transmit_prod_code character varying(3),
    transmit_countynum character varying(4),
    use_transmitter character varying(1)
);


ALTER TABLE public.nwrtransmitter OWNER TO awips;

--
-- TOC entry 1885 (class 1259 OID 19203)
-- Dependencies: 2079 6
-- Name: loctransmit; Type: VIEW; Schema: public; Owner: awips
--

CREATE VIEW loctransmit AS
    SELECT lc.lid, ct.call_sign FROM countynum lc, countytransmit ct WHERE ((((lc.county)::text = (ct.county)::text) AND ((lc.state)::text = (ct.state)::text)) AND ((ct.call_sign)::text IN (SELECT nwrtransmitter.call_sign FROM nwrtransmitter WHERE ((nwrtransmitter.use_transmitter)::text <> 'F'::text)))) GROUP BY lc.lid, ct.call_sign;


ALTER TABLE public.loctransmit OWNER TO awips;

--
-- TOC entry 1886 (class 1259 OID 19207)
-- Dependencies: 2080 6
-- Name: locview; Type: VIEW; Schema: public; Owner: awips
--

CREATE VIEW locview AS
    SELECT l.lid, l.name, l.lat, l.lon, l.rb, l.state, l.county, l.type, l.wfo, l.hsa, l.post, r.stream, r.gsno FROM (location l LEFT JOIN riverstat r ON (((l.lid)::text = (r.lid)::text)));


ALTER TABLE public.locview OWNER TO awips;

--
-- TOC entry 1887 (class 1259 OID 19212)
-- Dependencies: 2081 6
-- Name: locwfo; Type: VIEW; Schema: public; Owner: awips
--

CREATE VIEW locwfo AS
    SELECT l.lid, c.county, c.state, c.wfo, c.primary_back, c.secondary_back FROM location l, counties c WHERE (((l.county)::text = (c.county)::text) AND ((l.state)::text = (c.state)::text));


ALTER TABLE public.locwfo OWNER TO awips;

--
-- TOC entry 1888 (class 1259 OID 19216)
-- Dependencies: 6
-- Name: lowwater; Type: TABLE; Schema: public; Owner: awips; Tablespace: 
--

CREATE TABLE lowwater (
    lid character varying(8) NOT NULL,
    lwdat date NOT NULL,
    q integer,
    lwrem character varying(80),
    stage double precision
);


ALTER TABLE public.lowwater OWNER TO awips;

--
-- TOC entry 1889 (class 1259 OID 19219)
-- Dependencies: 6
-- Name: lwstmt; Type: TABLE; Schema: public; Owner: awips; Tablespace: 
--

CREATE TABLE lwstmt (
    lid character varying(8) NOT NULL,
    pe character varying(2) NOT NULL,
    lower_value double precision NOT NULL,
    upper_value double precision,
    criteria_rank integer NOT NULL,
    "statement" character varying(512),
    lw_criteria character varying(512),
    lw_source character varying(512)
);


ALTER TABLE public.lwstmt OWNER TO awips;

--
-- TOC entry 1890 (class 1259 OID 19222)
-- Dependencies: 6
-- Name: mbfcstptlist; Type: TABLE; Schema: public; Owner: awips; Tablespace: 
--

CREATE TABLE mbfcstptlist (
    lid character varying(8) NOT NULL,
    group_id character varying(8),
    ordinal integer
);


ALTER TABLE public.mbfcstptlist OWNER TO awips;

--
-- TOC entry 1891 (class 1259 OID 19225)
-- Dependencies: 6
-- Name: mbstnchk; Type: TABLE; Schema: public; Owner: awips; Tablespace: 
--

CREATE TABLE mbstnchk (
    lid character varying(8),
    numrpts integer
);


ALTER TABLE public.mbstnchk OWNER TO awips;

--
-- TOC entry 1892 (class 1259 OID 19228)
-- Dependencies: 6
-- Name: mbstnsall; Type: TABLE; Schema: public; Owner: awips; Tablespace: 
--

CREATE TABLE mbstnsall (
    lid character varying(8)
);


ALTER TABLE public.mbstnsall OWNER TO awips;

--
-- TOC entry 1893 (class 1259 OID 19231)
-- Dependencies: 6
-- Name: mbzerorpts; Type: TABLE; Schema: public; Owner: awips; Tablespace: 
--

CREATE TABLE mbzerorpts (
    lid character varying(8)
);


ALTER TABLE public.mbzerorpts OWNER TO awips;

--
-- TOC entry 1894 (class 1259 OID 19234)
-- Dependencies: 6
-- Name: moisture; Type: TABLE; Schema: public; Owner: awips; Tablespace: 
--

CREATE TABLE moisture (
    lid character varying(8) NOT NULL,
    pe character varying(2) NOT NULL,
    dur smallint NOT NULL,
    ts character varying(2) NOT NULL,
    extremum character varying(1) NOT NULL,
    obstime timestamp without time zone NOT NULL,
    value double precision,
    shef_qual_code character varying(1),
    quality_code integer,
    revision smallint,
    product_id character varying(10),
    producttime timestamp without time zone,
    postingtime timestamp without time zone
);


ALTER TABLE public.moisture OWNER TO awips;

--
-- TOC entry 1895 (class 1259 OID 19237)
-- Dependencies: 6
-- Name: monthlyvalues; Type: TABLE; Schema: public; Owner: awips; Tablespace: 
--

CREATE TABLE monthlyvalues (
    lid character varying(8) NOT NULL,
    pe character varying(2) NOT NULL,
    dur smallint NOT NULL,
    ts character varying(2) NOT NULL,
    extremum character varying(1) NOT NULL,
    adjustment character varying(1) NOT NULL,
    postingtime timestamp without time zone NOT NULL,
    jan_value double precision NOT NULL,
    feb_value double precision NOT NULL,
    mar_value double precision NOT NULL,
    apr_value double precision NOT NULL,
    may_value double precision NOT NULL,
    jun_value double precision NOT NULL,
    jul_value double precision NOT NULL,
    aug_value double precision NOT NULL,
    sep_value double precision NOT NULL,
    oct_value double precision NOT NULL,
    nov_value double precision NOT NULL,
    dec_value double precision NOT NULL
);


ALTER TABLE public.monthlyvalues OWNER TO awips;

--
-- TOC entry 1896 (class 1259 OID 19240)
-- Dependencies: 6
-- Name: mpe_gage_qc; Type: TABLE; Schema: public; Owner: awips; Tablespace: 
--

CREATE TABLE mpe_gage_qc (
    lid character varying(8) NOT NULL,
    pe character(2) NOT NULL,
    ts character(2) NOT NULL,
    remarks text,
    flags character varying(3)
);


ALTER TABLE public.mpe_gage_qc OWNER TO awips;

--
-- TOC entry 1897 (class 1259 OID 19246)
-- Dependencies: 6
-- Name: network; Type: TABLE; Schema: public; Owner: awips; Tablespace: 
--

CREATE TABLE network (
    network character varying(3) NOT NULL
);


ALTER TABLE public.network OWNER TO awips;

--
-- TOC entry 1898 (class 1259 OID 19249)
-- Dependencies: 6
-- Name: observer; Type: TABLE; Schema: public; Owner: awips; Tablespace: 
--

CREATE TABLE observer (
    lid character varying(8) NOT NULL,
    a1 character varying(30),
    a2 character varying(30),
    a3 character varying(30),
    city character varying(30),
    state character varying(2) NOT NULL,
    zip character varying(10),
    comm character varying(10) NOT NULL,
    dos date,
    gn character varying(1),
    hphone character varying(18),
    firstname character varying(12),
    lastname character varying(28),
    phone character varying(18),
    email character varying(60),
    ornr character varying(4),
    rate double precision,
    recip character varying(15) NOT NULL,
    rprt character varying(60),
    spons character varying(7) NOT NULL,
    ssn character varying(11),
    tsk character varying(13)
);


ALTER TABLE public.observer OWNER TO awips;

--
-- TOC entry 1899 (class 1259 OID 19252)
-- Dependencies: 6
-- Name: officenotes; Type: TABLE; Schema: public; Owner: awips; Tablespace: 
--

CREATE TABLE officenotes (
    topic character varying(8) NOT NULL,
    id character varying(8) NOT NULL,
    datatime timestamp without time zone,
    postingtime timestamp without time zone NOT NULL,
    updatetime timestamp without time zone,
    expiretime timestamp without time zone,
    note character varying(512)
);


ALTER TABLE public.officenotes OWNER TO awips;

--
-- TOC entry 1900 (class 1259 OID 19255)
-- Dependencies: 6
-- Name: ofsdatatrans; Type: TABLE; Schema: public; Owner: awips; Tablespace: 
--

CREATE TABLE ofsdatatrans (
    pe character varying(2) NOT NULL,
    dur smallint NOT NULL,
    extremum character varying(1) NOT NULL,
    ofs_data_type character varying(4),
    fwd_time_window real,
    bkw_time_window real
);


ALTER TABLE public.ofsdatatrans OWNER TO awips;

--
-- TOC entry 1901 (class 1259 OID 19258)
-- Dependencies: 6
-- Name: ofsstntrans; Type: TABLE; Schema: public; Owner: awips; Tablespace: 
--

CREATE TABLE ofsstntrans (
    lid character varying(8) NOT NULL,
    ofs_data_type character varying(4) NOT NULL,
    shef_source_code character varying(1) NOT NULL,
    ofs_lid character varying(8)
);


ALTER TABLE public.ofsstntrans OWNER TO awips;

--
-- TOC entry 1902 (class 1259 OID 19261)
-- Dependencies: 6
-- Name: pairedvalue; Type: TABLE; Schema: public; Owner: awips; Tablespace: 
--

CREATE TABLE pairedvalue (
    lid character varying(8) NOT NULL,
    pe character varying(2) NOT NULL,
    dur smallint NOT NULL,
    ts character varying(2) NOT NULL,
    extremum character varying(1) NOT NULL,
    probability real NOT NULL,
    validtime timestamp without time zone NOT NULL,
    basistime timestamp without time zone NOT NULL,
    ref_value integer NOT NULL,
    value double precision,
    shef_qual_code character varying(1),
    quality_code integer,
    revision smallint,
    product_id character varying(10),
    producttime timestamp without time zone,
    postingtime timestamp without time zone
);


ALTER TABLE public.pairedvalue OWNER TO awips;

--
-- TOC entry 1903 (class 1259 OID 19264)
-- Dependencies: 6
-- Name: perflog; Type: TABLE; Schema: public; Owner: awips; Tablespace: 
--

CREATE TABLE perflog (
    process character varying(10) NOT NULL,
    start_time timestamp without time zone NOT NULL,
    num_processed integer,
    num_reads integer,
    num_inserts integer,
    num_updates integer,
    num_deletes integer,
    elapsed_time double precision,
    cpu_time double precision,
    io_time double precision
);


ALTER TABLE public.perflog OWNER TO awips;

--
-- TOC entry 1904 (class 1259 OID 19267)
-- Dependencies: 6
-- Name: pointdatapresets; Type: TABLE; Schema: public; Owner: awips; Tablespace: 
--

CREATE TABLE pointdatapresets (
    preset_id character varying(8) NOT NULL,
    descr character varying(30),
    preset_rank smallint,
    preset_string character varying(512)
);


ALTER TABLE public.pointdatapresets OWNER TO awips;

--
-- TOC entry 1905 (class 1259 OID 19270)
-- Dependencies: 6
-- Name: postprocessor; Type: TABLE; Schema: public; Owner: awips; Tablespace: 
--

CREATE TABLE postprocessor (
    post_processor character varying(30) NOT NULL
);


ALTER TABLE public.postprocessor OWNER TO awips;

--
-- TOC entry 1906 (class 1259 OID 19273)
-- Dependencies: 6
-- Name: power; Type: TABLE; Schema: public; Owner: awips; Tablespace: 
--

CREATE TABLE power (
    lid character varying(8) NOT NULL,
    pe character varying(2) NOT NULL,
    dur smallint NOT NULL,
    ts character varying(2) NOT NULL,
    extremum character varying(1) NOT NULL,
    obstime timestamp without time zone NOT NULL,
    value double precision,
    shef_qual_code character varying(1),
    quality_code integer,
    revision smallint,
    product_id character varying(10),
    producttime timestamp without time zone,
    postingtime timestamp without time zone
);


ALTER TABLE public.power OWNER TO awips;

--
-- TOC entry 1907 (class 1259 OID 19276)
-- Dependencies: 6
-- Name: pressure; Type: TABLE; Schema: public; Owner: awips; Tablespace: 
--

CREATE TABLE pressure (
    lid character varying(8) NOT NULL,
    pe character varying(2) NOT NULL,
    dur smallint NOT NULL,
    ts character varying(2) NOT NULL,
    extremum character varying(1) NOT NULL,
    obstime timestamp without time zone NOT NULL,
    value double precision,
    shef_qual_code character varying(1),
    quality_code integer,
    revision smallint,
    product_id character varying(10),
    producttime timestamp without time zone,
    postingtime timestamp without time zone
);


ALTER TABLE public.pressure OWNER TO awips;

--
-- TOC entry 1908 (class 1259 OID 19279)
-- Dependencies: 6
-- Name: textproduct; Type: TABLE; Schema: public; Owner: awips; Tablespace: 
--

CREATE TABLE textproduct (
    product_id character varying(10) NOT NULL,
    producttime timestamp without time zone NOT NULL,
    postingtime timestamp without time zone NOT NULL,
    prodtype character varying(1) NOT NULL,
    issnum integer,
    product text
);


ALTER TABLE public.textproduct OWNER TO awips;

--
-- TOC entry 1909 (class 1259 OID 19285)
-- Dependencies: 2082 6
-- Name: prevprod; Type: VIEW; Schema: public; Owner: awips
--

CREATE VIEW prevprod AS
    SELECT textproduct.product_id, textproduct.producttime, textproduct.postingtime, textproduct.prodtype, textproduct.issnum FROM textproduct WHERE ((((textproduct.prodtype)::text <> 'O'::text) AND ((textproduct.prodtype)::text <> 'F'::text)) AND ((textproduct.prodtype)::text <> 'C'::text));


ALTER TABLE public.prevprod OWNER TO awips;

--
-- TOC entry 1910 (class 1259 OID 19289)
-- Dependencies: 6
-- Name: procvalue; Type: TABLE; Schema: public; Owner: awips; Tablespace: 
--

CREATE TABLE procvalue (
    lid character varying(8) NOT NULL,
    pe character varying(2) NOT NULL,
    dur smallint NOT NULL,
    ts character varying(2) NOT NULL,
    extremum character varying(1) NOT NULL,
    obstime timestamp without time zone NOT NULL,
    value double precision,
    shef_qual_code character varying(1),
    quality_code integer,
    revision smallint,
    product_id character varying(10),
    producttime timestamp without time zone,
    postingtime timestamp without time zone
);


ALTER TABLE public.procvalue OWNER TO awips;

--
-- TOC entry 1911 (class 1259 OID 19292)
-- Dependencies: 6
-- Name: productlink; Type: TABLE; Schema: public; Owner: awips; Tablespace: 
--

CREATE TABLE productlink (
    lid character varying(8) NOT NULL,
    product_id character varying(10) NOT NULL,
    producttime timestamp without time zone NOT NULL,
    postingtime timestamp without time zone NOT NULL
);


ALTER TABLE public.productlink OWNER TO awips;

--
-- TOC entry 1912 (class 1259 OID 19295)
-- Dependencies: 6
-- Name: proximity; Type: TABLE; Schema: public; Owner: awips; Tablespace: 
--

CREATE TABLE proximity (
    proximity character varying(6) NOT NULL
);


ALTER TABLE public.proximity OWNER TO awips;

--
-- TOC entry 1913 (class 1259 OID 19298)
-- Dependencies: 6
-- Name: pseudogageval; Type: TABLE; Schema: public; Owner: awips; Tablespace: 
--

CREATE TABLE pseudogageval (
    pseudo_gage_id character varying(8) NOT NULL,
    obstime timestamp without time zone NOT NULL,
    lat double precision,
    lon double precision,
    gage_value real,
    man_edited character varying(1),
    prev_gage_value real
);


ALTER TABLE public.pseudogageval OWNER TO awips;

--
-- TOC entry 1914 (class 1259 OID 19301)
-- Dependencies: 6
-- Name: pub; Type: TABLE; Schema: public; Owner: awips; Tablespace: 
--

CREATE TABLE pub (
    lid character varying(8) NOT NULL,
    pbegin date NOT NULL,
    ppub character varying(25) NOT NULL,
    pend date
);


ALTER TABLE public.pub OWNER TO awips;

--
-- TOC entry 1915 (class 1259 OID 19304)
-- Dependencies: 6
-- Name: purgedyndata; Type: TABLE; Schema: public; Owner: awips; Tablespace: 
--

CREATE TABLE purgedyndata (
    table_name character varying(18) NOT NULL,
    time_column_name character varying(18),
    num_hours_host integer,
    num_hours_backup integer
);


ALTER TABLE public.purgedyndata OWNER TO awips;

--
-- TOC entry 1916 (class 1259 OID 19307)
-- Dependencies: 6
-- Name: purgeproduct; Type: TABLE; Schema: public; Owner: awips; Tablespace: 
--

CREATE TABLE purgeproduct (
    product_id character varying(10) NOT NULL,
    num_versions integer,
    producttime timestamp without time zone,
    postingtime timestamp without time zone
);


ALTER TABLE public.purgeproduct OWNER TO awips;

--
-- TOC entry 1917 (class 1259 OID 19310)
-- Dependencies: 6
-- Name: radarloc; Type: TABLE; Schema: public; Owner: awips; Tablespace: 
--

CREATE TABLE radarloc (
    radid character varying(3) NOT NULL,
    name character varying(20),
    radid_prefix character varying(1),
    radar_num smallint,
    state character varying(2),
    lat double precision,
    lon double precision,
    elev double precision,
    tower_ht double precision,
    use_radar character varying(1),
    office_id character varying(5)
);


ALTER TABLE public.radarloc OWNER TO awips;

--
-- TOC entry 1918 (class 1259 OID 19313)
-- Dependencies: 6
-- Name: radarresp; Type: TABLE; Schema: public; Owner: awips; Tablespace: 
--

CREATE TABLE radarresp (
    radid character varying(3) NOT NULL,
    site_id character varying(5) NOT NULL
);


ALTER TABLE public.radarresp OWNER TO awips;

--
-- TOC entry 1919 (class 1259 OID 19316)
-- Dependencies: 6
-- Name: radiation; Type: TABLE; Schema: public; Owner: awips; Tablespace: 
--

CREATE TABLE radiation (
    lid character varying(8) NOT NULL,
    pe character varying(2) NOT NULL,
    dur smallint NOT NULL,
    ts character varying(2) NOT NULL,
    extremum character varying(1) NOT NULL,
    obstime timestamp without time zone NOT NULL,
    value double precision,
    shef_qual_code character varying(1),
    quality_code integer,
    revision smallint,
    product_id character varying(10),
    producttime timestamp without time zone,
    postingtime timestamp without time zone
);


ALTER TABLE public.radiation OWNER TO awips;

--
-- TOC entry 1920 (class 1259 OID 19319)
-- Dependencies: 6
-- Name: rating; Type: TABLE; Schema: public; Owner: awips; Tablespace: 
--

CREATE TABLE rating (
    lid character varying(8) NOT NULL,
    stage double precision NOT NULL,
    discharge double precision
);


ALTER TABLE public.rating OWNER TO awips;

--
-- TOC entry 1921 (class 1259 OID 19322)
-- Dependencies: 6
-- Name: ratingshift; Type: TABLE; Schema: public; Owner: awips; Tablespace: 
--

CREATE TABLE ratingshift (
    lid character varying(8) NOT NULL,
    date date NOT NULL,
    shift_amount double precision NOT NULL,
    active character varying(1)
);


ALTER TABLE public.ratingshift OWNER TO awips;

--
-- TOC entry 1922 (class 1259 OID 19325)
-- Dependencies: 6
-- Name: rawpc; Type: TABLE; Schema: public; Owner: awips; Tablespace: 
--

CREATE TABLE rawpc (
    lid character varying(8) NOT NULL,
    pe character varying(2) NOT NULL,
    dur smallint NOT NULL,
    ts character varying(2) NOT NULL,
    extremum character varying(1) NOT NULL,
    obstime timestamp without time zone NOT NULL,
    value double precision,
    shef_qual_code character varying(1),
    quality_code integer,
    revision smallint,
    product_id character varying(10),
    producttime timestamp without time zone,
    postingtime timestamp without time zone
);


ALTER TABLE public.rawpc OWNER TO awips;

--
-- TOC entry 1923 (class 1259 OID 19328)
-- Dependencies: 6
-- Name: rawpother; Type: TABLE; Schema: public; Owner: awips; Tablespace: 
--

CREATE TABLE rawpother (
    lid character varying(8) NOT NULL,
    pe character varying(2) NOT NULL,
    dur smallint NOT NULL,
    ts character varying(2) NOT NULL,
    extremum character varying(1) NOT NULL,
    obstime timestamp without time zone NOT NULL,
    value real,
    shef_qual_code character varying(1),
    quality_code integer,
    revision smallint,
    product_id character varying(10),
    producttime timestamp without time zone,
    postingtime timestamp without time zone
);


ALTER TABLE public.rawpother OWNER TO awips;

--
-- TOC entry 1924 (class 1259 OID 19331)
-- Dependencies: 6
-- Name: rawpp; Type: TABLE; Schema: public; Owner: awips; Tablespace: 
--

CREATE TABLE rawpp (
    lid character varying(8) NOT NULL,
    pe character varying(2) NOT NULL,
    dur smallint NOT NULL,
    ts character varying(2) NOT NULL,
    extremum character varying(1) NOT NULL,
    obstime timestamp without time zone NOT NULL,
    value double precision,
    shef_qual_code character varying(1),
    quality_code integer,
    revision smallint,
    product_id character varying(10),
    producttime timestamp without time zone,
    postingtime timestamp without time zone
);


ALTER TABLE public.rawpp OWNER TO awips;

--
-- TOC entry 1925 (class 1259 OID 19334)
-- Dependencies: 6
-- Name: refer; Type: TABLE; Schema: public; Owner: awips; Tablespace: 
--

CREATE TABLE refer (
    lid character varying(8) NOT NULL,
    reference character varying(70) NOT NULL
);


ALTER TABLE public.refer OWNER TO awips;

--
-- TOC entry 1926 (class 1259 OID 19337)
-- Dependencies: 6
-- Name: rejecteddata; Type: TABLE; Schema: public; Owner: awips; Tablespace: 
--

CREATE TABLE rejecteddata (
    lid character varying(8) NOT NULL,
    pe character varying(2) NOT NULL,
    dur smallint NOT NULL,
    ts character varying(2) NOT NULL,
    extremum character varying(1) NOT NULL,
    probability real NOT NULL,
    validtime timestamp without time zone NOT NULL,
    basistime timestamp without time zone NOT NULL,
    postingtime timestamp without time zone NOT NULL,
    value double precision,
    revision smallint,
    shef_qual_code character varying(1),
    product_id character varying(10),
    producttime timestamp without time zone,
    quality_code integer,
    reject_type character varying(1),
    userid character varying(32)
);


ALTER TABLE public.rejecteddata OWNER TO awips;

--
-- TOC entry 1927 (class 1259 OID 19340)
-- Dependencies: 6
-- Name: requiredperiod; Type: TABLE; Schema: public; Owner: awips; Tablespace: 
--

CREATE TABLE requiredperiod (
    period_req character varying(30) NOT NULL
);


ALTER TABLE public.requiredperiod OWNER TO awips;

--
-- TOC entry 1928 (class 1259 OID 19343)
-- Dependencies: 6
-- Name: rescap; Type: TABLE; Schema: public; Owner: awips; Tablespace: 
--

CREATE TABLE rescap (
    lid character varying(8) NOT NULL,
    elev double precision NOT NULL,
    "storage" double precision
);


ALTER TABLE public.rescap OWNER TO awips;

--
-- TOC entry 1929 (class 1259 OID 19346)
-- Dependencies: 6
-- Name: reservoir; Type: TABLE; Schema: public; Owner: awips; Tablespace: 
--

CREATE TABLE reservoir (
    lid character varying(8) NOT NULL,
    name character varying(20),
    "type" character varying(10) NOT NULL,
    "owner" character varying(10) NOT NULL,
    deadpool double precision,
    conserpool double precision,
    floodpool double precision,
    spillway double precision,
    sill double precision,
    top double precision,
    surchg double precision,
    elev double precision,
    gates integer,
    impounded date,
    uses character varying(8),
    damids character varying(2),
    damidn character varying(5)
);


ALTER TABLE public.reservoir OWNER TO awips;

--
-- TOC entry 1930 (class 1259 OID 19349)
-- Dependencies: 6
-- Name: reservoirmodel; Type: TABLE; Schema: public; Owner: awips; Tablespace: 
--

CREATE TABLE reservoirmodel (
    reservoir_model character varying(30) NOT NULL
);


ALTER TABLE public.reservoirmodel OWNER TO awips;

--
-- TOC entry 1931 (class 1259 OID 19352)
-- Dependencies: 6
-- Name: resowner; Type: TABLE; Schema: public; Owner: awips; Tablespace: 
--

CREATE TABLE resowner (
    "owner" character varying(10) NOT NULL
);


ALTER TABLE public.resowner OWNER TO awips;

--
-- TOC entry 1932 (class 1259 OID 19355)
-- Dependencies: 6
-- Name: rfc; Type: TABLE; Schema: public; Owner: awips; Tablespace: 
--

CREATE TABLE rfc (
    rfc character varying(5) NOT NULL
);


ALTER TABLE public.rfc OWNER TO awips;

--
-- TOC entry 1933 (class 1259 OID 19358)
-- Dependencies: 6
-- Name: rivermongroup; Type: TABLE; Schema: public; Owner: awips; Tablespace: 
--

CREATE TABLE rivermongroup (
    group_id character varying(8) NOT NULL,
    group_name character varying(32),
    ordinal integer,
    hsa character varying(3)
);


ALTER TABLE public.rivermongroup OWNER TO awips;

--
-- TOC entry 1934 (class 1259 OID 19361)
-- Dependencies: 6
-- Name: rivermonlocation; Type: TABLE; Schema: public; Owner: awips; Tablespace: 
--

CREATE TABLE rivermonlocation (
    lid character varying(8) NOT NULL,
    group_id character varying(8),
    ordinal integer
);


ALTER TABLE public.rivermonlocation OWNER TO awips;

--
-- TOC entry 1935 (class 1259 OID 19364)
-- Dependencies: 6
-- Name: riverstatus; Type: TABLE; Schema: public; Owner: awips; Tablespace: 
--

CREATE TABLE riverstatus (
    lid character varying(8) NOT NULL,
    pe character varying(2) NOT NULL,
    dur smallint,
    ts character varying(2) NOT NULL,
    extremum character varying(1),
    probability real,
    validtime timestamp without time zone,
    basistime timestamp without time zone,
    value double precision
);


ALTER TABLE public.riverstatus OWNER TO awips;

--
-- TOC entry 1936 (class 1259 OID 19367)
-- Dependencies: 6
-- Name: routingmethod; Type: TABLE; Schema: public; Owner: awips; Tablespace: 
--

CREATE TABLE routingmethod (
    hydraul_method character varying(30) NOT NULL
);


ALTER TABLE public.routingmethod OWNER TO awips;

--
-- TOC entry 1937 (class 1259 OID 19370)
-- Dependencies: 6
-- Name: rpffcstgroup; Type: TABLE; Schema: public; Owner: awips; Tablespace: 
--

CREATE TABLE rpffcstgroup (
    group_id character varying(8) NOT NULL,
    group_name character varying(32),
    ordinal integer,
    rec_all_included character varying(1)
);


ALTER TABLE public.rpffcstgroup OWNER TO awips;

--
-- TOC entry 1938 (class 1259 OID 19373)
-- Dependencies: 6
-- Name: rpfparams; Type: TABLE; Schema: public; Owner: awips; Tablespace: 
--

CREATE TABLE rpfparams (
    obshrs integer NOT NULL,
    fcsthrs integer NOT NULL,
    missval character varying(12),
    misscat character varying(12),
    misstim character varying(12),
    rvsexphrs integer NOT NULL,
    flsexphrs integer NOT NULL,
    flwexphrs integer NOT NULL
);


ALTER TABLE public.rpfparams OWNER TO awips;

--
-- TOC entry 1939 (class 1259 OID 19376)
-- Dependencies: 6
-- Name: rwbiasdyn; Type: TABLE; Schema: public; Owner: awips; Tablespace: 
--

CREATE TABLE rwbiasdyn (
    radid character varying(3) NOT NULL,
    office_id character varying(5) NOT NULL,
    obstime timestamp without time zone NOT NULL,
    memspan_ind smallint NOT NULL,
    numpairs double precision,
    sumgag real,
    sumrad real,
    bias real
);


ALTER TABLE public.rwbiasdyn OWNER TO awips;

--
-- TOC entry 1940 (class 1259 OID 19379)
-- Dependencies: 6
-- Name: rwbiasstat; Type: TABLE; Schema: public; Owner: awips; Tablespace: 
--

CREATE TABLE rwbiasstat (
    office_id character varying(5) NOT NULL,
    min_gr_value_bias real,
    npair_bias_select integer,
    npair_svar_update integer,
    std_cut integer,
    lag_cut integer,
    init_span integer,
    bias_qc_opt integer,
    num_span integer,
    mem_span1 real,
    mem_span2 real,
    mem_span3 real,
    mem_span4 real,
    mem_span5 real,
    mem_span6 real,
    mem_span7 real,
    mem_span8 real,
    mem_span9 real,
    mem_span10 real
);


ALTER TABLE public.rwbiasstat OWNER TO awips;

--
-- TOC entry 1941 (class 1259 OID 19382)
-- Dependencies: 6
-- Name: rwparams; Type: TABLE; Schema: public; Owner: awips; Tablespace: 
--

CREATE TABLE rwparams (
    rw_min_rain real,
    rw_sep_dist real,
    rw_lag0_ind_corr real,
    rw_lag0_cond_corr real,
    num_near_gages smallint,
    num_near_rad_bins smallint,
    def_cond_var_rad real,
    def_ind_corr_scl real,
    def_cond_corr_scl real,
    min_ind_corr_scl real,
    min_cond_corr_scl real,
    max_ind_corr_scl real,
    max_cond_corr_scl real,
    nn_srch_method smallint
);


ALTER TABLE public.rwparams OWNER TO awips;

--
-- TOC entry 1942 (class 1259 OID 19385)
-- Dependencies: 6
-- Name: rwprefs; Type: TABLE; Schema: public; Owner: awips; Tablespace: 
--

CREATE TABLE rwprefs (
    userid character varying(32) NOT NULL,
    state_overlay character varying(3),
    city_overlay character varying(3),
    county_overlay character varying(3),
    river_overlay character varying(3),
    basin_overlay character varying(3),
    radar_overlay character varying(3),
    num_hours_wind smallint,
    def_display_type character varying(10)
);


ALTER TABLE public.rwprefs OWNER TO awips;

--
-- TOC entry 1943 (class 1259 OID 19388)
-- Dependencies: 6
-- Name: rwradarresult; Type: TABLE; Schema: public; Owner: awips; Tablespace: 
--

CREATE TABLE rwradarresult (
    radid character varying(3) NOT NULL,
    obstime timestamp without time zone NOT NULL,
    num_gages smallint,
    rad_avail character varying(1),
    rw_bias_val_used double precision,
    mem_span_used double precision,
    edit_bias character varying(1),
    ignore_radar character varying(1)
);


ALTER TABLE public.rwradarresult OWNER TO awips;

--
-- TOC entry 1944 (class 1259 OID 19391)
-- Dependencies: 6
-- Name: rwresult; Type: TABLE; Schema: public; Owner: awips; Tablespace: 
--

CREATE TABLE rwresult (
    rfc character varying(8) NOT NULL,
    obstime timestamp without time zone NOT NULL,
    num_gag_avail smallint,
    num_rad_avail integer,
    num_pseudo_gages integer,
    sat_avail character varying(1),
    mapx_field_type character varying(10),
    draw_precip character varying(1),
    auto_save character varying(1),
    last_exec_time timestamp without time zone,
    last_save_time timestamp without time zone
);


ALTER TABLE public.rwresult OWNER TO awips;

--
-- TOC entry 1945 (class 1259 OID 19394)
-- Dependencies: 6
-- Name: s3postanalparams; Type: TABLE; Schema: public; Owner: awips; Tablespace: 
--

CREATE TABLE s3postanalparams (
    gg_weighting smallint,
    gg_min_gage_val real,
    gg_min_dist smallint,
    kernel_est_scale real,
    rhat real
);


ALTER TABLE public.s3postanalparams OWNER TO awips;

--
-- TOC entry 1946 (class 1259 OID 19397)
-- Dependencies: 6
-- Name: s3postanalprefs; Type: TABLE; Schema: public; Owner: awips; Tablespace: 
--

CREATE TABLE s3postanalprefs (
    userid character varying(32) NOT NULL,
    state_overlay character varying(3),
    city_overlay character varying(3),
    river_overlay character varying(3),
    basin_overlay character varying(3),
    radar_overlay character varying(3),
    num_hours_wind smallint
);


ALTER TABLE public.s3postanalprefs OWNER TO awips;

--
-- TOC entry 1947 (class 1259 OID 19400)
-- Dependencies: 6
-- Name: sacsmaparams; Type: TABLE; Schema: public; Owner: awips; Tablespace: 
--

CREATE TABLE sacsmaparams (
    basin_id character varying(8) NOT NULL,
    source character varying(20) NOT NULL,
    validtime timestamp without time zone NOT NULL,
    postingtime timestamp without time zone NOT NULL,
    uztwm double precision NOT NULL,
    uzfwm double precision NOT NULL,
    uzk double precision NOT NULL,
    pctim double precision NOT NULL,
    adimp double precision NOT NULL,
    riva double precision NOT NULL,
    zperc double precision NOT NULL,
    rexp double precision NOT NULL,
    lztwm double precision NOT NULL,
    lzfsm double precision NOT NULL,
    lzfpm double precision NOT NULL,
    lzsk double precision NOT NULL,
    lzpk double precision NOT NULL,
    pfree double precision NOT NULL,
    rserv double precision NOT NULL,
    side double precision NOT NULL,
    peadj double precision NOT NULL,
    pxadj double precision NOT NULL,
    efc double precision NOT NULL
);


ALTER TABLE public.sacsmaparams OWNER TO awips;

--
-- TOC entry 1948 (class 1259 OID 19403)
-- Dependencies: 6
-- Name: sacsmastate; Type: TABLE; Schema: public; Owner: awips; Tablespace: 
--

CREATE TABLE sacsmastate (
    basin_id character varying(8) NOT NULL,
    source character varying(20) NOT NULL,
    validtime timestamp without time zone NOT NULL,
    basistime timestamp without time zone NOT NULL,
    postingtime timestamp without time zone NOT NULL,
    uztwc double precision NOT NULL,
    uzfwc double precision NOT NULL,
    lztwc double precision NOT NULL,
    lzfsc double precision NOT NULL,
    lzfpc double precision NOT NULL,
    adimc double precision NOT NULL
);


ALTER TABLE public.sacsmastate OWNER TO awips;

--
-- TOC entry 1949 (class 1259 OID 19406)
-- Dependencies: 2083 6
-- Name: servicetableview; Type: VIEW; Schema: public; Owner: awips
--

CREATE VIEW servicetableview AS
    SELECT f.lid, l.name, r.stream, l.state, l.county, l.hsa FROM (fcstptservice f LEFT JOIN location l ON (((f.lid)::text = (l.lid)::text))), riverstat r WHERE ((f.lid)::text = (r.lid)::text);


ALTER TABLE public.servicetableview OWNER TO awips;

--
-- TOC entry 1950 (class 1259 OID 19411)
-- Dependencies: 6
-- Name: servicetype; Type: TABLE; Schema: public; Owner: awips; Tablespace: 
--

CREATE TABLE servicetype (
    service_type character varying(20) NOT NULL
);


ALTER TABLE public.servicetype OWNER TO awips;

--
-- TOC entry 1951 (class 1259 OID 19414)
-- Dependencies: 6
-- Name: shefdur; Type: TABLE; Schema: public; Owner: awips; Tablespace: 
--

CREATE TABLE shefdur (
    dur smallint NOT NULL,
    durcode character varying(1),
    name character varying(20)
);


ALTER TABLE public.shefdur OWNER TO awips;

--
-- TOC entry 1952 (class 1259 OID 19417)
-- Dependencies: 6
-- Name: shefex; Type: TABLE; Schema: public; Owner: awips; Tablespace: 
--

CREATE TABLE shefex (
    extremum character varying(1) NOT NULL,
    name character varying(20)
);


ALTER TABLE public.shefex OWNER TO awips;

--
-- TOC entry 1953 (class 1259 OID 19420)
-- Dependencies: 6
-- Name: shefpe; Type: TABLE; Schema: public; Owner: awips; Tablespace: 
--

CREATE TABLE shefpe (
    pe character varying(2) NOT NULL,
    name character varying(20),
    eng_unit character varying(8),
    met_unit character varying(8)
);


ALTER TABLE public.shefpe OWNER TO awips;

--
-- TOC entry 1954 (class 1259 OID 19423)
-- Dependencies: 6
-- Name: shefpetrans; Type: TABLE; Schema: public; Owner: awips; Tablespace: 
--

CREATE TABLE shefpetrans (
    pe character varying(3) NOT NULL,
    coded_value integer NOT NULL,
    value_trans character varying(80)
);


ALTER TABLE public.shefpetrans OWNER TO awips;

--
-- TOC entry 1955 (class 1259 OID 19426)
-- Dependencies: 6
-- Name: shefprob; Type: TABLE; Schema: public; Owner: awips; Tablespace: 
--

CREATE TABLE shefprob (
    probcode character varying(1) NOT NULL,
    probability real,
    name character varying(20)
);


ALTER TABLE public.shefprob OWNER TO awips;

--
-- TOC entry 1956 (class 1259 OID 19429)
-- Dependencies: 6
-- Name: shefqc; Type: TABLE; Schema: public; Owner: awips; Tablespace: 
--

CREATE TABLE shefqc (
    shef_qual_code character varying(1) NOT NULL,
    name character varying(20)
);


ALTER TABLE public.shefqc OWNER TO awips;

--
-- TOC entry 1957 (class 1259 OID 19432)
-- Dependencies: 6
-- Name: shefts; Type: TABLE; Schema: public; Owner: awips; Tablespace: 
--

CREATE TABLE shefts (
    ts character varying(2) NOT NULL,
    name character varying(20)
);


ALTER TABLE public.shefts OWNER TO awips;

--
-- TOC entry 1958 (class 1259 OID 19435)
-- Dependencies: 6
-- Name: snow; Type: TABLE; Schema: public; Owner: awips; Tablespace: 
--

CREATE TABLE snow (
    lid character varying(8) NOT NULL,
    pe character varying(2) NOT NULL,
    dur smallint NOT NULL,
    ts character varying(2) NOT NULL,
    extremum character varying(1) NOT NULL,
    obstime timestamp without time zone NOT NULL,
    value double precision,
    shef_qual_code character varying(1),
    quality_code integer,
    revision smallint,
    product_id character varying(10),
    producttime timestamp without time zone,
    postingtime timestamp without time zone
);


ALTER TABLE public.snow OWNER TO awips;

--
-- TOC entry 1959 (class 1259 OID 19438)
-- Dependencies: 6
-- Name: snowmethod; Type: TABLE; Schema: public; Owner: awips; Tablespace: 
--

CREATE TABLE snowmethod (
    snow_method character varying(30) NOT NULL
);


ALTER TABLE public.snowmethod OWNER TO awips;

--
-- TOC entry 1960 (class 1259 OID 19441)
-- Dependencies: 6
-- Name: sshpconfig; Type: TABLE; Schema: public; Owner: awips; Tablespace: 
--

CREATE TABLE sshpconfig (
    lid character varying(8) NOT NULL,
    basin_id character varying(8) NOT NULL,
    postingtime timestamp without time zone NOT NULL,
    model_pref character varying(10) NOT NULL,
    auto_process character varying(1) NOT NULL,
    source_pref character varying(20) NOT NULL,
    use_static_evap character varying(1) NOT NULL,
    use_blend character varying(1) NOT NULL,
    blend_method character varying(10) NOT NULL,
    blend_hours integer NOT NULL
);


ALTER TABLE public.sshpconfig OWNER TO awips;

--
-- TOC entry 1961 (class 1259 OID 19444)
-- Dependencies: 6
-- Name: state; Type: TABLE; Schema: public; Owner: awips; Tablespace: 
--

CREATE TABLE state (
    state character varying(2) NOT NULL,
    name character varying(20)
);


ALTER TABLE public.state OWNER TO awips;

--
-- TOC entry 1962 (class 1259 OID 19447)
-- Dependencies: 2084 6
-- Name: stationlist; Type: VIEW; Schema: public; Owner: awips
--

CREATE VIEW stationlist AS
    SELECT l.lid, l.name, o.firstname, o.lastname, l.rb, l.county, l.wfo, o.hphone, o.phone AS ophone FROM (location l LEFT JOIN observer o ON (((l.lid)::text = (o.lid)::text)));


ALTER TABLE public.stationlist OWNER TO awips;

--
-- TOC entry 1963 (class 1259 OID 19452)
-- Dependencies: 2085 6
-- Name: statprof; Type: VIEW; Schema: public; Owner: awips
--

CREATE VIEW statprof AS
    SELECT l.lid, l.name, r.primary_pe, r.stream, r.fs, r.wstg, r.fq, r.action_flow, r.zd, r.mile, d.reach, d.proximity FROM location l, (riverstat r LEFT JOIN descrip d ON (((r.lid)::text = (d.lid)::text))) WHERE (((((l.lid)::text = (r.lid)::text) AND (((l.type)::text !~~ '%I%'::text) OR (l.type IS NULL))) AND (r.zd IS NOT NULL)) AND (r.mile IS NOT NULL));


ALTER TABLE public.statprof OWNER TO awips;

--
-- TOC entry 1964 (class 1259 OID 19457)
-- Dependencies: 6
-- Name: stuff1; Type: TABLE; Schema: public; Owner: awips; Tablespace: 
--

CREATE TABLE stuff1 (
    lid character varying(8),
    basistime timestamp without time zone,
    validtime timestamp without time zone,
    value double precision
);


ALTER TABLE public.stuff1 OWNER TO awips;

--
-- TOC entry 1965 (class 1259 OID 19460)
-- Dependencies: 6
-- Name: stuff2; Type: TABLE; Schema: public; Owner: awips; Tablespace: 
--

CREATE TABLE stuff2 (
    lid character varying(8),
    minor_stage double precision,
    basistime timestamp without time zone,
    validtime timestamp without time zone,
    value double precision
);


ALTER TABLE public.stuff2 OWNER TO awips;

--
-- TOC entry 1966 (class 1259 OID 19463)
-- Dependencies: 6
-- Name: telem; Type: TABLE; Schema: public; Owner: awips; Tablespace: 
--

CREATE TABLE telem (
    lid character varying(8) NOT NULL,
    "type" character varying(10),
    payor character varying(10),
    cost double precision,
    criteria character varying(50),
    "owner" character varying(10),
    phone character varying(12),
    sensorid character varying(10),
    rptfreq character varying(4),
    "notify" character varying(1)
);


ALTER TABLE public.telem OWNER TO awips;

--
-- TOC entry 1967 (class 1259 OID 19466)
-- Dependencies: 6
-- Name: telmowner; Type: TABLE; Schema: public; Owner: awips; Tablespace: 
--

CREATE TABLE telmowner (
    "owner" character varying(10) NOT NULL
);


ALTER TABLE public.telmowner OWNER TO awips;

--
-- TOC entry 1968 (class 1259 OID 19469)
-- Dependencies: 6
-- Name: telmpayor; Type: TABLE; Schema: public; Owner: awips; Tablespace: 
--

CREATE TABLE telmpayor (
    payor character varying(10) NOT NULL
);


ALTER TABLE public.telmpayor OWNER TO awips;

--
-- TOC entry 1969 (class 1259 OID 19472)
-- Dependencies: 6
-- Name: telmtype; Type: TABLE; Schema: public; Owner: awips; Tablespace: 
--

CREATE TABLE telmtype (
    "type" character varying(10) NOT NULL
);


ALTER TABLE public.telmtype OWNER TO awips;

--
-- TOC entry 1970 (class 1259 OID 19475)
-- Dependencies: 6
-- Name: temperature; Type: TABLE; Schema: public; Owner: awips; Tablespace: 
--

CREATE TABLE temperature (
    lid character varying(8) NOT NULL,
    pe character varying(2) NOT NULL,
    dur smallint NOT NULL,
    ts character varying(2) NOT NULL,
    extremum character varying(1) NOT NULL,
    obstime timestamp without time zone NOT NULL,
    value double precision,
    shef_qual_code character varying(1),
    quality_code integer,
    revision smallint,
    product_id character varying(10),
    producttime timestamp without time zone,
    postingtime timestamp without time zone
);


ALTER TABLE public.temperature OWNER TO awips;

--
-- TOC entry 1971 (class 1259 OID 19478)
-- Dependencies: 6
-- Name: timezone; Type: TABLE; Schema: public; Owner: awips; Tablespace: 
--

CREATE TABLE timezone (
    tzone character varying(8) NOT NULL,
    name character varying(30)
);


ALTER TABLE public.timezone OWNER TO awips;

--
-- TOC entry 1972 (class 1259 OID 19481)
-- Dependencies: 6
-- Name: unitgraph; Type: TABLE; Schema: public; Owner: awips; Tablespace: 
--

CREATE TABLE unitgraph (
    lid character varying(8) NOT NULL,
    area_id character varying(8) NOT NULL,
    model character varying(10) NOT NULL,
    dur integer NOT NULL,
    ordinal integer NOT NULL,
    discharge double precision NOT NULL
);


ALTER TABLE public.unitgraph OWNER TO awips;

--
-- TOC entry 1973 (class 1259 OID 19484)
-- Dependencies: 6
-- Name: unkstn; Type: TABLE; Schema: public; Owner: awips; Tablespace: 
--

CREATE TABLE unkstn (
    lid character varying(8) NOT NULL,
    product_id character varying(10),
    producttime timestamp without time zone,
    postingtime timestamp without time zone
);


ALTER TABLE public.unkstn OWNER TO awips;

--
-- TOC entry 1974 (class 1259 OID 19487)
-- Dependencies: 6
-- Name: unkstnvalue; Type: TABLE; Schema: public; Owner: awips; Tablespace: 
--

CREATE TABLE unkstnvalue (
    lid character varying(8) NOT NULL,
    pe character varying(2) NOT NULL,
    dur smallint NOT NULL,
    ts character varying(2) NOT NULL,
    extremum character varying(1) NOT NULL,
    obstime timestamp without time zone NOT NULL,
    value double precision,
    revision smallint,
    shef_qual_code character varying(1),
    product_id character varying(10),
    producttime timestamp without time zone,
    postingtime timestamp without time zone
);


ALTER TABLE public.unkstnvalue OWNER TO awips;

--
-- TOC entry 1975 (class 1259 OID 19490)
-- Dependencies: 6
-- Name: userprefs; Type: TABLE; Schema: public; Owner: awips; Tablespace: 
--

CREATE TABLE userprefs (
    userid character varying(32) NOT NULL,
    title integer,
    statlist integer,
    sortlist integer,
    fieldlist integer
);


ALTER TABLE public.userprefs OWNER TO awips;

--
-- TOC entry 1976 (class 1259 OID 19493)
-- Dependencies: 2086 6
-- Name: view_akf; Type: VIEW; Schema: public; Owner: awips
--

CREATE VIEW view_akf AS
    SELECT crest.lid, max(crest.stage) AS maxstg FROM crest crest WHERE ((crest.lid)::text ~~ 'B%'::text) GROUP BY crest.lid ORDER BY crest.lid;


ALTER TABLE public.view_akf OWNER TO awips;

--
-- TOC entry 1977 (class 1259 OID 19497)
-- Dependencies: 6
-- Name: vtecaction; Type: TABLE; Schema: public; Owner: awips; Tablespace: 
--

CREATE TABLE vtecaction (
    "action" character varying(3) NOT NULL,
    name character varying(25)
);


ALTER TABLE public.vtecaction OWNER TO awips;

--
-- TOC entry 1978 (class 1259 OID 19500)
-- Dependencies: 6
-- Name: vteccause; Type: TABLE; Schema: public; Owner: awips; Tablespace: 
--

CREATE TABLE vteccause (
    immed_cause character varying(2) NOT NULL,
    name character varying(25)
);


ALTER TABLE public.vteccause OWNER TO awips;

--
-- TOC entry 1979 (class 1259 OID 19503)
-- Dependencies: 6
-- Name: vtecevent; Type: TABLE; Schema: public; Owner: awips; Tablespace: 
--

CREATE TABLE vtecevent (
    geoid character varying(24) NOT NULL,
    product_id character varying(10) NOT NULL,
    producttime timestamp without time zone NOT NULL,
    productmode character varying(1),
    "action" character varying(3),
    office_id character varying(5),
    phenom character varying(2),
    signif character varying(1),
    etn smallint,
    begintime timestamp without time zone,
    endtime timestamp without time zone,
    severity character varying(1),
    immed_cause character varying(2),
    risetime timestamp without time zone,
    cresttime timestamp without time zone,
    falltime timestamp without time zone,
    record character varying(2),
    risets character varying(2),
    crests character varying(2),
    fallts character varying(2),
    crest_value double precision,
    expiretime timestamp without time zone
);


ALTER TABLE public.vtecevent OWNER TO awips;

--
-- TOC entry 1980 (class 1259 OID 19506)
-- Dependencies: 6
-- Name: vtecphenom; Type: TABLE; Schema: public; Owner: awips; Tablespace: 
--

CREATE TABLE vtecphenom (
    phenom character varying(2) NOT NULL,
    name character varying(25)
);


ALTER TABLE public.vtecphenom OWNER TO awips;

--
-- TOC entry 1981 (class 1259 OID 19509)
-- Dependencies: 6
-- Name: vtecpractice; Type: TABLE; Schema: public; Owner: awips; Tablespace: 
--

CREATE TABLE vtecpractice (
    geoid character varying(24) NOT NULL,
    product_id character varying(10) NOT NULL,
    producttime timestamp without time zone NOT NULL,
    productmode character varying(1),
    "action" character varying(3),
    office_id character varying(5),
    phenom character varying(2),
    signif character varying(1),
    etn smallint,
    begintime timestamp without time zone,
    endtime timestamp without time zone,
    severity character varying(1),
    immed_cause character varying(2),
    risetime timestamp without time zone,
    cresttime timestamp without time zone,
    falltime timestamp without time zone,
    record character varying(2),
    risets character varying(2),
    crests character varying(2),
    fallts character varying(2),
    crest_value double precision,
    expiretime timestamp without time zone
);


ALTER TABLE public.vtecpractice OWNER TO awips;

--
-- TOC entry 1982 (class 1259 OID 19512)
-- Dependencies: 6
-- Name: vtecrecord; Type: TABLE; Schema: public; Owner: awips; Tablespace: 
--

CREATE TABLE vtecrecord (
    record character varying(2) NOT NULL,
    name character varying(25)
);


ALTER TABLE public.vtecrecord OWNER TO awips;

--
-- TOC entry 1983 (class 1259 OID 19515)
-- Dependencies: 6
-- Name: vtecsever; Type: TABLE; Schema: public; Owner: awips; Tablespace: 
--

CREATE TABLE vtecsever (
    severity character varying(1) NOT NULL,
    name character varying(25)
);


ALTER TABLE public.vtecsever OWNER TO awips;

--
-- TOC entry 1984 (class 1259 OID 19518)
-- Dependencies: 6
-- Name: vtecsignif; Type: TABLE; Schema: public; Owner: awips; Tablespace: 
--

CREATE TABLE vtecsignif (
    signif character varying(1) NOT NULL,
    name character varying(25)
);


ALTER TABLE public.vtecsignif OWNER TO awips;

--
-- TOC entry 1985 (class 1259 OID 19521)
-- Dependencies: 6
-- Name: waterquality; Type: TABLE; Schema: public; Owner: awips; Tablespace: 
--

CREATE TABLE waterquality (
    lid character varying(8) NOT NULL,
    pe character varying(2) NOT NULL,
    dur smallint NOT NULL,
    ts character varying(2) NOT NULL,
    extremum character varying(1) NOT NULL,
    obstime timestamp without time zone NOT NULL,
    value double precision,
    shef_qual_code character varying(1),
    quality_code integer,
    revision smallint,
    product_id character varying(10),
    producttime timestamp without time zone,
    postingtime timestamp without time zone
);


ALTER TABLE public.waterquality OWNER TO awips;

--
-- TOC entry 1986 (class 1259 OID 19524)
-- Dependencies: 6
-- Name: watsupcoordagency; Type: TABLE; Schema: public; Owner: awips; Tablespace: 
--

CREATE TABLE watsupcoordagency (
    watsup_coord_agency character varying(64) NOT NULL
);


ALTER TABLE public.watsupcoordagency OWNER TO awips;

--
-- TOC entry 1987 (class 1259 OID 19527)
-- Dependencies: 6
-- Name: watsupcriterion; Type: TABLE; Schema: public; Owner: awips; Tablespace: 
--

CREATE TABLE watsupcriterion (
    watsup_criterion character varying(30) NOT NULL
);


ALTER TABLE public.watsupcriterion OWNER TO awips;

--
-- TOC entry 1988 (class 1259 OID 19530)
-- Dependencies: 6
-- Name: watsupmethod; Type: TABLE; Schema: public; Owner: awips; Tablespace: 
--

CREATE TABLE watsupmethod (
    watsup_method character varying(50) NOT NULL
);


ALTER TABLE public.watsupmethod OWNER TO awips;

--
-- TOC entry 1989 (class 1259 OID 19533)
-- Dependencies: 6
-- Name: watsuprespagency; Type: TABLE; Schema: public; Owner: awips; Tablespace: 
--

CREATE TABLE watsuprespagency (
    watsup_resp_agency character varying(64) NOT NULL
);


ALTER TABLE public.watsuprespagency OWNER TO awips;

--
-- TOC entry 1990 (class 1259 OID 19536)
-- Dependencies: 6
-- Name: weather; Type: TABLE; Schema: public; Owner: awips; Tablespace: 
--

CREATE TABLE weather (
    lid character varying(8) NOT NULL,
    pe character varying(2) NOT NULL,
    dur smallint NOT NULL,
    ts character varying(2) NOT NULL,
    extremum character varying(1) NOT NULL,
    obstime timestamp without time zone NOT NULL,
    value double precision,
    shef_qual_code character varying(1),
    quality_code integer,
    revision smallint,
    product_id character varying(10),
    producttime timestamp without time zone,
    postingtime timestamp without time zone
);


ALTER TABLE public.weather OWNER TO awips;

--
-- TOC entry 1991 (class 1259 OID 19539)
-- Dependencies: 6
-- Name: wfo; Type: TABLE; Schema: public; Owner: awips; Tablespace: 
--

CREATE TABLE wfo (
    wfo character varying(3) NOT NULL
);


ALTER TABLE public.wfo OWNER TO awips;

--
-- TOC entry 1992 (class 1259 OID 19542)
-- Dependencies: 6
-- Name: wind; Type: TABLE; Schema: public; Owner: awips; Tablespace: 
--

CREATE TABLE wind (
    lid character varying(8) NOT NULL,
    pe character varying(2) NOT NULL,
    dur smallint NOT NULL,
    ts character varying(2) NOT NULL,
    extremum character varying(1) NOT NULL,
    obstime timestamp without time zone NOT NULL,
    value double precision,
    shef_qual_code character varying(1),
    quality_code integer,
    revision smallint,
    product_id character varying(10),
    producttime timestamp without time zone,
    postingtime timestamp without time zone
);


ALTER TABLE public.wind OWNER TO awips;

--
-- TOC entry 1993 (class 1259 OID 19545)
-- Dependencies: 6
-- Name: yunique; Type: TABLE; Schema: public; Owner: awips; Tablespace: 
--

CREATE TABLE yunique (
    lid character varying(8) NOT NULL,
    pe character varying(2) NOT NULL,
    dur smallint NOT NULL,
    ts character varying(2) NOT NULL,
    extremum character varying(1) NOT NULL,
    obstime timestamp without time zone NOT NULL,
    value double precision,
    shef_qual_code character varying(1),
    quality_code integer,
    revision smallint,
    product_id character varying(10),
    producttime timestamp without time zone,
    postingtime timestamp without time zone
);


ALTER TABLE public.yunique OWNER TO awips;

--
-- TOC entry 1994 (class 1259 OID 19548)
-- Dependencies: 6
-- Name: zonenum; Type: TABLE; Schema: public; Owner: awips; Tablespace: 
--

CREATE TABLE zonenum (
    lid character varying(8) NOT NULL,
    state character varying(2) NOT NULL,
    zonenum character varying(3) NOT NULL
);


ALTER TABLE public.zonenum OWNER TO awips;

CREATE TABLE eligzon_location (eligzon_state varchar(2) not null, eligzon_zonenum varchar(3) not null, locations_lid varchar(8) not null, primary key (eligzon_state, eligzon_zonenum, locations_lid));

ALTER TABLE public.eligzon_location OWNER TO awips;

CREATE TABLE counties_location (counties_county varchar(20) not null, counties_state varchar(2) not null, locations_1_lid varchar(8) not null, primary key (counties_county, counties_state, locations_1_lid));

ALTER TABLE public.counties_location OWNER TO awips;

CREATE TABLE counties_nwrtransmitter (counties_county varchar(20) not null, counties_state varchar(2) not null, nwrtransmitters_call_sign varchar(6) not null, primary key (counties_county, counties_state, nwrtransmitters_call_sign));

ALTER TABLE public.counties_nwrtransmitter OWNER TO awips;

--
-- TOC entry 1995 (class 1259 OID 19551)
-- Dependencies: 2087 6
-- Name: zoneinfo; Type: VIEW; Schema: public; Owner: awips
--

CREATE VIEW zoneinfo AS
    SELECT z.lid, z.state, z.zonenum, e.descr FROM zonenum z, eligzon e WHERE (((z.state)::text = (e.state)::text) AND ((z.zonenum)::text = (e.zonenum)::text));


ALTER TABLE public.zoneinfo OWNER TO awips;

--
-- TOC entry 20 (class 1255 OID 18883)
-- Dependencies: 6 723
-- Name: delete_location(character varying); Type: FUNCTION; Schema: public; Owner: awips
--

CREATE FUNCTION delete_location(character varying) RETURNS integer
    AS $_$
        DECLARE postlid ALIAS FOR $1;
                iret_code INT;
BEGIN
  	--
  	-- Delete riverstation records
  	--
  	iret_code := delete_riverstat(postlid);

        --
        -- Delete reservoir related entries
        --
	iret_code := delete_resvr(postlid);

	--
        -- Delete qc related records
        --
        DELETE FROM  IngestFilter    WHERE lid = postlid;
 	DELETE FROM  LocDataLimits   WHERE lid = postlid;
	DELETE FROM  RejectedData    WHERE lid = postlid;
	DELETE FROM  AlertAlarmVal   WHERE lid = postlid;
	DELETE FROM  AdjustFactor    WHERE lid = postlid;

	--
	-- Delete location related entries
	--
        DELETE FROM  Contacts        WHERE lid = postlid;
        DELETE FROM  Dcp             WHERE lid = postlid;
        DELETE FROM  Observer        WHERE lid = postlid;
        DELETE FROM  Telem           WHERE lid = postlid;
        DELETE FROM  Gage            WHERE lid = postlid;
 
	DELETE FROM  ContingencyValue WHERE lid = postlid;
        DELETE FROM  FcstDischarge   WHERE lid = postlid;
        DELETE FROM  FcstHeight      WHERE lid = postlid;
        DELETE FROM  FcstPrecip      WHERE lid = postlid;
        DELETE FROM  FcstTemperature WHERE lid = postlid;
        DELETE FROM  FcstOther       WHERE lid = postlid;
 	DELETE FROM  ProcValue       WHERE lid = postlid;
 	DELETE FROM  CommentValue    WHERE lid = postlid;
  	DELETE FROM  PairedValue     WHERE lid = postlid;
	DELETE FROM  UnkStnValue     WHERE lid = postlid;
 	DELETE FROM  UnkStn          WHERE lid = postlid;

        DELETE FROM  LatestObsValue  WHERE lid = postlid;
        DELETE FROM  RiverStatus     WHERE lid = postlid;
        DELETE FROM  RiverMonLocation     WHERE lid = postlid;

        DELETE FROM  Agricultural    WHERE lid = postlid;
        DELETE FROM  Discharge       WHERE lid = postlid;
        DELETE FROM  Evaporation     WHERE lid = postlid;
        DELETE FROM  FishCount       WHERE lid = postlid;
        DELETE FROM  FloodTs         WHERE lid = postlid;
	DELETE FROM  GateDam         WHERE lid = postlid;
        DELETE FROM  Ground          WHERE lid = postlid;
        DELETE FROM  Height          WHERE lid = postlid;
	DELETE FROM  Ice             WHERE lid = postlid;
        DELETE FROM  Lake            WHERE lid = postlid;
        DELETE FROM  Moisture        WHERE lid = postlid;
        DELETE FROM  Power           WHERE lid = postlid;
	DELETE FROM  Pressure        WHERE lid = postlid;
	DELETE FROM  Radiation       WHERE lid = postlid;
        DELETE FROM  Snow            WHERE lid = postlid;
        DELETE FROM  Temperature     WHERE lid = postlid;
        DELETE FROM  WaterQuality    WHERE lid = postlid;
        DELETE FROM  Weather         WHERE lid = postlid;
        DELETE FROM  Wind            WHERE lid = postlid;
        DELETE FROM  YUnique         WHERE lid = postlid;
 
        DELETE FROM  ArealFcst       WHERE lid = postlid;
        DELETE FROM  ArealObs        WHERE lid = postlid;

        DELETE FROM  MonthlyValues   WHERE lid = postlid;

        DELETE FROM  CurPC           WHERE lid = postlid;
        DELETE FROM  CurPP           WHERE lid = postlid; 
        DELETE FROM  HourlyPC        WHERE lid = postlid;
        DELETE FROM  HourlyPP        WHERE lid = postlid;
        DELETE FROM  RawPC           WHERE lid = postlid;
        DELETE FROM  RawPP           WHERE lid = postlid;
        DELETE FROM  RawPother       WHERE lid = postlid;
        DELETE FROM  DailyPP         WHERE lid = postlid;

	DELETE FROM  LocArea         WHERE lid = postlid;
        DELETE FROM  LocExtAgency    WHERE lid = postlid;
	DELETE FROM  LocImage        WHERE lid = postlid;
        DELETE FROM  OFSStnTrans     WHERE lid = postlid;
	DELETE FROM  ProductLink     WHERE lid = postlid;
 
 	DELETE FROM  StnClass 	     WHERE lid = postlid;

        -- delete from FcstPt tables where lid is an "upstream segment" point

        DELETE FROM  FcstPtDeterm    WHERE upstream_seg = postlid;
        DELETE FROM  FcstPtESP       WHERE upstream_seg = postlid;

        DELETE FROM  Location        WHERE lid = postlid;
 
RETURN 0;
END;
$_$
    LANGUAGE plpgsql;


ALTER FUNCTION public.delete_location(character varying) OWNER TO awips;

--
-- TOC entry 21 (class 1255 OID 18884)
-- Dependencies: 6 723
-- Name: delete_radar(character); Type: FUNCTION; Schema: public; Owner: awips
--

CREATE FUNCTION delete_radar(character) RETURNS integer
    AS $_$
DECLARE delradid ALIAS FOR $1;
BEGIN
	DELETE FROM   DHRAdapt		  WHERE radid = delradid;
	DELETE FROM   DSPAdapt		  WHERE radid = delradid;
	DELETE FROM   DHRRadar		  WHERE radid = delradid;
	DELETE FROM   DSPRadar		  WHERE radid = delradid;
	DELETE FROM   DpaAdapt		  WHERE radid = delradid;
	DELETE FROM   RWRadarResult       WHERE radid = delradid;
	DELETE FROM   RWBiasDyn           WHERE radid = delradid;
	DELETE FROM   DPARadar            WHERE radid = delradid;
	DELETE FROM   RadarLoc            WHERE radid = delradid;
RETURN 0;
END;
$_$
    LANGUAGE plpgsql;


ALTER FUNCTION public.delete_radar(character) OWNER TO awips;

--
-- TOC entry 22 (class 1255 OID 18885)
-- Dependencies: 6 723
-- Name: delete_resvr(character varying); Type: FUNCTION; Schema: public; Owner: awips
--

CREATE FUNCTION delete_resvr(character varying) RETURNS integer
    AS $_$
DECLARE postlid ALIAS FOR $1;
BEGIN
	DELETE FROM rescap    WHERE lid = postlid;
	DELETE FROM reservoir WHERE lid = postlid;
RETURN 0;
END;
$_$
    LANGUAGE plpgsql;


ALTER FUNCTION public.delete_resvr(character varying) OWNER TO awips;

--
-- TOC entry 23 (class 1255 OID 18886)
-- Dependencies: 6 723
-- Name: delete_riverstat(character varying); Type: FUNCTION; Schema: public; Owner: awips
--

CREATE FUNCTION delete_riverstat(character varying) RETURNS integer
    AS $_$
DECLARE postlid ALIAS FOR $1;
BEGIN
        -- 
        -- Delete riverstation related records from tables                    
        --  including Fcst Point services tables
        -- 
	DELETE FROM  Hgstation    WHERE lid = postlid;
        DELETE FROM  Benchmark    WHERE lid = postlid; 
        DELETE FROM  Countynum    WHERE lid = postlid; 
        DELETE FROM  Crest        WHERE lid = postlid; 
        DELETE FROM  Datum        WHERE lid = postlid; 
        DELETE FROM  Descrip      WHERE lid = postlid; 
	DELETE FROM  Flood        WHERE lid = postlid; 
        DELETE FROM  Floodcat     WHERE lid = postlid; 
        DELETE FROM  Floodstmt    WHERE lid = postlid; 
        DELETE FROM  FpPrevProd   WHERE lid = postlid; 
        DELETE FROM  Lowwater     WHERE lid = postlid; 
        DELETE FROM  LWstmt       WHERE lid = postlid; 
        DELETE FROM  Pub          WHERE lid = postlid; 
        DELETE FROM  Rating       WHERE lid = postlid; 
        DELETE FROM  RatingShift  WHERE lid = postlid; 
        DELETE FROM  Refer        WHERE lid = postlid; 
        DELETE FROM  RpfFcstPoint WHERE lid = postlid;
        DELETE FROM  SSHPConfig   WHERE lid = postlid;
        DELETE FROM  UnitGraph    WHERE lid = postlid;
	DELETE FROM  Zonenum      WHERE lid = postlid; 

        DELETE FROM  FcstPtWatSup    WHERE lid = postlid;
        DELETE FROM  FcstPtDeterm    WHERE lid = postlid;
        DELETE FROM  FcstPtESP       WHERE lid = postlid;
        DELETE FROM  FcstPtService   WHERE lid = postlid;

        DELETE FROM  Riverstat    WHERE lid = postlid; 
RETURN 0;
END;
$_$
    LANGUAGE plpgsql;


ALTER FUNCTION public.delete_riverstat(character varying) OWNER TO awips;

--
-- TOC entry 24 (class 1255 OID 18887)
-- Dependencies: 6 723
-- Name: fcst_pe(character varying, character varying, integer, character varying, character varying, real, timestamp without time zone, timestamp without time zone, double precision, character varying, integer, integer, character varying, timestamp without time zone, timestamp without time zone, integer); Type: FUNCTION; Schema: public; Owner: awips
--

CREATE FUNCTION fcst_pe(character varying, character varying, integer, character varying, character varying, real, timestamp without time zone, timestamp without time zone, double precision, character varying, integer, integer, character varying, timestamp without time zone, timestamp without time zone, integer) RETURNS integer
    AS $_$
        DECLARE fcst_record     fcstheight%ROWTYPE;
		newlid          ALIAS FOR   $1;	
		newpe           ALIAS FOR   $2;
		newdur          ALIAS FOR   $3;
		newts           ALIAS FOR   $4;
		newextr         ALIAS FOR   $5;
		newprob         ALIAS FOR   $6;
		newvalidtime    ALIAS FOR   $7;
		newbasistime    ALIAS FOR   $8;
		newvalue        ALIAS FOR   $9;
		newrawqual      ALIAS FOR   $10;
		newqc           ALIAS FOR   $11;
		newrev	        ALIAS FOR   $12;
		newprid         ALIAS FOR   $13;
		newprodtime     ALIAS FOR   $14;
		newposttime     ALIAS FOR   $15;
		do_update       ALIAS FOR   $16;
                conf_update     integer;

BEGIN
    conf_update := 0;
------------------------------------------------------
--Post forecast height data
--
  if newpe like 'H%' then
     SELECT INTO fcst_record * FROM FcstHeight
     WHERE lid = newlid
     AND   pe = newpe
     AND   dur = newdur
     AND   ts = newts
     AND   extremum = newextr
     AND   probability  = newprob
     AND   basistime = newbasistime
     AND   validtime = newvalidtime;

     if FOUND then
        if fcst_record.value = -9999. then
           conf_update := 1;
        else
           if do_update = 2 then
              if fcst_record.value <> newvalue then
                 conf_update := 1;
              end if;
           else
              if do_update = 1 then
                 conf_update := 1;
              end if;
           end if;
        end if;
        
        if conf_update = 1 then
           UPDATE FcstHeight SET
	   value          = newvalue,
	   shef_qual_code = newrawqual,
	   quality_code   = newqc,
	   revision       = newrev,
	   product_id     = newprid,
	   producttime    = newprodtime,
	   postingtime    = newposttime
	   WHERE lid = newlid and pe  = newpe and
	   dur = newdur and ts  = newts and
	   extremum     = newextr and
	   probability  = newprob and
	   validtime    = newvalidtime and
	   basistime    = newbasistime;
        end if;
     else
	INSERT INTO FcstHeight
	VALUES(newlid, newpe, newdur, newts, newextr, newprob, newvalidtime,
        newbasistime, newvalue, newrawqual, newqc, newrev, newprid, newprodtime,
        newposttime);				
     end if;

--
--Process forecast precip data
--
  elsif newpe like 'P%' then
        SELECT INTO fcst_record * FROM FcstPrecip 
        WHERE lid = newlid
        AND   pe = newpe
        AND   dur = newdur
        AND   ts = newts
        AND   extremum = newextr
        AND   probability  = newprob
        AND   basistime = newbasistime
        AND   validtime = newvalidtime;

        if FOUND then
           if fcst_record.value = -9999. then
              conf_update := 1;
           else
              if do_update = 2 then
                 if fcst_record.value <> newvalue then 
                    conf_update := 1;
                 end if;
              else
                 if do_update = 1 then
                    conf_update := 1;
                 end if;
              end if;
           end if;

           if conf_update = 1 then
              UPDATE FcstPrecip SET 
              value          = newvalue,
              shef_qual_code = newrawqual,
              quality_code   = newqc,
              revision       = newrev,
              product_id     = newprid,
              producttime    = newprodtime,
              postingtime    = newposttime
              WHERE lid = newlid and pe  = newpe and
              dur = newdur and ts  = newts and
              extremum     = newextr and
              probability  = newprob and
              validtime    = newvalidtime and
              basistime    = newbasistime;
           end if;
        else
           INSERT INTO FcstPrecip
           VALUES( newlid, newpe, newdur, newts, newextr, newprob, newvalidtime,
           newbasistime,newvalue, newrawqual, newqc, newrev,newprid,newprodtime,
           newposttime);
        end if;

--
--Process forecast discharge data
--
  elsif newpe like 'Q%' then
        SELECT INTO fcst_record * FROM FcstDischarge
        WHERE lid = newlid
        AND   pe = newpe
        AND   dur = newdur
        AND   ts = newts
        AND   extremum = newextr
        AND   probability  = newprob
        AND   basistime = newbasistime
        AND   validtime = newvalidtime;

        if FOUND then
           if fcst_record.value = -9999. then
              conf_update := 1;
           else
              if do_update = 2 then
                 if fcst_record.value <> newvalue then
                    conf_update := 1;
                 end if;
              else
                 if do_update = 1 then
                    conf_update := 1;
                 end if;
              end if;
           end if;
           
           if conf_update = 1 then
              UPDATE FcstDischarge SET
              value          = newvalue,
              shef_qual_code = newrawqual,
              quality_code   = newqc,
              revision       = newrev,
              product_id     = newprid,
              producttime    = newprodtime,
              postingtime    = newposttime
              WHERE lid = newlid and pe  = newpe and
              dur = newdur and ts  = newts and
              extremum     = newextr and
              probability  = newprob and
              validtime    = newvalidtime and
              basistime    = newbasistime;
           end if;
        else
           INSERT INTO FcstDischarge
           VALUES( newlid, newpe, newdur, newts, newextr, newprob, newvalidtime,
           newbasistime,newvalue,newrawqual, newqc, newrev, newprid,newprodtime,
           newposttime);
        end if;

--
--Process forecast temperature amounts
--
  elsif newpe like 'T%' then
        SELECT INTO fcst_record * FROM FcstTemperature
        WHERE lid = newlid
        AND   pe = newpe
        AND   dur = newdur
        AND   ts = newts
        AND   extremum = newextr
        AND   probability  = newprob
        AND   basistime = newbasistime
        AND   validtime = newvalidtime;

        if FOUND then
           if fcst_record.value = -9999. then
              conf_update := 1;
           else
              if do_update = 2 then
                 if fcst_record.value <> newvalue then 
                    conf_update := 1;
                 end if;
              else
                 if do_update = 1 then
                    conf_update := 1;
                 end if;
              end if;
           end if;

           if conf_update = 1 then
              UPDATE FcstTemperature SET
              value          = newvalue,
              shef_qual_code = newrawqual,
              quality_code   = newqc,
              revision       = newrev,
              product_id     = newprid,
              producttime    = newprodtime,
              postingtime    = newposttime
              WHERE lid = newlid and pe  = newpe and
              dur = newdur and ts  = newts and
              extremum     = newextr and
              probability  = newprob and
              validtime    = newvalidtime and
              basistime    = newbasistime;
           end if;
        else
           INSERT INTO FcstTemperature
           VALUES( newlid, newpe, newdur, newts, newextr, newprob, newvalidtime,
           newbasistime,newvalue,newrawqual,newqc, newrev, newprid, newprodtime,
           newposttime);
        end if;

--
--Process forecast other data    
--
  else         
       SELECT INTO fcst_record * FROM FcstOther
       WHERE lid = newlid
       AND   pe = newpe
       AND   dur = newdur
       AND   ts = newts
       AND   extremum = newextr
       AND   probability  = newprob
       AND   basistime = newbasistime
       AND   validtime = newvalidtime;

       if FOUND then
          if fcst_record.value = -9999. then
             conf_update := 1;
          else
             if do_update = 2 then
                if fcst_record.value <> newvalue then 
                   conf_update := 1;
                end if;
             else
                if do_update = 1 then
                   conf_update := 1;
                end if;
             end if;
          end if;

          if conf_update = 1 then
             UPDATE FcstOther SET
             value          = newvalue,
             shef_qual_code = newrawqual,
             quality_code   = newqc,
             revision       = newrev,
             product_id     = newprid,
             producttime    = newprodtime,
             postingtime    = newposttime
             WHERE lid = newlid and pe  = newpe and
             dur = newdur and ts  = newts and
             extremum     = newextr and
             probability  = newprob and
             validtime    = newvalidtime and
             basistime    = newbasistime;
          end if;
       else
          INSERT INTO FcstOther
          VALUES( newlid, newpe, newdur, newts, newextr, newprob, newvalidtime,
          newbasistime,newvalue,newrawqual,newqc, newrev, newprid, newprodtime,
          newposttime);
       end if;
  end if;

RETURN NULL;
END;
$_$
    LANGUAGE plpgsql;


ALTER FUNCTION public.fcst_pe(character varying, character varying, integer, character varying, character varying, real, timestamp without time zone, timestamp without time zone, double precision, character varying, integer, integer, character varying, timestamp without time zone, timestamp without time zone, integer) OWNER TO awips;

--
-- TOC entry 25 (class 1255 OID 18888)
-- Dependencies: 6 723
-- Name: latestobs(character varying, character varying, integer, character varying, character varying, timestamp without time zone, double precision, character varying, integer, integer, character varying, timestamp without time zone, timestamp without time zone, integer); Type: FUNCTION; Schema: public; Owner: awips
--

CREATE FUNCTION latestobs(character varying, character varying, integer, character varying, character varying, timestamp without time zone, double precision, character varying, integer, integer, character varying, timestamp without time zone, timestamp without time zone, integer) RETURNS integer
    AS $_$
	--
	--	Local variables
	--
	DECLARE lobs         latestobsvalue%ROWTYPE;
                newlid       ALIAS FOR  $1;
                newpe        ALIAS FOR  $2;
                newdur       ALIAS FOR  $3;
                newts        ALIAS FOR  $4;
                newextr      ALIAS FOR  $5;
                newobstime   ALIAS FOR  $6;
                newvalue     ALIAS FOR  $7;
                newrawqual   ALIAS FOR  $8;
                newqc        ALIAS FOR  $9;
                newrev       ALIAS FOR  $10;
                newprid      ALIAS FOR  $11;
                newprodtime  ALIAS FOR  $12;
                newposttime  ALIAS FOR  $13;
                do_update    ALIAS FOR  $14;

	--
	--	Process latestobs data
	--
BEGIN
         if do_update = 1 or do_update = 2 then
             UPDATE latestobsvalue SET
                 obstime        = newobstime,
                 value          = newvalue,
                 shef_qual_code = newrawqual,
                 quality_code   = newqc, 
                 revision       = newrev,
                 product_id     = newprid,
                 producttime    = newprodtime,
                 postingtime    = newposttime
                 WHERE lid = newlid and pe = newpe and
                 dur = newdur and ts = newts and
                 extremum = newextr and obstime <= newobstime;  
         else
             UPDATE latestobsvalue SET
                 obstime        = newobstime,
                 value          = newvalue,
                 shef_qual_code = newrawqual,
                 quality_code   = newqc, 
                 revision       = newrev,
                 product_id     = newprid,
                 producttime    = newprodtime,
                 postingtime    = newposttime
                 WHERE lid = newlid and pe = newpe and
                 dur = newdur and ts = newts and
                 extremum = newextr and obstime < newobstime; 
         end if;
	     
	 if (NOT FOUND) then    
             SELECT INTO lobs * FROM latestobsvalue
             WHERE lid = newlid
             AND   pe = newpe
             AND   dur = newdur
             AND   ts = newts
             AND   extremum = newextr;
             if (NOT FOUND) then    
                 INSERT INTO latestobsvalue
                 VALUES ( newlid,      newpe,       newdur,   newts,
                          newextr,     newobstime,  newvalue, newrev,
		          newrawqual,  newqc,       newprid,
		          newprodtime, newposttime);
	     end if;
	 end if;	

RETURN 0;
END;
$_$
    LANGUAGE plpgsql;


ALTER FUNCTION public.latestobs(character varying, character varying, integer, character varying, character varying, timestamp without time zone, double precision, character varying, integer, integer, character varying, timestamp without time zone, timestamp without time zone, integer) OWNER TO awips;

--
-- TOC entry 26 (class 1255 OID 18889)
-- Dependencies: 6 723
-- Name: load_obs_river(character varying, character varying, character varying); Type: FUNCTION; Schema: public; Owner: awips
--

CREATE FUNCTION load_obs_river(character varying, character varying, character varying) RETURNS integer
    AS $_$
	--
	-- Local variables
	--
        DECLARE rstat        riverstatus%ROWTYPE;
                postlid      ALIAS FOR  $1;
                postpe       ALIAS FOR  $2;
                postts       ALIAS FOR  $3;

	        curdur       int;
	        curextr      char(1);
	        curtime	     timestamp;
	        curvalue     float = -9999.;
	        qc_threshold int = 1073741824;
                rstat_found  boolean;
BEGIN
	-- Determine if record already exists
        SELECT INTO rstat * FROM riverstatus
	WHERE lid = postlid and pe = postpe and ts = postts;
		
        rstat_found := FOUND;
		
	-- Get the latest valid value from the appropriate table
	-- note that for all the queries, only data that is instantaneous
	-- and having a null extremum is considered.  this constraint results
	-- from the fact that we are trying to find the latest obstime for 
	-- a given lid, pe, ts but the tables being read also have dur and 
	-- extremum as key fields.  this approach is considered ok since
	-- observed data is rarely ever of anything but null extremum and
	-- instantaneous duration.  a more robust alternative that uses the
	-- maximum value, regardless of duration or extremum, for the 
	-- latest time was considered, but not implemented since this
	-- would add more processing time just to consider a case
	-- that would very rarely, if ever, occur.
	
	if postpe like 'Q%' then
	   SELECT INTO curtime max(obstime)
                FROM discharge
		WHERE lid = postlid and pe = postpe and ts = postts and 
		dur = 0 and extremum = 'Z' and quality_code >= qc_threshold	
                and value != -9999.;
	   SELECT INTO curdur, curextr, curvalue 
                dur, extremum, "value"
	   	FROM discharge
		WHERE lid = postlid and pe = postpe and ts = postts and
		dur = 0 and extremum = 'Z' and quality_code >= qc_threshold and
		obstime = curtime;
        elsif postpe like 'H%' then
	   SELECT INTO curtime max(obstime)
                FROM height
		WHERE lid = postlid and pe = postpe and ts = postts and 
		dur = 0 and extremum = 'Z' and quality_code >= qc_threshold
                and value != -9999.;
	   SELECT INTO curdur, curextr, curvalue
                dur, extremum, "value"
                FROM height
		WHERE lid = postlid and pe = postpe and ts = postts and 
		dur = 0 and extremum = 'Z' and quality_code >= qc_threshold and
		obstime = curtime;
	end if;
			 
	-- If no value found, then delete any record that may be
	-- in the riverstatus table, which can occur when 
	-- this function is called by an application deleting the
	-- last item for the lid,pe,ts combination
	
	if rstat_found = FALSE AND curvalue IS NULL then
		RETURN 0;
	elsif rstat_found AND curvalue IS NULL then
	        DELETE FROM riverstatus
                   WHERE lid = postlid and pe = postpe and ts = postts;
	
	-- If entry exists already, then update it
	-- on updates, dont bother resetting basistime and probability
        elsif rstat_found then
                UPDATE riverstatus
                  SET   dur        = curdur,
		  	extremum   = curextr,
		        validtime  = curtime,
                        "value"      = curvalue
                  WHERE lid = postlid and pe = postpe and ts = postts;

	-- Insert new entry
	-- set probability to default and basistime to NULL
        else
                INSERT INTO RiverStatus
                VALUES
                (
                        postlid,   postpe,    curdur,
			postts,    curextr,   -1.,  
			curtime,   NULL,      curvalue
                );
        end if;
RETURN 0;
END;
$_$
    LANGUAGE plpgsql;


ALTER FUNCTION public.load_obs_river(character varying, character varying, character varying) OWNER TO awips;

--
-- TOC entry 27 (class 1255 OID 18890)
-- Dependencies: 6 723
-- Name: obs_pe(character varying, character varying, integer, character varying, character varying, timestamp without time zone, double precision, character varying, integer, integer, character varying, timestamp without time zone, timestamp without time zone, integer); Type: FUNCTION; Schema: public; Owner: awips
--

CREATE FUNCTION obs_pe(character varying, character varying, integer, character varying, character varying, timestamp without time zone, double precision, character varying, integer, integer, character varying, timestamp without time zone, timestamp without time zone, integer) RETURNS integer
    AS $_$
DECLARE obs_rec       height%ROWTYPE;
        newlid        ALIAS FOR   $1;
        newpe         ALIAS FOR   $2;
        newdur        ALIAS FOR   $3;
        newts         ALIAS FOR   $4;
        newextr       ALIAS FOR   $5;
        newobstime    ALIAS FOR   $6;
        newvalue      ALIAS FOR   $7;
        newrawqual    ALIAS FOR   $8;
        newqc         ALIAS FOR   $9;
        newrev        ALIAS FOR   $10;
        newprid       ALIAS FOR   $11;
        newprodtime   ALIAS FOR   $12;
        newposttime   ALIAS FOR   $13;
        do_update     ALIAS FOR   $14;
        conf_update   integer;
BEGIN
    conf_update := 0;
------------------------------------------------------
-- Process the data based on the PE attribute
-- The more common PE are placed earliest in this if-elseif stack
-- The order is height (H), pressure (Pspecial), precip (Pother),
-- discharge (Q), snow (S), temperature (T), wind (U), then...
-- agricultural (A), evaporation (E), fishcount(F), ground (G),
-- ice (I), lake (L), moisture (M), gatedam (N), radiation (R),
-- power (V), waterquality (W), weather (X), yunique (Y).
------------------------------------------------------
--
--Process height data
--
  if newpe like 'H%' then				

     SELECT INTO obs_rec * FROM height
     WHERE lid = newlid
     AND   pe = newpe
     AND   dur = newdur
     AND   ts = newts
     AND   extremum = newextr
     AND   obstime = newobstime;

     if FOUND then
        if obs_rec.value = -9999. then
           conf_update := 1;
        else
           if do_update = 2 then
              if obs_rec.value <> newvalue then
                 conf_update := 1;
              end if;
           else 
              if do_update = 1 then
                 conf_update := 1;
              end if;
           end if;
        end if;

        if conf_update = 1 then
           UPDATE height SET
           value          = newvalue,
           shef_qual_code = newrawqual,
           quality_code   = newqc,
           revision       = newrev,
           product_id     = newprid,
           producttime    = newprodtime,
           postingtime    = newposttime
           WHERE lid = newlid and pe = newpe and
           dur = newdur and ts = newts and
           extremum = newextr and obstime = newobstime;
         end if;

    else
       INSERT INTO height
       VALUES ( newlid, newpe, newdur, newts, newextr, newobstime, 
       newvalue, newrawqual, newqc, newrev, newprid, newprodtime, 
       newposttime);
    end if;
		   
--
--Process pressure data
--
  elsif newpe in ('PA', 'PD', 'PE', 'PL') then
        SELECT INTO obs_rec * FROM pressure
        WHERE lid = newlid
        AND   pe = newpe
        AND   dur = newdur
        AND   ts = newts
        AND   extremum = newextr
        AND   obstime = newobstime;

        if FOUND then
           if obs_rec.value = -9999. then
              conf_update := 1;
           else
              if do_update = 2 then
                 if obs_rec.value <> newvalue then
                    conf_update := 1;
                 end if;
              else 
                 if do_update = 1 then
                    conf_update := 1;
                 end if;
              end if;
           end if;

           if conf_update = 1 then
              UPDATE pressure SET
              value          = newvalue,
              shef_qual_code = newrawqual,
              quality_code   = newqc,
              revision       = newrev,
              product_id     = newprid,
              producttime    = newprodtime,
              postingtime    = newposttime
              WHERE lid = newlid and pe = newpe and
              dur = newdur and ts = newts and
              extremum = newextr and obstime = newobstime;
           end if;
       else
           INSERT INTO pressure
           VALUES ( newlid, newpe, newdur, newts, newextr, newobstime, 
           newvalue, newrawqual, newqc, newrev, newprid, newprodtime, 
           newposttime);
       end if;

--
--Process Discharge data
--
  elsif newpe like 'Q%' then                
        SELECT INTO obs_rec * FROM discharge
	WHERE lid = newlid
        AND   pe = newpe
        AND   dur = newdur
        AND   ts = newts
        AND   extremum = newextr
        AND   obstime = newobstime;

        if FOUND then
           if obs_rec.value = -9999. then
              conf_update := 1;
           else
              if do_update = 2 then
                 if obs_rec.value <> newvalue then
                    conf_update := 1;
                 end if;
              else
                 if do_update = 1 then
                    conf_update := 1;
                 end if;
              end if;
           end if;

           if conf_update = 1 then
              UPDATE discharge SET
              value          = newvalue,
              shef_qual_code = newrawqual,
              quality_code   = newqc,
              revision       = newrev,
              product_id     = newprid,
              producttime    = newprodtime,
              postingtime    = newposttime
              WHERE lid = newlid and pe = newpe and
              dur = newdur and ts = newts and
              extremum = newextr and obstime = newobstime;
           end if;
       else
           INSERT INTO discharge
           VALUES ( newlid, newpe, newdur, newts, newextr, newobstime,
           newvalue, newrawqual, newqc, newrev, newprid, newprodtime,
           newposttime);
       end if;

--
--Process snow data
--
  elsif newpe like 'S%' then
   
        SELECT INTO obs_rec * FROM snow
        WHERE lid = newlid
        AND   pe = newpe
        AND   dur = newdur
        AND   ts = newts
        AND   extremum = newextr
        AND   obstime = newobstime;

        if FOUND then
           if obs_rec.value = -9999. then
              conf_update := 1;
           else
              if do_update = 2 then
                 if obs_rec.value <> newvalue then
                    conf_update := 1;
                 end if;
              else
                 if do_update = 1 then
                    conf_update := 1;
                 end if;
              end if;
           end if;

           if conf_update = 1 then
              UPDATE snow SET
              value          = newvalue,
              shef_qual_code = newrawqual,
              quality_code   = newqc,
              revision       = newrev,
              product_id     = newprid,
              producttime    = newprodtime,
              postingtime    = newposttime
              WHERE lid = newlid and pe = newpe and
              dur = newdur and ts = newts and
              extremum = newextr and obstime = newobstime;
           end if;
       else
           INSERT INTO snow
           VALUES ( newlid, newpe, newdur, newts, newextr, newobstime,
           newvalue, newrawqual, newqc, newrev, newprid, newprodtime,
           newposttime);
       end if;
	
--
--Process temperature data
--
  elsif newpe like 'T%' then                
   
        SELECT INTO obs_rec * FROM temperature
        WHERE lid = newlid
        AND   pe = newpe
        AND   dur = newdur
        AND   ts = newts
        AND   extremum = newextr
        AND   obstime = newobstime;

        if FOUND then
           if obs_rec.value = -9999. then
              conf_update := 1;
           else
              if do_update = 2 then
                 if obs_rec.value <> newvalue then
                    conf_update := 1;
                 end if;
              else
                 if do_update = 1 then
                    conf_update := 1;
                 end if;
              end if;
           end if;

           if conf_update = 1  then
              UPDATE temperature SET
              value          = newvalue,
              shef_qual_code = newrawqual,
              quality_code   = newqc,
              revision       = newrev,
              product_id     = newprid,
              producttime    = newprodtime,
              postingtime    = newposttime
              WHERE lid = newlid and pe = newpe and
              dur = newdur and ts = newts and
              extremum = newextr and obstime = newobstime;
           end if;
        else
           INSERT INTO temperature
           VALUES ( newlid, newpe, newdur, newts, newextr, newobstime,
           newvalue, newrawqual, newqc, newrev, newprid, newprodtime,
           newposttime);
        end if;
   
 --
 --Process wind data
 --
   elsif newpe like 'U%' then                
   
        SELECT INTO obs_rec * FROM wind
        WHERE lid = newlid
        AND   pe = newpe
        AND   dur = newdur
        AND   ts = newts
        AND   extremum = newextr
        AND   obstime = newobstime;

        if FOUND then
           if obs_rec.value = -9999. then
              conf_update := 1;
           else
              if do_update = 2 then
                 if obs_rec.value <> newvalue then
                    conf_update := 1;
                 end if;
              else
                 if do_update = 1 then
                    conf_update := 1;
                 end if;
              end if;
           end if;

           if conf_update = 1 then
              UPDATE wind SET
              value      = newvalue,
              shef_qual_code = newrawqual,
              quality_code   = newqc,
              revision       = newrev,
              product_id     = newprid,
              producttime    = newprodtime,
              postingtime    = newposttime
              WHERE lid = newlid and pe = newpe and
              dur = newdur and ts = newts and
              extremum = newextr and obstime = newobstime;
           end if;
        else
           INSERT INTO wind
           VALUES ( newlid, newpe, newdur, newts, newextr, newobstime,
           newvalue, newrawqual, newqc, newrev, newprid, newprodtime,
           newposttime);
        end if;
	
--
--Process agricultural data
--

  elsif newpe like 'A%' then                
   
        SELECT INTO obs_rec * FROM agricultural
        WHERE lid = newlid
        AND   pe = newpe
        AND   dur = newdur
        AND   ts = newts
        AND   extremum = newextr
        AND   obstime = newobstime;

        if FOUND then
           if obs_rec.value = -9999. then
              conf_update := 1;
           else
              if do_update = 2 then
                 if obs_rec.value <> newvalue then
                    conf_update := 1;
                 end if;
              else
                 if do_update = 1 then
                    conf_update := 1;
                 end if;
              end if;
           end if;

           if conf_update = 1 then
              UPDATE agricultural SET
              value          = newvalue,
              shef_qual_code = newrawqual,
              quality_code   = newqc,
              revision       = newrev,
              product_id     = newprid,
              producttime    = newprodtime,
              postingtime    = newposttime
              WHERE lid = newlid and pe = newpe and
              dur = newdur and ts = newts and
              extremum = newextr and obstime = newobstime;
           end if;
        else
           INSERT INTO agricultural
           VALUES ( newlid, newpe, newdur, newts, newextr, newobstime,
           newvalue, newrawqual, newqc, newrev, newprid, newprodtime,
           newposttime);
        end if;
   
 --
 --Process evaporation data
 --
   elsif newpe like 'E%' then
   
         SELECT INTO obs_rec * FROM evaporation
         WHERE lid = newlid
         AND   pe = newpe
         AND   dur = newdur
         AND   ts = newts
         AND   extremum = newextr
         AND   obstime = newobstime;

         if FOUND then
            if obs_rec.value = -9999. then
               conf_update := 1;
            else
               if do_update = 2 then
                  if obs_rec.value <> newvalue then
                     conf_update := 1;
                  end if;
               else
                  if do_update = 1 then
                     conf_update := 1;
                  end if;
               end if;
            end if;

            if conf_update = 1 then
               UPDATE evaporation SET
               value          = newvalue,
               shef_qual_code = newrawqual,
               quality_code   = newqc,
               revision       = newrev,
               product_id     = newprid,
               producttime    = newprodtime,
               postingtime    = newposttime
               WHERE lid = newlid and pe = newpe and
               dur = newdur and ts = newts and
               extremum = newextr and obstime = newobstime;
            end if;
         else
            INSERT INTO evaporation
            VALUES ( newlid, newpe, newdur, newts, newextr, newobstime,
            newvalue, newrawqual, newqc, newrev, newprid, newprodtime,
            newposttime);
         end if;

--
--Process fish count data
--
  elsif newpe like 'F%' then
   
        SELECT INTO obs_rec * FROM fishcount
        WHERE lid = newlid
        AND   pe = newpe
        AND   dur = newdur
        AND   ts = newts
        AND   extremum = newextr
        AND   obstime = newobstime;

        if FOUND then
           if obs_rec.value = -9999. then
              conf_update := 1;
           else
              if do_update = 2 then
                 if obs_rec.value <> newvalue then
                    conf_update := 1;
                 end if;
              else
                 if do_update = 1 then
                    conf_update := 1;
                 end if;
              end if;
           end if;

           if conf_update = 1 then
              UPDATE fishcount SET
              value          = newvalue,
              shef_qual_code = newrawqual,
              quality_code   = newqc,
              revision       = newrev,
              product_id     = newprid,
              producttime    = newprodtime,
              postingtime    = newposttime
              WHERE lid = newlid and pe = newpe and
              dur = newdur and ts = newts and
              extremum = newextr and obstime = newobstime;
           end if;
        else
           INSERT INTO fishcount
           VALUES ( newlid, newpe, newdur, newts, newextr, newobstime,
           newvalue, newrawqual, newqc, newrev, newprid, newprodtime,
           newposttime);
        end if;

--
--Process ground data
--
  elsif newpe like 'G%' then                
   
        SELECT INTO obs_rec * FROM ground
        WHERE lid = newlid
        AND   pe = newpe
        AND   dur = newdur
        AND   ts = newts
        AND   extremum = newextr
        AND   obstime = newobstime;

        if FOUND then
           if obs_rec.value = -9999. then
              conf_update := 1;
           else
              if do_update = 2 then
                 if obs_rec.value <> newvalue then
                    conf_update := 1;
                 end if;
              else
                 if do_update = 1 then
                    conf_update := 1;
                 end if;
              end if;
           end if;

           if conf_update = 1 then
              UPDATE ground SET
              value          = newvalue,
              shef_qual_code = newrawqual,
              quality_code   = newqc,
              revision       = newrev,
              product_id     = newprid,
              producttime    = newprodtime,
              postingtime    = newposttime
              WHERE lid = newlid and pe = newpe and
              dur = newdur and ts = newts and
              extremum = newextr and obstime = newobstime;
           end if;
        else
           INSERT INTO ground
           VALUES ( newlid, newpe, newdur, newts, newextr, newobstime,
           newvalue, newrawqual, newqc, newrev, newprid, newprodtime,
           newposttime);
        end if;

--
--Process Ice data
--
  elsif newpe like 'I%' then                               
   
        SELECT INTO obs_rec * FROM ice
        WHERE lid = newlid
        AND   pe = newpe
        AND   dur = newdur
        AND   ts = newts
        AND   extremum = newextr
        AND   obstime = newobstime;

        if FOUND then
           if obs_rec.value = -9999. then
              conf_update := 1;
           else
              if do_update = 2 then
                 if obs_rec.value <> newvalue then
                    conf_update := 1;
                 end if;
              else
                 if do_update = 1 then
                    conf_update := 1;
                 end if;
              end if;
           end if;

           if conf_update = 1 then
              UPDATE ice SET
              value          = newvalue,
              shef_qual_code = newrawqual,
              quality_code   = newqc,
              revision       = newrev,
              product_id     = newprid,
              producttime    = newprodtime,
              postingtime    = newposttime
              WHERE lid = newlid and pe = newpe and
              dur = newdur and ts = newts and
              extremum = newextr and obstime = newobstime;
           end if;
        else
           INSERT INTO ice
           VALUES ( newlid, newpe, newdur, newts, newextr, newobstime,
           newvalue, newrawqual, newqc, newrev, newprid, newprodtime,
           newposttime);
        end if;
		      
--
--Process lake data
--
  elsif newpe like 'L%' then                                
   
        SELECT INTO obs_rec * FROM lake
        WHERE lid = newlid
        AND   pe = newpe
        AND   dur = newdur
        AND   ts = newts
        AND   extremum = newextr
        AND   obstime = newobstime;

        if FOUND then
           if obs_rec.value = -9999. then
              conf_update := 1;
           else
              if do_update = 2 then
                 if obs_rec.value <> newvalue then
                    conf_update := 1;
                 end if;
              else
                 if do_update = 1 then
                    conf_update := 1;
                 end if;
              end if;
           end if;

           if conf_update = 1 then
              UPDATE lake SET
              value          = newvalue,
              shef_qual_code = newrawqual,
              quality_code   = newqc,
              revision       = newrev,
              product_id     = newprid,
              producttime    = newprodtime,
              postingtime    = newposttime
              WHERE lid = newlid and pe = newpe and
              dur = newdur and ts = newts and
              extremum = newextr and obstime = newobstime;
           end if;
        else
           INSERT INTO lake
           VALUES ( newlid, newpe, newdur, newts, newextr, newobstime,
           newvalue, newrawqual, newqc, newrev, newprid, newprodtime,
           newposttime);
        end if;

--
--Process moisture data
--
  elsif newpe like 'M%' then
   
        SELECT INTO obs_rec * FROM moisture
        WHERE lid = newlid
        AND   pe = newpe
        AND   dur = newdur
        AND   ts = newts
        AND   extremum = newextr
        AND   obstime = newobstime;

        if FOUND then
           if obs_rec.value = -9999. then
              conf_update := 1;
           else
              if do_update = 2 then
                 if obs_rec.value <> newvalue then
                    conf_update := 1;
                 end if;
              else
                 if do_update = 1 then
                    conf_update := 1;
                 end if;
              end if;
           end if;

           if conf_update = 1 then
              UPDATE moisture SET
              value      = newvalue,
              shef_qual_code = newrawqual,
              quality_code   = newqc,
              revision       = newrev,
              product_id     = newprid,
              producttime    = newprodtime,
              postingtime    = newposttime
              WHERE lid = newlid and pe = newpe and
              dur = newdur and ts = newts and
              extremum = newextr and obstime = newobstime;
          end if; 
        else
           INSERT INTO moisture
           VALUES ( newlid, newpe, newdur, newts, newextr, newobstime,
           newvalue, newrawqual, newqc, newrev, newprid, newprodtime,
           newposttime);
        end if;

--
--Process gate and dam data
--
  elsif newpe like 'N%' then
   
        SELECT INTO obs_rec * FROM gatedam
        WHERE lid = newlid
        AND   pe = newpe
        AND   dur = newdur
        AND   ts = newts
        AND   extremum = newextr
        AND   obstime = newobstime;

        if FOUND then
           if obs_rec.value = -9999. then
              conf_update := 1;
           else
              if do_update = 2 then
                 if obs_rec.value <> newvalue then
                    conf_update := 1;
                 end if;
              else
                 if do_update = 1 then
                    conf_update := 1;
                 end if;
              end if;
           end if;

           if conf_update = 1 then
              UPDATE gatedam SET
              value          = newvalue,
              shef_qual_code = newrawqual,
              quality_code   = newqc,
              revision       = newrev,
              product_id     = newprid,
              producttime    = newprodtime,
              postingtime    = newposttime
              WHERE lid = newlid and pe = newpe and
              dur = newdur and ts = newts and
              extremum = newextr and obstime = newobstime;
           end if; 
        else
           INSERT INTO gatedam
           VALUES ( newlid, newpe, newdur, newts, newextr, newobstime,
           newvalue, newrawqual, newqc, newrev, newprid, newprodtime,
           newposttime);
        end if;

--
--Process radiation data
--
  elsif newpe like 'R%' then                                
   
        SELECT INTO obs_rec * FROM radiation
        WHERE lid = newlid
        AND   pe = newpe
        AND   dur = newdur
        AND   ts = newts
        AND   extremum = newextr
        AND   obstime = newobstime;

        if FOUND then
           if obs_rec.value = -9999. then
              conf_update := 1;
           else
              if do_update = 2 then
                 if obs_rec.value <> newvalue then
                    conf_update := 1;
                 end if;
              else
                  if do_update = 1 then
                     conf_update := 1;
                  end if;
              end if;
           end if;

           if conf_update = 1 then
              UPDATE radiation SET
              value          = newvalue,
              shef_qual_code = newrawqual,
              quality_code   = newqc,
              revision       = newrev,
              product_id     = newprid,
              producttime    = newprodtime,
              postingtime    = newposttime
              WHERE lid = newlid and pe = newpe and
              dur = newdur and ts = newts and
              extremum = newextr and obstime = newobstime;
           end if;
        else
           INSERT INTO radiation
           VALUES ( newlid, newpe, newdur, newts, newextr, newobstime,
           newvalue, newrawqual, newqc, newrev, newprid, newprodtime,
           newposttime);
        end if;

--
--Process generation and generator data
--
  elsif newpe like 'V%' then                
   
        SELECT INTO obs_rec * FROM power
        WHERE lid = newlid
        AND   pe = newpe
        AND   dur = newdur
        AND   ts = newts
        AND   extremum = newextr
        AND   obstime = newobstime;

        if FOUND then
           if obs_rec.value = -9999. then
              conf_update := 1;
           else
              if do_update = 2 then
                 if obs_rec.value <> newvalue then
                    conf_update := 1;
                 end if;
              else
                 if do_update = 1 then
                    conf_update := 1;
                 end if;
              end if;
           end if;

           if conf_update = 1 then
              UPDATE power SET
              value      = newvalue,
              shef_qual_code = newrawqual,
              quality_code   = newqc,
              revision       = newrev,
              product_id     = newprid,
              producttime    = newprodtime,
              postingtime    = newposttime
              WHERE lid = newlid and pe = newpe and
              dur = newdur and ts = newts and
              extremum = newextr and obstime = newobstime;
           end if; 
        else
           INSERT INTO power
           VALUES ( newlid, newpe, newdur, newts, newextr, newobstime,
           newvalue, newrawqual, newqc, newrev, newprid, newprodtime,
           newposttime);
        end if;

--
--Process water quality data
--
  elsif newpe like 'W%' then		      
   
        SELECT INTO obs_rec * FROM waterquality
        WHERE lid = newlid
        AND   pe = newpe
        AND   dur = newdur
        AND   ts = newts
        AND   extremum = newextr
        AND   obstime = newobstime;

        if FOUND then
           if obs_rec.value = -9999. then
              conf_update := 1;
           else
              if do_update = 2 then
                 if obs_rec.value <> newvalue then
                    conf_update := 1;
                 end if;
              else
                 if do_update = 1 then
                    conf_update := 1;
                 end if;
              end if;
           end if;

           if conf_update = 1 then
              UPDATE waterquality SET
              value          = newvalue,
              shef_qual_code = newrawqual,
              quality_code   = newqc,
              revision       = newrev,
              product_id     = newprid,
              producttime    = newprodtime,
              postingtime    = newposttime
              WHERE lid = newlid and pe = newpe and
              dur = newdur and ts = newts and
              extremum = newextr and obstime = newobstime;
           end if;
        else
           INSERT INTO waterquality
           VALUES ( newlid, newpe, newdur, newts, newextr, newobstime,
           newvalue, newrawqual, newqc, newrev, newprid, newprodtime,
           newposttime);
        end if;

--
--Process weather data
--
  elsif newpe like 'X%' then		      
   
        SELECT INTO obs_rec * FROM weather
        WHERE lid = newlid
        AND   pe = newpe
        AND   dur = newdur
        AND   ts = newts
        AND   extremum = newextr
        AND   obstime = newobstime;

        if FOUND then
           if obs_rec.value = -9999. then
              conf_update := 1;
           else
              if do_update = 2 then
                 if obs_rec.value <> newvalue then
                    conf_update := 1;
                 end if;
              else
                 if do_update = 1 then
                    conf_update := 1;
                 end if;
              end if;
           end if;

           if conf_update = 1 then
              UPDATE weather SET
              value      = newvalue,
              shef_qual_code = newrawqual,
              quality_code   = newqc,
              revision       = newrev,
              product_id     = newprid,
              producttime    = newprodtime,
              postingtime    = newposttime
              WHERE lid = newlid and pe = newpe and
              dur = newdur and ts = newts and
              extremum = newextr and obstime = newobstime;
           end if;
        else
           INSERT INTO weather
           VALUES ( newlid, newpe, newdur, newts, newextr, newobstime,
           newvalue, newrawqual, newqc, newrev, newprid, newprodtime,
           newposttime);
        end if;

--
--Process Yunique data
--
  elsif newpe like 'Y%' then                
   
        SELECT INTO obs_rec * FROM yunique
        WHERE lid = newlid
        AND   pe = newpe
        AND   dur = newdur
        AND   ts = newts
        AND   extremum = newextr
        AND   obstime = newobstime;

        if FOUND then
           if obs_rec.value = -9999. then
              conf_update := 1;
           else
              if do_update = 2 then
                 if obs_rec.value <> newvalue then
                    conf_update := 1;
                 end if;
              else
                 if do_update = 1 then
                    conf_update := 1;
                 end if;
              end if;
           end if;

           if conf_update = 1 then
              UPDATE yunique SET
              value          = newvalue,
              shef_qual_code = newrawqual,
              quality_code   = newqc,
              revision       = newrev,
              product_id     = newprid,
              producttime    = newprodtime,
              postingtime    = newposttime
              WHERE lid = newlid and pe = newpe and
              dur = newdur and ts = newts and
              extremum = newextr and obstime = newobstime;
           end if;
        else
           INSERT INTO yunique
           VALUES ( newlid, newpe, newdur, newts, newextr, newobstime,
           newvalue, newrawqual, newqc, newrev, newprid, newprodtime,
           newposttime);
        end if;

  end if;

RETURN 0;
END;
$_$
    LANGUAGE plpgsql;


ALTER FUNCTION public.obs_pe(character varying, character varying, integer, character varying, character varying, timestamp without time zone, double precision, character varying, integer, integer, character varying, timestamp without time zone, timestamp without time zone, integer) OWNER TO awips;

--
-- TOC entry 28 (class 1255 OID 18892)
-- Dependencies: 723 6
-- Name: obs_precip(character varying, character varying, integer, character varying, character varying, timestamp without time zone, double precision, character varying, integer, integer, character varying, timestamp without time zone, timestamp without time zone, integer); Type: FUNCTION; Schema: public; Owner: awips
--

CREATE FUNCTION obs_precip(character varying, character varying, integer, character varying, character varying, timestamp without time zone, double precision, character varying, integer, integer, character varying, timestamp without time zone, timestamp without time zone, integer) RETURNS integer
    AS $_$
        DECLARE obs_precip_rec rawpp%ROWTYPE;
                newlid         ALIAS FOR   $1;
                newpe          ALIAS FOR   $2;
                newdur         ALIAS FOR   $3;
                newts          ALIAS FOR   $4;
                newextr        ALIAS FOR   $5;
                newobstime     ALIAS FOR   $6;
                newvalue       ALIAS FOR   $7;
                newrawqual     ALIAS FOR   $8;
                newqc          ALIAS FOR   $9;
                newrev         ALIAS FOR   $10;
                newprid        ALIAS FOR   $11;
                newprodtime    ALIAS FOR   $12;
                newposttime    ALIAS FOR   $13;
                do_update      ALIAS FOR   $14;
                conf_update    integer;

BEGIN
    conf_update := 0;

--Process the data based on the PE attribute
--Post data to the appropriate table as instructed
--      
	
--
--Post PC data
--
  if (newpe = 'PC') then				
      SELECT INTO obs_precip_rec * FROM rawPC 
      WHERE lid = newlid
      AND   pe  = newpe
      AND   dur = newdur
      AND   ts  = newts
      AND   extremum = newextr
      AND   obstime = newobstime;

      if FOUND then
         if obs_precip_rec.value = -9999. then
            conf_update := 1;
         else
            if do_update = 2 then
               if obs_precip_rec.value <> newvalue then 
                  conf_update := 1;
               end if;
            else
               if do_update = 1 then
                  conf_update := 1;
               end if;
            end if;
         end if;

         if conf_update = 1 then
            UPDATE rawPC SET
	    value          = newvalue,
	    shef_qual_code = newrawqual,
	    quality_code   = newqc, 
	    revision       = newrev,
	    product_id     = newprid,
	    producttime    = newprodtime,
	    postingtime    = newposttime
	    WHERE lid = newlid and pe = newpe and
	    dur = newdur and ts = newts and
	    extremum = newextr and obstime = newobstime;
	 end if;

      else
         INSERT INTO rawPC
         VALUES ( newlid, newpe, newdur, newts, newextr, newobstime, newvalue, 
         newrawqual, newqc, newrev, newprid, newprodtime, newposttime);
      end if;

-- 
--Post PP data  
--
  elsif (newpe = 'PP') then				
        SELECT INTO obs_precip_rec * FROM rawPP 
        WHERE lid = newlid
        AND   pe  = newpe
        AND   dur = newdur
        AND   ts  = newts
        AND   extremum = newextr
        AND   obstime = newobstime;

        if FOUND then
           if obs_precip_rec.value = -9999. then
              conf_update := 1;
           else
              if do_update = 2 then
                 if obs_precip_rec.value <> newvalue then
                    conf_update := 1;
                 end if;
              else
                 if do_update = 1 then
                    conf_update := 1;
                 end if;
              end if;
           end if;

           if conf_update = 1 then
              UPDATE rawPP SET
              value          = newvalue,
              shef_qual_code = newrawqual,
              quality_code   = newqc,
              revision       = newrev,
              product_id     = newprid,
              producttime    = newprodtime,
              postingtime    = newposttime
              WHERE lid = newlid and pe = newpe and
              dur = newdur and ts = newts and
              extremum = newextr and obstime = newobstime;
           end if;
        else
           INSERT INTO rawPP
           VALUES ( newlid, newpe, newdur, newts, newextr, newobstime, newvalue,
           newrawqual, newqc, newrev, newprid, newprodtime, newposttime);
        end if;

-- 
--Post precip data that is neither PC nor PP
--    
  else				
     SELECT INTO obs_precip_rec * FROM rawPother
     WHERE lid = newlid
     AND   pe  = newpe
     AND   dur = newdur
     AND   ts  = newts
     AND   extremum = newextr
     AND   obstime = newobstime;

     if FOUND then
        if obs_precip_rec.value = -9999. then
           conf_update := 1;
        else
           if do_update = 2 then
              if obs_precip_rec.value <> newvalue then
                 conf_update := 1;
              end if;
           else
              if do_update = 1 then
                 conf_update := 1;
              end if;
           end if;
        end if;

        if conf_update = 1 then
           UPDATE rawPother SET
           value          = newvalue,
           shef_qual_code = newrawqual,
           quality_code   = newqc,
           revision       = newrev,
           product_id     = newprid,
           producttime    = newprodtime,
           postingtime    = newposttime
           WHERE lid = newlid and pe = newpe and
           dur = newdur and ts = newts and
           extremum = newextr and obstime = newobstime;
        end if;

      else
        INSERT INTO rawPother
        VALUES ( newlid, newpe, newdur, newts, newextr, newobstime, newvalue,
        newrawqual, newqc, newrev, newprid, newprodtime, newposttime);
      end if;
  end if;
	   
RETURN 0;
END;
$_$
    LANGUAGE plpgsql;


ALTER FUNCTION public.obs_precip(character varying, character varying, integer, character varying, character varying, timestamp without time zone, double precision, character varying, integer, integer, character varying, timestamp without time zone, timestamp without time zone, integer) OWNER TO awips;

--
-- TOC entry 29 (class 1255 OID 18893)
-- Dependencies: 6 723
-- Name: obs_rawpc_ins(); Type: FUNCTION; Schema: public; Owner: awips
--

CREATE FUNCTION obs_rawpc_ins() RETURNS trigger
    AS $$
BEGIN
    INSERT INTO CurPC
    VALUES (
            NEW.lid,
            NEW.pe,
            NEW.dur,
            NEW.ts,
            NEW.extremum,
            NEW.obstime,
            NEW.value,
            NEW.shef_qual_code,
            NEW.quality_code,
            NEW.revision,
            NEW.product_id,
            NEW.producttime,
            NEW.postingtime
           );
RETURN NEW;
END;
$$
    LANGUAGE plpgsql;


ALTER FUNCTION public.obs_rawpc_ins() OWNER TO awips;

--
-- TOC entry 30 (class 1255 OID 18894)
-- Dependencies: 6 723
-- Name: obs_rawpc_upd(); Type: FUNCTION; Schema: public; Owner: awips
--

CREATE FUNCTION obs_rawpc_upd() RETURNS trigger
    AS $$
DECLARE counter int;
BEGIN
    --Update the info in CurPC
      SELECT INTO counter count(*) FROM CurPC
      WHERE   lid      = NEW.lid
      and     pe       = NEW.pe
      and     dur      = NEW.dur
      and     ts       = NEW.ts
      and     extremum = NEW.extremum
      and     obstime  = NEW.obstime;

      if (counter > 0) then
          UPDATE CurPC
          SET   value          = NEW.value,
          shef_qual_code = NEW.shef_qual_code,
          quality_code   = NEW.quality_code,
          revision       = NEW.revision,
          product_id     = NEW.product_id,
          producttime    = NEW.producttime,
          postingtime    = NEW.postingtime
          WHERE lid      = NEW.lid
          and pe       = NEW.pe
          and dur      = NEW.dur
          and ts       = NEW.ts
          and extremum = NEW.extremum
          and obstime  = NEW.obstime;
      else
          INSERT INTO CurPC
          VALUES ( NEW.lid, NEW.pe, NEW.dur, NEW.ts, NEW.extremum, NEW.obstime,
          NEW.value, NEW.shef_qual_code, NEW.quality_code, NEW.revision,
          NEW.product_id, NEW.producttime, NEW.postingtime);

          DELETE FROM CurPC
          WHERE lid      = OLD.lid
          AND   pe       = OLD.pe
          AND   dur      = OLD.dur
          AND   ts       = OLD.ts
          AND   extremum = OLD.extremum
          AND   obstime  = OLD.obstime;
      end if;

RETURN NEW;
END;
$$
    LANGUAGE plpgsql;


ALTER FUNCTION public.obs_rawpc_upd() OWNER TO awips;

--
-- TOC entry 31 (class 1255 OID 18895)
-- Dependencies: 6 723
-- Name: obs_rawpp_ins(); Type: FUNCTION; Schema: public; Owner: awips
--

CREATE FUNCTION obs_rawpp_ins() RETURNS trigger
    AS $$
BEGIN
    INSERT INTO CurPP
    VALUES ( NEW.lid, NEW.pe, NEW.dur, NEW.ts, NEW.extremum, NEW.obstime,
    NEW.value, NEW.shef_qual_code, NEW.quality_code, NEW.revision, 
    NEW.product_id, NEW.producttime, NEW.postingtime);
RETURN NEW;
END;
$$
    LANGUAGE plpgsql;


ALTER FUNCTION public.obs_rawpp_ins() OWNER TO awips;

--
-- TOC entry 32 (class 1255 OID 18896)
-- Dependencies: 6 723
-- Name: obs_rawpp_upd(); Type: FUNCTION; Schema: public; Owner: awips
--

CREATE FUNCTION obs_rawpp_upd() RETURNS trigger
    AS $$
DECLARE counter int;
BEGIN
    --Update the info in CurPP
    SELECT INTO counter count(*) FROM CurPP
    WHERE   lid      = NEW.lid
    and     pe       = NEW.pe
    and     dur      = NEW.dur
    and     ts       = NEW.ts
    and     extremum = NEW.extremum
    and     obstime  = NEW.obstime;

    if (counter > 0) then
       UPDATE CurPP
       SET   value          = NEW.value,
       shef_qual_code = NEW.shef_qual_code,
       quality_code   = NEW.quality_code,
       revision       = NEW.revision,
       product_id     = NEW.product_id,
       producttime    = NEW.producttime,
       postingtime    = NEW.postingtime
       WHERE lid      = NEW.lid
       and pe         = NEW.pe
       and dur        = NEW.dur
       and ts         = NEW.ts
       and extremum   = NEW.extremum
       and obstime    = NEW.obstime;
    else
       INSERT INTO CurPP
       VALUES ( NEW.lid, NEW.pe, NEW.dur, NEW.ts, NEW.extremum, NEW.obstime,
       NEW.value, NEW.shef_qual_code, NEW.quality_code, NEW.revision,
       NEW.product_id, NEW.producttime, NEW.postingtime);

       DELETE FROM CurPP
       WHERE lid      = OLD.lid
       AND   pe       = OLD.pe
       AND   dur      = OLD.dur
       AND   ts       = OLD.ts
       AND   extremum = OLD.extremum
       AND   obstime  = OLD.obstime;
    end if;

RETURN NEW;
END;
$$
    LANGUAGE plpgsql;


ALTER FUNCTION public.obs_rawpp_upd() OWNER TO awips;

--
-- TOC entry 33 (class 1255 OID 18897)
-- Dependencies: 6 723
-- Name: obs_river_ins(); Type: FUNCTION; Schema: public; Owner: awips
--

CREATE FUNCTION obs_river_ins() RETURNS trigger
    AS $$
DECLARE river RiverStatus%ROWTYPE;
BEGIN
    --
    -- dont insert data that is definitely bad or missing                  
    -- note the hardcoded number
	
    if (NEW.quality_code < 1073741824 or NEW.value = -9999.) then
        RETURN NEW;
    end if;
	 
    SELECT INTO river * FROM RiverStatus
    WHERE lid = NEW.lid
    AND   pe = NEW.pe
    AND   ts = NEW.ts;

    if FOUND then
	-- need to check for NULL obstime or the logical check will fail
	-- if updating, dont bother with null probability or basistime fields
		 	
	UPDATE RiverStatus 
	SET   dur = NEW.dur,
	extremum  = NEW.extremum,
	validtime = NEW.obstime,
	value     = NEW.value		
	WHERE lid = NEW.lid and pe = NEW.pe
	and ts = NEW.ts and (validtime < NEW.obstime or validtime IS NULL);
    else		
	INSERT INTO RiverStatus
	VALUES ( NEW.lid, NEW.pe, NEW.dur, NEW.ts, NEW.extremum, -1., 
	NEW.obstime, NULL, NEW.value);            
    end if;
				    
RETURN NEW;
END;
$$
    LANGUAGE plpgsql;


ALTER FUNCTION public.obs_river_ins() OWNER TO awips;

--
-- TOC entry 34 (class 1255 OID 18898)
-- Dependencies: 723 6
-- Name: obs_river_upd(); Type: FUNCTION; Schema: public; Owner: awips
--

CREATE FUNCTION obs_river_upd() RETURNS trigger
    AS $$
DECLARE
    iret_code INT;
BEGIN
    if (NEW.value = OLD.value AND NEW.obstime = OLD.obstime AND NEW.value != -9999.)
    then
        RETURN NEW;
    end if;
    iret_code := load_obs_river (NEW.lid, NEW.pe, NEW.ts);
RETURN NEW;
END;
$$
    LANGUAGE plpgsql;


ALTER FUNCTION public.obs_river_upd() OWNER TO awips;

--
-- TOC entry 35 (class 1255 OID 18899)
-- Dependencies: 6
-- Name: plpgsql_call_handler(); Type: FUNCTION; Schema: public; Owner: postgres
--

CREATE FUNCTION plpgsql_call_handler() RETURNS language_handler
    AS '$libdir/plpgsql', 'plpgsql_call_handler'
    LANGUAGE c;


