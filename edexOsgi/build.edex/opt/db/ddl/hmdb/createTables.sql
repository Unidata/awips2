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
--
-- PostgreSQL database dump
--

SET client_encoding = 'SQL_ASCII';
SET standard_conforming_strings = off;
SET check_function_bodies = false;
SET client_min_messages = warning;
SET escape_string_warning = off;

--
-- Name: SCHEMA public; Type: COMMENT; Schema: -; Owner: postgres
--

COMMENT ON SCHEMA public IS 'Standard public schema';


SET search_path = public, pg_catalog;

SET default_tablespace = '';

SET default_with_oids = false;

--
-- Name: boolean_values; Type: TABLE; Schema: public; Owner: pguser; Tablespace: 
--

CREATE TABLE boolean_values (
    element_id integer NOT NULL,
    element_value smallint NOT NULL,
    weather_type character varying(20) NOT NULL,
    varchar_of_wx character(20),
    intensity_of_wx character varying(10),
    wx_full_string character varying(10) NOT NULL,
    wx_type_substr character varying(5) NOT NULL,
    varchar_wx_substr character(5),
    ints_wx_substr character varying(5),
    weather_category character varying(15),
    precip_state character varying(1),
    occurring_flag character varying(1) DEFAULT 'Y'::character varying,
    misg_indicator character varying(1) DEFAULT 'N'::character varying
);


ALTER TABLE public.boolean_values OWNER TO pguser;

--
-- Name: bufr_identifier; Type: TABLE; Schema: public; Owner: pguser; Tablespace: 
--

CREATE TABLE bufr_identifier (
    bufr_descriptor smallint NOT NULL,
    bufr_f smallint,
    bufr_x smallint,
    bufr_y smallint,
    bufr_name character varying(63)
);


ALTER TABLE public.bufr_identifier OWNER TO pguser;

--
-- Name: cat_values; Type: TABLE; Schema: public; Owner: pguser; Tablespace: 
--

CREATE TABLE cat_values (
    element_id integer NOT NULL,
    category_value smallint NOT NULL,
    lower_range_value real NOT NULL,
    upper_range_value real NOT NULL,
    category_text character varying(10)
);


ALTER TABLE public.cat_values OWNER TO pguser;

--
-- Name: categorical_ele; Type: TABLE; Schema: public; Owner: pguser; Tablespace: 
--

CREATE TABLE categorical_ele (
    element_id integer NOT NULL,
    num_ranges smallint NOT NULL,
    max_cat_value smallint,
    min_cat_value smallint,
    misg_cat_value smallint DEFAULT -9999,
    text_flag character varying(1) DEFAULT 'N'::character varying,
    range_value_units character varying(15),
    scale_factor real DEFAULT 1.00000000
);


ALTER TABLE public.categorical_ele OWNER TO pguser;

--
-- Name: cli_asos_daily; Type: TABLE; Schema: public; Owner: pguser; Tablespace: 
--

CREATE TABLE cli_asos_daily (
    station_code character varying(9) NOT NULL,
    valid_time time without time zone,
    "year" smallint,
    day_of_year character varying(5) NOT NULL,
    maxtemp_cal smallint,
    maxtemp_cal_time time without time zone,
    mintemp_cal smallint,
    mintemp_cal_time time without time zone,
    maxtemp_day smallint,
    mintemp_day smallint,
    min_press real,
    min_press_time time without time zone,
    equiv_water real,
    pcp_hr_amt_01 real,
    pcp_hr_amt_02 real,
    pcp_hr_amt_03 real,
    pcp_hr_amt_04 real,
    pcp_hr_amt_05 real,
    pcp_hr_amt_06 real,
    pcp_hr_amt_07 real,
    pcp_hr_amt_08 real,
    pcp_hr_amt_09 real,
    pcp_hr_amt_10 real,
    pcp_hr_amt_11 real,
    pcp_hr_amt_12 real,
    pcp_hr_amt_13 real,
    pcp_hr_amt_14 real,
    pcp_hr_amt_15 real,
    pcp_hr_amt_16 real,
    pcp_hr_amt_17 real,
    pcp_hr_amt_18 real,
    pcp_hr_amt_19 real,
    pcp_hr_amt_20 real,
    pcp_hr_amt_21 real,
    pcp_hr_amt_22 real,
    pcp_hr_amt_23 real,
    pcp_hr_amt_24 real,
    twomin_wspd real,
    max2min_wdir smallint,
    max2min_wspd smallint,
    max2min_wnd_time time without time zone,
    pkwnd_dir smallint,
    pkwnd_spd smallint,
    pkwnd_time time without time zone,
    wx1 smallint,
    wx2 smallint,
    wx3 smallint,
    wx4 smallint,
    wx5 smallint,
    min_sun smallint,
    percent_sun real,
    solid_precip real,
    snowdepth smallint,
    sky_cover real,
    avg_sky_cover real,
    remarks character varying(20)
);


ALTER TABLE public.cli_asos_daily OWNER TO pguser;

--
-- Name: cli_asos_monthly; Type: TABLE; Schema: public; Owner: pguser; Tablespace: 
--

CREATE TABLE cli_asos_monthly (
    station_code character varying(9) NOT NULL,
    "year" smallint,
    "month" smallint NOT NULL,
    maxtemp_mon smallint,
    maxtemp_day1 smallint,
    maxtemp_day2 smallint,
    maxtemp_day3 smallint,
    mintemp_mon smallint,
    mintemp_day1 smallint,
    mintemp_day2 smallint,
    mintemp_day3 smallint,
    avg_daily_max real,
    avg_daily_min real,
    avg_mon_temp real,
    days_maxt_blo_fzg smallint,
    days_maxt_hot smallint,
    days_mint_blo_fzg smallint,
    days_mint_blo_0f smallint,
    heating smallint,
    cooling smallint,
    mean_stn_press real,
    mean_sea_press real,
    max_sea_press real,
    max_slp_date character varying(8),
    max_slp_occur character varying(1),
    min_sea_press real,
    min_slp_date character varying(8),
    min_slp_occur character varying(1),
    month_precip real,
    days_hundreth smallint,
    days_tenth smallint,
    days_half smallint,
    days_inch smallint,
    max_24h_pcp real,
    max_24h_pcp_day1 smallint,
    max_24h_pcp_day2 smallint,
    shrtdurpcp5 real,
    shrtdurpcp10 real,
    shrtdurpcp15 real,
    shrtdurpcp20 real,
    shrtdurpcp30 real,
    shrtdurpcp45 real,
    shrtdurpcp60 real,
    shrtdurpcp80 real,
    shrtdurpcp100 real,
    shrtdurpcp120 real,
    shrtdurpcp150 real,
    shrtdurpcp180 real,
    shrtdurpcp_date5 character varying(8),
    shrtdurpcp_date10 character varying(8),
    shrtdurpcp_date15 character varying(8),
    shrtdurpcp_date20 character varying(8),
    shrtdurpcp_date30 character varying(8),
    shrtdurpcp_date45 character varying(8),
    shrtdurpcp_date60 character varying(8),
    shrtdurpcp_date80 character varying(8),
    shrtdurpcp_date100 character varying(8),
    shrtdurpcp_date120 character varying(8),
    shrtdurpcp_date150 character varying(8),
    shrtdurpcp_date180 character varying(8),
    hrs_sun real,
    percent_sun real,
    max_24h_snow real,
    max_24h_snow_date1 smallint,
    max_24h_snow_date2 smallint,
    snowdepth smallint,
    snowdepth_date smallint,
    clear_days smallint,
    pcloud_days smallint,
    cloud_days smallint,
    remarks character varying(20)
);


ALTER TABLE public.cli_asos_monthly OWNER TO pguser;

--
-- Name: cli_freezedates; Type: TABLE; Schema: public; Owner: pguser; Tablespace: 
--

CREATE TABLE cli_freezedates (
    inform_id integer,
    early_freeze date,
    last_freeze date,
    norm_early_freeze date,
    norm_last_freeze date,
    last_year_freeze date,
    rec_early_freeze date,
    rec_last_freeze date
);


ALTER TABLE public.cli_freezedates OWNER TO pguser;

--
-- Name: cli_mon_season_yr; Type: TABLE; Schema: public; Owner: pguser; Tablespace: 
--

CREATE TABLE cli_mon_season_yr (
    inform_id integer,
    period_type integer,
    period_start date,
    period_end date,
    max_temp smallint,
    max_temp_qc smallint,
    day_max_temp1 date,
    day_max_temp2 date,
    day_max_temp3 date,
    max_temp_mean real,
    avg_max_temp_qc smallint,
    mean_temp real,
    mean_temp_qc smallint,
    min_temp smallint,
    min_temp_qc smallint,
    day_min_temp1 date,
    day_min_temp2 date,
    day_min_temp3 date,
    min_temp_mean real,
    avg_min_temp_qc smallint,
    num_max_ge_90f smallint,
    max_temp_ge90_qc smallint,
    num_max_le_32f smallint,
    max_temp_le32_qc smallint,
    num_max_ge_t1f smallint,
    num_max_ge_t2f smallint,
    num_max_le_t3f smallint,
    num_min_le_32f smallint,
    min_temp_le32_qc smallint,
    num_min_le_0f smallint,
    min_temp_le0_qc smallint,
    num_min_ge_t4f smallint,
    num_min_le_t5f smallint,
    num_min_le_t6f smallint,
    precip_total double precision,
    precip_qc smallint,
    precip_max_24h double precision,
    precip_max_24h_qc smallint,
    pcp_24h_start_day1 character varying(13),
    pcp_24h_start_day2 character varying(13),
    pcp_24h_start_day3 character varying(13),
    pcp_24h_end_day1 character varying(13),
    pcp_24h_end_day2 character varying(13),
    pcp_24h_end_day3 character varying(13),
    precip_storm_max double precision,
    pcp_stm_start_day1 character varying(13),
    pcp_stm_start_day2 character varying(13),
    pcp_stm_start_day3 character varying(13),
    pcp_stm_end_day1 character varying(13),
    pcp_stm_end_day2 character varying(13),
    pcp_stm_end_day3 character varying(13),
    precip_mean_day double precision,
    num_prcp_ge_01 smallint,
    precip_ge01_qc smallint,
    num_prcp_ge_10 smallint,
    precip_ge10_qc smallint,
    num_prcp_ge_50 smallint,
    precip_ge50_qc smallint,
    num_prcp_ge_100 smallint,
    precip_ge100_qc smallint,
    num_prcp_ge_p1 smallint,
    num_prcp_ge_p2 smallint,
    snow_total double precision,
    snow_max_24h double precision,
    snow_max_24h_qc smallint,
    sno_24h_start_day1 character varying(13),
    sno_24h_start_day2 character varying(13),
    sno_24h_start_day3 character varying(13),
    sno_24h_end_day1 character varying(13),
    sno_24h_end_day2 character varying(13),
    sno_24h_end_day3 character varying(13),
    snow_max_storm double precision,
    sno_stm_start_day1 character varying(13),
    sno_stm_start_day2 character varying(13),
    sno_stm_start_day3 character varying(13),
    sno_stm_end_day1 character varying(13),
    sno_stm_end_day2 character varying(13),
    sno_stm_end_day3 character varying(13),
    snow_water double precision,
    snow_july1 double precision,
    snow_water_july1 double precision,
    snow_ground_mean double precision,
    snow_ground_max smallint,
    max_depth_qc smallint,
    snow_ground_date1 date,
    snow_ground_date2 date,
    snow_ground_date3 date,
    num_snow_ge_tr smallint,
    num_snow_ge_1 smallint,
    num_snow_ge_s1 smallint,
    num_heat_total smallint,
    heat_qc smallint,
    num_heat_1july smallint,
    num_cool_total smallint,
    cool_qc smallint,
    num_cool_1jan smallint,
    avg_wind_spd real,
    result_wind_dir smallint,
    result_wind_spd smallint,
    max_wind_spd real,
    max_wind_dir1 smallint,
    max_wind_dir2 smallint,
    max_wind_dir3 smallint,
    max_wind_date1 date,
    max_wind_date2 date,
    max_wind_date3 date,
    max_gust_spd real,
    max_gust_dir1 smallint,
    max_gust_dir2 smallint,
    max_gust_dir3 smallint,
    max_gust_date1 date,
    max_gust_date2 date,
    max_gust_date3 date,
    poss_sun smallint,
    poss_sun_qc smallint,
    mean_sky_cover real,
    num_fair smallint,
    fair_days_qc smallint,
    num_pc smallint,
    pc_days_qc smallint,
    num_mc smallint,
    mc_days_qc smallint,
    num_t smallint,
    num_p smallint,
    num_rrr smallint,
    num_rr smallint,
    num_r smallint,
    num_zrr smallint,
    num_zr smallint,
    num_a smallint,
    num_sss smallint,
    num_ss smallint,
    num_s smallint,
    num_ip smallint,
    num_f smallint,
    num_fquarter smallint,
    num_h smallint,
    mean_rh smallint
);


ALTER TABLE public.cli_mon_season_yr OWNER TO pguser;

--
-- Name: cli_sta_setup; Type: TABLE; Schema: public; Owner: pguser; Tablespace: 
--

CREATE TABLE cli_sta_setup (
    station_id integer NOT NULL,
    station_code character varying(9),
    cli_sta_name character varying(30),
    office_id character varying(7),
    latitude_n double precision,
    longitude_e double precision,
    hours_ahead_utc smallint,
    std_all_year smallint
);


ALTER TABLE public.cli_sta_setup OWNER TO pguser;

--
-- Name: climate_day_config; Type: TABLE; Schema: public; Owner: pguser; Tablespace: 
--

CREATE TABLE climate_day_config (
    prod_id character varying(3) NOT NULL,
    time_of_day character varying(2) NOT NULL
);


ALTER TABLE public.climate_day_config OWNER TO pguser;

--
-- Name: climate_period; Type: TABLE; Schema: public; Owner: pguser; Tablespace: 
--

CREATE TABLE climate_period (
    station_id integer NOT NULL,
    normal_start smallint,
    normal_end smallint,
    record_start smallint,
    record_end smallint
);


ALTER TABLE public.climate_period OWNER TO pguser;

--
-- Name: climo_dates; Type: TABLE; Schema: public; Owner: pguser; Tablespace: 
--

CREATE TABLE climo_dates (
    snow_season_beg character varying(5),
    snow_year_beg character varying(5),
    precip_season_beg character varying(5),
    precip_year_beg character varying(5),
    snow_season_end character varying(5),
    snow_year_end character varying(5),
    precip_season_end character varying(5),
    precip_year_end character varying(5)
);


ALTER TABLE public.climo_dates OWNER TO pguser;

--
-- Name: contin_int_ele; Type: TABLE; Schema: public; Owner: pguser; Tablespace: 
--

CREATE TABLE contin_int_ele (
    element_id integer NOT NULL,
    maximum_value smallint,
    minimum_value smallint,
    missing_value smallint DEFAULT -9999,
    data_value_units character varying(15),
    scale_factor real DEFAULT 1.00000000,
    add_factor smallint,
    bounded_flag character varying(1) NOT NULL
);


ALTER TABLE public.contin_int_ele OWNER TO pguser;

--
-- Name: contin_real_ele; Type: TABLE; Schema: public; Owner: pguser; Tablespace: 
--

CREATE TABLE contin_real_ele (
    element_id integer NOT NULL,
    maximum_value real,
    minimum_value real,
    missing_value real DEFAULT -9999.00000000,
    data_value_units character varying(15),
    scale_factor real DEFAULT 1.00000000,
    bounded_flag character varying(1) NOT NULL
);


ALTER TABLE public.contin_real_ele OWNER TO pguser;

--
-- Name: coordinates_2d; Type: TABLE; Schema: public; Owner: pguser; Tablespace: 
--

CREATE TABLE coordinates_2d (
    coord_type character varying(2) NOT NULL,
    coord_system character varying(20) NOT NULL,
    map_proj_id character varying(10),
    first_coord_def character varying(10) NOT NULL,
    second_coord_def character varying(10) NOT NULL,
    first_coord_unit character varying(15),
    second_coord_unit character varying(15)
);


ALTER TABLE public.coordinates_2d OWNER TO pguser;

--
-- Name: daily_climate; Type: TABLE; Schema: public; Owner: pguser; Tablespace: 
--

CREATE TABLE daily_climate (
    station_id integer NOT NULL,
    date date NOT NULL,
    max_temp smallint,
    max_temp_time time without time zone,
    max_temp_meth smallint,
    min_temp smallint,
    min_temp_time time without time zone,
    min_temp_meth smallint,
    precip real,
    precip_meth smallint,
    snow real,
    snow_meth smallint,
    snow_ground real,
    ground_meth smallint,
    heat integer,
    cool integer,
    max_wind_dir smallint,
    max_wind_spd real,
    max_wind_time time without time zone,
    max_wind_meth smallint,
    max_gust_dir smallint,
    max_gust_spd real,
    max_gust_time time without time zone,
    max_gust_meth smallint,
    result_wind_dir smallint,
    result_wind_spd smallint,
    result_x double precision,
    result_y double precision,
    num_wind_obs integer,
    avg_wind_speed real,
    avg_wind_meth smallint,
    min_sun smallint,
    min_sun_meth smallint,
    percent_pos_sun smallint,
    poss_sun_meth smallint,
    avg_sky_cover real,
    sky_cover_meth smallint,
    max_rh smallint,
    max_rh_hour smallint,
    min_rh smallint,
    min_rh_hour smallint,
    max_slp real,
    min_slp real,
    wx_meth smallint,
    number_obs_wx smallint,
    wx_1 smallint,
    wx_2 smallint,
    wx_3 smallint,
    wx_4 smallint,
    wx_5 smallint,
    wx_6 smallint,
    wx_7 smallint,
    wx_8 smallint,
    wx_9 smallint,
    wx_10 smallint,
    wx_11 smallint,
    wx_12 smallint,
    wx_13 smallint,
    wx_14 smallint,
    wx_15 smallint,
    wx_16 smallint,
    wx_17 smallint,
    wx_18 smallint
);


ALTER TABLE public.daily_climate OWNER TO pguser;

--
-- Name: data_source; Type: TABLE; Schema: public; Owner: pguser; Tablespace: 
--

CREATE TABLE data_source (
    source_process character varying(20) NOT NULL,
    src_process_id smallint NOT NULL,
    src_descrip character varying(100)
);


ALTER TABLE public.data_source OWNER TO pguser;

--
-- Name: data_src_version; Type: TABLE; Schema: public; Owner: pguser; Tablespace: 
--

CREATE TABLE data_src_version (
    source_version_id smallint NOT NULL,
    source_process character varying(20),
    version_number smallint NOT NULL,
    effective_date date DEFAULT ('now'::text)::date,
    supersede_date date
);


ALTER TABLE public.data_src_version OWNER TO pguser;

--
-- Name: day_climate_extreme; Type: TABLE; Schema: public; Owner: postgres; Tablespace: 
--

CREATE TABLE day_climate_extreme (
    station_code character varying(9) NOT NULL,
    day_of_year character varying(5) NOT NULL,
    h_max_temp_record1 smallint,
    h_max_temp_record2 smallint,
    h_max_temp_record3 smallint,
    h_max_temp_rec_yr1 smallint,
    h_max_temp_rec_yr2 smallint,
    h_max_temp_rec_yr3 smallint,
    l_min_temp_record1 smallint,
    l_min_temp_record2 smallint,
    l_min_temp_record3 smallint,
    l_min_temp_rec_yr1 smallint,
    l_min_temp_rec_yr2 smallint,
    l_min_temp_rec_yr3 smallint,
    l_max_temp_record1 smallint,
    l_max_temp_record2 smallint,
    l_max_temp_record3 smallint,
    l_max_temp_rec_yr1 smallint,
    l_max_temp_rec_yr2 smallint,
    l_max_temp_rec_yr3 smallint,
    h_min_temp_record1 smallint,
    h_min_temp_record2 smallint,
    h_min_temp_record3 smallint,
    h_min_temp_rec_yr1 smallint,
    h_min_temp_rec_yr2 smallint,
    h_min_temp_rec_yr3 smallint,
    precip_day_max1 real,
    precip_day_max2 real,
    precip_day_max3 real,
    precip_day_max_yr1 smallint,
    precip_day_max_yr2 smallint,
    precip_day_max_yr3 smallint
);


ALTER TABLE public.day_climate_extreme OWNER TO postgres;

--
-- Name: day_climate_norm; Type: TABLE; Schema: public; Owner: pguser; Tablespace: 
--

CREATE TABLE day_climate_norm (
    station_id integer NOT NULL,
    day_of_year character varying(5) NOT NULL,
    mean_temp real,
    max_temp_record smallint,
    max_temp_mean smallint,
    min_temp_record smallint,
    min_temp_mean smallint,
    max_temp_rec_yr1 smallint,
    max_temp_rec_yr2 smallint,
    max_temp_rec_yr3 smallint,
    min_temp_rec_yr1 smallint,
    min_temp_rec_yr2 smallint,
    min_temp_rec_yr3 smallint,
    precip_mean real,
    precip_day_max real,
    precip_day_max_yr1 smallint,
    precip_day_max_yr2 smallint,
    precip_day_max_yr3 smallint,
    snow_mean real,
    snow_day_max real,
    snow_day_max_yr1 smallint,
    snow_day_max_yr2 smallint,
    snow_day_max_yr3 smallint,
    snow_ground_mean real,
    heat_day_mean integer,
    cool_day_mean integer
);


ALTER TABLE public.day_climate_norm OWNER TO pguser;

--
-- Name: defined_values; Type: TABLE; Schema: public; Owner: pguser; Tablespace: 
--

CREATE TABLE defined_values (
    element_id integer NOT NULL,
    defined_value smallint NOT NULL
);


ALTER TABLE public.defined_values OWNER TO pguser;

--
-- Name: discrete_ele; Type: TABLE; Schema: public; Owner: pguser; Tablespace: 
--

CREATE TABLE discrete_ele (
    element_id integer NOT NULL,
    num_defined_vals smallint NOT NULL,
    maximum_value smallint,
    minimum_value smallint,
    missing_value smallint DEFAULT -9999,
    data_value_units character varying(15),
    scale_factor real DEFAULT 1.00000000,
    add_factor smallint
);


ALTER TABLE public.discrete_ele OWNER TO pguser;

--
-- Name: discrete_values; Type: TABLE; Schema: public; Owner: pguser; Tablespace: 
--

CREATE TABLE discrete_values (
    element_id integer NOT NULL,
    discrete_value smallint NOT NULL,
    value_number smallint
);


ALTER TABLE public.discrete_values OWNER TO pguser;

--
-- Name: dqd; Type: TABLE; Schema: public; Owner: pguser; Tablespace: 
--

CREATE TABLE dqd (
    dqd character varying(1) NOT NULL,
    dqd_description character varying(20),
    dqd_method character varying(20),
    dqd_hierarchy smallint
);


ALTER TABLE public.dqd OWNER TO pguser;

--
-- Name: ele_src_version; Type: TABLE; Schema: public; Owner: pguser; Tablespace: 
--

CREATE TABLE ele_src_version (
    ele_src_version integer NOT NULL,
    prod_version smallint,
    source_version_id smallint,
    src_ele_name character varying(20),
    element_id integer
);


ALTER TABLE public.ele_src_version OWNER TO pguser;

--
-- Name: ele_src_version_ele_src_version_seq; Type: SEQUENCE; Schema: public; Owner: pguser
--

CREATE SEQUENCE ele_src_version_ele_src_version_seq
    START WITH 1
    INCREMENT BY 1
    NO MAXVALUE
    NO MINVALUE
    CACHE 1;


ALTER TABLE public.ele_src_version_ele_src_version_seq OWNER TO pguser;

--
-- Name: ele_src_version_ele_src_version_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: pguser
--

ALTER SEQUENCE ele_src_version_ele_src_version_seq OWNED BY ele_src_version.ele_src_version;


--
-- Name: ele_src_version_ele_src_version_seq; Type: SEQUENCE SET; Schema: public; Owner: pguser
--

SELECT pg_catalog.setval('ele_src_version_ele_src_version_seq', 1, false);


--
-- Name: element_relat; Type: TABLE; Schema: public; Owner: pguser; Tablespace: 
--

CREATE TABLE element_relat (
    ele_relat_id character varying(15) NOT NULL,
    ele_relat_type character varying(20),
    num_relat_eles smallint
);


ALTER TABLE public.element_relat OWNER TO pguser;

--
-- Name: forecast_backup; Type: TABLE; Schema: public; Owner: pguser; Tablespace: 
--

CREATE TABLE forecast_backup (
    backup_status character varying(1) NOT NULL,
    backup_description character varying(63)
);


ALTER TABLE public.forecast_backup OWNER TO pguser;

--
-- Name: fss_categ_multi; Type: TABLE; Schema: public; Owner: pguser; Tablespace: 
--

CREATE TABLE fss_categ_multi (
    fss_rpt_instance integer NOT NULL,
    element_id integer NOT NULL,
    wx_ele_number smallint NOT NULL,
    element_value smallint NOT NULL,
    dqd character varying(1) DEFAULT 'Z'::character varying
);


ALTER TABLE public.fss_categ_multi OWNER TO pguser;

--
-- Name: fss_categ_single; Type: TABLE; Schema: public; Owner: pguser; Tablespace: 
--

CREATE TABLE fss_categ_single (
    fss_rpt_instance integer NOT NULL,
    element_id integer NOT NULL,
    element_value smallint NOT NULL,
    dqd character varying(1) DEFAULT 'Z'::character varying
);


ALTER TABLE public.fss_categ_single OWNER TO pguser;

--
-- Name: fss_cloud_layer; Type: TABLE; Schema: public; Owner: pguser; Tablespace: 
--

CREATE TABLE fss_cloud_layer (
    fss_rpt_instance integer NOT NULL,
    element_id integer NOT NULL,
    layer_number smallint NOT NULL,
    element_value smallint NOT NULL,
    dqd character varying(1) DEFAULT 'Z'::character varying
);


ALTER TABLE public.fss_cloud_layer OWNER TO pguser;

--
-- Name: fss_contin_real; Type: TABLE; Schema: public; Owner: pguser; Tablespace: 
--

CREATE TABLE fss_contin_real (
    fss_rpt_instance integer NOT NULL,
    element_id integer NOT NULL,
    element_value real NOT NULL,
    dqd character varying(1) DEFAULT 'Z'::character varying
);


ALTER TABLE public.fss_contin_real OWNER TO pguser;

--
-- Name: fss_report; Type: TABLE; Schema: public; Owner: pguser; Tablespace: 
--

CREATE TABLE fss_report (
    fss_rpt_instance integer NOT NULL,
    station_id integer,
    report_type character varying(20),
    report_subtype character varying(10),
    augmentation character varying(3),
    correction character varying(1) DEFAULT 'F'::character varying,
    valid_dtime timestamp without time zone NOT NULL,
    nominal_dtime timestamp without time zone,
    origin_dtime timestamp without time zone,
    time_zone_id character varying(2) DEFAULT 'UT'::character varying,
    observing_mode smallint,
    prod_version smallint,
    source_version_id smallint,
    source_status smallint,
    wx_ele_count smallint,
    cloud_layers smallint
);


ALTER TABLE public.fss_report OWNER TO pguser;

--
-- Name: fss_report_fss_rpt_instance_seq; Type: SEQUENCE; Schema: public; Owner: pguser
--

CREATE SEQUENCE fss_report_fss_rpt_instance_seq
    INCREMENT BY 1
    NO MAXVALUE
    NO MINVALUE
    CACHE 1;


ALTER TABLE public.fss_report_fss_rpt_instance_seq OWNER TO pguser;

--
-- Name: fss_report_fss_rpt_instance_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: pguser
--

ALTER SEQUENCE fss_report_fss_rpt_instance_seq OWNED BY fss_report.fss_rpt_instance;


--
-- Name: fss_report_fss_rpt_instance_seq; Type: SEQUENCE SET; Schema: public; Owner: pguser
--

SELECT pg_catalog.setval('fss_report_fss_rpt_instance_seq', 160692, true);


--
-- Name: fss_wx_period; Type: TABLE; Schema: public; Owner: pguser; Tablespace: 
--

CREATE TABLE fss_wx_period (
    fss_rpt_instance integer NOT NULL,
    element_id integer NOT NULL,
    element_value smallint NOT NULL,
    element_num smallint NOT NULL,
    wx_begin_dtime time without time zone,
    wx_end_dtime time without time zone
);


ALTER TABLE public.fss_wx_period OWNER TO pguser;

--
-- Name: hydromet_element; Type: TABLE; Schema: public; Owner: pguser; Tablespace: 
--

CREATE TABLE hydromet_element (
    element_id integer NOT NULL,
    bufr_descriptor smallint,
    element_name character varying(20) NOT NULL,
    element_descrip character varying(100),
    element_subtype character varying(25) NOT NULL,
    phys_element_id character varying(15),
    duration_type character varying(25),
    aggregation character varying(10),
    product_name character varying(20),
    source_process character varying(20),
    revision_number smallint
);


ALTER TABLE public.hydromet_element OWNER TO pguser;

--
-- Name: hydromet_element_element_id_seq; Type: SEQUENCE; Schema: public; Owner: pguser
--

CREATE SEQUENCE hydromet_element_element_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MAXVALUE
    NO MINVALUE
    CACHE 1;


ALTER TABLE public.hydromet_element_element_id_seq OWNER TO pguser;

--
-- Name: hydromet_element_element_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: pguser
--

ALTER SEQUENCE hydromet_element_element_id_seq OWNED BY hydromet_element.element_id;


--
-- Name: hydromet_element_element_id_seq; Type: SEQUENCE SET; Schema: public; Owner: pguser
--

SELECT pg_catalog.setval('hydromet_element_element_id_seq', 1, false);


--
-- Name: issuance_type; Type: TABLE; Schema: public; Owner: pguser; Tablespace: 
--

CREATE TABLE issuance_type (
    issuance_type_id smallint NOT NULL,
    issuance_type character varying(20)
);


ALTER TABLE public.issuance_type OWNER TO pguser;

--
-- Name: issuing_office; Type: TABLE; Schema: public; Owner: pguser; Tablespace: 
--

CREATE TABLE issuing_office (
    office_id character varying(7) NOT NULL,
    station_id integer,
    office_type character varying(3),
    office_name character varying(64),
    elevation_units character varying(15),
    elevation_value real DEFAULT -9999.00000000,
    elevation_datum character varying(3) DEFAULT 'MSL'::character varying,
    coord_type character varying(2),
    first_coord_val double precision DEFAULT -9999.00000000,
    second_coord_val double precision DEFAULT -9999.00000000,
    city character varying(80),
    state character varying(2),
    wmo_hdr_geo_area smallint
);


ALTER TABLE public.issuing_office OWNER TO pguser;

--
-- Name: station_location; Type: TABLE; Schema: public; Owner: pguser; Tablespace: 
--

CREATE TABLE station_location (
    station_id integer NOT NULL,
    elevation_units character varying(15),
    elevation_value real DEFAULT -9999.00000000,
    elevation_datum character varying(3) DEFAULT 'MSL'::character varying,
    coord_type character varying(2),
    first_coord_val double precision DEFAULT -9999.00000000,
    second_coord_val double precision DEFAULT -9999.00000000,
    first_valid_date date DEFAULT ('now'::text)::date,
    last_valid_date date,
    current_flag character varying(1),
    station_code character varying(9) NOT NULL,
    station_name character varying(80),
    county_borough character varying(30),
    state_province character varying(2),
    country character varying(2) DEFAULT 'US'::character varying,
    hours_ahead_utc smallint DEFAULT 0,
    wban integer
);


ALTER TABLE public.station_location OWNER TO pguser;

--
-- Name: local_stations; Type: VIEW; Schema: public; Owner: pguser
--

CREATE VIEW local_stations AS
    SELECT x0.station_id, x0.elevation_units, x0.elevation_value, x0.elevation_datum, x0.coord_type, x0.first_coord_val, x0.second_coord_val, x0.first_valid_date, x0.last_valid_date, x0.current_flag, x0.station_code, x0.station_name, x0.county_borough, x0.state_province, x0.country, x0.hours_ahead_utc, x0.wban FROM station_location x0 WHERE (x0.station_id > 2000000000);


ALTER TABLE public.local_stations OWNER TO pguser;

--
-- Name: map_proj_coords; Type: TABLE; Schema: public; Owner: pguser; Tablespace: 
--

CREATE TABLE map_proj_coords (
    map_proj_id character varying(10) NOT NULL,
    map_proj character varying(30) NOT NULL,
    hemisphere character varying(8) DEFAULT 'NORTHERN'::character varying,
    lat_convention character varying(5) DEFAULT 'NORTH'::character varying,
    lon_convention character varying(4) DEFAULT 'EAST'::character varying,
    first_lat_value double precision,
    second_lat_value double precision,
    first_lon_value double precision,
    first_lon_type character varying(10),
    rotation_angle real DEFAULT 0.00000000,
    true_lat_value double precision,
    true_lon_value double precision,
    delta_xy_units character varying(15),
    delta_x_i_value real,
    delta_y_j_value real,
    origin_lat_value double precision,
    origin_lon_value double precision,
    num_of_xi_points integer,
    num_of_yj_points integer,
    lastx_lat_value double precision,
    lasty_lon_value double precision,
    earth_radius real DEFAULT 6371200.00000000,
    radius_units character varying(15) DEFAULT 'm'::character varying
);


ALTER TABLE public.map_proj_coords OWNER TO pguser;

--
-- Name: mon_climate_norm; Type: TABLE; Schema: public; Owner: pguser; Tablespace: 
--

CREATE TABLE mon_climate_norm (
    station_id integer NOT NULL,
    month_of_year smallint NOT NULL,
    period_type smallint NOT NULL,
    max_temp_mean real,
    max_temp_record smallint,
    day_max_temp_rec1 date,
    day_max_temp_rec2 date,
    day_max_temp_rec3 date,
    min_temp_mean real,
    min_temp_record smallint,
    day_min_temp_rec1 date,
    day_min_temp_rec2 date,
    day_min_temp_rec3 date,
    norm_mean_temp real,
    norm_mean_max_temp real,
    norm_mean_min_temp real,
    num_max_ge_90f real,
    num_max_le_32f real,
    num_min_le_32f real,
    num_min_le_0f real,
    precip_pd_mean real,
    precip_pd_max real,
    precip_pd_max_yr1 smallint,
    precip_pd_max_yr2 smallint,
    precip_pd_max_yr3 smallint,
    precip_period_min real,
    precip_pd_min_yr1 smallint,
    precip_pd_min_yr2 smallint,
    precip_pd_min_yr3 smallint,
    precip_day_norm real,
    num_prcp_ge_01 real,
    num_prcp_ge_10 real,
    num_prcp_ge_50 real,
    num_prcp_ge_100 real,
    snow_pd_mean real,
    snow_pd_max real,
    snow_pd_max_yr1 smallint,
    snow_pd_max_yr2 smallint,
    snow_pd_max_yr3 smallint,
    snow_24h_begin1 date,
    snow_24h_begin2 date,
    snow_24h_begin3 date,
    snow_max_24h_rec real,
    snow_24h_end1 date,
    snow_24h_end2 date,
    snow_24h_end3 date,
    snow_water_pd_norm real,
    snow_ground_norm real,
    snow_ground_max smallint,
    day_snow_grnd_max1 date,
    day_snow_grnd_max2 date,
    day_snow_grnd_max3 date,
    num_snow_ge_tr real,
    num_snow_ge_1 real,
    heat_pd_mean integer,
    cool_pd_mean integer
);


ALTER TABLE public.mon_climate_norm OWNER TO pguser;

--
-- Name: mtr_status; Type: TABLE; Schema: public; Owner: pguser; Tablespace: 
--

CREATE TABLE mtr_status (
    process_dtime character varying(13) NOT NULL,
    process_status smallint,
    finish_dtime timestamp without time zone
);


ALTER TABLE public.mtr_status OWNER TO pguser;

--
-- Name: phys_ele_relat; Type: TABLE; Schema: public; Owner: pguser; Tablespace: 
--

CREATE TABLE phys_ele_relat (
    ele_relat_id character varying(15) NOT NULL,
    phys_element_id character varying(15) NOT NULL
);


ALTER TABLE public.phys_ele_relat OWNER TO pguser;

--
-- Name: physical_element; Type: TABLE; Schema: public; Owner: pguser; Tablespace: 
--

CREATE TABLE physical_element (
    phys_element_id character varying(15) NOT NULL,
    phys_element_name character varying(30) NOT NULL,
    phys_element_descr character varying(100),
    phys_ele_subtype character varying(20),
    units_class character varying(31),
    relationships smallint DEFAULT 0
);


ALTER TABLE public.physical_element OWNER TO pguser;

--
-- Name: physical_units; Type: TABLE; Schema: public; Owner: pguser; Tablespace: 
--

CREATE TABLE physical_units (
    physical_units character varying(15) NOT NULL,
    units_id smallint,
    units_long_name character varying(30) NOT NULL,
    units_class character varying(31),
    units_system character varying(10)
);


ALTER TABLE public.physical_units OWNER TO pguser;

--
-- Name: prod_list; Type: TABLE; Schema: public; Owner: pguser; Tablespace: 
--

CREATE TABLE prod_list (
    issuing_office character varying(4) NOT NULL,
    office_icao_id character varying(5) NOT NULL,
    ccc_id character varying(4) NOT NULL,
    nnn_id character varying(4) NOT NULL,
    xxx_id character varying(4) NOT NULL
);


ALTER TABLE public.prod_list OWNER TO pguser;

--
-- Name: product; Type: TABLE; Schema: public; Owner: pguser; Tablespace: 
--

CREATE TABLE product (
    product_name character varying(20) NOT NULL,
    description character varying(127),
    product_id smallint NOT NULL,
    product_usage character varying(20),
    product_grade character varying(20),
    product_areal character varying(20),
    product_temporal character varying(20),
    product_source character varying(20),
    internal_external character varying(10)
);


ALTER TABLE public.product OWNER TO pguser;

--
-- Name: product_version; Type: TABLE; Schema: public; Owner: pguser; Tablespace: 
--

CREATE TABLE product_version (
    prod_version smallint NOT NULL,
    product_name character varying(20),
    version_number smallint NOT NULL,
    version_descrip character varying(200),
    effective_date date,
    supersede_date date,
    current_flag character varying(1) NOT NULL
);


ALTER TABLE public.product_version OWNER TO pguser;

--
-- Name: relat_type; Type: TABLE; Schema: public; Owner: pguser; Tablespace: 
--

CREATE TABLE relat_type (
    ele_relat_type character varying(20) NOT NULL,
    relat_descrip character varying(100)
);


ALTER TABLE public.relat_type OWNER TO pguser;

--
-- Name: rpt; Type: TABLE; Schema: public; Owner: pguser; Tablespace: 
--

CREATE TABLE rpt (
    report_type character varying(9) NOT NULL,
    "mode" integer,
    icao_loc_id character varying(5) NOT NULL,
    wmo_dd character varying(7) NOT NULL,
    afos_dd character varying(7),
    origin timestamp without time zone,
    date timestamp without time zone NOT NULL,
    nominal timestamp without time zone NOT NULL,
    report character varying(255),
    "prior" integer,
    lat double precision,
    lon double precision,
    elev double precision,
    state character varying(3),
    country character varying(3),
    name character varying(20),
    sequence_num integer NOT NULL,
    status integer
);


ALTER TABLE public.rpt OWNER TO pguser;

--
-- Name: sta_agency_codes; Type: TABLE; Schema: public; Owner: pguser; Tablespace: 
--

CREATE TABLE sta_agency_codes (
    station_id integer NOT NULL,
    naming_agency character varying(10) NOT NULL,
    agency_subsystem character varying(10) NOT NULL,
    location_type character varying(20),
    agency_sta_code character varying(10) NOT NULL,
    agency_sta_name character varying(30)
);


ALTER TABLE public.sta_agency_codes OWNER TO pguser;

--
-- Name: time_zone; Type: TABLE; Schema: public; Owner: pguser; Tablespace: 
--

CREATE TABLE time_zone (
    time_zone_id character varying(2) NOT NULL,
    time_zone_name character varying(3),
    time_zone_descrip character varying(50),
    local_minus_utc interval,
    daylight_saving character varying(1) DEFAULT 'N'::character varying
);


ALTER TABLE public.time_zone OWNER TO pguser;

--
-- Name: units_class; Type: TABLE; Schema: public; Owner: pguser; Tablespace: 
--

CREATE TABLE units_class (
    units_class character varying(31) NOT NULL
);


ALTER TABLE public.units_class OWNER TO pguser;

--
-- Name: units_conversion; Type: TABLE; Schema: public; Owner: pguser; Tablespace: 
--

CREATE TABLE units_conversion (
    convert_from character varying(15) NOT NULL,
    convert_to character varying(15) NOT NULL,
    units_class character varying(31) NOT NULL,
    multiply_by double precision NOT NULL,
    then_add double precision NOT NULL,
    reference character varying(255)
);


ALTER TABLE public.units_conversion OWNER TO pguser;

--
-- Name: units_system; Type: TABLE; Schema: public; Owner: pguser; Tablespace: 
--

CREATE TABLE units_system (
    units_system character varying(10) NOT NULL,
    units_sys_descrip character varying(50)
);


ALTER TABLE public.units_system OWNER TO pguser;

--
-- Name: units_translations; Type: TABLE; Schema: public; Owner: pguser; Tablespace: 
--

CREATE TABLE units_translations (
    alien_units_desig character varying(31) NOT NULL,
    naming_agency character varying(10) NOT NULL,
    agency_subsystem character varying(10) NOT NULL,
    awips_units_desig character varying(15) NOT NULL
);


ALTER TABLE public.units_translations OWNER TO pguser;

--
-- Name: weather_category; Type: TABLE; Schema: public; Owner: pguser; Tablespace: 
--

CREATE TABLE weather_category (
    weather_category character varying(15) NOT NULL,
    wx_cat_descrip character varying(60)
);


ALTER TABLE public.weather_category OWNER TO pguser;

--
-- Name: wmo_state_region; Type: TABLE; Schema: public; Owner: pguser; Tablespace: 
--

CREATE TABLE wmo_state_region (
    state character varying(2) NOT NULL,
    wmo_header_region smallint NOT NULL
);


ALTER TABLE public.wmo_state_region OWNER TO pguser;

--
-- Name: ele_src_version; Type: DEFAULT; Schema: public; Owner: pguser
--

ALTER TABLE ele_src_version ALTER COLUMN ele_src_version SET DEFAULT nextval('ele_src_version_ele_src_version_seq'::regclass);


--
-- Name: fss_rpt_instance; Type: DEFAULT; Schema: public; Owner: pguser
--

ALTER TABLE fss_report ALTER COLUMN fss_rpt_instance SET DEFAULT nextval('fss_report_fss_rpt_instance_seq'::regclass);


--
-- Name: element_id; Type: DEFAULT; Schema: public; Owner: pguser
--

ALTER TABLE hydromet_element ALTER COLUMN element_id SET DEFAULT nextval('hydromet_element_element_id_seq'::regclass);


