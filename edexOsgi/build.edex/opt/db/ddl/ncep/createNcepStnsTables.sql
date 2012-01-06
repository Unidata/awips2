--
-- PostgreSQL database dump 
--
--
-- Name: SCHEMA public; Type: COMMENT; Schema: -; Owner: postgres
--
-------------------------------------------------
-- create stns tables
-- ---------------------------------------------
-- Create airep_waypnts table
DROP TABLE IF EXISTS stns.airep_waypnts CASCADE;
CREATE TABLE stns.airep_waypnts(
pkey  SERIAL PRIMARY KEY,
        station_id varchar(8)      NOT NULL,
        latitude  double precision NOT NULL,
        longitude double precision NOT NULL,
        ICAO_code varchar(4) NOT NULL,
        count_repeated_stationId smallint DEFAULT 1
);

-- Create buoys table
DROP TABLE IF EXISTS stns.BUOYS CASCADE; 
CREATE TABLE stns.BUOYS ( 
	PKEY SERIAL PRIMARY KEY, 
        STNUM int, 
        NAME varchar(32), 
        COUNTRY char(2), 
        LAT double precision, 
        LON double precision
);

-- Create cities table
DROP TABLE IF EXISTS stns.cities CASCADE;
CREATE TABLE stns.cities(
pkey  SERIAL PRIMARY KEY,
        station_id varchar(8)      NOT NULL,
        station_number varchar(6)      NOT NULL,
        name varchar(32)        NOT NULL,
        state varchar(2) NOT NULL,
        country varchar(2) NOT NULL,
        latitude  double precision NOT NULL,
        longitude double precision NOT NULL,
        priority smallint NOT NULL
);

-- Create climreg table
DROP TABLE IF EXISTS  stns.climreg CASCADE;
CREATE TABLE stns.climreg(
pkey  SERIAL PRIMARY KEY,
abbreviation varchar(12),
 full_name varchar(32)
);

-- create stns.countyclust table
DROP TABLE IF EXISTS stns.countyclustwfo CASCADE;
CREATE TABLE stns.countyclustwfo (
	pkey SERIAL PRIMARY KEY,
	wfo char(3),
	clustername varchar(32),
	cntyfipscode varchar(50)
);

-- create stns.countyclust table
DROP TABLE IF EXISTS stns.countycluststate CASCADE;
CREATE TABLE stns.countycluststate (
        pkey SERIAL PRIMARY KEY,
        state char(2),
        countyname varchar(32),
        cntycitifipscode varchar(50)
);

-- Create cnty_clst table
DROP TABLE IF EXISTS stns.cnty_clst CASCADE;
CREATE TABLE stns.cnty_clst(
pkey  SERIAL PRIMARY KEY,
        station_id varchar(8)      NOT NULL,
        station_number varchar(6)      NOT NULL,
        name varchar(32)        NOT NULL,
        state varchar(2) NOT NULL,
        country varchar(2) NOT NULL,
        latitude  double precision NOT NULL,
        longitude double precision NOT NULL,
        wfo_code varchar(3) NOT NULL
);

-- Create coastal table
DROP TABLE IF EXISTS stns.coastal CASCADE;
CREATE TABLE stns.coastal(
pkey  SERIAL PRIMARY KEY,
        station_id varchar(8)      NOT NULL,
        station_number varchar(6)      NOT NULL,
        name varchar(32)        NOT NULL,
        state varchar(2) NOT NULL,
        country varchar(2) NOT NULL,
        latitude  double precision NOT NULL,
        longitude double precision NOT NULL,
        elevation int NOT NULL,
        priority smallint NOT NULL
);

-- Create coordPts table
DROP TABLE IF EXISTS stns.coordPts CASCADE;
CREATE TABLE stns.coordPts(
pkey  SERIAL PRIMARY KEY,
        station_id varchar(8)      NOT NULL,
        station_number varchar(6)      NOT NULL,
        name varchar(32)        NOT NULL,
        latitude  double precision NOT NULL,
        longitude double precision NOT NULL,
        priority smallint NOT NULL
);

-- Create countynam table
DROP TABLE IF EXISTS stns.countynam CASCADE; 
CREATE TABLE stns.countynam ( 
	PKEY SERIAL PRIMARY KEY, 
        STID varchar(8), 
        STNUM int, 
        NAME varchar(32), 
        STATE char(2), 
        COUNTRY char(2), 
        LAT double precision, 
        LON double precision, 
        WFO varchar(3) 
);

-- Create county table
DROP TABLE IF EXISTS stns.county CASCADE;
CREATE TABLE stns.county(
pkey  SERIAL PRIMARY KEY,
        station_id varchar(8)      NOT NULL,
        station_number varchar(6)      NOT NULL,
        name varchar(32)        NOT NULL,
        state varchar(2) NOT NULL,
        country varchar(2) NOT NULL,
        latitude  double precision NOT NULL,
        longitude double precision NOT NULL,
        wfo_code  varchar(3) NOT NULL 
);

-- Create cpcstns table
DROP TABLE IF EXISTS stns.CPCSTNS CASCADE; 
CREATE TABLE stns.CPCSTNS ( 
	PKEY SERIAL PRIMARY KEY, 
        STID varchar(4), 
        STNUM int, 
        NAME varchar(32), 
        STATE char(2), 
        COUNTRY char(2), 
        LAT double precision, 
        LON double precision, 
        ELEV int, 
        PRI int
);
-- Create dlwx table
DROP TABLE IF EXISTS stns.DLWX CASCADE; 
CREATE TABLE stns.DLWX ( 
	PKEY SERIAL PRIMARY KEY, 
        STID varchar(4), 
        STNUM int, 
        NAME varchar(32), 
        STATE char(2), 
        COUNTRY char(2), 
        LAT double precision, 
        LON double precision, 
        ELEV int, 
        PRI int
);
-- Create ffgzon table
DROP TABLE IF EXISTS stns.FFGZON CASCADE; 
CREATE TABLE stns.FFGZON ( 
	PKEY SERIAL PRIMARY KEY, 
        STID varchar(8), 
        STNUM int, 
        NAME varchar(32), 
        STATE char(2), 
        COUNTRY char(2), 
        LAT double precision, 
        LON double precision, 
        WFO varchar(8)        
);
-- Create firezones table
DROP TABLE IF EXISTS stns.FIREZONES CASCADE; 
CREATE TABLE stns.FIREZONES ( 
	PKEY SERIAL PRIMARY KEY, 
        STID varchar(8), 
        STNUM int, 
        NAME varchar(32), 
        STATE char(2), 
        COUNTRY char(2), 
        LAT double precision, 
        LON double precision, 
        WFO varchar(8)        
);

--create geog table
DROP TABLE IF EXISTS stns.geog CASCADE;

CREATE TABLE stns.geog(
pkey SERIAL PRIMARY KEY,
geog_code varchar(8) NOT NULL, 
geog_name varchar(20) NOT NULL,
center_lat double precision NOT NULL,
center_lon double precision  NOT NULL,
ll_lat double precision NOT NULL,
ll_lon double precision NOT NULL,
ur_lat double precision NOT NULL,
ur_lon double precision NOT NULL,
projection_string varchar(30) NOT NULL);

-- create GFSMOS table
DROP TABLE IF EXISTS stns.gfsmos CASCADE;
CREATE TABLE stns.gfsmos(
pkey  SERIAL PRIMARY KEY,
        station_id varchar(8)      NOT NULL,
        station_number varchar(6)      NOT NULL,
        name varchar(32)        NOT NULL,
        state varchar(2) NOT NULL,
        country varchar(2) NOT NULL,
        latitude  double precision NOT NULL,
        longitude double precision NOT NULL,
        elevation int NOT NULL,
        priority smallint NOT NULL
);
-- create idft table
DROP TABLE IF EXISTS stns.idft CASCADE;
CREATE TABLE stns.idft(
        point_id  SERIAL PRIMARY KEY,
        latitude  double precision NOT NULL,
        longitude double precision NOT NULL
);

-- create inactive table
DROP TABLE IF EXISTS stns.inactive CASCADE;
CREATE TABLE stns.inactive(
pkey  SERIAL PRIMARY KEY,
        station_id varchar(8)      NOT NULL,
        station_number varchar(6)      NOT NULL,
        name varchar(32)        NOT NULL,
        state varchar(2) NOT NULL,
        country varchar(2) NOT NULL,
        latitude  double precision NOT NULL,
        longitude double precision NOT NULL,
        elevation int NOT NULL,
        priority smallint NOT NULL
);

-- create intlsig table
DROP TABLE IF EXISTS stns.intlsig CASCADE;
CREATE TABLE stns.intlsig(
pkey  SERIAL PRIMARY KEY,
        station_id varchar(8)      NOT NULL,
        station_number varchar(6)      NOT NULL,
        name varchar(32)        NOT NULL,
        state varchar(2) NOT NULL,
        country varchar(2) NOT NULL,
        latitude  double precision NOT NULL,
        longitude double precision NOT NULL,
        elevation int NOT NULL
);

-- create island table
DROP TABLE IF EXISTS stns.island CASCADE;
CREATE TABLE stns.island(
pkey  SERIAL PRIMARY KEY,
        name varchar(32)        NOT NULL,
        country varchar(2) NOT NULL,
        latitude  double precision NOT NULL,
        longitude double precision NOT NULL,
        elevation int NOT NULL
);

-- Create lsfstns table
DROP TABLE IF EXISTS stns.lsfstns CASCADE;
CREATE TABLE stns.lsfstns(
pkey  SERIAL PRIMARY KEY,
        station_id varchar(8)      NOT NULL,
        station_number varchar(6)      NOT NULL,
        name varchar(32)        NOT NULL,
        state varchar(2) NOT NULL,
        country varchar(2) NOT NULL,
        latitude  double precision NOT NULL,
        longitude double precision NOT NULL,
        elevation int NOT NULL,
        priority smallint NOT NULL,
        misc smallint NOT NULL
);

--create mardel table
DROP TABLE IF EXISTS stns.mardel CASCADE;
CREATE TABLE stns.mardel(
pkey serial primary key,
zone_id varchar(6) NOT NULL);

--create marinenames table
DROP TABLE IF EXISTS stns.marinenames CASCADE;
CREATE TABLE stns.marinenames(
pkey serial primary key,
marine_id varchar(6) NOT NULL,
name varchar(175) NOT NULL);

--create marine table
DROP TABLE IF EXISTS stns.marine CASCADE;
CREATE TABLE stns.marine(
pkey  SERIAL PRIMARY KEY,
        station_id varchar(8)      NOT NULL,
        station_number varchar(6)      NOT NULL,
        name varchar(32)        NOT NULL,
        state varchar(2) NOT NULL,
        country varchar(2) NOT NULL,
        latitude  double precision NOT NULL,
        longitude double precision NOT NULL,
        elevation int NOT NULL
);

--create msfstns table
DROP TABLE IF EXISTS stns.msfstns CASCADE;
CREATE TABLE stns.msfstns(
pkey  SERIAL PRIMARY KEY,
        station_id varchar(8)      NOT NULL,
        name varchar(32)        NOT NULL,
        state varchar(2) NOT NULL,
        country varchar(2) NOT NULL,
        latitude  double precision NOT NULL,
        longitude double precision NOT NULL,
        elevation int NOT NULL
);

--create mzcntys table
DROP TABLE IF EXISTS stns.mzcntys CASCADE;
CREATE TABLE stns.mzcntys(
pkey  SERIAL PRIMARY KEY,
        station_id varchar(8)      NOT NULL,
        station_number varchar(6)      NOT NULL,
        name varchar(32)        NOT NULL,
        state varchar(2) NOT NULL,
        country varchar(2) NOT NULL,
        latitude  double precision NOT NULL,
        longitude double precision NOT NULL,
        wfo_code varchar(3) NOT NULL
);

-- create nexrad table
DROP TABLE IF EXISTS stns.nexrad CASCADE;
CREATE TABLE stns.nexrad(
pkey  SERIAL PRIMARY KEY,
        station_id varchar(8)      NOT NULL,
        station_number varchar(6)      NOT NULL,
        name varchar(32)        NOT NULL,
        state varchar(2) NOT NULL,
        country varchar(2) NOT NULL,
        latitude  double precision NOT NULL,
        longitude double precision NOT NULL,
        elevation int NOT NULL,
        owner varchar(3) NOT NULL
);


--create ngmmos table
DROP TABLE IF EXISTS stns.ngmmos CASCADE;
CREATE TABLE stns.ngmmos(
pkey  SERIAL PRIMARY KEY,
        station_id varchar(8)      NOT NULL,
        station_number varchar(6)      NOT NULL,
        name varchar(32)        NOT NULL,
        state varchar(2) NOT NULL,
        country varchar(2) NOT NULL,
        latitude  double precision NOT NULL,
        longitude double precision NOT NULL,
        elevation int NOT NULL,
        priority smallint NOT NULL
);

-- create permclust table
DROP TABLE IF EXISTS stns.permclust CASCADE;
CREATE TABLE stns.permclust(
        pkey  SERIAL PRIMARY KEY,
        wfo varchar(3) NOT NULL,
        clustername varchar(32) NOT NULL,
        cntyfipscode varchar(221) NOT NULL
);

-- create pirep_navaids table
DROP TABLE IF EXISTS stns.pirep_navaids CASCADE;
CREATE TABLE stns.pirep_navaids(
pkey  SERIAL PRIMARY KEY,
        station_id varchar(8)      NOT NULL,
        name varchar(32)        NOT NULL,
        state varchar(2) NOT NULL,
        country varchar(2) NOT NULL,
        latitude  double precision NOT NULL,
        longitude double precision NOT NULL,
        elevation int NOT NULL,
        priority smallint NOT NULL
);

-- create stns.riverbas table
DROP TABLE IF EXISTS stns.riverbas CASCADE;
CREATE TABLE stns.riverbas(
pkey  SERIAL PRIMARY KEY,
        station_id varchar(8)      NOT NULL DEFAULT '--',
        station_number varchar(6)      NOT NULL DEFAULT '--',
        name varchar(32)        NOT NULL DEFAULT '--',
        country varchar(2) NOT NULL,
        latitude  double precision NOT NULL,
        longitude double precision NOT NULL,
        wfo_code varchar(7) NOT NULL DEFAULT '--'
);

--create stns.scdstn table
DROP TABLE IF EXISTS stns.scdstn CASCADE;
CREATE TABLE stns.scdstn(
pkey  SERIAL PRIMARY KEY,
        station_id varchar(8)      NOT NULL,
        station_number varchar(6)      NOT NULL,
        name varchar(32)        NOT NULL,
        state varchar(2) NOT NULL,
        country varchar(2) NOT NULL,
        latitude  double precision NOT NULL,
        longitude double precision NOT NULL,
        elevation int NOT NULL,
        priority smallint NOT NULL
);

-- create stns.sfstns table
DROP TABLE IF EXISTS stns.sfstns CASCADE;
CREATE TABLE stns.sfstns(
pkey  SERIAL PRIMARY KEY,
        station_id varchar(8)      NOT NULL,
        station_number varchar(6)      NOT NULL,
        name varchar(32)        NOT NULL,
        state varchar(2) NOT NULL,
        country varchar(2) NOT NULL,
        latitude  double precision NOT NULL,
        longitude double precision NOT NULL,
        elevation int NOT NULL,
        priority smallint NOT NULL
);

-- create stns.shef_COOP1 table 
DROP TABLE IF EXISTS stns.shef_COOP1 CASCADE;
CREATE TABLE stns.shef_COOP1(
pkey  SERIAL PRIMARY KEY,
        station_id varchar(8)      NOT NULL,
        station_name varchar(32)        NOT NULL,
        state varchar(2) NOT NULL,
        country varchar(2) NOT NULL,
        latitude  double precision NOT NULL,
        longitude double precision NOT NULL,
        elevation int NOT NULL,
        station_type varchar(10) NOT NULL
);

-- create stns.shef_COOP2 table
DROP TABLE IF EXISTS stns.shef_COOP2 CASCADE;
CREATE TABLE stns.shef_COOP2(
pkey  SERIAL PRIMARY KEY,
        station_id varchar(8)      NOT NULL,
        station_name varchar(32)        NOT NULL,
        state varchar(2) NOT NULL,
        country varchar(2) NOT NULL,
        latitude  double precision NOT NULL,
        longitude double precision NOT NULL,
        elevation int NOT NULL,
        station_type varchar(10) NOT NULL
);

-- create stns.shef_COOP3 table
DROP TABLE IF EXISTS stns.shef_COOP3 CASCADE;
CREATE TABLE stns.shef_COOP3(
pkey  SERIAL PRIMARY KEY,
        station_id varchar(8)      NOT NULL,
        station_name varchar(32)        NOT NULL,
        state varchar(2) NOT NULL,
        country varchar(2) NOT NULL,
        latitude  double precision NOT NULL,
        longitude double precision NOT NULL,
        elevation int NOT NULL,
        station_type varchar(10) NOT NULL
);

-- create stns.shef_COOP4 table
DROP TABLE IF EXISTS stns.shef_COOP4 CASCADE;
CREATE TABLE stns.shef_COOP4(
pkey  SERIAL PRIMARY KEY,
        station_id varchar(8)      NOT NULL,
        station_name varchar(32)        NOT NULL,
        state varchar(2) NOT NULL,
        country varchar(2) NOT NULL,
        latitude  double precision NOT NULL,
        longitude double precision NOT NULL,
        elevation int NOT NULL,
        station_type varchar(10) NOT NULL
);

-- create stns.shef_COOP table
DROP TABLE IF EXISTS stns.shef_COOP CASCADE;
CREATE TABLE stns.shef_COOP(
pkey  SERIAL PRIMARY KEY,
        station_id varchar(8)      NOT NULL,
        station_name varchar(32)        NOT NULL,
        state varchar(2) NOT NULL,
        country varchar(2) NOT NULL,
        latitude  double precision NOT NULL,
        longitude double precision NOT NULL,
        elevation int NOT NULL,
        station_type varchar(10) NOT NULL
);

-- create stns.shef_master table
DROP TABLE IF EXISTS stns.shef_master CASCADE;
CREATE TABLE stns.shef_master(
pkey  SERIAL PRIMARY KEY,
        station_id varchar(8)      NOT NULL,
        station_name varchar(32)        NOT NULL,
        state varchar(2) NOT NULL,
        country varchar(2) NOT NULL,
        latitude  double precision NOT NULL,
        longitude double precision NOT NULL,
        elevation int NOT NULL,
        station_type varchar(10) NOT NULL
);

-- create stns.shpexception table
DROP TABLE IF EXISTS stns.shpexception CASCADE;
CREATE TABLE stns.shpexception(
pkey  SERIAL PRIMARY KEY,
        station_id varchar(8)      NOT NULL,
        station_number varchar(6) NOT NULL,
        station_name varchar(32)        NOT NULL,
        state varchar(2) NOT NULL,
        country varchar(2) NOT NULL,
        latitude  double precision NOT NULL,
        longitude double precision NOT NULL,
        wfo_code varchar(3) NOT NULL
);

-- create stns.snap table
DROP TABLE IF EXISTS stns.snap CASCADE;
CREATE TABLE stns.snap(
pkey  SERIAL PRIMARY KEY,
        station_id varchar(8)      NOT NULL,
        station_number varchar(6)      NOT NULL,
        station_name varchar(32)        NOT NULL,
        latitude  double precision NOT NULL,
        longitude double precision NOT NULL,
        priority smallint NOT NULL
);

-- create stns.snap_8 table
DROP TABLE IF EXISTS stns.snap_8 CASCADE;
CREATE TABLE stns.snap_8(
pkey SERIAL PRIMARY KEY,
	station_id varchar(8)	NOT NULL,
	station_number varchar(8)	NOT NULL,
	station_name varchar(16)	NOT NULL,
	latitude double precision NOT NULL,
	longitude double precision NOT NULL,
	priority smallint NOT NULL
);

-- create stns.snstns table
DROP TABLE IF EXISTS stns.snstns CASCADE;
CREATE TABLE stns.snstns(
pkey  SERIAL PRIMARY KEY,
        station_id varchar(8)      NOT NULL,
        station_number varchar(6)      NOT NULL,
        station_name varchar(32)        NOT NULL,
        state varchar(2) NOT NULL,
        country varchar(2) NOT NULL,
        latitude  double precision NOT NULL,
        longitude double precision NOT NULL,
        elevation int NOT NULL,
        priority smallint NOT NULL
);

-- Create snworld table
DROP TABLE IF EXISTS stns.SNWORLD CASCADE; 
CREATE TABLE stns.SNWORLD
( PKEY SERIAL PRIMARY KEY,
  STID varchar(4)  NOT NULL,
  STNUM int  NOT NULL,
  NAME varchar(32)  NOT NULL,
  STATE char(2)  NOT NULL,
  COUNTRY char(2)  NOT NULL,
  LAT double precision  NOT NULL,
  LON double precision  NOT NULL,
  ELEV int  NOT NULL,
  PRI int NOT NULL
);
-- Create spcwatch table
DROP TABLE IF EXISTS stns.SPCWATCH CASCADE; 
CREATE TABLE stns.SPCWATCH ( 
	PKEY SERIAL PRIMARY KEY,
        STID varchar(8)  NOT NULL,
        STNUM int  NOT NULL,
        NAME varchar(32)  NOT NULL,
        STATE char(2)  NOT NULL,
        COUNTRY char(2)  NOT NULL,
        LAT double precision  NOT NULL,
        LON double precision  NOT NULL,
	ELEV int NOT NULL
);
-- Create state table
DROP TABLE IF EXISTS stns.STATE CASCADE; 
CREATE TABLE stns.STATE ( 
	PKEY SERIAL PRIMARY KEY,
        STATEID varchar(2)  NOT NULL,
        STATENAME varchar(32)  NOT NULL
);
-- Create stns_II90 table
DROP TABLE IF EXISTS stns.STNS_II90 CASCADE; 
CREATE TABLE stns.STNS_II90 ( 
	PKEY SERIAL PRIMARY KEY,
        STNUM int  NOT NULL,
        NAME varchar(32)  NOT NULL,
        STATE char(2)  NOT NULL,
        COUNTRY char(2)  NOT NULL,
        LAT double precision  NOT NULL,
        LON double precision  NOT NULL,
	ELEV int NOT NULL ,
	PRI  int NOT NULL
);
-- Create systns table
DROP TABLE IF EXISTS stns.SYSTNS CASCADE; 
CREATE TABLE stns.SYSTNS ( 
	PKEY SERIAL PRIMARY KEY,
        STID varchar(8)  NOT NULL,
        STNUM int  NOT NULL,
        NAME varchar(32)  NOT NULL,
        STATE char(2)  NOT NULL,
        COUNTRY char(2)  NOT NULL,
        LAT double precision  NOT NULL,
        LON double precision  NOT NULL,
	ELEV int NOT NULL
);
-- Create syworld table
DROP TABLE IF EXISTS stns.SYWORLD CASCADE; 
CREATE TABLE stns.SYWORLD ( 
	PKEY SERIAL PRIMARY KEY,
        STID varchar(8)  NOT NULL,
        STNUM int  NOT NULL,
        NAME varchar(32)  NOT NULL,
        STATE char(2)  NOT NULL,
        COUNTRY char(2)  NOT NULL,
        LAT double precision  NOT NULL,
        LON double precision  NOT NULL,
	ELEV int NOT NULL
);
-- Create tafstn table
DROP TABLE IF EXISTS stns.TAFSTN CASCADE; 
CREATE TABLE stns.TAFSTN ( 
	PKEY SERIAL PRIMARY KEY,
        STID varchar(8)  NOT NULL,
        STNUM int  NOT NULL,
        NAME varchar(32)  NOT NULL,
        STATE char(2)  NOT NULL,
        COUNTRY char(2)  NOT NULL,
        LAT double precision  NOT NULL,
        LON double precision  NOT NULL,
	ELEV int  NOT NULL,
        PRI int NOT NULL,
	MISC char(3) NOT NULL
);
-- Create tcabkpt table
DROP TABLE IF EXISTS stns.TCABKPT CASCADE; 
CREATE TABLE stns.TCABKPT ( 
	PKEY SERIAL PRIMARY KEY,
        STID varchar(8)  NOT NULL,
        STNUM int  NOT NULL,
        NAME varchar(32)  NOT NULL,
        STATE char(2)  NOT NULL,
        COUNTRY char(2)  NOT NULL,
        LAT double precision  NOT NULL,
        LON double precision  NOT NULL,
        PRI int NOT NULL ,
	TBCHRS varchar(20) NOT NULL
);
-- Create tcabkpt_island table
DROP TABLE IF EXISTS stns.TCABKPT_ISLAND CASCADE; 
CREATE TABLE stns.TCABKPT_ISLAND ( 
	PKEY SERIAL PRIMARY KEY,
        STID varchar(8)  NOT NULL,
        STNUM int  NOT NULL,
        NAME varchar(32)  NOT NULL,
        STATE char(2)  NOT NULL,
        COUNTRY char(2)  NOT NULL,
        LAT double precision  NOT NULL,
        LON double precision  NOT NULL,
        PRI int NOT NULL
);
-- Create tcabkpt_land table
DROP TABLE IF EXISTS stns.TCABKPT_LAND CASCADE; 
CREATE TABLE stns.TCABKPT_LAND ( 
	PKEY SERIAL PRIMARY KEY,
        STID varchar(8)  NOT NULL,
        STNUM int  NOT NULL,
        NAME varchar(32)  NOT NULL,
        STATE char(2)  NOT NULL,
        COUNTRY char(2)  NOT NULL,
        LAT double precision  NOT NULL,
        LON double precision  NOT NULL,
        PRI int  NOT NULL
);
-- Create tcabkptlz table
DROP TABLE IF EXISTS stns.TCABKPTLZ CASCADE; 
CREATE TABLE stns.TCABKPTLZ ( 
	PKEY SERIAL PRIMARY KEY,
        STID varchar(8)  NOT NULL,
        STNUM int  NOT NULL,
        NAME varchar(32)  NOT NULL,
        STATE char(2)  NOT NULL,
        COUNTRY char(2)  NOT NULL,
        LAT double precision  NOT NULL,
        LON double precision  NOT NULL,
        PRI int NOT NULL,
	TBCHRS varchar(20)  NOT NULL
);
-- Create tcabkpt_ovl table
DROP TABLE IF EXISTS stns.TCABKPT_OVL CASCADE; 
CREATE TABLE stns.TCABKPT_OVL ( 
	PKEY SERIAL PRIMARY KEY,
        STID varchar(8)  NOT NULL,
        STNUM int  NOT NULL,
        NAME varchar(32)  NOT NULL,
        STATE char(2)  NOT NULL,
        COUNTRY char(2)  NOT NULL,
        LAT double precision  NOT NULL,
        LON double precision  NOT NULL,
        PRI int NOT NULL
);
-- Create tcabkpt_water table
DROP TABLE IF EXISTS stns.TCABKPT_WATER CASCADE; 
CREATE TABLE stns.TCABKPT_WATER ( 
	PKEY SERIAL PRIMARY KEY,
        STID varchar(8)  NOT NULL,
        STNUM int  NOT NULL,
        NAME varchar(32)  NOT NULL,
        STATE char(2)  NOT NULL,
        COUNTRY char(2)  NOT NULL,
        LAT double precision  NOT NULL,
        LON double precision  NOT NULL,
        PRI int NOT NULL
);
-- Create tpc_countries table
DROP TABLE IF EXISTS stns.TPC_COUNTRIES CASCADE; 
CREATE TABLE stns.TPC_COUNTRIES ( 
	PKEY SERIAL PRIMARY KEY,
        ALPHA varchar(8)  NOT NULL,
        FIPS int  NOT NULL,
        NAME varchar(32)  NOT NULL,
        COUNTRY char(2)  NOT NULL,
        LAT double precision  NOT NULL,
        LON double precision NOT NULL
);
-- Create tpc_states table
DROP TABLE IF EXISTS stns.TPC_STATES CASCADE; 
CREATE TABLE stns.TPC_STATES ( 
	PKEY SERIAL PRIMARY KEY,
        ALPHA varchar(8)  NOT NULL,
        FIPS int  NOT NULL,
        NAME varchar(32)  NOT NULL,
        STATE char(2)  NOT NULL,
        COUNTRY char(2)  NOT NULL,
        LAT double precision  NOT NULL,
        LON double precision NOT NULL
);
-- Create volcano table
DROP TABLE IF EXISTS stns.VOLCANO CASCADE; 
CREATE TABLE stns.VOLCANO
( PKEY SERIAL PRIMARY KEY,
  STID varchar(8)  NOT NULL,
  STNUM int  NOT NULL,
  NAME varchar(32)  NOT NULL,
  LAT double precision  NOT NULL,
  LON double precision  NOT NULL,
  ELEV int  NOT NULL,
  LOCATION varchar(20)  NOT NULL
);
-- Create volcano_small table
DROP TABLE IF EXISTS stns.VOLCANO_SMALL CASCADE; 
CREATE TABLE stns.VOLCANO_SMALL
( PKEY SERIAL PRIMARY KEY,
  STID varchar(8)  NOT NULL,
  STNUM int  NOT NULL,
  NAME varchar(32)  NOT NULL,
  STATE char(2)  NOT NULL,
  COUNTRY char(2)  NOT NULL,
  LAT double precision  NOT NULL,
  LON double precision  NOT NULL,
  ELEV int  NOT NULL
);
-- Create vors table
DROP TABLE IF EXISTS stns.VORS CASCADE; 
CREATE TABLE stns.VORS
( PKEY SERIAL PRIMARY KEY,
  STID varchar(8)  NOT NULL,
  STNUM int  NOT NULL,
  NAME varchar(32)  NOT NULL,
  STATE char(2)  NOT NULL,
  COUNTRY char(2)  NOT NULL,
  LAT double precision  NOT NULL,
  LON double precision NOT NULL
);
-- Create wfo table
DROP TABLE IF EXISTS stns.WFO CASCADE; 
CREATE TABLE stns.WFO
( PKEY SERIAL PRIMARY KEY,
  STID varchar(8)  NOT NULL,
  STNUM int  NOT NULL,
  NAME varchar(32)  NOT NULL,
  STATE char(2)  NOT NULL,
  COUNTRY char(2)  NOT NULL,
  LAT double precision  NOT NULL,
  LON double precision  NOT NULL,
  ELEV int   NOT NULL
);
-- Create wrqpf table
DROP TABLE IF EXISTS stns.WRQPF CASCADE; 
CREATE TABLE stns.WRQPF
( PKEY SERIAL PRIMARY KEY,
  STID varchar(8)  NOT NULL,
  STNUM int  NOT NULL,
  NAME varchar(32)  NOT NULL,
  STATE char(2)  NOT NULL,
  COUNTRY char(2)  NOT NULL,
  LAT double precision  NOT NULL,
  LON double precision  NOT NULL,
  ELEV int  NOT NULL,
  PRI int    NOT NULL
);
-- Create xrainsort table
DROP TABLE IF EXISTS stns.XRAINSORT CASCADE; 
CREATE TABLE stns.XRAINSORT
( PKEY SERIAL PRIMARY KEY,
  STID varchar(4)  NOT NULL,
  STNUM int  NOT NULL,
  NAME varchar(32)  NOT NULL,
  STATE char(2)  NOT NULL,
  COUNTRY char(2)  NOT NULL,
  LAT double precision  NOT NULL,
  LON double precision  NOT NULL,
  ELEV int  NOT NULL,
  PRI int  NOT NULL
);
-- Create zones table
DROP TABLE IF EXISTS stns.ZONES CASCADE; 
CREATE TABLE stns.ZONES
( PKEY SERIAL PRIMARY KEY,
  STID varchar(8)  NOT NULL,
  STNUM int  NOT NULL,
  NAME varchar(32)  NOT NULL,
  STATE char(2)  NOT NULL,
  COUNTRY char(2)  NOT NULL,
  LAT double precision  NOT NULL,
  LON double precision  NOT NULL,
  WFO char(3)  NOT NULL
);

