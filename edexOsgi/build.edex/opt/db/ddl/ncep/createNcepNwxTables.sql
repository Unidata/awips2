--
-- PostgreSQL database dump 
--
--
-- Name: SCHEMA public; Type: COMMENT; Schema: -; Owner: postgres
--
-------------------------------------------------
-- create nwx product tables
-- ---------------------------------------------

-- create nwx.observeddataproducts table
DROP TABLE IF EXISTS nwx.observeddataproducts CASCADE;
CREATE TABLE nwx.observeddataproducts(
id SERIAL PRIMARY KEY,
productname varchar(60) NOT  NULL,
producttablename varchar(30) NOT NULL,
producttype varchar(20) NOT NULL
);
-- create nwx.radartext88d table
DROP TABLE IF EXISTS nwx.radartext88d CASCADE;
CREATE TABLE nwx.radartext88d(
id SERIAL PRIMARY KEY,
productname varchar(60) NOT  NULL,
producttablename varchar(30) NOT NULL,
producttype varchar(20) NOT NULL
);

-- create nwx.aviationforecasts table
DROP TABLE IF EXISTS nwx.aviationforecasts CASCADE;
CREATE TABLE nwx.aviationforecasts(
id SERIAL PRIMARY KEY,
productname varchar(60) NOT  NULL,
producttablename varchar(30) NOT NULL,
producttype varchar(20) NOT NULL
);

-- create nwx.canadianproducts table
DROP TABLE IF EXISTS nwx.canadianproducts CASCADE;
CREATE TABLE nwx.canadianproducts(
id SERIAL PRIMARY KEY,
productname varchar(60) NOT  NULL,
producttablename varchar(30) NOT NULL,
producttype varchar(20) NOT NULL
);

-- create nwx.cpcproducts table
DROP TABLE IF EXISTS nwx.cpcproducts CASCADE;
CREATE TABLE nwx.cpcproducts(
id SERIAL PRIMARY KEY,
productname varchar(60) NOT  NULL,
producttablename varchar(30) NOT NULL,
producttype varchar(20) NOT NULL
);

-- create nwx.datatypegrouplist table
DROP TABLE IF EXISTS nwx.datatypegrouplist CASCADE;
CREATE TABLE nwx.datatypegrouplist(
id SERIAL PRIMARY KEY,
datatypegroupname varchar(60) NOT  NULL,
datatypegrouptablename varchar(30) NOT NULL
);

-- create nwx.fireweatherproducts table
DROP TABLE IF EXISTS nwx.fireweatherproducts CASCADE;
CREATE TABLE nwx.fireweatherproducts(
id SERIAL PRIMARY KEY,
productname varchar(60) NOT  NULL,
producttablename varchar(30) NOT NULL,
producttype varchar(20) NOT NULL
);

-- create nwx.flashflood table
DROP TABLE IF EXISTS nwx.flashflood CASCADE;
CREATE TABLE nwx.flashflood(
id SERIAL PRIMARY KEY,
productname varchar(60) NOT  NULL,
producttablename varchar(30) NOT NULL,
producttype varchar(20) NOT NULL
);

-- create nwx.hpcheatindex table
DROP TABLE IF EXISTS nwx.hpcheatindex CASCADE;
CREATE TABLE nwx.hpcheatindex(
id SERIAL PRIMARY KEY,
productname varchar(60) NOT  NULL,
producttablename varchar(30) NOT NULL,
producttype varchar(20) NOT NULL
);

-- create nwx.hpcproducts table
DROP TABLE IF EXISTS nwx.hpcproducts CASCADE;
CREATE TABLE nwx.hpcproducts(
id SERIAL PRIMARY KEY,
productname varchar(60) NOT  NULL,
producttablename varchar(30) NOT NULL,
producttype varchar(20) NOT NULL
);

-- create nwx.marine table
DROP TABLE IF EXISTS nwx.marine CASCADE;
CREATE TABLE nwx.marine(
id SERIAL PRIMARY KEY,
productname varchar(60) NOT  NULL,
producttablename varchar(30) NOT NULL,
producttype varchar(20) NOT NULL
);

-- create nwx.mos table
DROP TABLE IF EXISTS nwx.mos CASCADE;
CREATE TABLE nwx.mos(
id SERIAL PRIMARY KEY,
productname varchar(60) NOT  NULL,
producttablename varchar(30) NOT NULL,
producttype varchar(20) NOT NULL
);

-- create nwx.nhcproducts table
DROP TABLE IF EXISTS nwx.nhcproducts CASCADE;
CREATE TABLE nwx.nhcproducts(
id SERIAL PRIMARY KEY,
productname varchar(60) NOT  NULL,
producttablename varchar(30) NOT NULL,
producttype varchar(20) NOT NULL
);

-- create nwx.observeddata table
--DROP TABLE IF EXISTS nwx.observeddata CASCADE;
--CREATE TABLE nwx.observeddata(
--id SERIAL PRIMARY KEY,
--productname varchar(60) NOT  NULL,
--producttablename varchar(30) NOT NULL,
--producttype varchar(20) NOT NULL
--);

-- create nwx.ptfcstproducts table
DROP TABLE IF EXISTS nwx.ptfcstproducts CASCADE;
CREATE TABLE nwx.ptfcstproducts(
id SERIAL PRIMARY KEY,
productname varchar(60) NOT  NULL,
producttablename varchar(30) NOT NULL,
producttype varchar(20) NOT NULL
);

-- create nwx.publicproducts table
DROP TABLE IF EXISTS nwx.publicproducts CASCADE;
CREATE TABLE nwx.publicproducts(
id SERIAL PRIMARY KEY,
productname varchar(60) NOT  NULL,
producttablename varchar(30) NOT NULL,
producttype varchar(20) NOT NULL
);

-- create nwx.reconcarcah table
DROP TABLE IF EXISTS nwx.reconcarcah CASCADE;
CREATE TABLE nwx.reconcarcah(
id SERIAL PRIMARY KEY,
productname varchar(60) NOT  NULL,
producttablename varchar(30) NOT NULL,
producttype varchar(20) NOT NULL
);

--create nwx.spcproducts table
DROP TABLE IF EXISTS nwx.spcproducts CASCADE;
CREATE TABLE nwx.spcproducts(
id SERIAL PRIMARY KEY,
productname varchar(60) NOT  NULL,
producttablename varchar(30) NOT NULL,
producttype varchar(20) NOT NULL
);

-- create nwx.tropicalpacific table
DROP TABLE IF EXISTS nwx.tropicalpacific CASCADE;
CREATE TABLE nwx.tropicalpacific(
id SERIAL PRIMARY KEY,
productname varchar(60) NOT  NULL,
producttablename varchar(30) NOT NULL,
producttype varchar(20) NOT NULL
);

-- create nwx.volcanoproducts table
DROP TABLE IF EXISTS nwx.volcanoproducts CASCADE;
CREATE TABLE nwx.volcanoproducts(
id SERIAL PRIMARY KEY,
productname varchar(60) NOT  NULL,
producttablename varchar(30) NOT NULL,
producttype varchar(20) NOT NULL
);

-- create nwx.watcheswarnings table
DROP TABLE IF EXISTS nwx.watcheswarnings CASCADE;
CREATE TABLE nwx.watcheswarnings(
id SERIAL PRIMARY KEY,
productname varchar(60) NOT  NULL,
producttablename varchar(30) NOT NULL,
producttype varchar(20) NOT NULL
);

-------------------------------------------------
-- create nwx bulletin tables
-- ---------------------------------------------

-- create nwx.ada table
DROP TABLE IF EXISTS nwx.ada CASCADE;
CREATE TABLE nwx.ada(
        id SERIAL PRIMARY KEY,
        productid varchar(6) NOT  NULL,
        stnid varchar(8) NOT NULL,
        stnname varchar(32) NOT NULL,
        state varchar(2)  NOT NULL,
        country varchar(2)  NOT NULL,
        latitude  double precision NOT NULL,
        longitude double precision NOT NULL,
        elevation int NOT NULL DEFAULT 0
);


-- create nwx.afd table
DROP TABLE IF EXISTS nwx.afd CASCADE;
CREATE TABLE nwx.afd(
id  SERIAL PRIMARY KEY,
        productid varchar(6)      NOT NULL,
        stnid    varchar(8)        NOT NULL,
        stnname varchar(32)        NOT NULL,
        state varchar(2) NOT NULL,
        country varchar(2) NOT NULL,
        latitude  double precision NOT NULL,
        longitude double precision NOT NULL,
        elevation int NOT NULL
);

-- create nwx.afm table
DROP TABLE IF EXISTS nwx.afm CASCADE;
CREATE TABLE nwx.afm(
id  SERIAL PRIMARY KEY,
        productid varchar(6)      NOT NULL,
        stnid    varchar(8)        NOT NULL,
        stnname varchar(32)        NOT NULL,
        state varchar(2) NOT NULL,
        country varchar(2) NOT NULL,
        latitude  double precision NOT NULL,
        longitude double precision NOT NULL,
        elevation int NOT NULL
);


-- create nwx.ago table
DROP TABLE IF EXISTS nwx.ago CASCADE;
CREATE TABLE nwx.ago(
id  SERIAL PRIMARY KEY,
        productid varchar(6)      NOT NULL,
        stnid    varchar(8)        NOT NULL,
        stnname varchar(32)        NOT NULL,
        state varchar(2) NOT NULL,
        country varchar(2) NOT NULL,
        latitude  double precision NOT NULL,
        longitude double precision NOT NULL,
        elevation int NOT NULL
);

--create nwx.airmet table
DROP TABLE IF EXISTS nwx.airmet CASCADE;
CREATE TABLE nwx.airmet(
id  SERIAL PRIMARY KEY,
        productid varchar(6)      NOT NULL,
        stnid    varchar(8)        NOT NULL,
        stnname varchar(32)        NOT NULL,
        state varchar(2) NOT NULL,
        country varchar(2) NOT NULL,
        latitude  double precision NOT NULL,
        longitude double precision NOT NULL,
        elevation int NOT NULL
);

-- create nwx.atlhurr table
DROP TABLE IF EXISTS nwx.atlhurr CASCADE;
CREATE TABLE nwx.atlhurr(
id  SERIAL PRIMARY KEY,
        productid varchar(6)      NOT NULL,
        stnid    varchar(8)        NOT NULL,
        stnname varchar(32)        NOT NULL,
        state varchar(2) NOT NULL,
        country varchar(2) NOT NULL,
        latitude  double precision NOT NULL,
        longitude double precision NOT NULL,
        elevation int NOT NULL
);

-- create nwx.ccf table
DROP TABLE IF EXISTS nwx.ccf CASCADE;
CREATE TABLE nwx.ccf(
id  SERIAL PRIMARY KEY,
        productid varchar(6)      NOT NULL,
        stnid    varchar(8)        NOT NULL,
        stnname varchar(32)        NOT NULL,
        state varchar(2) NOT NULL,
        country varchar(2) NOT NULL,
        latitude  double precision NOT NULL,
        longitude double precision NOT NULL,
        elevation int NOT NULL
);

-- create nwx.cfw table
DROP TABLE IF EXISTS nwx.cfw CASCADE;
CREATE TABLE nwx.cfw(
id  SERIAL PRIMARY KEY,
        productid varchar(6)      NOT NULL,
        stnid    varchar(8)        NOT NULL,
        stnname varchar(32)        NOT NULL,
        state varchar(2) NOT NULL,
        country varchar(2) NOT NULL,
        latitude  double precision NOT NULL,
        longitude double precision NOT NULL,
        elevation int NOT NULL DEFAULT 0
);

-- create nwx.cgdata table
DROP TABLE IF EXISTS nwx.cgdata CASCADE;
CREATE TABLE nwx.cgdata(
id  SERIAL PRIMARY KEY,
        productid varchar(6)      NOT NULL,
        stnid    varchar(8)        NOT NULL,
        stnname varchar(32)        NOT NULL,
        state varchar(2) NOT NULL,
        country varchar(2) NOT NULL,
        latitude  double precision NOT NULL,
        longitude double precision NOT NULL,
        elevation int NOT NULL DEFAULT 0
);

-- create nwx.cli table
DROP TABLE IF EXISTS nwx.cli CASCADE;
CREATE TABLE nwx.cli(
id  SERIAL PRIMARY KEY,
        productid varchar(6)      NOT NULL,
        stnid    varchar(8)        NOT NULL,
        stnname varchar(32)        NOT NULL,
        state varchar(2) NOT NULL,
        country varchar(2) NOT NULL,
        latitude  double precision NOT NULL,
        longitude double precision NOT NULL,
        elevation int NOT NULL
);

-- create nwx.clm table
DROP TABLE IF EXISTS nwx.clm CASCADE;
CREATE TABLE nwx.clm(
id  SERIAL PRIMARY KEY,
        productid varchar(6)      NOT NULL,
        stnid    varchar(8)        NOT NULL,
        stnname varchar(32)        NOT NULL,
        state varchar(2) NOT NULL,
        country varchar(2) NOT NULL,
        latitude  double precision NOT NULL,
        longitude double precision NOT NULL,
        elevation int NOT NULL
);

--create nwx.consig table
DROP TABLE IF EXISTS nwx.consig CASCADE;
CREATE TABLE nwx.consig(
id  SERIAL PRIMARY KEY,
        productid varchar(6)      NOT NULL,
        stnid    varchar(8)        NOT NULL,
        stnname varchar(32)        NOT NULL,
        state varchar(2) NOT NULL,
        country varchar(2) NOT NULL,
        latitude  double precision NOT NULL,
        longitude double precision NOT NULL,
        elevation int NOT NULL
);

-- create nwx.cwf table
DROP TABLE IF EXISTS nwx.cwf CASCADE;
CREATE TABLE nwx.cwf(
id  SERIAL PRIMARY KEY,
        productid varchar(6)      NOT NULL,
        stnid    varchar(8)        NOT NULL,
        stnname varchar(32)        NOT NULL,
        state varchar(2) NOT NULL,
        country varchar(2) NOT NULL,
        latitude  double precision NOT NULL,
        longitude double precision NOT NULL,
        elevation int NOT NULL DEFAULT 0
);


--create nwx.cwsu table
DROP TABLE IF EXISTS nwx.cwsu CASCADE;
CREATE TABLE nwx.cwsu(
id  SERIAL PRIMARY KEY,
        productid varchar(6)      NOT NULL,
        stnid    varchar(8)        NOT NULL,
        stnname varchar(32)        NOT NULL,
        state varchar(2) NOT NULL,
        country varchar(2) NOT NULL,
        latitude  double precision NOT NULL,
        longitude double precision NOT NULL,
        elevation int NOT NULL
);


-- create nwx.day1 table
DROP TABLE IF EXISTS nwx.day1 CASCADE;
CREATE TABLE nwx.day1(
id  SERIAL PRIMARY KEY,
        productid varchar(6)      NOT NULL,
        stnid    varchar(8)        NOT NULL,
        stnname varchar(32)        NOT NULL,
        state varchar(2) NOT NULL,
        country varchar(2) NOT NULL,
        latitude  double precision NOT NULL,
        longitude double precision NOT NULL,
        elevation int NOT NULL
);

-- create nwx.day1pfw table
DROP TABLE IF EXISTS nwx.day1pfw CASCADE;
CREATE TABLE nwx.day1pfw(
id  SERIAL PRIMARY KEY,
        productid varchar(6)      NOT NULL,
        stnid    varchar(8)        NOT NULL,
        stnname varchar(32)        NOT NULL,
        state varchar(2) NOT NULL,
        country varchar(2) NOT NULL,
        latitude  double precision NOT NULL,
        longitude double precision NOT NULL,
        elevation int NOT NULL
);

-- create nwx.day1pts table
DROP TABLE IF EXISTS nwx.day1pts CASCADE;
CREATE TABLE nwx.day1pts(
id  SERIAL PRIMARY KEY,
        productid varchar(6)      NOT NULL,
        stnid    varchar(8)        NOT NULL,
        stnname varchar(32)        NOT NULL,
        state varchar(2) NOT NULL,
        country varchar(2) NOT NULL,
        latitude  double precision NOT NULL,
        longitude double precision NOT NULL,
        elevation int NOT NULL
);

-- create nwx.day2 table
DROP TABLE IF EXISTS nwx.day2 CASCADE;
CREATE TABLE nwx.day2(
id  SERIAL PRIMARY KEY,
        productid varchar(6)      NOT NULL,
        stnid    varchar(8)        NOT NULL,
        stnname varchar(32)        NOT NULL,
        state varchar(2) NOT NULL,
        country varchar(2) NOT NULL,
        latitude  double precision NOT NULL,
        longitude double precision NOT NULL,
        elevation int NOT NULL
);

-- create nwx.day2pfw table
DROP TABLE IF EXISTS nwx.day2pfw CASCADE;
CREATE TABLE nwx.day2pfw(
id  SERIAL PRIMARY KEY,
        productid varchar(6)      NOT NULL,
        stnid    varchar(8)        NOT NULL,
        stnname varchar(32)        NOT NULL,
        state varchar(2) NOT NULL,
        country varchar(2) NOT NULL,
        latitude  double precision NOT NULL,
        longitude double precision NOT NULL,
        elevation int NOT NULL
);

-- create nwx.day2pts table
DROP TABLE IF EXISTS nwx.day2pts CASCADE;
CREATE TABLE nwx.day2pts(
id  SERIAL PRIMARY KEY,
        productid varchar(6)      NOT NULL,
        stnid    varchar(8)        NOT NULL,
        stnname varchar(32)        NOT NULL,
        state varchar(2) NOT NULL,
        country varchar(2) NOT NULL,
        latitude  double precision NOT NULL,
        longitude double precision NOT NULL,
        elevation int NOT NULL
);

-- create nwx.day3 table
DROP TABLE IF EXISTS nwx.day3 CASCADE;
CREATE TABLE nwx.day3(
id  SERIAL PRIMARY KEY,
        productid varchar(6)      NOT NULL,
        stnid    varchar(8)        NOT NULL,
        stnname varchar(32)        NOT NULL,
        state varchar(2) NOT NULL,
        country varchar(2) NOT NULL,
        latitude  double precision NOT NULL,
        longitude double precision NOT NULL,
        elevation int NOT NULL
);


-- create nwx.day38pf table
DROP TABLE IF EXISTS nwx.day38pf CASCADE;
CREATE TABLE nwx.day38pf(
id  SERIAL PRIMARY KEY,
        productid varchar(6)      NOT NULL,
        stnid    varchar(8)        NOT NULL,
        stnname varchar(32)        NOT NULL,
        state varchar(2) NOT NULL,
        country varchar(2) NOT NULL,
        latitude  double precision NOT NULL,
        longitude double precision NOT NULL,
        elevation int NOT NULL
);


-- create nwx.day3pts table
DROP TABLE IF EXISTS nwx.day3pts CASCADE;
CREATE TABLE nwx.day3pts(
id  SERIAL PRIMARY KEY,
        productid varchar(6)      NOT NULL,
        stnid    varchar(8)        NOT NULL,
        stnname varchar(32)        NOT NULL,
        state varchar(2) NOT NULL,
        country varchar(2) NOT NULL,
        latitude  double precision NOT NULL,
        longitude double precision NOT NULL,
        elevation int NOT NULL
);

-- create nwx.day48 table
DROP TABLE IF EXISTS nwx.day48 CASCADE;
CREATE TABLE nwx.day48(
id  SERIAL PRIMARY KEY,
        productid varchar(6)      NOT NULL,
        stnid    varchar(8)        NOT NULL,
        stnname varchar(32)        NOT NULL,
        state varchar(2) NOT NULL,
        country varchar(2) NOT NULL,
        latitude  double precision NOT NULL,
        longitude double precision NOT NULL,
        elevation int NOT NULL
);


-- create nwx.day4pts table
DROP TABLE IF EXISTS nwx.day4pts CASCADE;
CREATE TABLE nwx.day4pts(
id  SERIAL PRIMARY KEY,
        productid varchar(6)      NOT NULL,
        stnid    varchar(8)        NOT NULL,
        stnname varchar(32)        NOT NULL,
        state varchar(2) NOT NULL,
        country varchar(2) NOT NULL,
        latitude  double precision NOT NULL,
        longitude double precision NOT NULL,
        elevation int NOT NULL
);

-- create nwx.drought table
DROP TABLE IF EXISTS nwx.drought CASCADE;
CREATE TABLE nwx.drought(
id  SERIAL PRIMARY KEY,
        productid varchar(6)      NOT NULL,
        stnid    varchar(8)        NOT NULL,
        stnname varchar(32)        NOT NULL,
        state varchar(2) NOT NULL,
        country varchar(2) NOT NULL,
        latitude  double precision NOT NULL,
        longitude double precision NOT NULL,
        elevation int NOT NULL
);

-- create nwx.dsa table
DROP TABLE IF EXISTS nwx.dsa CASCADE;
CREATE TABLE nwx.dsa(
id  SERIAL PRIMARY KEY,
        productid varchar(6)      NOT NULL,
        stnid    varchar(8)        NOT NULL,
        stnname varchar(32)        NOT NULL,
        state varchar(2) NOT NULL,
        country varchar(2) NOT NULL,
        latitude  double precision NOT NULL,
        longitude double precision NOT NULL,
        elevation int NOT NULL
);

-- create nwx.etagd table
DROP TABLE IF EXISTS nwx.etagd CASCADE;
CREATE TABLE nwx.etagd(
id  SERIAL PRIMARY KEY,
        productid varchar(6)      NOT NULL,
        stnid    varchar(8)        NOT NULL,
        stnname varchar(32)        NOT NULL,
        state varchar(2) NOT NULL,
        country varchar(2) NOT NULL,
        latitude  double precision NOT NULL,
        longitude double precision NOT NULL,
        elevation int NOT NULL
);


-- create nwx.faa table
DROP TABLE IF EXISTS nwx.faa CASCADE;
CREATE TABLE nwx.faa(
id  SERIAL PRIMARY KEY,
        productid varchar(6)      NOT NULL,
        stnid    varchar(8)        NOT NULL,
        stnname varchar(32)        NOT NULL,
        state varchar(2) NOT NULL,
        country varchar(2) NOT NULL,
        latitude  double precision NOT NULL,
        longitude double precision NOT NULL,
        elevation int NOT NULL
);

-- create nwx.ffg table
DROP TABLE IF EXISTS nwx.ffg CASCADE;
CREATE TABLE nwx.ffg(
id  SERIAL PRIMARY KEY,
        productid varchar(6)      NOT NULL,
        stnid    varchar(8)        NOT NULL,
        stnname varchar(32)        NOT NULL,
        state varchar(2) NOT NULL,
        country varchar(2) NOT NULL,
        latitude  double precision NOT NULL,
        longitude double precision NOT NULL,
        elevation int NOT NULL
);

-- create nwx.f610 table
DROP TABLE IF EXISTS nwx.f610 CASCADE;
CREATE TABLE nwx.f610(
id  SERIAL PRIMARY KEY,
        productid varchar(6)      NOT NULL,
        stnid    varchar(8)        NOT NULL,
        stnname varchar(32)        NOT NULL,
        state varchar(2) NOT NULL,
        country varchar(2) NOT NULL,
        latitude  double precision NOT NULL,
        longitude double precision NOT NULL,
        elevation int NOT NULL
);


-- create nwx.fln table
DROP TABLE IF EXISTS nwx.fln CASCADE;
CREATE TABLE nwx.fln(
id  SERIAL PRIMARY KEY,
        productid varchar(6)      NOT NULL,
        stnid    varchar(8)        NOT NULL,
        stnname varchar(32)        NOT NULL,
        state varchar(2) NOT NULL,
        country varchar(2) NOT NULL,
        latitude  double precision NOT NULL,
        longitude double precision NOT NULL,
        elevation int NOT NULL DEFAULT 0
);


-- create nwx.flw table
DROP TABLE IF EXISTS nwx.flw CASCADE;
CREATE TABLE nwx.flw(
id  SERIAL PRIMARY KEY,
        productid varchar(6)      NOT NULL,
        stnid    varchar(8)        NOT NULL,
        stnname varchar(32)        NOT NULL,
        state varchar(2) NOT NULL,
        country varchar(2) NOT NULL,
        latitude  double precision NOT NULL,
        longitude double precision NOT NULL,
        elevation int NOT NULL DEFAULT 0
);

-- create nwx.fwddy1 table
DROP TABLE IF EXISTS nwx.fwddy1 CASCADE;
CREATE TABLE nwx.fwddy1(
id  SERIAL PRIMARY KEY,
        productid varchar(6)      NOT NULL,
        stnid    varchar(8)        NOT NULL,
        stnname varchar(32)        NOT NULL,
        state varchar(2) NOT NULL,
        country varchar(2) NOT NULL,
        latitude  double precision NOT NULL,
        longitude double precision NOT NULL,
        elevation int NOT NULL
);


-- create nwx.fwddy2 table
DROP TABLE IF EXISTS nwx.fwddy2 CASCADE;
CREATE TABLE nwx.fwddy2(
id  SERIAL PRIMARY KEY,
        productid varchar(6)      NOT NULL,
        stnid    varchar(8)        NOT NULL,
        stnname varchar(32)        NOT NULL,
        state varchar(2) NOT NULL,
        country varchar(2) NOT NULL,
        latitude  double precision NOT NULL,
        longitude double precision NOT NULL,
        elevation int NOT NULL
);


-- create nwx.fwddy38 table
DROP TABLE IF EXISTS nwx.fwddy38 CASCADE;
CREATE TABLE nwx.fwddy38(
id  SERIAL PRIMARY KEY,
        productid varchar(6)      NOT NULL,
        stnid    varchar(8)        NOT NULL,
        stnname varchar(32)        NOT NULL,
        state varchar(2) NOT NULL,
        country varchar(2) NOT NULL,
        latitude  double precision NOT NULL,
        longitude double precision NOT NULL,
        elevation int NOT NULL
);


--create nwx.fzl table
DROP TABLE IF EXISTS nwx.fzl CASCADE;
CREATE TABLE nwx.fzl(
id  SERIAL PRIMARY KEY,
        productid varchar(6)      NOT NULL,
        stnid    varchar(8)        NOT NULL,
        stnname varchar(32)        NOT NULL,
        state varchar(2) NOT NULL,
        country varchar(2) NOT NULL,
        latitude  double precision NOT NULL,
        longitude double precision NOT NULL,
        elevation int NOT NULL
);

-- create nwx.gfsmos table
DROP TABLE IF EXISTS nwx.gfsmos CASCADE;
CREATE TABLE nwx.gfsmos(
id  SERIAL PRIMARY KEY,
        productid varchar(6)      NOT NULL,
        stnid    varchar(8)        NOT NULL,
        stnname varchar(32)        NOT NULL,
        state varchar(2) NOT NULL,
        country varchar(2) NOT NULL,
        latitude  double precision NOT NULL,
        longitude double precision NOT NULL,
        elevation int NOT NULL
);

-- create nwx.hawaii table
DROP TABLE IF EXISTS nwx.hawaii CASCADE;
CREATE TABLE nwx.hawaii(
id  SERIAL PRIMARY KEY,
        productid varchar(6)      NOT NULL,
        stnid    varchar(8)        NOT NULL,
        stnname varchar(32)        NOT NULL,
        state varchar(2) NOT NULL,
        country varchar(2) NOT NULL,
        latitude  double precision NOT NULL,
        longitude double precision NOT NULL,
        elevation int NOT NULL
);

-- create nwx.heat table
DROP TABLE IF EXISTS nwx.heat CASCADE;
CREATE TABLE nwx.heat(
id  SERIAL PRIMARY KEY,
        productid varchar(6)      NOT NULL,
        stnid    varchar(8)        NOT NULL,
        stnname varchar(32)        NOT NULL,
        state varchar(2) NOT NULL,
        country varchar(2) NOT NULL,
        latitude  double precision NOT NULL,
        longitude double precision NOT NULL,
        elevation int NOT NULL
);

-- create nwx.hsf table
DROP TABLE IF EXISTS nwx.hsf CASCADE;
CREATE TABLE nwx.hsf(
id  SERIAL PRIMARY KEY,
        productid varchar(6)      NOT NULL,
        stnid    varchar(8)        NOT NULL,
        stnname varchar(32)        NOT NULL,
        state varchar(2) NOT NULL,
        country varchar(2) NOT NULL,
        latitude  double precision NOT NULL,
        longitude double precision NOT NULL,
        elevation int NOT NULL
);


-- create nwx.hurrcn table
DROP TABLE IF EXISTS nwx.hurrcn CASCADE;
CREATE TABLE nwx.hurrcn(
id  SERIAL PRIMARY KEY,
        productid varchar(6)      NOT NULL,
        stnid    varchar(8)        NOT NULL,
        stnname varchar(32)        NOT NULL,
        state varchar(2) NOT NULL,
        country varchar(2) NOT NULL,
        latitude  double precision NOT NULL,
        longitude double precision NOT NULL,
        elevation int NOT NULL
);

--create nwx.intl table
DROP TABLE IF EXISTS nwx.intl CASCADE;
CREATE TABLE nwx.intl(
id  SERIAL PRIMARY KEY,
        productid varchar(6)      NOT NULL,
        stnid    varchar(8)        NOT NULL,
        stnname varchar(32)        NOT NULL,
        state varchar(2) NOT NULL,
        country varchar(2) NOT NULL,
        latitude  double precision NOT NULL,
        longitude double precision NOT NULL,
        elevation int NOT NULL
);

--create nwx.intsig table
DROP TABLE IF EXISTS nwx.intsig CASCADE;
CREATE TABLE nwx.intsig(
id  SERIAL PRIMARY KEY,
        productid varchar(6)      NOT NULL,
        stnid    varchar(8)        NOT NULL,
        stnname varchar(32)        NOT NULL,
        state varchar(2) NOT NULL,
        country varchar(2) NOT NULL,
        latitude  double precision NOT NULL,
        longitude double precision NOT NULL,
        elevation int NOT NULL
);


-- create nwx.lfp table
DROP TABLE IF EXISTS nwx.lfp CASCADE;
CREATE TABLE nwx.lfp(
id  SERIAL PRIMARY KEY,
        productid varchar(6)      NOT NULL,
        stnid    varchar(8)        NOT NULL,
        stnname varchar(32)        NOT NULL,
        state varchar(2) NOT NULL,
        country varchar(2) NOT NULL,
        latitude  double precision NOT NULL,
        longitude double precision NOT NULL,
        elevation int NOT NULL
);


-- create nwx.lsfstns table
DROP TABLE IF EXISTS nwx.lsfstns CASCADE;
CREATE TABLE nwx.lsfstns(
id  SERIAL PRIMARY KEY,
        productid varchar(6)      NOT NULL,
        stnid    varchar(8)        NOT NULL,
        stnname varchar(32)        NOT NULL,
        state varchar(2) NOT NULL,
        country varchar(2) NOT NULL,
        latitude  double precision NOT NULL,
        longitude double precision NOT NULL,
        elevation int NOT NULL
);


-- create nwx.lsr table
DROP TABLE IF EXISTS nwx.lsr CASCADE;
CREATE TABLE nwx.lsr(
id  SERIAL PRIMARY KEY,
        productid varchar(6)      NOT NULL,
        stnid    varchar(8)        NOT NULL,
        stnname varchar(32)        NOT NULL,
        state varchar(2) NOT NULL,
        country varchar(2) NOT NULL,
        latitude  double precision NOT NULL,
        longitude double precision NOT NULL,
        elevation int NOT NULL
);

-- create nwx.man table
DROP TABLE IF EXISTS nwx.man CASCADE;
CREATE TABLE nwx.man(
id  SERIAL PRIMARY KEY,
        productid varchar(6)      NOT NULL,
        stnid    varchar(8)        NOT NULL,
        stnname varchar(32)        NOT NULL,
        state varchar(2) NOT NULL,
        country varchar(2) NOT NULL,
        latitude  double precision NOT NULL,
        longitude double precision NOT NULL,
        elevation int NOT NULL DEFAULT 0
);


-- create nwx.marnmos table
DROP TABLE IF EXISTS nwx.marnmos CASCADE;
CREATE TABLE nwx.marnmos(
id  SERIAL PRIMARY KEY,
        productid varchar(6)      NOT NULL,
        stnid    varchar(8)        NOT NULL,
        stnname varchar(32)        NOT NULL,
        state varchar(2) NOT NULL,
        country varchar(2) NOT NULL,
        latitude  double precision NOT NULL,
        longitude double precision NOT NULL,
        elevation int NOT NULL DEFAULT 0
);

-- create nwx.meso table
DROP TABLE IF EXISTS nwx.meso CASCADE;
CREATE TABLE nwx.meso(
id  SERIAL PRIMARY KEY,
        productid varchar(6)      NOT NULL,
        stnid    varchar(8)        NOT NULL,
        stnname varchar(32)        NOT NULL,
        state varchar(2) NOT NULL,
        country varchar(2) NOT NULL,
        latitude  double precision NOT NULL,
        longitude double precision NOT NULL,
        elevation int NOT NULL
);

-- create nwx.mim table
DROP TABLE IF EXISTS nwx.mim CASCADE;
CREATE TABLE nwx.mim(
id  SERIAL PRIMARY KEY,
        productid varchar(6)      NOT NULL,
        stnid    varchar(8)        NOT NULL,
        stnname varchar(32)        NOT NULL,
        state varchar(2) NOT NULL,
        country varchar(2) NOT NULL,
        latitude  double precision NOT NULL,
        longitude double precision NOT NULL,
        elevation int NOT NULL
);

-- create nwx.mis table
DROP TABLE IF EXISTS nwx.mis CASCADE;
CREATE TABLE nwx.mis(
id  SERIAL PRIMARY KEY,
        productid varchar(6)      NOT NULL,
        stnid    varchar(8)        NOT NULL,
        stnname varchar(32)        NOT NULL,
        state varchar(2) NOT NULL,
        country varchar(2) NOT NULL,
        latitude  double precision NOT NULL,
        longitude double precision NOT NULL,
        elevation int NOT NULL
);


--create nwx.mis_avn table
DROP TABLE IF EXISTS nwx.mis_avn CASCADE;
CREATE TABLE nwx.mis_avn(
id  SERIAL PRIMARY KEY,
        productid varchar(6)      NOT NULL,
        stnid    varchar(8)        NOT NULL,
        stnname varchar(32)        NOT NULL,
        state varchar(2) NOT NULL,
        country varchar(2) NOT NULL,
        latitude  double precision NOT NULL,
        longitude double precision NOT NULL,
        elevation int NOT NULL
);

-- create nwx.mrp table
DROP TABLE IF EXISTS nwx.mrp CASCADE;
CREATE TABLE nwx.mrp(
id  SERIAL PRIMARY KEY,
        productid varchar(6)      NOT NULL,
        stnid    varchar(8)        NOT NULL,
        stnname varchar(32)        NOT NULL,
        state varchar(2) NOT NULL,
        country varchar(2) NOT NULL,
        latitude  double precision NOT NULL,
        longitude double precision NOT NULL,
        elevation int NOT NULL DEFAULT 0
);

-- create nwx.n30 table
DROP TABLE IF EXISTS nwx.n30 CASCADE;
CREATE TABLE nwx.n30(
id  SERIAL PRIMARY KEY,
        productid varchar(6)      NOT NULL,
        stnid    varchar(8)        NOT NULL,
        stnname varchar(32)        NOT NULL,
        state varchar(2) NOT NULL,
        country varchar(2) NOT NULL,
        latitude  double precision NOT NULL,
        longitude double precision NOT NULL,
        elevation int NOT NULL
);

-- create nwx.n90 table
DROP TABLE IF EXISTS nwx.n90 CASCADE;
CREATE TABLE nwx.n90(
id  SERIAL PRIMARY KEY,
        productid varchar(6)      NOT NULL,
        stnid    varchar(8)        NOT NULL,
        stnname varchar(32)        NOT NULL,
        state varchar(2) NOT NULL,
        country varchar(2) NOT NULL,
        latitude  double precision NOT NULL,
        longitude double precision NOT NULL,
        elevation int NOT NULL
);

-- create nwx.n610 table
DROP TABLE IF EXISTS nwx.n610 CASCADE;
CREATE TABLE nwx.n610(
id  SERIAL PRIMARY KEY,
        productid varchar(6)      NOT NULL,
        stnid    varchar(8)        NOT NULL,
        stnname varchar(32)        NOT NULL,
        state varchar(2) NOT NULL,
        country varchar(2) NOT NULL,
        latitude  double precision NOT NULL,
        longitude double precision NOT NULL,
        elevation int NOT NULL
);

-- create nwx.ngmgd table
DROP TABLE IF EXISTS nwx.ngmgd CASCADE;
CREATE TABLE nwx.ngmgd(
id  SERIAL PRIMARY KEY,
        productid varchar(6)      NOT NULL,
        stnid    varchar(8)        NOT NULL,
        stnname varchar(32)        NOT NULL,
        state varchar(2) NOT NULL,
        country varchar(2) NOT NULL,
        latitude  double precision NOT NULL,
        longitude double precision NOT NULL,
        elevation int NOT NULL
);

-- create nwx.ngmmos table
DROP TABLE IF EXISTS nwx.ngmmos CASCADE;
CREATE TABLE nwx.ngmmos(
id  SERIAL PRIMARY KEY,
        productid varchar(6)      NOT NULL,
        stnid    varchar(8)        NOT NULL,
        stnname varchar(32)        NOT NULL,
        state varchar(2) NOT NULL,
        country varchar(2) NOT NULL,
        latitude  double precision NOT NULL,
        longitude double precision NOT NULL,
        elevation int NOT NULL
);

-- create nwx.now table
DROP TABLE IF EXISTS nwx.now CASCADE;
CREATE TABLE nwx.now(
id  SERIAL PRIMARY KEY,
        productid varchar(6)      NOT NULL,
        stnid    varchar(8)        NOT NULL,
        stnname varchar(32)        NOT NULL,
        state varchar(2) NOT NULL,
        country varchar(2) NOT NULL,
        latitude  double precision NOT NULL,
        longitude double precision NOT NULL,
        elevation int NOT NULL
);

-- create nwx.off table
DROP TABLE IF EXISTS nwx.off CASCADE;
CREATE TABLE nwx.off(
id  SERIAL PRIMARY KEY,
        productid varchar(6)      NOT NULL,
        stnid    varchar(8)        NOT NULL,
        stnname varchar(32)        NOT NULL,
        state varchar(2) NOT NULL,
        country varchar(2) NOT NULL,
        latitude  double precision NOT NULL,
        longitude double precision NOT NULL,
        elevation int NOT NULL DEFAULT 0
);

-- create nwx.omr table
DROP TABLE IF EXISTS nwx.omr CASCADE;
CREATE TABLE nwx.omr(
id  SERIAL PRIMARY KEY,
        productid varchar(6)      NOT NULL,
        stnid    varchar(8)        NOT NULL,
        stnname varchar(32)        NOT NULL,
        state varchar(2) NOT NULL,
        country varchar(2) NOT NULL,
        latitude  double precision NOT NULL,
        longitude double precision NOT NULL,
        elevation int NOT NULL DEFAULT 0
);


--create nwx.osarea table
DROP TABLE IF EXISTS nwx.osarea CASCADE;
CREATE TABLE nwx.osarea(
id  SERIAL PRIMARY KEY,
        productid varchar(6)      NOT NULL,
        stnid    varchar(8)        NOT NULL,
        stnname varchar(32)        NOT NULL,
        state varchar(2) NOT NULL,
        country varchar(2) NOT NULL,
        latitude  double precision NOT NULL,
        longitude double precision NOT NULL,
        elevation int NOT NULL
);


-- create nwx.pfm table
DROP TABLE IF EXISTS nwx.pfm CASCADE;
CREATE TABLE nwx.pfm(
id  SERIAL PRIMARY KEY,
        productid varchar(6)      NOT NULL,
        stnid    varchar(8)        NOT NULL,
        stnname varchar(32)        NOT NULL,
        state varchar(2) NOT NULL,
        country varchar(2) NOT NULL,
        latitude  double precision NOT NULL,
        longitude double precision NOT NULL,
        elevation int NOT NULL DEFAULT 0
);

-- create nwx.pmdak table
DROP TABLE IF EXISTS nwx.pmdak CASCADE;
CREATE TABLE nwx.pmdak(
id  SERIAL PRIMARY KEY,
        productid varchar(6)      NOT NULL,
        stnid    varchar(8)        NOT NULL,
        stnname varchar(32)        NOT NULL,
        state varchar(2) NOT NULL,
        country varchar(2) NOT NULL,
        latitude  double precision NOT NULL,
        longitude double precision NOT NULL,
        elevation int NOT NULL
);

-- create nwx.pmdca table
DROP TABLE IF EXISTS nwx.pmdca CASCADE;
CREATE TABLE nwx.pmdca(
id  SERIAL PRIMARY KEY,
        productid varchar(6)      NOT NULL,
        stnid    varchar(8)        NOT NULL,
        stnname varchar(32)        NOT NULL,
        state varchar(2) NOT NULL,
        country varchar(2) NOT NULL,
        latitude  double precision NOT NULL,
        longitude double precision NOT NULL,
        elevation int NOT NULL
);


-- create nwx.pmdhi table
DROP TABLE IF EXISTS nwx.pmdhi CASCADE;
CREATE TABLE nwx.pmdhi(
id  SERIAL PRIMARY KEY,
        productid varchar(6)      NOT NULL,
        stnid    varchar(8)        NOT NULL,
        stnname varchar(32)        NOT NULL,
        state varchar(2) NOT NULL,
        country varchar(2) NOT NULL,
        latitude  double precision NOT NULL,
        longitude double precision NOT NULL,
        elevation int NOT NULL
);

-- create nwx.pmdhmd table
DROP TABLE IF EXISTS nwx.pmdhmd CASCADE;
CREATE TABLE nwx.pmdhmd(
id  SERIAL PRIMARY KEY,
        productid varchar(6)      NOT NULL,
        stnid    varchar(8)        NOT NULL,
        stnname varchar(32)        NOT NULL,
        state varchar(2) NOT NULL,
        country varchar(2) NOT NULL,
        latitude  double precision NOT NULL,
        longitude double precision NOT NULL,
        elevation int NOT NULL
);

-- create nwx.pmdthr table
DROP TABLE IF EXISTS nwx.pmdthr CASCADE;
CREATE TABLE nwx.pmdthr(
id  SERIAL PRIMARY KEY,
        productid varchar(6)      NOT NULL,
        stnid    varchar(8)        NOT NULL,
        stnname varchar(32)        NOT NULL,
        state varchar(2) NOT NULL,
        country varchar(2) NOT NULL,
        latitude  double precision NOT NULL,
        longitude double precision NOT NULL,
        elevation int NOT NULL
);

-- create nwx.pmdsa table
DROP TABLE IF EXISTS nwx.pmdsa CASCADE;
CREATE TABLE nwx.pmdsa(
id  SERIAL PRIMARY KEY,
        productid varchar(6)      NOT NULL,
        stnid    varchar(8)        NOT NULL,
        stnname varchar(32)        NOT NULL,
        state varchar(2) NOT NULL,
        country varchar(2) NOT NULL,
        latitude  double precision NOT NULL,
        longitude double precision NOT NULL,
        elevation int NOT NULL
);

-- create nwx.pubout table
DROP TABLE IF EXISTS nwx.pubout CASCADE;
CREATE TABLE nwx.pubout(
id  SERIAL PRIMARY KEY,
        productid varchar(6)      NOT NULL,
        stnid    varchar(8)        NOT NULL,
        stnname varchar(32)        NOT NULL,
        state varchar(2) NOT NULL,
        country varchar(2) NOT NULL,
        latitude  double precision NOT NULL,
        longitude double precision NOT NULL,
        elevation int NOT NULL
);


-- create nwx.pachurr table
DROP TABLE IF EXISTS nwx.pachurr CASCADE;
CREATE TABLE nwx.pachurr(
id  SERIAL PRIMARY KEY,
        productid varchar(6)      NOT NULL,
        stnid    varchar(8)        NOT NULL,
        stnname varchar(32)        NOT NULL,
        state varchar(2) NOT NULL,
        country varchar(2) NOT NULL,
        latitude  double precision NOT NULL,
        longitude double precision NOT NULL,
        elevation int NOT NULL
);

-- create nwx.pls table
DROP TABLE IF EXISTS nwx.pls CASCADE;
CREATE TABLE nwx.pls(
id  SERIAL PRIMARY KEY,
        productid varchar(6)      NOT NULL,
        stnid    varchar(8)        NOT NULL,
        stnname varchar(32)        NOT NULL,
        state varchar(2) NOT NULL,
        country varchar(2) NOT NULL,
        latitude  double precision NOT NULL,
        longitude double precision NOT NULL,
        elevation int NOT NULL
);

-- create nwx.pmdepd table
DROP TABLE IF EXISTS nwx.pmdepd CASCADE;
CREATE TABLE nwx.pmdepd(
id  SERIAL PRIMARY KEY,
        productid varchar(6)      NOT NULL,
        stnid    varchar(8)        NOT NULL,
        stnname varchar(32)        NOT NULL,
        state varchar(2) NOT NULL,
        country varchar(2) NOT NULL,
        latitude  double precision NOT NULL,
        longitude double precision NOT NULL,
        elevation int NOT NULL
);

-- create nwx.pmdspd table
DROP TABLE IF EXISTS nwx.pmdspd CASCADE;
CREATE TABLE nwx.pmdspd(
id  SERIAL PRIMARY KEY,
        productid varchar(6)      NOT NULL,
        stnid    varchar(8)        NOT NULL,
        stnname varchar(32)        NOT NULL,
        state varchar(2) NOT NULL,
        country varchar(2) NOT NULL,
        latitude  double precision NOT NULL,
        longitude double precision NOT NULL,
        elevation int NOT NULL
);

-- create nwx.preepd table
DROP TABLE IF EXISTS nwx.preepd CASCADE;
CREATE TABLE nwx.preepd(
id  SERIAL PRIMARY KEY,
        productid varchar(6)      NOT NULL,
        stnid    varchar(8)        NOT NULL,
        stnname varchar(32)        NOT NULL,
        state varchar(2) NOT NULL,
        country varchar(2) NOT NULL,
        latitude  double precision NOT NULL,
        longitude double precision NOT NULL,
        elevation int NOT NULL
);

-- create nwx.qpferd table
DROP TABLE IF EXISTS nwx.qpferd CASCADE;
CREATE TABLE nwx.qpferd(
id  SERIAL PRIMARY KEY,
        productid varchar(6)      NOT NULL,
        stnid    varchar(8)        NOT NULL,
        stnname varchar(32)        NOT NULL,
        state varchar(2) NOT NULL,
        country varchar(2) NOT NULL,
        latitude  double precision NOT NULL,
        longitude double precision NOT NULL,
        elevation int NOT NULL
);

-- create nwx.qpfhsd table
DROP TABLE IF EXISTS nwx.qpfhsd CASCADE;
CREATE TABLE nwx.qpfhsd(
id  SERIAL PRIMARY KEY,
        productid varchar(6)      NOT NULL,
        stnid    varchar(8)        NOT NULL,
        stnname varchar(32)        NOT NULL,
        state varchar(2) NOT NULL,
        country varchar(2) NOT NULL,
        latitude  double precision NOT NULL,
        longitude double precision NOT NULL,
        elevation int NOT NULL
);


-- create nwx.qpfpfd table
DROP TABLE IF EXISTS nwx.qpfpfd CASCADE;
CREATE TABLE nwx.qpfpfd(
id  SERIAL PRIMARY KEY,
        productid varchar(6)      NOT NULL,
        stnid    varchar(8)        NOT NULL,
        stnname varchar(32)        NOT NULL,
        state varchar(2) NOT NULL,
        country varchar(2) NOT NULL,
        latitude  double precision NOT NULL,
        longitude double precision NOT NULL,
        elevation int NOT NULL
);

-- create nwx.rdf table
DROP TABLE IF EXISTS nwx.rdf CASCADE;
CREATE TABLE nwx.rdf(
id  SERIAL PRIMARY KEY,
        productid varchar(6)      NOT NULL,
        stnid    varchar(8)        NOT NULL,
        stnname varchar(32)        NOT NULL,
        state varchar(2) NOT NULL,
        country varchar(2) NOT NULL,
        latitude  double precision NOT NULL,
        longitude double precision NOT NULL,
        elevation int NOT NULL DEFAULT 0
);


-- create nwx.rtp table
DROP TABLE IF EXISTS nwx.rtp CASCADE;
CREATE TABLE nwx.rtp(
id  SERIAL PRIMARY KEY,
        productid varchar(6)      NOT NULL,
        stnid    varchar(8)        NOT NULL,
        stnname varchar(32)        NOT NULL,
        state varchar(2) NOT NULL,
        country varchar(2) NOT NULL,
        latitude  double precision NOT NULL,
        longitude double precision NOT NULL,
        elevation int NOT NULL
);

-- create nwx.rvs table
DROP TABLE IF EXISTS nwx.rvs CASCADE;
CREATE TABLE nwx.rvs(
id  SERIAL PRIMARY KEY,
        productid varchar(6)      NOT NULL,
        stnid    varchar(8)        NOT NULL,
        stnname varchar(32)        NOT NULL,
        state varchar(2) NOT NULL,
        country varchar(2) NOT NULL,
        latitude  double precision NOT NULL,
        longitude double precision NOT NULL,
        elevation int NOT NULL DEFAULT 0
);

-- create nwx.satest table
DROP TABLE IF EXISTS nwx.satest CASCADE;
CREATE TABLE nwx.satest(
id  SERIAL PRIMARY KEY,
        productid varchar(6)      NOT NULL,
        stnid    varchar(8)        NOT NULL,
        stnname varchar(32)        NOT NULL,
        state varchar(2) NOT NULL,
        country varchar(2) NOT NULL,
        latitude  double precision NOT NULL,
        longitude double precision NOT NULL,
        elevation int NOT NULL
);

-- create nwx.scs table
DROP TABLE IF EXISTS nwx.scs CASCADE;
CREATE TABLE nwx.scs(
id  SERIAL PRIMARY KEY,
        productid varchar(6)      NOT NULL,
        stnid    varchar(8)        NOT NULL,
        stnname varchar(32)        NOT NULL,
        state varchar(2) NOT NULL,
        country varchar(2) NOT NULL,
        latitude  double precision NOT NULL,
        longitude double precision NOT NULL,
        elevation int NOT NULL
);

-- create nwx.sdm table
DROP TABLE IF EXISTS nwx.sdm CASCADE;
CREATE TABLE nwx.sdm(
id  SERIAL PRIMARY KEY,
        productid varchar(6)      NOT NULL,
        stnid    varchar(8)        NOT NULL,
        stnname varchar(32)        NOT NULL,
        state varchar(2) NOT NULL,
        country varchar(2) NOT NULL,
        latitude  double precision NOT NULL,
        longitude double precision NOT NULL,
        elevation int NOT NULL
);

-- create nwx.sfd table
DROP TABLE IF EXISTS nwx.sfd CASCADE;
CREATE TABLE nwx.sfd(
id  SERIAL PRIMARY KEY,
        productid varchar(6)      NOT NULL,
        stnid    varchar(8)        NOT NULL,
        stnname varchar(32)        NOT NULL,
        state varchar(2) NOT NULL,
        country varchar(2) NOT NULL,
        latitude  double precision NOT NULL,
        longitude double precision NOT NULL,
        elevation int NOT NULL
);

-- create nwx.sfp table
DROP TABLE IF EXISTS nwx.sfp CASCADE;
CREATE TABLE nwx.sfp(
id  SERIAL PRIMARY KEY,
        productid varchar(6)      NOT NULL,
        stnid    varchar(8)        NOT NULL,
        stnname varchar(32)        NOT NULL,
        state varchar(2) NOT NULL,
        country varchar(2) NOT NULL,
        latitude  double precision NOT NULL,
        longitude double precision NOT NULL,
        elevation int NOT NULL
);

-- create nwx.sft table
DROP TABLE IF EXISTS nwx.sft CASCADE;
CREATE TABLE nwx.sft(
id  SERIAL PRIMARY KEY,
        productid varchar(6)      NOT NULL,
        stnid    varchar(8)        NOT NULL,
        stnname varchar(32)        NOT NULL,
        state varchar(2) NOT NULL,
        country varchar(2) NOT NULL,
        latitude  double precision NOT NULL,
        longitude double precision NOT NULL,
        elevation int NOT NULL
);

-- create nwx.sfstns table
DROP TABLE IF EXISTS nwx.sfstns CASCADE;
CREATE TABLE nwx.sfstns(
id  SERIAL PRIMARY KEY,
        productid varchar(6)      NOT NULL,
        stnid    varchar(8)        NOT NULL,
        stnname varchar(32)        NOT NULL,
        state varchar(2) NOT NULL,
        country varchar(2) NOT NULL,
        latitude  double precision NOT NULL,
        longitude double precision NOT NULL,
        elevation int NOT NULL
);

-- create nwx.snstns table
DROP TABLE IF EXISTS nwx.snstns CASCADE;
CREATE TABLE nwx.snstns(
id  SERIAL PRIMARY KEY,
        productid varchar(6)      NOT NULL,
        stnid    varchar(8)        NOT NULL,
        stnname varchar(32)        NOT NULL,
        state varchar(2) NOT NULL,
        country varchar(2) NOT NULL,
        latitude  double precision NOT NULL,
        longitude double precision NOT NULL,
        elevation int NOT NULL
);

--create nwx.sigmet table
DROP TABLE IF EXISTS nwx.sigmet CASCADE;
CREATE TABLE nwx.sigmet(
id  SERIAL PRIMARY KEY,
        productid varchar(6)      NOT NULL,
        stnid    varchar(8)        NOT NULL,
        stnname varchar(32)        NOT NULL,
        state varchar(2) NOT NULL,
        country varchar(2) NOT NULL,
        latitude  double precision NOT NULL,
        longitude double precision NOT NULL,
        elevation int NOT NULL
);

--create nwx.spcadm table
DROP TABLE IF EXISTS nwx.spcadm CASCADE;
CREATE TABLE nwx.spcadm(
id  SERIAL PRIMARY KEY,
        productid varchar(6)      NOT NULL,
        stnid    varchar(8)        NOT NULL,
        stnname varchar(32)        NOT NULL,
        state varchar(2) NOT NULL,
        country varchar(2) NOT NULL,
        latitude  double precision NOT NULL,
        longitude double precision NOT NULL,
        elevation int NOT NULL
);

--create nwx.srp table
DROP TABLE IF EXISTS nwx.srp CASCADE;
CREATE TABLE nwx.srp(
id  SERIAL PRIMARY KEY,
        productid varchar(6)      NOT NULL,
        stnid    varchar(8)        NOT NULL,
        stnname varchar(32)        NOT NULL,
        state varchar(2) NOT NULL,
        country varchar(2) NOT NULL,
        latitude  double precision NOT NULL,
        longitude double precision NOT NULL,
        elevation int NOT NULL
);


-- create nwx.stadts table
DROP TABLE IF EXISTS nwx.stadts CASCADE;
CREATE TABLE nwx.stadts(
id  SERIAL PRIMARY KEY,
        productid varchar(6)      NOT NULL,
        stnid    varchar(8)        NOT NULL,
        stnname varchar(32)        NOT NULL,
        state varchar(2) NOT NULL,
        country varchar(2) NOT NULL,
        latitude  double precision NOT NULL,
        longitude double precision NOT NULL,
        elevation int NOT NULL
);


-- create nwx.stahry table
DROP TABLE IF EXISTS nwx.stahry CASCADE;
CREATE TABLE nwx.stahry(
id  SERIAL PRIMARY KEY,
        productid varchar(6)      NOT NULL,
        stnid    varchar(8)        NOT NULL,
        stnname varchar(32)        NOT NULL,
        state varchar(2) NOT NULL,
        country varchar(2) NOT NULL,
        latitude  double precision NOT NULL,
        longitude double precision NOT NULL,
        elevation int NOT NULL
);


-- create nwx.stations table
DROP TABLE IF EXISTS nwx.stations CASCADE;
CREATE TABLE nwx.stations(
id  SERIAL PRIMARY KEY,
        productid varchar(6)      NOT NULL,
        stnid    varchar(8)        NOT NULL,
        stnname varchar(32)        NOT NULL,
        state varchar(2) NOT NULL,
        country varchar(2) NOT NULL,
        latitude  double precision NOT NULL,
        longitude double precision NOT NULL,
        elevation int NOT NULL
);

-- create nwx.status table
DROP TABLE IF EXISTS nwx.status CASCADE;
CREATE TABLE nwx.status(
id  SERIAL PRIMARY KEY,
        productid varchar(6)      NOT NULL,
        stnid    varchar(8)        NOT NULL,
        stnname varchar(32)        NOT NULL,
        state varchar(2) NOT NULL,
        country varchar(2) NOT NULL,
        latitude  double precision NOT NULL,
        longitude double precision NOT NULL,
        elevation int NOT NULL
);

--create nwx.sus table
DROP TABLE IF EXISTS nwx.sus CASCADE;
CREATE TABLE nwx.sus(
id  SERIAL PRIMARY KEY,
        productid varchar(6)      NOT NULL,
        stnid    varchar(8)        NOT NULL,
        stnname varchar(32)        NOT NULL,
        state varchar(2) NOT NULL,
        country varchar(2) NOT NULL,
        latitude  double precision NOT NULL,
        longitude double precision NOT NULL,
        elevation int NOT NULL
);

-- create nwx.svrwx table
DROP TABLE IF EXISTS nwx.svrwx CASCADE;
CREATE TABLE nwx.svrwx(
id  SERIAL PRIMARY KEY,
        productid varchar(6)      NOT NULL,
        stnid    varchar(8)        NOT NULL,
        stnname varchar(32)        NOT NULL,
        state varchar(2) NOT NULL,
        country varchar(2) NOT NULL,
        latitude  double precision NOT NULL,
        longitude double precision NOT NULL,
        elevation int NOT NULL
);

-- create table nwx.taf
DROP TABLE IF EXISTS nwx.taf CASCADE;
CREATE TABLE nwx.taf(
id  SERIAL PRIMARY KEY,
        productid varchar(6)      NOT NULL,
        stnid    varchar(8)        NOT NULL,
        stnname varchar(32)        NOT NULL,
        state varchar(2) NOT NULL,
        country varchar(2) NOT NULL,
        latitude  double precision NOT NULL,
        longitude double precision NOT NULL,
        elevation int NOT NULL
);


-- create nwx.tafstn table
DROP TABLE IF EXISTS nwx.tafstn CASCADE;
CREATE TABLE nwx.tafstn(
id  SERIAL PRIMARY KEY,
        productid varchar(6)      NOT NULL,
        stnid    varchar(8)        NOT NULL,
        stnname varchar(32)        NOT NULL,
        state varchar(2) NOT NULL,
        country varchar(2) NOT NULL,
        latitude  double precision NOT NULL,
        longitude double precision NOT NULL,
        elevation int NOT NULL
);

-- create nwx.tcd table
DROP TABLE IF EXISTS nwx.tcd CASCADE;
CREATE TABLE nwx.tcd(
id  SERIAL PRIMARY KEY,
        productid varchar(6)      NOT NULL,
        stnid    varchar(8)        NOT NULL,
        stnname varchar(32)        NOT NULL,
        state varchar(2) NOT NULL,
        country varchar(2) NOT NULL,
        latitude  double precision NOT NULL,
        longitude double precision NOT NULL,
        elevation int NOT NULL
);


-- create nwx.tce table
DROP TABLE IF EXISTS nwx.tce CASCADE;
CREATE TABLE nwx.tce(
id  SERIAL PRIMARY KEY,
        productid varchar(6)      NOT NULL,
        stnid    varchar(8)        NOT NULL,
        stnname varchar(32)        NOT NULL,
        state varchar(2) NOT NULL,
        country varchar(2) NOT NULL,
        latitude  double precision NOT NULL,
        longitude double precision NOT NULL,
        elevation int NOT NULL
);


-- create nwx.tcm table
DROP TABLE IF EXISTS nwx.tcm CASCADE;
CREATE TABLE nwx.tcm(
id  SERIAL PRIMARY KEY,
        productid varchar(6)      NOT NULL,
        stnid    varchar(8)        NOT NULL,
        stnname varchar(32)        NOT NULL,
        state varchar(2) NOT NULL,
        country varchar(2) NOT NULL,
        latitude  double precision NOT NULL,
        longitude double precision NOT NULL,
        elevation int NOT NULL
);

-- create nwx.tcp table
DROP TABLE IF EXISTS nwx.tcp CASCADE;
CREATE TABLE nwx.tcp(
id  SERIAL PRIMARY KEY,
        productid varchar(6)      NOT NULL,
        stnid    varchar(8)        NOT NULL,
        stnname varchar(32)        NOT NULL,
        state varchar(2) NOT NULL,
        country varchar(2) NOT NULL,
        latitude  double precision NOT NULL,
        longitude double precision NOT NULL,
        elevation int NOT NULL
);

-- create nwx.tcu table
DROP TABLE IF EXISTS nwx.tcu CASCADE;
CREATE TABLE nwx.tcu(
id  SERIAL PRIMARY KEY,
        productid varchar(6)      NOT NULL,
        stnid    varchar(8)        NOT NULL,
        stnname varchar(32)        NOT NULL,
        state varchar(2) NOT NULL,
        country varchar(2) NOT NULL,
        latitude  double precision NOT NULL,
        longitude double precision NOT NULL,
        elevation int NOT NULL
);



-- create nwx.tornftl table
DROP TABLE IF EXISTS nwx.tornftl CASCADE;
CREATE TABLE nwx.tornftl(
id  SERIAL PRIMARY KEY,
        productid varchar(6)      NOT NULL,
        stnid    varchar(8)        NOT NULL,
        stnname varchar(32)        NOT NULL,
        state varchar(2) NOT NULL,
        country varchar(2) NOT NULL,
        latitude  double precision NOT NULL,
        longitude double precision NOT NULL,
        elevation int NOT NULL
);

-- create nwx.tornmon table
DROP TABLE IF EXISTS nwx.tornmon CASCADE;
CREATE TABLE nwx.tornmon(
id  SERIAL PRIMARY KEY,
        productid varchar(6)      NOT NULL,
        stnid    varchar(8)        NOT NULL,
        stnname varchar(32)        NOT NULL,
        state varchar(2) NOT NULL,
        country varchar(2) NOT NULL,
        latitude  double precision NOT NULL,
        longitude double precision NOT NULL,
        elevation int NOT NULL
);

-- create nwx.twd table
DROP TABLE IF EXISTS nwx.twd CASCADE;
CREATE TABLE nwx.twd(
id  SERIAL PRIMARY KEY,
        productid varchar(6)      NOT NULL,
        stnid    varchar(8)        NOT NULL,
        stnname varchar(32)        NOT NULL,
        state varchar(2) NOT NULL,
        country varchar(2) NOT NULL,
        latitude  double precision NOT NULL,
        longitude double precision NOT NULL,
        elevation int NOT NULL
);

-- create nwx.two table
DROP TABLE IF EXISTS nwx.two CASCADE;
CREATE TABLE nwx.two(
id  SERIAL PRIMARY KEY,
        productid varchar(6)      NOT NULL,
        stnid    varchar(8)        NOT NULL,
        stnname varchar(32)        NOT NULL,
        state varchar(2) NOT NULL,
        country varchar(2) NOT NULL,
        latitude  double precision NOT NULL,
        longitude double precision NOT NULL,
        elevation int NOT NULL
);

-- create nwx.tws table
DROP TABLE IF EXISTS nwx.tws CASCADE;
CREATE TABLE nwx.tws(
id  SERIAL PRIMARY KEY,
        productid varchar(6)      NOT NULL,
        stnid    varchar(8)        NOT NULL,
        stnname varchar(32)        NOT NULL,
        state varchar(2) NOT NULL,
        country varchar(2) NOT NULL,
        latitude  double precision NOT NULL,
        longitude double precision NOT NULL,
        elevation int NOT NULL
);

-- create nwx.uvi table
DROP TABLE IF EXISTS nwx.uvi CASCADE;
CREATE TABLE nwx.uvi(
id  SERIAL PRIMARY KEY,
        productid varchar(6)      NOT NULL,
        stnid    varchar(8)        NOT NULL,
        stnname varchar(32)        NOT NULL,
        state varchar(2) NOT NULL,
        country varchar(2) NOT NULL,
        latitude  double precision NOT NULL,
        longitude double precision NOT NULL,
        elevation int NOT NULL
);


-- create nwx.vlcsig table
DROP TABLE IF EXISTS nwx.vlcsig CASCADE;
CREATE TABLE nwx.vlcsig(
id  SERIAL PRIMARY KEY,
        productid varchar(6)      NOT NULL,
        stnid    varchar(8)        NOT NULL,
        stnname varchar(32)        NOT NULL,
        state varchar(2) NOT NULL,
        country varchar(2) NOT NULL,
        latitude  double precision NOT NULL,
        longitude double precision NOT NULL,
        elevation int NOT NULL
);

-- create nwx.watbox table
DROP TABLE IF EXISTS nwx.watbox CASCADE;
CREATE TABLE nwx.watbox(
id  SERIAL PRIMARY KEY,
        productid varchar(6)      NOT NULL,
        stnid    varchar(8)        NOT NULL,
        stnname varchar(32)        NOT NULL,
        state varchar(2) NOT NULL,
        country varchar(2) NOT NULL,
        latitude  double precision NOT NULL,
        longitude double precision NOT NULL,
        elevation int NOT NULL
);


-- create nwx.watcnty table
DROP TABLE IF EXISTS nwx.watcnty CASCADE;
CREATE TABLE nwx.watcnty(
id  SERIAL PRIMARY KEY,
        productid varchar(6)      NOT NULL,
        stnid    varchar(8)        NOT NULL,
        stnname varchar(32)        NOT NULL,
        state varchar(2) NOT NULL,
        country varchar(2) NOT NULL,
        latitude  double precision NOT NULL,
        longitude double precision NOT NULL,
        elevation int NOT NULL
);


-- create nwx.watndsc table
DROP TABLE IF EXISTS nwx.watndsc CASCADE;
CREATE TABLE nwx.watndsc(
id  SERIAL PRIMARY KEY,
        productid varchar(6)      NOT NULL,
        stnid    varchar(8)        NOT NULL,
        stnname varchar(32)        NOT NULL,
        state varchar(2) NOT NULL,
        country varchar(2) NOT NULL,
        latitude  double precision NOT NULL,
        longitude double precision NOT NULL,
        elevation int NOT NULL
);


-- create nwx.watsum table
DROP TABLE IF EXISTS nwx.watsum CASCADE;
CREATE TABLE nwx.watsum(
id  SERIAL PRIMARY KEY,
        productid varchar(6)      NOT NULL,
        stnid    varchar(8)        NOT NULL,
        stnname varchar(32)        NOT NULL,
        state varchar(2) NOT NULL,
        country varchar(2) NOT NULL,
        latitude  double precision NOT NULL,
        longitude double precision NOT NULL,
        elevation int NOT NULL
);


-- create nwx.wou table
DROP TABLE IF EXISTS nwx.wou CASCADE;
CREATE TABLE nwx.wou(
id  SERIAL PRIMARY KEY,
        productid varchar(6)      NOT NULL,
        stnid    varchar(8)        NOT NULL,
        stnname varchar(32)        NOT NULL,
        state varchar(2) NOT NULL,
        country varchar(2) NOT NULL,
        latitude  double precision NOT NULL,
        longitude double precision NOT NULL,
        elevation int NOT NULL
);

-- create nwx.wwp table
DROP TABLE IF EXISTS nwx.wwp CASCADE;
CREATE TABLE nwx.wwp(
id  SERIAL PRIMARY KEY,
        productid varchar(6)      NOT NULL,
        stnid    varchar(8)        NOT NULL,
        stnname varchar(32)        NOT NULL,
        state varchar(2) NOT NULL,
        country varchar(2) NOT NULL,
        latitude  double precision NOT NULL,
        longitude double precision NOT NULL,
        elevation int NOT NULL
);

-- create nwx.zfp table
DROP TABLE IF EXISTS nwx.zfp CASCADE;
CREATE TABLE nwx.zfp(
id  SERIAL PRIMARY KEY,
        productid varchar(6)      NOT NULL,
        stnid    varchar(8)        NOT NULL,
        stnname varchar(32)        NOT NULL,
        state varchar(2) NOT NULL,
        country varchar(2) NOT NULL,
        latitude  double precision NOT NULL,
        longitude double precision NOT NULL,
        elevation int NOT NULL
);


































