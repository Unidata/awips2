-- create nwx.adminmessages table
DROP TABLE IF EXISTS nwx.adminmessages CASCADE;
CREATE TABLE nwx.adminmessages(
id SERIAL PRIMARY KEY,
productname varchar(60) NOT  NULL,
producttablename varchar(30) NOT NULL,
producttype varchar(20) NOT NULL
);

-- create nwx.cmcam table
DROP TABLE IF EXISTS nwx.cmcam CASCADE;
CREATE TABLE nwx.cmcam(
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

-- create nwx.ncfam table
DROP TABLE IF EXISTS nwx.ncfam CASCADE;
CREATE TABLE nwx.ncfam(
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

-- create nwx.nesdisam table
DROP TABLE IF EXISTS nwx.nesdisam CASCADE;
CREATE TABLE nwx.nesdisam(
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

-- create nwx.nesdispm table
DROP TABLE IF EXISTS nwx.nesdispm CASCADE;
CREATE TABLE nwx.nesdispm(
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

-- create nwx.snwstgam table
DROP TABLE IF EXISTS nwx.nwstgam CASCADE;
CREATE TABLE nwx.nwstgam(
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

-- Drop nwx.sdm table
DROP TABLE IF EXISTS nwx.sdm CASCADE;

-- create nwx.sdmam table
DROP TABLE IF EXISTS nwx.sdmam CASCADE;
CREATE TABLE nwx.sdmam(
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

-- create nwx.sdmim table
DROP TABLE IF EXISTS nwx.sdmim CASCADE;
CREATE TABLE nwx.sdmim(
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

-- create nwx.sdmdhm table
DROP TABLE IF EXISTS nwx.sdmdhm CASCADE;
CREATE TABLE nwx.sdmdhm(
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

INSERT INTO nwx.datatypegrouplist (datatypegroupname,datatypegrouptablename) values ('Admin Messages','nwx.adminmessages');
INSERT INTO nwx.adminmessages(productname,producttablename,producttype) values ('SDM Administrative Messages','nwx.sdmam','sdmam');
INSERT INTO nwx.sdmam (productid,stnid,stnname,state,country,latitude,longitude,elevation) values ('NOUS42','KWNO','NMC','MD','US',38.82,-76.87,86);
INSERT INTO nwx.adminmessages(productname,producttablename,producttype) values ('SDM International Messages','nwx.sdmim','sdmim');
INSERT INTO nwx.sdmim (productid,stnid,stnname,state,country,latitude,longitude,elevation) values ('NPXX10','KWNO','NMC','MD','US',38.82,-76.87,86);
INSERT INTO nwx.adminmessages(productname,producttablename,producttype) values ('SDM DHS Hazards Messages','nwx.sdmdhm','sdmdhm');
INSERT INTO nwx.sdmdhm (productid,stnid,stnname,state,country,latitude,longitude,elevation) values ('NOUS71','KWNO','NMC','MD','US',38.82,-76.87,86);
INSERT INTO nwx.adminmessages(productname,producttablename,producttype) values ('CMC Administrative Messages','nwx.cmcam','cmcam');
INSERT INTO nwx.cmcam (productid,stnid,stnname,state,country,latitude,longitude,elevation) values ('NOCN05', 'CWAO','MONTREAL_VAAC','CN','CN',45.47,-73.75,-9999);
INSERT INTO nwx.cmcam (productid,stnid,stnname,state,country,latitude,longitude,elevation) values ('AACN01', 'CWAO','MONTREAL_VAAC','CN','CN',45.47,-73.75,-9999);
INSERT INTO nwx.adminmessages(productname,producttablename,producttype) values ('NWSTG Administrative Messages','nwx.nwstgam','nwstgam');
INSERT INTO nwx.nwstgam (productid,stnid,stnname,state,country,latitude,longitude,elevation) values ('NOXX01', 'KWBC','NMC','MD','US',38.82,-76.87,86);
INSERT INTO nwx.adminmessages(productname,producttablename,producttype) values ('NCF Administrative Messages','nwx.ncfam','ncfam');
INSERT INTO nwx.ncfam (productid,stnid,stnname,state,country,latitude,longitude,elevation) values ('NOUS72', 'KNCF','NMC','MD','US',38.82,-76.87,86);
INSERT INTO nwx.adminmessages(productname,producttablename,producttype) values ('NESDIS Product Anomaly Messages','nwx.nesdispm','nesdispm');
INSERT INTO nwx.nesdispm (productid,stnid,stnname,state,country,latitude,longitude,elevation) values ('NOUS71', 'KNES','NESDIS','MD','US',38.82,-76.87,86);
INSERT INTO nwx.adminmessages(productname,producttablename,producttype) values ('NESDIS Administrative Messages','nwx.nesdisam','nesdisam');
INSERT INTO nwx.nesdisam (productid,stnid,stnname,state,country,latitude,longitude,elevation) values ('NOUS72', 'KNES','NESDIS','MD','US',38.82,-76.87,86);

INSERT INTO awips.nctext_inputfile_type VALUES (181,'sdmdhm','W');
INSERT INTO awips.nctext_inputfile_type VALUES (182,'cmcam','W');
INSERT INTO awips.nctext_inputfile_type VALUES (183,'nwstgam','W');
INSERT INTO awips.nctext_inputfile_type VALUES (184,'ncfam','W');
INSERT INTO awips.nctext_inputfile_type VALUES (185,'nesdispm','W');
INSERT INTO awips.nctext_inputfile_type VALUES (186,'nesdisam','W');
INSERT INTO awips.nctext_inputfile_type VALUES (185,'sdmam','B');
INSERT INTO awips.nctext_inputfile_type VALUES (186,'sdmim','W');

DELETE from nwx.hpcproducts where productname='SDM Messages';
DELETE from nwx.hpcproducts where productname='International Messages';
DROP TABLE nwx.sdm;
DROP TABLE nwx.intl;

