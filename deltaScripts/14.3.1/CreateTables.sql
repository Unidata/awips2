-- Script to create new tables in the IHFS db for dual-pol work

create table DAARadar
(
radid		varchar(3)     not null,
obstime		TIMESTAMP   not null,
minoff		smallint,
maxvalh		FLOAT4,
maxvald		FLOAT4,
s1_bias_value	FLOAT4,
producttime	TIMESTAMP,
null_product_flag       smallint,
coverage_dur            integer,
grid_filename	varchar(20)
)
;

ALTER TABLE        DAARadar 
  ADD CONSTRAINT     daaradar_pk 
  PRIMARY KEY        (radid, obstime);
	
ALTER TABLE        DAARadar 
  ADD CONSTRAINT     daaradar_radloc_fk 
  FOREIGN KEY        (radid) 
  REFERENCES         RadarLoc (radid) 
  MATCH FULL;
	

revoke all on DAARadar from public;
grant select, insert, update, delete on DAARadar to public;

-- Add Table


create table DAABiasDyn
(
radid         varchar(3)    not null,
office_id     varchar(5)         not null,
obstime       TIMESTAMP                not null,
memspan_ind   smallint                 not null,
numpairs      FLOAT8,
sumgag        FLOAT4,
sumrad        FLOAT4,
bias          FLOAT4
);

ALTER TABLE        DAABiasDyn 
  ADD CONSTRAINT     daabiasdynparams_pk 
  PRIMARY KEY        (radid, office_id, obstime, memspan_ind);
	
revoke all on DAABiasDyn from public;
grant  select, insert, update, delete on DAABiasDyn to public;

--


create table DAARadarResult
(
radid             varchar(3)       not null,
obstime           TIMESTAMP  not null,
num_gages         smallint,
rad_avail         varchar(1),
rw_bias_val_used  FLOAT8,
mem_span_used     FLOAT8,
edit_bias         varchar(1),
ignore_radar      varchar(1)
);

ALTER TABLE        DAARadarResult 
  ADD CONSTRAINT     daaradarresult_pk 
  PRIMARY KEY        (radid, obstime);
	
revoke all on DAARadarResult from public;
grant  select, insert, update, delete on DAARadarResult to public;

-- Add Table HPERadarResult

create table HPERadarResult
(
hpe_productname		varchar(30)     not null,
producttime             TIMESTAMP       not null,
num_radar_avail	        smallint,
bias_source             varchar(20),
radar_data_source	varchar(1)
)
;

ALTER TABLE        HPERadarResult 
  ADD CONSTRAINT     hperadarresult_pk 
  PRIMARY KEY        (hpe_productname, producttime);
	
revoke all on HPERadarResult from public;
grant select, insert, update, delete on HPERadarResult to public;


-- Add DSARadar, DPRRadar tables, etc
create table DSARadar		
(
radid		varchar(3)     not null,					
obstime		TIMESTAMP      not null,
volcovpat    smallint,
opermode     smallint,
maxval       FLOAT4,
scale        FLOAT4,
setoff       FLOAT4,
begin_time   TIMESTAMP,
end_time     TIMESTAMP,
j_beg_date   smallint,
j_beg_time   smallint,
j_end_date   smallint,
j_end_time   smallint,
mean_field_bias smallint,
nullproductflag smallint,
grid_filename	varchar(20)	-- file name of location of grid data
)
;

ALTER TABLE        DSARadar 
  ADD CONSTRAINT     DSAradar_pk 
  PRIMARY KEY        (radid, obstime);
	
ALTER TABLE        DSARadar 
  ADD CONSTRAINT     DSAradar_radloc_fk 
  FOREIGN KEY        (radid) 
  REFERENCES         RadarLoc (radid) 
  MATCH FULL;

revoke all on DSARadar from public;
grant select, insert, update, delete on DSARadar to public;

-- Add Table DSAAdapt

create table DSAAdapt         
(
radid                varchar(3)  not null,
obstime              timestamp  not null,
num_of_adap          smallint,
default_ml_depth     FLOAT4,
ml_overide_flag      varchar(8),
kdp_mult             FLOAT4,
kdp_power            FLOAT4,
z_r_mult             FLOAT4,
z_r_power            FLOAT4,
zdr_z_mult           FLOAT4,
zdr_z_power          FLOAT4,
zdr_zdr_power        FLOAT4,
min_corr_precip      FLOAT4,
min_corr_kdp         FLOAT4,
refl_max             FLOAT4,
kdp_max_beam_blk     FLOAT4,
max_usability_blk    FLOAT4,
kdp_min_usage_rate   FLOAT4,
ws_mult              FLOAT4,
gr_mult              FLOAT4,
rh_mult              FLOAT4,
ds_mult              FLOAT4,
ic_mult              FLOAT4,
grid_is_full         FLOAT4,
paif_rate            FLOAT4,
paif_area            FLOAT4,
rain_time_thresh     FLOAT4,
num_zones            FLOAT4,
max_precip_rate      FLOAT4,
restart_time         FLOAT4,
max_interp_time      FLOAT4,
max_hourly_acc       FLOAT4,
time_bias            FLOAT4,
num_grpairs          FLOAT4,
reset_bias           FLOAT4,
longst_lag           FLOAT4
)
;

ALTER TABLE        DSAAdapt 
  ADD CONSTRAINT     dsaadapt_pk 
  PRIMARY KEY        (radid, obstime);

ALTER TABLE        DSAAdapt 
  ADD CONSTRAINT     dsaadapt_rad_fk 
  FOREIGN KEY        (radid) 
  REFERENCES         RadarLoc (radid) 
  MATCH FULL;

revoke all on DSAAdapt from public;
grant select, insert, update, delete on DSAAdapt to public;


-- Add Table DPRRadar

create table DPRRadar			
(
radid		varchar(3)     not null,					
obstime		TIMESTAMP   not null, 			
volcovpat    smallint,
opermode     smallint,
maxval       FLOAT4,
scale        FLOAT4,
setoff       FLOAT4,
j_end_date   integer,
j_end_time   integer,
mean_field_bias smallint,
precipdetectedflag smallint,
grid_filename	varchar(20)	-- file name of location of grid data
)
;

ALTER TABLE        DPRRadar 
  ADD CONSTRAINT     DPRradar_pk 
  PRIMARY KEY        (radid, obstime);
	
ALTER TABLE        DPRRadar 
  ADD CONSTRAINT     DPRradar_radloc_fk 
  FOREIGN KEY        (radid) 
  REFERENCES         RadarLoc (radid) 
  MATCH FULL;

revoke all on DPRRadar from public;
grant select, insert, update, delete on DPRRadar to public;
