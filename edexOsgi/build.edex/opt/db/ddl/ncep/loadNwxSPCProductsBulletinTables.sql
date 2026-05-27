
--load nwx.day1 table
copy nwx.day1 (productid,stnid,stnname,state,country,latitude,longitude,elevation) FROM stdin with delimiter as ',';
ACUS01,KWNS,NORMAN,OK,US,35.22,-97.47,362
\.

--load nwx.day1pfw table
copy nwx.day1pfw (productid,stnid,stnname,state,country,latitude,longitude,elevation) FROM stdin with delimiter as ',';
FNUS31,KWNS,NORMAN,OK,US,35.22,-97.47,362
\.


--load nwx.day1pts table
copy nwx.day1pts (productid,stnid,stnname,state,country,latitude,longitude,elevation) FROM stdin with delimiter as ',';
WUUS01,KWNS,NORMAN,OK,US,35.22,-97.47,362
\.


--load nwx.day2 table
copy nwx.day2 (productid,stnid,stnname,state,country,latitude,longitude,elevation) FROM stdin with delimiter as ',';
ACUS02,KWNS,NORMAN,OK,US,35.22,-97.47,362
\.


--load nwx.day2pfw table
copy nwx.day2pfw (productid,stnid,stnname,state,country,latitude,longitude,elevation) FROM stdin with delimiter as ',';
FNUS32,KWNS,NORMAN,OK,US,35.22,-97.47,362
\.


--load nwx.day2pts table
copy nwx.day2pts (productid,stnid,stnname,state,country,latitude,longitude,elevation) FROM stdin with delimiter as ',';
WUUS02,KWNS,NORMAN,OK,US,35.22,-97.47,362
\.


--load nwx.day3 table
copy nwx.day3 (productid,stnid,stnname,state,country,latitude,longitude,elevation) FROM stdin with delimiter as ',';
ACUS03,KWNS,KANSAS_CITY_MUNI__&,MO,US,39.12,-94.60,231
\.


--load nwx.day38pf table
copy nwx.day38pf (productid,stnid,stnname,state,country,latitude,longitude,elevation) FROM stdin with delimiter as ',';
FNUS38,KWNS,NORMAN,OK,US,35.22,-97.47,362
\.


--load nwx.day3pts table
copy nwx.day3pts (productid,stnid,stnname,state,country,latitude,longitude,elevation) FROM stdin with delimiter as ',';
WUUS03,KWNS,NORMAN,OK,US,35.22,-97.47,362
\.


--load nwx.day48 table
copy nwx.day48 (productid,stnid,stnname,state,country,latitude,longitude,elevation) FROM stdin with delimiter as ',';
ACUS48,KWNS,KANSAS_CITY_MUNI__&,MO,US,39.12,-94.60,231
\.


--load nwx.day4pts table
copy nwx.day4pts (productid,stnid,stnname,state,country,latitude,longitude,elevation) FROM stdin with delimiter as ',';
WUUS48,KWNS,NORMAN,OK,US,35.22,-97.47,362
\.


--load nwx.fwddy1 table
copy nwx.fwddy1 (productid,stnid,stnname,state,country,latitude,longitude,elevation) FROM stdin with delimiter as ',';
FNUS21,KWNS,NORMAN,OK,US,35.22,-97.47,362
\.

--load nwx.fwddy2 table
copy nwx.fwddy2 (productid,stnid,stnname,state,country,latitude,longitude,elevation) FROM stdin with delimiter as ',';
FNUS22,KWNS,NORMAN,OK,US,35.22,-97.47,362
\.

--load nwx.fwddy38 table
copy nwx.fwddy38 (productid,stnid,stnname,state,country,latitude,longitude,elevation) FROM stdin with delimiter as ',';
FNUS28,KWNS,NORMAN,OK,US,35.22,-97.47,362
\.

--load nwx.meso table
copy nwx.meso (productid,stnid,stnname,state,country,latitude,longitude,elevation) FROM stdin with delimiter as ',';
ACUS11,KWNS,NORMAN,OK,US,35.22,-97.47,362
\.


--load nwx.pubout table
copy nwx.pubout (productid,stnid,stnname,state,country,latitude,longitude,elevation) FROM stdin with delimiter as ',';
WOUS40,KWNS,NORMAN,OK,US,35.22,-97.47,362
\.


--load nwx.spcadm table
copy nwx.spcadm (productid,stnid,stnname,state,country,latitude,longitude,elevation) FROM stdin with delimiter as ',';
NOUS74,KWNS,NORMAN,OK,US,35.22,-97.47,362
\.


--load nwx.stadts table
copy nwx.stadts (productid,stnid,stnname,state,country,latitude,longitude,elevation) FROM stdin with delimiter as ',';
NWUS20,KWNS,NORMAN,OK,US,35.22,-97.47,362
\.


--load nwx.stahry table
copy nwx.stahry (productid,stnid,stnname,state,country,latitude,longitude,elevation) FROM stdin with delimiter as ',';
NWUS22,KWNS,NORMAN,OK,US,35.22,-97.47,362
\.


--load nwx.status table
copy nwx.status (productid,stnid,stnname,state,country,latitude,longitude,elevation) FROM stdin with delimiter as ',';
WOUS20,KWNS,NORMAN,OK,US,35.22,-97.47,362
\.


--load nwx.svrwx table
copy nwx.svrwx (productid,stnid,stnname,state,country,latitude,longitude,elevation) FROM stdin with delimiter as ',';
NWUS20,KWNS,NORMAN,OK,US,35.22,-97.47,362
\.


--load nwx.tornftl table
copy nwx.tornftl (productid,stnid,stnname,state,country,latitude,longitude,elevation) FROM stdin with delimiter as ',';
NWUS23,KWNS,KANSAS_CITY_MUNI__&,MO,US,39.12,-94.60,231
\.


--load nwx.tornmon table
copy nwx.tornmon (productid,stnid,stnname,state,country,latitude,longitude,elevation) FROM stdin with delimiter as ',';
NWUS21,KWNS,KANSAS_CITY_MUNI__&,MO,US,39.12,-94.60,231
\.


--load nwx.watbox table
copy nwx.watbox (productid,stnid,stnname,state,country,latitude,longitude,elevation) FROM stdin with delimiter as ',';
WWUS20,KWNS,NORMAN,OK,US,35.22,-97.47,362
\.


--load nwx.watcnty table
copy nwx.watcnty (productid,stnid,stnname,state,country,latitude,longitude,elevation) FROM stdin with delimiter as ',';
WWUS50,KWNS,NORMAN,OK,US,35.22,-97.47,362
\.


--load nwx.watndsc table
copy nwx.watndsc (productid,stnid,stnname,state,country,latitude,longitude,elevation) FROM stdin with delimiter as ',';
WWUS30,KWNS,NORMAN,OK,US,35.22,-97.47,362
\.


--load nwx.watsum table
copy nwx.watsum (productid,stnid,stnname,state,country,latitude,longitude,elevation) FROM stdin with delimiter as ',';
WWUS60,KWNS,NORMAN,OK,US,35.22,-97.47,362
\.


--load nwx.wou table
copy nwx.wou (productid,stnid,stnname,state,country,latitude,longitude,elevation) FROM stdin with delimiter as ',';
WOUS64,KWNS,NORMAN,OK,US,35.22,-97.47,362
\.


--load nwx.wwp table
copy nwx.wwp (productid,stnid,stnname,state,country,latitude,longitude,elevation) FROM stdin with delimiter as ',';
WWUS40,KWNS,NORMAN,OK,US,35.22,-97.47,362
\.

