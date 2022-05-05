-- load nwx.sdmam table
copy nwx.sdmam (productid,stnid,stnname,state,country,latitude,longitude,elevation) FROM stdin with delimiter as ',';
NOUS42,KWNO,NMC,MD,US,38.82,-76.87,86
\.

-- load nwx.sdmim table
copy nwx.sdmim (productid,stnid,stnname,state,country,latitude,longitude,elevation) FROM stdin with delimiter as ',';
NPXX10,KWNO,NMC,MD,US,38.82,-76.87,86
\.

-- load nwx.sdmdhm table
copy nwx.sdmdhm (productid,stnid,stnname,state,country,latitude,longitude,elevation) FROM stdin with delimiter as ',';
NOUS71,KWNO,NMC,MD,US,38.82,-76.87,86
\.

-- load nwx.nwstgam table
copy nwx.nwstgam (productid,stnid,stnname,state,country,latitude,longitude,elevation) FROM stdin with delimiter as ',';
NOXX01,KWBC,NMC,MD,US,38.82,-76.87,86
\.

-- load nwx.ncfam table
copy nwx.ncfam (productid,stnid,stnname,state,country,latitude,longitude,elevation) FROM stdin with delimiter as ',';
NOUS72,KNCF,NMC,MD,US,38.82,-76.87,86
\.

-- load nwx.nesdispm table
copy nwx.nesdispm (productid,stnid,stnname,state,country,latitude,longitude,elevation) FROM stdin with delimiter as ',';
NOUS71,KNES,NMC,MD,US,38.82,-76.87,86
\.

-- load nwx.nesdisam table
copy nwx.nesdisam (productid,stnid,stnname,state,country,latitude,longitude,elevation) FROM stdin with delimiter as ',';
NOUS72,KNES,NMC,MD,US,38.82,-76.87,86
\.

-- load nwx.cmcam table
copy nwx.cmcam (productid,stnid,stnname,state,country,latitude,longitude,elevation) FROM stdin with delimiter as ',';
NOCN05,CWAO,MONTREAL_VAAC,CN,CN,45.47,-73.75,-9999
AACN01,CWAO,MONTREAL_VAAC,CN,CN,45.47,-73.75,-9999
\.