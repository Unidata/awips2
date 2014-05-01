
--load nwx.drought table
copy nwx.drought (productid,stnid,stnname,state,country,latitude,longitude,elevation) FROM stdin with delimiter as ',';
FXUS25,KWNC,NCEP,MD,US,38.82,-76.87,86
\.


--load nwx.f610 table
copy nwx.f610 (productid,stnid,stnname,state,country,latitude,longitude,elevation) FROM stdin with delimiter as ',';
FEUS40,KWBC,NMC,MD,US,38.82,-76.87,86
\.


--load nwx.hawaii table
copy nwx.hawaii (productid,stnid,stnname,state,country,latitude,longitude,elevation) FROM stdin with delimiter as ',';
FXHW40,KWBC,NMC,MD,US,38.82,-76.87,86
\.


--load nwx.n30 table
copy nwx.n30 (productid,stnid,stnname,state,country,latitude,longitude,elevation) FROM stdin with delimiter as ',';
FXUS07,KWBC,NMC,MD,US,38.82,-76.87,86
\.


--load nwx.n90 table
copy nwx.n90 (productid,stnid,stnname,state,country,latitude,longitude,elevation) FROM stdin with delimiter as ',';
FXUS05,KWBC,NMC,MD,US,38.82,-76.87,86
\.


--load nwx.n610 table
copy nwx.n610 (productid,stnid,stnname,state,country,latitude,longitude,elevation) FROM stdin with delimiter as ',';
FXUS06,KWBC,NMC,MD,US,38.82,-76.87,86
\.


--load nwx.pmdthr table
copy nwx.pmdthr (productid,stnid,stnname,state,country,latitude,longitude,elevation) FROM stdin with delimiter as ',';
FXUS21,KWNC,NCEP_CPC,MD,US,38.81,-76.87,0
\.


--load nwx.uvi table
copy nwx.uvi (productid,stnid,stnname,state,country,latitude,longitude,elevation) FROM stdin with delimiter as ',';
AEUS41,KWBC,NCEP_NMC,MD,US,38.81,-76.87,0
\.

