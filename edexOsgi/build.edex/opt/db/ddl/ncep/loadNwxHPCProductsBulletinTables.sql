--load nwx.intl table
copy nwx.intl (productid,stnid,stnname,state,country,latitude,longitude,elevation) FROM stdin with delimiter as ',';
NPXX10,KWNO,NMC,MD,US,38.82,-76.87,86
\.

--load nwx.pmdak table
copy nwx.pmdak (productid,stnid,stnname,state,country,latitude,longitude,elevation) FROM stdin with delimiter as ',';
FXAK02,KWNH,NCEP_HPC,MD,US,38.81,-76.87,0
\.

--load nwx.pmdca table
copy nwx.pmdca (productid,stnid,stnname,state,country,latitude,longitude,elevation) FROM stdin with delimiter as ',';
FXCA20,KWNH,NCEP_HPC,MD,US,38.81,-76.87,0
FXCA20,KWBC,NCEP_NMC,MD,US,39.51,-77.57,0
\.

--load nwx.pmdepd table
copy nwx.pmdepd (productid,stnid,stnname,state,country,latitude,longitude,elevation) FROM stdin with delimiter as ',';
FXUS02,KWBC,NCEP_NMC,MD,US,39.51,-77.57,0
\.

--load nwx.pmdhi table
copy nwx.pmdhi (productid,stnid,stnname,state,country,latitude,longitude,elevation) FROM stdin with delimiter as ',';
FXHW01,KWNH,NCEP_HPC,MD,US,38.81,-76.87,0
\.

--load nwx.pmdhmd table
copy nwx.pmdhmd (productid,stnid,stnname,state,country,latitude,longitude,elevation) FROM stdin with delimiter as ',';
FXUS10,KWNH,NCEP_HPC,MD,US,38.81,-76.87,0
\.

--load nwx.pmdsa table
copy nwx.pmdsa (productid,stnid,stnname,state,country,latitude,longitude,elevation) FROM stdin with delimiter as ',';
FXSA20,KWNH,NCEP_HPC,MD,US,38.81,-76.87,0
FXSA20,KWBC,NCEP_NMC,MD,US,39.51,-77.57,0
\.

--load nwx.pmdspd table
copy nwx.pmdspd (productid,stnid,stnname,state,country,latitude,longitude,elevation) FROM stdin with delimiter as ',';
FXUS01,KWBC,NCEP_NMC,MD,US,39.51,-77.57,0
\.

--load nwx.preepd table
copy nwx.preepd (productid,stnid,stnname,state,country,latitude,longitude,elevation) FROM stdin with delimiter as ',';
FXUS02,KWNH,NCEP_HPC,MD,US,38.81,-76.87,0
\.


--load nwx.qpferd table
copy nwx.qpferd (productid,stnid,stnname,state,country,latitude,longitude,elevation) FROM stdin with delimiter as ',';
FOUS30,KWBC,NCEP_NMC,MD,US,38.81,-76.87,0
\.

--load nwx.qpfhsd table
copy nwx.qpfhsd (productid,stnid,stnname,state,country,latitude,longitude,elevation) FROM stdin with delimiter as ',';
FOUS11,KWBC,NCEP_NMC,MD,US,38.81,-76.87,0
\.

--load nwx.qpfpfd table
copy nwx.qpfpfd (productid,stnid,stnname,state,country,latitude,longitude,elevation) FROM stdin with delimiter as ',';
FXUS04,KWBC,NCEP_NMC,MD,US,38.81,-76.87,0
\.

--load nwx.scs table
copy nwx.scs (productid,stnid,stnname,state,country,latitude,longitude,elevation) FROM stdin with delimiter as ',';
FPUS20,KWNH,NCEP_HPC,MD,US,38.81,-77.00,0
FPUS21,KWNH,NCEP_HPC,MD,US,39.30,-76.50,0
FPUS22,KWNH,NCEP_HPC,MD,US,39.81,-76.00,0
FPUS23,KWNH,NCEP_HPC,MD,US,40.30,-75.50,0
FPUS45,KGGW,GLASGOW,MT,US,48.20,-106.62,0
FPUS46,KMFR,MEDFORD,OR,US,42.36,-122.87,0
FPUS46,KMTR,SAN_FRANCISCO,CA,US,36.59,-121.84,0
FPUS20,KWBN,HONULULU,HI,US,19.08,-155.57,0
\.

--load nwx.sdm table
copy nwx.sdm (productid,stnid,stnname,state,country,latitude,longitude,elevation) FROM stdin with delimiter as ',';
NOUS42,KWNO,NMC,MD,US,38.82,-76.87,86
\.


--load nwx.srp table
copy nwx.srp (productid,stnid,stnname,state,country,latitude,longitude,elevation) FROM stdin with delimiter as ',';
FSUS02,KWBC,NMC,MD,US,38.82,-76.87,86
\.


--load nwx.sus table
copy nwx.sus (productid,stnid,stnname,state,country,latitude,longitude,elevation) FROM stdin with delimiter as ',';
ASUS01,KWBC,NMC,MD,US,38.82,-76.87,86
\.




