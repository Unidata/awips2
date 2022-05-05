
--load nwx.heat table
copy nwx.heat (productid, stnid, stnname, state, country, latitude, longitude, elevation) FROM stdin with delimiter as ',';
FMUS23,KWNH,NCEP_HPC,MD,US,39.00,-85.00,86
FMUS24,KWNH,NCEP_HPC,MD,US,39.00,-110.00,86
\.

