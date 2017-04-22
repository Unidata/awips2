copy ncgrib.vcrdgrib1 (id, name, units, gnam, scale) FROM stdin with delimiter as ',' ;
001,Surface of earth (inc sea sfc)     ,null            ,NONE,0
002,Cloud base level                   ,null            ,CLDL,0
004,Level of 0 deg C isotherm (low)    ,null            ,FRZL,0
006,Maximum wind speed level           ,null            ,MWSL,0
007,Tropopause level                   ,null            ,TROP,0
008,Nominal top of atmosphere          ,KPa             ,TOPA,0
100,Isobaric surface                   ,hPa             ,PRES,0
101,Layer between two isobaric sfcs    ,kPa             ,PRES,1
102,Mean sea-level                     ,null            ,NONE,0
103,Specified altitude                 ,m               ,HGHT,0
104,Layer between two spec alts        ,hm              ,HGHT,2
105,Spec height level (above grnd)     ,m               ,HGHT,0
106,Layer between 2 hght lvls (agl)    ,hm              ,HGHT,2
107,Sigma level                        ,1/10000         ,SGMA,0
108,Layer between two sigma levels     ,1/100           ,SGMA,2
111,Depth below land surface           ,cm              ,DPTH,0
112,Layer btwn 2 dpths below lnd sfc   ,cm              ,DPTH,0
113,Isentropic surface                 ,K               ,THTA,0
116,Pressure difference layer          ,hPa             ,PDLY,0
117,Potential vorticity                ,10**-6/km**2/kg ,POTV,0
121,Layer btwn 2 isobaric sfc high    ,1100-hPa        ,PRES,0
125,Height level above ground (hi prec), cm             ,HGHT,0
128,Layer between 2 sigma lvls high   ,1.1-1/1000      ,SGMA,1
141,Layer btwn 2 isobaric sfc mixed   ,kPa & 1100-hPa  ,PRES,0
160,Depth below sea-level              ,m               ,DPTH,0
200,Entire atmosphere as a layer       ,null            ,NONE,0
201,Entire ocean as a layer            ,null            ,NONE,0
204,Level of 0 deg C isotherm (high)   ,null            ,FRZH,0
215,Low level cloud ceiling            ,null            ,CEIL,0
248,Shallow convective cloud bottom level,null          ,SCBL,0
249,Shallow convective cloud top level ,  null          ,SCTL,0
251,Deep convective cloud bottom level ,  null          ,DCBL,0
252,Deep convective cloud top level    ,  null          ,DCTL,0
\.
