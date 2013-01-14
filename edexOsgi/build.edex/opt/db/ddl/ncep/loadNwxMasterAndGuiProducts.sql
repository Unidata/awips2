--load nwx.radartext88d table
copy nwx.radartext88d (productname,producttablename,producttype) FROM stdin with delimiter as ',' ;
88D Free Text Message,nwx.stations,ftm
\.


--load nwx.aviationforecasts table
copy nwx.aviationforecasts (productname,producttablename,producttype) FROM stdin with delimiter as ',' ;
Area Forecasts,nwx.faa,area
TAFs,nwx.taf,fts
Convective SIGMETS,nwx.consig,conv
International SIGMETS,nwx.intsig,intl
SIGMETS,nwx.sigmet,sgmt
AIRMETS,nwx.airmet,airm
Offshore Area Forecasts,nwx.osarea,offsh
CWA,nwx.cwsu,cwa
Met Impact Statements,nwx.mis_avn,mis
\.


--load nwx.canadianproducts table
copy nwx.canadianproducts (productname,producttablename,producttype) FROM stdin with delimiter as ',' ;
Canadian TOR WARNING,nwx.stations,tornado
Canadian SVR WARNING,nwx.stations,severe
Canadian SPECIAL WX STMT,nwx.stations,sps
Canadian GEN WARN/SVR T WATCH,nwx.stations,warwatch
Canadian Central OUTLOOK,nwx.stations,coutlook
Canadian WEATHER WARN SUMMARY,nwx.stations,warnsumm
\.


--load nwx.cpcproducts table
copy nwx.cpcproducts (productname,producttablename,producttype) FROM stdin with delimiter as ',' ;
6-10 Day Discussion,nwx.n610,n610
6-10 Day Forecast,nwx.f610,f610
30 Day Forecast,nwx.n30,n30
90 Day Forecast,nwx.n90,n90
Hawaiian 30/90 Day Forecast,nwx.hawaii,hawaii
Threats Assessment Discussion,nwx.pmdthr,PMDTHR
US Drought Monitor Analysis,nwx.drought,drought
UVI,nwx.uvi,UVI
\.


-- load nwx.datatypegrouplist table

copy nwx.datatypegrouplist (datatypegroupname,datatypegrouptablename) FROM stdin with delimiter as ',' ;
Observed Data,nwx.observeddataproducts
88D Radar Text,nwx.radartext88d
Canadian Products,nwx.canadianproducts
Public Products,nwx.publicproducts
Pt Fcst Products,nwx.ptfcstproducts
Watches/Warnings,nwx.watcheswarnings
SPC Products,nwx.spcproducts
NHC Products,nwx.nhcproducts
Tropical Pacific,nwx.tropicalpacific
Recon CARCAH,nwx.reconcarcah
Flash Flood,nwx.flashflood
Marine,nwx.marine
Aviation Forecasts,nwx.aviationforecasts
MOS,nwx.mos
HPC Products,nwx.hpcproducts
HPC Heat Index,nwx.hpcheatindex
CPC Products,nwx.cpcproducts
Volcano Products,nwx.volcanoproducts
Fire Weather Products,nwx.fireweatherproducts
\.

--load nwx.observeddataproducts table
copy nwx.observeddataproducts (productname,producttablename,producttype) FROM stdin with delimiter as ',' ;
Surface Hourlies,nwx.sfstns,hrly
Sounding Data,nwx.snstns,snd
Synoptic Data,nwx.lsfstns,syn
Agriculture Obs,nwx.ago,AGO
TAFs Decoded,nwx.tafstn,taf
RADAT,nwx.fzl,FZL
\.

--load nwx.fireweatherproducts table
copy nwx.fireweatherproducts (productname,producttablename,producttype) FROM stdin with delimiter as ',' ;
Fire Wx Forecasts,nwx.stations,FWF
Red Flag Warn/Fire Wx Watch,nwx.stations,RFW
Fire FWM,nwx.stations,FWM
Rangeland Fire Discussion,nwx.stations,RFD
SPC DAY 1,nwx.fwddy1,fwddy1
SPC DAY 2,nwx.fwddy2,fwddy2
SPC Fire Day 3-8 Otlk,nwx.fwddy38,fwddy38
SPC Fire Day 1 Otlk Points,nwx.day1pfw,pfwfd1
SPC Fire Day 2 Otlk Points,nwx.day1pfw,pfwfd2
SPC Fire Day 3-8 Otlk Points,nwx.day38pf,pfwf38
\.

--load nwx.flashflood table
copy nwx.flashflood (productname,producttablename,producttype) FROM stdin with delimiter as ',' ;
FF Guidance,nwx.ffg,FFG
Satellite Precip Estimates,nwx.satest,satest
Flash Flood Watch,nwx.stations,FFA
Flash Flood Warning,nwx.stations,FFW
Flash Flood Statements,nwx.stations,FFS
Flood Statements,nwx.stations,FLS
Flood Warning,nwx.flw,FLW
National Flood Summary,nwx.fln,FLN
Local River Statement,nwx.rvs,RVS
Rainfall Rate Part1,nwx.stations,RR1
Rainfall Rate Part2,nwx.stations,RR2
Rainfall Rate Part3,nwx.stations,RR3
Rainfall Rate Part4,nwx.stations,RR4
Rainfall Rate Part5,nwx.stations,RR5
Rainfall Rate Part6,nwx.stations,RR6
Rainfall Rate Part7,nwx.stations,RR7
Rainfall Rate Part8,nwx.stations,RR8
Rainfall Rate Part9,nwx.stations,RR9
Rainfall Rate AHOS,nwx.stations,RRA
Rainfall Misc Data,nwx.stations,RRM
\.

--load nwx.hpcheatindex table
copy nwx.hpcheatindex (productname,producttablename,producttype) FROM stdin with delimiter as ',' ;
Mean Heat Index,nwx.heat,hmean
Max Heat Index,nwx.heat,hmax
Min Heat Index,nwx.heat,hmin
\.

--load nwx.hpcproducts table 
copy nwx.hpcproducts (productname,producttablename,producttype) FROM stdin with delimiter as ',' ;
Extended Prelim,nwx.preepd,PREEPD
Extended Final,nwx.pmdepd,PMDEPD
Basic Wx,nwx.pmdspd,PMDSPD
QPF,nwx.qpfpfd,QPFPFD
Excessive Rain,nwx.qpferd,QPFERD
Heavy Snow,nwx.qpfhsd,QPFHSD
Coded Front Fcst,nwx.srp,srp
Coded Front Anl,nwx.sus,sus
Hawaii Discussion,nwx.pmdhi,PMDHI
Alaska Discussion,nwx.pmdak,PMDAK
S Amer Discussion,nwx.pmdsa,PMDSA
Caribbean Discussion,nwx.pmdca,PMDCA
SDM Messages,nwx.sdm,sdm
International Messages,nwx.intl,intl
Storm Summaries,nwx.stations,storm
Model Diag Discussion,nwx.pmdhmd,PMDHMD
Selected Cities,nwx.scs,SCS
\.


--load nwx.marine table
copy nwx.marine (productname,producttablename,producttype) FROM stdin with delimiter as ',' ;
Coastal Forecasts,nwx.cwf,CWF
Offshore Forecasts,nwx.off,OFF
High Seas Forecasts,nwx.hsf,HSF
Marine Interpretation Message,nwx.mim,MIM
Special Marine Warnings,nwx.stations,SMW
Coastal Flood Watch,nwx.cfw,CFW
Marine Weather Statements,nwx.stations,MWS
Coast Guard Data,nwx.cgdata,cgr
Plain Language Ship Reports,nwx.pls,PLS
Other Marine Reports,nwx.omr,OMR
Marine Wind Wave Guidance,nwx.mrp,MRP
\.


--load nwx.mos table
copy nwx.mos (productname,producttablename,producttype) FROM stdin with delimiter as ',' ;
NGM MOS,nwx.ngmmos,ngmmos
ETA MOS,nwx.gfsmos,etamos
GFS MOS,nwx.gfsmos,gfsmos
GFSX MOS,nwx.gfsmos,gfsxmos
NGM GUID,nwx.ngmgd,ngmgd
ETA GUID,nwx.etagd,etagd
MARINE MOS,nwx.marnmos,marnmos
\.


--load nwx.nhcproducts table
copy nwx.nhcproducts (productname,producttablename,producttype) FROM stdin with delimiter as ',' ;
Outlooks,nwx.hurrcn,outlk
Hurricane Discussions,nwx.hurrcn,disc
Public Bulletins,nwx.hurrcn,pblc
Marine Bulletins,nwx.hurrcn,mar
Model Forecasts,nwx.hurrcn,mdl
Recon Flights,nwx.hurrcn,rcn
Tropical Discussions,nwx.hurrcn,tdsc
Strike Probabilities,nwx.hurrcn,strk
\.


-- load nwx.observeddata table
-- copy nwx.observeddata (productname,producttablename,producttype) FROM stdin with delimiter as ',' ;
-- Surface Hourlies,nwx.sfstns,hrly
-- Sounding Data,nwx.snstns,snd
-- Synoptic Data,nwx.lsfstns,syn 
-- Agriculture Obs,nwx.ago,AGO 
-- TAFs Decoded,nwx.tafstn,taf
-- RADAT,nwx.fzl,FZL
-- Raob Mandatory Obs,nwx.man,man
-- \.


--load nwx.ptfcstproducts table
copy nwx.ptfcstproducts (productname,producttablename,producttype) FROM stdin with delimiter as ',' ;
Revised Digital Forecasts,nwx.rdf,RDF
Point Forecast Matrices,nwx.pfm,PFM
Area Forecast Matrices,nwx.afm,AFM
\.


--load nwx.publicproducts table
copy nwx.publicproducts (productname,producttablename,producttype) FROM stdin with delimiter as ',' ;
Area Fcst Discussion,nwx.afd,AFD
State Fcst Discussion,nwx.sfd,area
State Forecasts,nwx.sfp,SFP
Zone Forecasts,nwx.zfp,ZFP
Local Forecasts,nwx.lfp,LFP
Hazardous Weather Outlook,nwx.stations,HWO
Coded Cities,nwx.ccf,CCF
Local Storm Reports,nwx.lsr,LSR
Record Event Statements,nwx.stations,RER
Tabular State Forecasts,nwx.sft,SFT
Regional Weather Roundups,nwx.stations,RWR
Regional Temp/Precip Tables,nwx.rtp,RTP
Daily Climo Reports,nwx.cli,CLI
Monthly Climo Reports,nwx.clm,CLM
NOW casts,nwx.now,NOW
Public Info Statement,nwx.stations,PNS
Misc Local Statement,nwx.mis,MIS
Alarm/Alert Urgent Message,nwx.ada,ADA
\.


--load nwx.reconcarcah table
copy nwx.reconcarcah (productname,producttablename,producttype) FROM stdin with delimiter as ',' ;
POD,nwx.atlhurr,pod
RECCO non-tropical Atlantic,nwx.atlhurr,antreco
RECCO Atlantic,nwx.atlhurr,areco
Vortex Message Atlantic,nwx.atlhurr,avortex
Supp Vortex Message Atlantic,nwx.atlhurr,asupvort
Dropsonde data Atlantic,nwx.atlhurr,adrops
RECCO non-tropical Pacific,nwx.pachurr,pntreco
RECCO Pacific,nwx.pachurr,preco
Vortex Message Pacific,nwx.pachurr,pvortex
Supp Vortex Message Pacific,nwx.pachurr,psupvort
Dropsonde data Pacific,nwx.pachurr,pdrops
Dropsonde data WPAC Pacific,nwx.pachurr,wpdrops
P-3 ASDL Atlantic,nwx.atlhurr,aasdl
P-3 ASDL Pacific,nwx.pachurr,pasdl
\.


--load nwx.spcproducts table
copy nwx.spcproducts (productname,producttablename,producttype) FROM stdin with delimiter as ',' ;
Day 1 Outlook,nwx.day1,day1
Day 2 Outlook,nwx.day2,day2
Day 3 Outlook,nwx.day3,day3
Meso Discussions,nwx.meso,meso
Current Svr Tstm Torn Watches,nwx.watndsc,wtch2
All Svr Tstm Torn Watches,nwx.watbox,watch
Watch without disc,nwx.watndsc,wtch2
Watch County List SEV,nwx.watcnty,sev
Watch Summary SEVMKC,nwx.watsum,sevmkc
Status Reports,nwx.status,stat
Severe Wx Summary,nwx.svrwx,svr
Public Outlook,nwx.pubout,public
Day 1 Outlook Points,nwx.day1pts,ptsdy1
Day 2 Outlook Points,nwx.day2pts,ptsdy2
Day 3 Outlook Points,nwx.day3pts,ptsdy3
Hourly Statistics,nwx.stahry,hry
Daily Statistics,nwx.stadts,dts
Watch Outline Update,nwx.wou,wou
Day 4-8 Outlook,nwx.day48,day48
Day 4-8 Outlook Points,nwx.day4pts,ptsd48
Fire Day 1 Otlk,nwx.fwddy1,fwddy1
Fire Day 2 Otlk,nwx.fwddy2,fwddy2
Fire Day 3-8 Otlk,nwx.fwddy38,fwddy38
Fire Day 1 Otlk Points,nwx.day1pfw,pfwfd1
Fire Day 2 Otlk Points,nwx.day1pfw,pfwfd2
Fire Day 3-8 Otlk Points,nwx.day38pf,pfwf38
SPC Admin Message,nwx.spcadm,adm
Tornado Mnth Stats,nwx.tornmon,monthly
Tornado Annual Fatal,nwx.tornftl,tornfatl
Watch Probabilities,nwx.wwp,wwp
\.


--load nwx.tropicalpacific table
copy nwx.tropicalpacific (productname,producttablename,producttype) FROM stdin with delimiter as ',' ;
Tropical Wx Outlook & Summary,nwx.two,TWO
Tropical Weather Summary,nwx.tws,TWS
Tropical Weather Discussion,nwx.twd,TWD
Mar/Avn Trop Cyclone Advisory,nwx.tcm,TCM
Public Trop Cyclone Advisory,nwx.tcp,TCP
Tropical Cyclone Discussion,nwx.tcd,TCD
Trop Cyclone Pos Estimate,nwx.tce,TCE
Tropical Cyclone Update,nwx.tcu,TCU
Unnum Depr & Susp Area Adv,nwx.dsa,DSA
\.


--load volcanoproducts table
copy nwx.volcanoproducts (productname,producttablename,producttype) FROM stdin with delimiter as ',' ;
Volcano Warnings,nwx.vlcsig,vlcw
Volcanic Ash Advisory,nwx.stations,volc
Volcanic Ash Forecast,nwx.stations,vlcf
\.


--load nwx.watcheswarnings table
copy nwx.watcheswarnings (productname,producttablename,producttype) FROM stdin with delimiter as ',' ;
Severe Thunderstorm Warning,nwx.stations,SVR
Tornado Warning,nwx.stations,TOR
Winter Weather Warning,nwx.stations,WSW
Severe Local Storms,nwx.stations,SLS
Watch County Notif,nwx.stations,WCN
Non Precipitating Warning,nwx.stations,NPW
Severe Weather Statements,nwx.stations,SVS
Special Weather Statements,nwx.stations,SPS
State Weather Summaries,nwx.stations,RWS
Hurricane Local Statements,nwx.stations,HLS
\.


