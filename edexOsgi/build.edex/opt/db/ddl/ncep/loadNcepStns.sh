#!/bin/bash
# -----------------------------------------------------------------
# ! script to load stations tables into the NCEP database
# !
# ! $1 = install directory
# ! $2 = DB port number
# ! $3 = username
# ! $4 = script directory
# ! $5 = log file path
# !
# -----------------------------------------------------------------
${1}/bin/psql -d ncep -U ${3} -q -p ${2} -f ${4}/loadAirepWaypnts.sql  >> ${5} 2>&1
${1}/bin/psql -d ncep -U ${3} -q -p ${2} -f ${4}/loadBuoys.sql  >> ${5} 2>&1
${1}/bin/psql -d ncep -U ${3} -q -p ${2} -f ${4}/loadClimReg.sql  >> ${5} 2>&1
${1}/bin/psql -d ncep -U ${3} -q -p ${2} -f ${4}/loadCities.sql  >> ${5} 2>&1
${1}/bin/psql -d ncep -U ${3} -q -p ${2} -f ${4}/loadCntyclst.sql  >> ${5} 2>&1
${1}/bin/psql -d ncep -U ${3} -q -p ${2} -f ${4}/loadCoastal.sql  >> ${5} 2>&1
${1}/bin/psql -d ncep -U ${3} -q -p ${2} -f ${4}/loadCoordPts.sql  >> ${5} 2>&1
${1}/bin/psql -d ncep -U ${3} -q -p ${2} -f ${4}/loadNcStnsCountyclust.sql  >> ${5} 2>&1
${1}/bin/psql -d ncep -U ${3} -q -p ${2} -f ${4}/loadCountynam.sql  >> ${5} 2>&1
${1}/bin/psql -d ncep -U ${3} -q -p ${2} -f ${4}/loadCounty.sql  >> ${5} 2>&1
${1}/bin/psql -d ncep -U ${3} -q -p ${2} -f ${4}/loadCpcstns.sql  >> ${5} 2>&1
${1}/bin/psql -d ncep -U ${3} -q -p ${2} -f ${4}/loadDlwx.sql  >> ${5} 2>&1
${1}/bin/psql -d ncep -U ${3} -q -p ${2} -f ${4}/loadFfgzon.sql  >> ${5} 2>&1
${1}/bin/psql -d ncep -U ${3} -q -p ${2} -f ${4}/loadFirezones.sql  >> ${5} 2>&1
${1}/bin/psql -d ncep -U ${3} -q -p ${2} -f ${4}/loadGeog.sql  >> ${5} 2>&1
${1}/bin/psql -d ncep -U ${3} -q -p ${2} -f ${4}/loadGfsmos.sql  >> ${5} 2>&1
${1}/bin/psql -d ncep -U ${3} -q -p ${2} -f ${4}/loadIdft.sql  >> ${5} 2>&1
${1}/bin/psql -d ncep -U ${3} -q -p ${2} -f ${4}/loadInactive.sql  >> ${5} 2>&1
${1}/bin/psql -d ncep -U ${3} -q -p ${2} -f ${4}/loadIntlsig.sql  >> ${5} 2>&1
${1}/bin/psql -d ncep -U ${3} -q -p ${2} -f ${4}/loadIsland.sql  >> ${5} 2>&1
${1}/bin/psql -d ncep -U ${3} -q -p ${2} -f ${4}/loadLsfstns.sql  >> ${5} 2>&1
${1}/bin/psql -d ncep -U ${3} -q -p ${2} -f ${4}/loadMardel.sql  >> ${5} 2>&1
${1}/bin/psql -d ncep -U ${3} -q -p ${2} -f ${4}/loadMarinenames.sql  >> ${5} 2>&1
${1}/bin/psql -d ncep -U ${3} -q -p ${2} -f ${4}/loadMarine.sql  >> ${5} 2>&1
${1}/bin/psql -d ncep -U ${3} -q -p ${2} -f ${4}/loadMsfstns.sql  >> ${5} 2>&1
${1}/bin/psql -d ncep -U ${3} -q -p ${2} -f ${4}/loadMzcntys.sql  >> ${5} 2>&1
${1}/bin/psql -d ncep -U ${3} -q -p ${2} -f ${4}/loadNcSat.sql  >> ${5} 2>&1
${1}/bin/psql -d ncep -U ${3} -q -p ${2} -f ${4}/loadNexrad.sql  >> ${5} 2>&1
${1}/bin/psql -d ncep -U ${3} -q -p ${2} -f ${4}/loadNgmmos.sql  >> ${5} 2>&1
${1}/bin/psql -d ncep -U ${3} -q -p ${2} -f ${4}/loadPermclust.sql  >> ${5} 2>&1
${1}/bin/psql -d ncep -U ${3} -q -p ${2} -f ${4}/loadPirepNavaids.sql  >> ${5} 2>&1
${1}/bin/psql -d ncep -U ${3} -q -p ${2} -f ${4}/loadRiverbas.sql  >> ${5} 2>&1
${1}/bin/psql -d ncep -U ${3} -q -p ${2} -f ${4}/loadScdstn.sql  >> ${5} 2>&1
${1}/bin/psql -d ncep -U ${3} -q -p ${2} -f ${4}/loadSfstns.sql  >> ${5} 2>&1
${1}/bin/psql -d ncep -U ${3} -q -p ${2} -f ${4}/loadShef_COOP1.sql  >> ${5} 2>&1
${1}/bin/psql -d ncep -U ${3} -q -p ${2} -f ${4}/loadShef_COOP2.sql  >> ${5} 2>&1
${1}/bin/psql -d ncep -U ${3} -q -p ${2} -f ${4}/loadShef_COOP3.sql  >> ${5} 2>&1
${1}/bin/psql -d ncep -U ${3} -q -p ${2} -f ${4}/loadShef_COOP4.sql  >> ${5} 2>&1
${1}/bin/psql -d ncep -U ${3} -q -p ${2} -f ${4}/loadShef_COOP.sql  >> ${5} 2>&1
${1}/bin/psql -d ncep -U ${3} -q -p ${2} -f ${4}/loadShef_master.sql  >> ${5} 2>&1
${1}/bin/psql -d ncep -U ${3} -q -p ${2} -f ${4}/loadShpexception.sql  >> ${5} 2>&1
${1}/bin/psql -d ncep -U ${3} -q -p ${2} -f ${4}/loadSnap.sql  >> ${5} 2>&1
${1}/bin/psql -d ncep -U ${3} -q -p ${2} -f ${4}/loadSnap8.sql  >> ${5} 2>&1
${1}/bin/psql -d ncep -U ${3} -q -p ${2} -f ${4}/loadSnstns.sql  >> ${5} 2>&1
${1}/bin/psql -d ncep -U ${3} -q -p ${2} -f ${4}/loadSnworld.sql  >> ${5} 2>&1
${1}/bin/psql -d ncep -U ${3} -q -p ${2} -f ${4}/loadSpcwatch.sql  >> ${5} 2>&1
${1}/bin/psql -d ncep -U ${3} -q -p ${2} -f ${4}/loadState.sql  >> ${5} 2>&1
${1}/bin/psql -d ncep -U ${3} -q -p ${2} -f ${4}/loadStns_II90.sql  >> ${5} 2>&1
${1}/bin/psql -d ncep -U ${3} -q -p ${2} -f ${4}/loadSystns.sql  >> ${5} 2>&1
${1}/bin/psql -d ncep -U ${3} -q -p ${2} -f ${4}/loadSyworld.sql  >> ${5} 2>&1
${1}/bin/psql -d ncep -U ${3} -q -p ${2} -f ${4}/loadTafstn.sql  >> ${5} 2>&1
${1}/bin/psql -d ncep -U ${3} -q -p ${2} -f ${4}/loadTcabkpt_island.sql  >> ${5} 2>&1
${1}/bin/psql -d ncep -U ${3} -q -p ${2} -f ${4}/loadTcabkpt_land.sql  >> ${5} 2>&1
${1}/bin/psql -d ncep -U ${3} -q -p ${2} -f ${4}/loadTcabkptlz.sql  >> ${5} 2>&1
${1}/bin/psql -d ncep -U ${3} -q -p ${2} -f ${4}/loadTcabkpt_ovl.sql  >> ${5} 2>&1
${1}/bin/psql -d ncep -U ${3} -q -p ${2} -f ${4}/loadTcabkpt.sql  >> ${5} 2>&1
${1}/bin/psql -d ncep -U ${3} -q -p ${2} -f ${4}/loadTcabkpt_water.sql  >> ${5} 2>&1
${1}/bin/psql -d ncep -U ${3} -q -p ${2} -f ${4}/loadTpc_countries.sql  >> ${5} 2>&1
${1}/bin/psql -d ncep -U ${3} -q -p ${2} -f ${4}/loadTpc_states.sql  >> ${5} 2>&1
${1}/bin/psql -d ncep -U ${3} -q -p ${2} -f ${4}/loadVolcano_small.sql  >> ${5} 2>&1
${1}/bin/psql -d ncep -U ${3} -q -p ${2} -f ${4}/loadVolcano.sql  >> ${5} 2>&1
${1}/bin/psql -d ncep -U ${3} -q -p ${2} -f ${4}/loadVors.sql  >> ${5} 2>&1
${1}/bin/psql -d ncep -U ${3} -q -p ${2} -f ${4}/loadWfo.sql  >> ${5} 2>&1
${1}/bin/psql -d ncep -U ${3} -q -p ${2} -f ${4}/loadWrqpf.sql  >> ${5} 2>&1
${1}/bin/psql -d ncep -U ${3} -q -p ${2} -f ${4}/loadXrainsort.sql  >> ${5} 2>&1
${1}/bin/psql -d ncep -U ${3} -q -p ${2} -f ${4}/loadZones.sql  >> ${5} 2>&1
