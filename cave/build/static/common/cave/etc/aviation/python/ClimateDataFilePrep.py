##
# This software was developed and / or modified by Raytheon Company,
# pursuant to Contract DG133W-05-CQ-1067 with the US Government.
# 
# U.S. EXPORT CONTROLLED TECHNICAL DATA
# This software product contains export-restricted data whose
# export/transfer/disclosure is restricted by U.S. law. Dissemination
# to non-U.S. persons whether in the United States or abroad requires
# an export license or other authorization.
# 
# Contractor Name:        Raytheon Company
# Contractor Address:     6825 Pine Street, Suite 340
#                         Mail Stop B8
#                         Omaha, NE 68106
#                         402.291.0100
# 
# See the AWIPS II Master Rights File ("Master Rights File.pdf") for
# further licensing information.
##
#
#    Name:
#       %PM%
#       %PID%
#
#    Status:
#       %PS%
#    
#    History:
#       %PL%
#
#    Change Document History:
#       %PIRC%
#

import ClimateFileAttribs as attribs
import re, time, os, stat
import tables

# Compression scheme
Complib = 'zlib'

# These must match values in NetCDF file
p_dim = 4   # precipitation data dimension
o_dim = 2   # past weather data dimension
c_dim = 6   # cloud layers
d_dim = 7   # present weather data dimension
s_dim = 3   # supplementary wind data dimension

# NetCDF -> HDF5 variable names translation table
Transl = {'altimeter': 'alti_meter', \
        'isobar_code': 'iso_bar_code', \
        'isobar_dim': 'iso_bar_dim' \
        }

# Station data record
class Info(tables.IsDescription):
    year = tables.Int16Col()
    station_id = tables.Int32Col()
    wban_id = tables.Int32Col()
    call_id = tables.StringCol(6)
    lat = tables.Float32Col()
    lon = tables.Float32Col()
    elevation = tables.Int16Col()

# Climate data record
class Obs(tables.IsDescription):
    #date_time = tables.Int32Col(indexed=True, pos=1)
    #year = tables.Int16Col(indexed=True, pos=2)
    #yday = tables.Int16Col(indexed=True, pos=3)
    #hour = tables.Int8Col(indexed=True, pos=4)
    date_time = tables.Int32Col(pos=1)
    year = tables.Int16Col(pos=2)
    yday = tables.Int16Col(pos=3)
    hour = tables.Int8Col(pos=4)
    source = tables.StringCol(1, pos=5)
    type = tables.StringCol(6, pos=6)
    wind_dir = tables.Int16Col(pos=7)
    wdir_type = tables.StringCol(1, pos=8)
    wind_spd = tables.Float32Col(pos=9)
    cig = tables.UInt16Col(pos=10)
    cig_tool = tables.StringCol(1, pos=11)
    cavok = tables.StringCol(1, pos=12)
    vis = tables.Int32Col(pos=13)
    vis_var_code = tables.StringCol(1, pos=14)
    temp = tables.Float32Col(pos=15)
    dewt = tables.Float32Col(pos=16)
    pres = tables.Float32Col(pos=17)
    prec_period = tables.UInt8Col(shape=p_dim, pos=18)
    prec_depth = tables.Float32Col(shape=p_dim, pos=19)
    prec_code = tables.UInt8Col(shape=p_dim, pos=20)
    dur_code = tables.UInt8Col(pos=21)
    char_code = tables.StringCol(1, pos=22)
    disc_code = tables.UInt8Col(pos=23)
    water_dep = tables.Int16Col(pos=24)
    snow_dim = tables.Int16Col(pos=25)
    snow_code = tables.UInt8Col(pos=26)
    liq_dim = tables.Float32Col(pos=27)
    liq_code = tables.UInt8Col(pos=28)
    snow_period = tables.UInt8Col(shape=p_dim, pos=29)
    snow_accu = tables.Int16Col(shape=p_dim, pos=30)
    accu_cond = tables.UInt8Col(shape=p_dim, pos=31)
    pres_wx_code = tables.UInt8Col(pos=32)
    pt_mwx_code = tables.UInt8Col(shape=o_dim, pos=33)
    pt_mwx_pd = tables.UInt8Col(shape=o_dim, pos=34)
    pt_awx_code = tables.UInt8Col(shape=o_dim, pos=35)
    pt_awx_pd = tables.UInt8Col(shape=o_dim, pos=36)
    rw_vis_dir = tables.Int16Col(pos=37)
    rw_code = tables.StringCol(1, pos=38)
    rw_vis_dim = tables.Int16Col(pos=39)
    cov_code = tables.UInt8Col(shape=c_dim, pos=40)
    bs_hi_dim = tables.Int32Col(shape=c_dim, pos=41)
    cloud_code = tables.UInt8Col(shape=c_dim, pos=42)
    cov_sum_st_code = tables.UInt8Col(shape=c_dim, pos=43)
    cov_sum_code = tables.UInt8Col(shape=c_dim, pos=44)
    cov_sum_st_dim = tables.Int32Col(shape=c_dim, pos=45)
    cov_sum_char_code = tables.UInt8Col(shape=c_dim, pos=46)
    total_cov_code = tables.UInt8Col(pos=47)
    total_opa_code = tables.UInt8Col(pos=48)
    low_cov_code = tables.UInt8Col(pos=49)
    low_cld_gen_code = tables.UInt8Col(pos=50)
    low_cld_dim = tables.Int32Col(pos=51)
    mid_cld_gen_code = tables.UInt8Col(pos=52)
    hi_cld_gen_code = tables.UInt8Col(pos=53)
    st_cov_code = tables.UInt8Col(shape=c_dim, pos=54)
    st_cld_tp_hi = tables.Int32Col(shape=c_dim, pos=55)
    st_cld_type = tables.UInt8Col(shape=c_dim, pos=56)
    st_cld_tp_code = tables.UInt8Col(shape=c_dim, pos=57)
    sun_dur = tables.Int16Col(pos=58)
    hail_size = tables.Float32Col(pos=59)
    g2s_code = tables.UInt8Col(pos=60)
    mint_period = tables.Float32Col(pos=61)
    mint = tables.Float32Col(pos=62)
    x_tp_period = tables.Float32Col(shape=o_dim, pos=63)
    x_tp_code = tables.UInt8Col(shape=o_dim, pos=64)
    x_tp = tables.Float32Col(shape=o_dim, pos=65)
    altimeter = tables.Float32Col(pos=66)
    st_pres = tables.Float32Col(pos=67)
    pres_tr = tables.UInt8Col(pos=68)
    pres_chg_3h = tables.Float32Col(pos=69)
    pres_chg_24h = tables.Float32Col(pos=70)
    isobar_code = tables.UInt8Col(pos=71)
    isobar_dim = tables.Int16Col(pos=72)
    wx_vic_code = tables.UInt8Col(shape=d_dim, pos=73)
    pres_wxm_code = tables.UInt8Col(shape=d_dim, pos=74)
    sup_wd_code = tables.UInt8Col(shape=s_dim, pos=75)
    sup_wd_prd = tables.UInt8Col(shape=s_dim, pos=76)
    sup_wd_spd = tables.Float32Col(shape=s_dim, pos=77)
    wd_gust = tables.Float32Col(pos=78)

class PrepFiles:

    def __init__(self,stn,hdf_file):
	self.hdf_file = hdf_file
	self.__setup_info()
	self.__setup_obs()
	return

    def __setup_info(self):
	if os.path.exists(self.hdf_file): os.chmod(self.hdf_file,stat.S_IRUSR|stat.S_IWUSR|stat.S_IWGRP|stat.S_IRGRP)
	h5fh = tables.openFile(self.hdf_file,'w') 
	info = h5fh.createTable(h5fh.root,'info',Info,'Site Info')
	attribs.setInfoAttrs(info)
	h5fh.close()

    def __setup_obs(self):
	h5fh = tables.openFile(self.hdf_file,'a')
	filters = tables.Filters(complevel=1, complib=Complib, shuffle=1)
	obs = h5fh.createTable(h5fh.root,'obs',Obs,'Surface Observation',filters=filters)
	attribs.setObsAttrs(obs)
	h5fh.close()
