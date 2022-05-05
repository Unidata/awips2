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

##
# This is a base file that is not intended to be overridden.
##

import os
import stat

import tables

# Missing data in HDF file
MissingFloat = 1.0e+15-1.0
MissingUInt8 = (1<<8)-1
MissingInt16 = (1<<15)-1
MissingUInt16 = (1<<16)-1
MissingInt32 = (1<<30)-1    # 31 is too big
MissingUInt32 = (1<<32)-1

# These must match values in NetCDF file
p_dim = 4   # precipitation data dimension
o_dim = 2   # past weather data dimension
c_dim = 6   # cloud layers
d_dim = 7   # present weather data dimension
s_dim = 3   # supplementary wind data dimension

# NetCDF -> HDF5 variable names translation table
Transl = {'altimeter': 'alti_meter',
          'isobar_code': 'iso_bar_code',
          'isobar_dim': 'iso_bar_dim'
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

class PrepFiles(object):

    def __init__(self, stn, hdf_file):
        self.hdf_file = hdf_file
        self.__setup_info()
        self.__setup_obs()

    def __setup_info(self):
        if os.path.exists(self.hdf_file):
            os.chmod(self.hdf_file,stat.S_IRUSR|stat.S_IWUSR|stat.S_IWGRP|stat.S_IRGRP)
        with tables.open_file(self.hdf_file, 'w') as h5fh:
            info = h5fh.create_table(h5fh.root, 'info', Info, 'Site Info')
            self.__setInfoAttrs(info)

    def __setup_obs(self):
        with tables.open_file(self.hdf_file, 'a') as h5fh:
            filters = tables.Filters(complevel=1, shuffle=True)
            obs = h5fh.create_table(h5fh.root, 'obs', Obs, 'Surface Observation', filters=filters)
            self.__setObsAttrs(obs)

    ##############################################################################
    # Functions setting table attributes
    def __setInfoAttrs(self, table):
        table.attrs.station_id = {'name': 'FIXED-WEATHER-STATION USAF MASTER '+
                'STATION CATALOG identifier',
            'fill': MissingUInt32, 'units': 'code'}
        table.attrs.wban_id = {'name': 'FIXED-WEATHER-STATION NCDCWBAN '+
                'identifier',
            'fill': MissingUInt32, 'units': 'code'}
        table.attrs.call_id = {'name': 'FIXED-WEATHER-STATION call letter '+
                'identifier'}
        table.attrs.lat = {'name': 'GEOPHYSICAL-POPINT-OBSERVATION latitude '+
                'coordinate',
            'fill': MissingFloat, 'units': 'degree'}
        table.attrs.lat = {'name': 'GEOPHYSICAL-POPINT-OBSERVATION longitude '+
                'coordinate',
            'fill': MissingFloat, 'units': 'degree'}
        table.attrs.elevation = {'name': 'Elevation relative to the mean sea '+
                'level',
            'fill': MissingInt16, 'units': 'm'}

    def __setObsAttrs(self, table):
        table.attrs.date_time = {'name': 'GEOPHYSICAL-POINT-OBSERVATION time', 
            'units': 's', 'fill': 0}
        table.attrs.year = {'name': 'year of date_time', 
            'units': 'year since 1900', 'fill': 0}
        table.attrs.yday = {'name': 'day of year of date_time', 
            'units': 'day', 'fill': 0}
        table.attrs.hour = {'name': 'hour of date_time', 
            'units': 'hour', 'fill': 0}
        table.attrs.source = {'name': 'GEOPHYSICAL-POINT-OBSERVATION data '+
                'source flag'}
        table.attrs.type = {'name': 'GEOPHYSICAL-REPORT-TYPE code'}
        table.attrs.wind_dir = {'name': 'WIND-OBSERVATION direction angle', 
            'units': 'degree', 'fill': MissingInt16}
        table.attrs.wdir_type = {'name': 'WIND-OBSERVATION type code'}
        table.attrs.wind_spd = {'name': 'WIND-OBSERVATION speed rate', 
            'units': 'm/s', 'fill': MissingFloat}
        table.attrs.cig = {'name': 'SKY-CONDITION-OBSERVATION ceiling height '+
                'dimension',
            'units': 'm', 'fill': MissingUInt16, 'unlimited': 22000}
        table.attrs.cig_tool = {'name': 'SKY-CONDITION-OBSERVATION ceiling '+
                'determination code'}
        table.attrs.cavok = {'name': 'SKY-CONDITION-OBSERVATION CAVOK code'}
        table.attrs.vis = {'name': 'VISIBILITY-OBSERVATION distance dimension', 
            'units': 'm', 'fill': MissingInt32}
        table.attrs.vis_var_code = {'name': 'VISIBILITY-OBSERVATION variability '+
                'code'}
        table.attrs.temp = {'name': 'AIR-TEMPERATURE-OBSERVATION air temperature', 
            'units': 'degree Celsius', 'fill': MissingFloat}
        table.attrs.dewt = {'name': 'AIR-TEMPERATURE-OBSERVATION dew point', 
            'units': 'degree Celsius', 'fill': MissingFloat}
        table.attrs.pres = {'name': 'ATMOSPHERIC-PRESSURE-OBSERVATION sea level '+
                'pressure rate', 
            'units': 'hectopascals', 'fill': MissingFloat}
        table.attrs.prec_period = {'name': 'LIQUID-PRECIPITATION period quantity',
            'units': 'hour', 'fill': MissingUInt8}
        table.attrs.prec_depth = {'name': 'LIQUID-PRECIPITATION depth dimension',
            'units': 'mm', 'fill': MissingFloat}
        table.attrs.prec_code = {'name': 'LIQUID-PRECIPITATION condition code',
            'units': 'code', 'fill': MissingUInt8}
        table.attrs.dur_code = {'name': 'PRECIPITATION-OBSERVATION-HISTORY '+
                'condition code', 
            'units': 'code', 'fill': MissingUInt8}
        table.attrs.char_code = {'name': 'PRECIPITATION-OBSERVATION-HISTORY '+
                'characteristic code'}
        table.attrs.disc_code = {'name': 'PRECIPITATION-BOGUS-OBSERVATION'+
                'discrepancy code',
            'units': 'code', 'fill': MissingUInt8}
        table.attrs.water_dep = {'name': 'PRECIPITATION-BOGUS-OBSERVATION'+
                'estimated water equivalency dimension', 
            'units': 'mm', 'fill': MissingInt16}
        table.attrs.snow_dim = {'name': 'SNOW-DEPTH dimension',
            'units': 'cm', 'fill': MissingInt16}
        table.attrs.snow_code = {'name': 'SNOW-DEPTH condition code',
            'units': 'code', 'fill': MissingUInt8}
        table.attrs.liq_dim = {'name': 'SNOW-DEPTH equivalent water depth '+
                'dimension',
            'units': 'mm', 'fill': MissingFloat}
        table.attrs.liq_code = {'name': 'SNOW-DEPTH equivalent water '+
                'condition code',
            'units': 'code', 'fill': MissingUInt8}
        table.attrs.snow_period = {'name': 'SNOW-ACCUMULATION period quantity',
            'units': 'hour', 'fill': MissingUInt8}
        table.attrs.snow_accu = {'name': 'SNOW-ACCUMULATION depth dimension',
            'units': 'cm', 'fill': MissingInt16}
        table.attrs.accu_cond = {'name': 'SNOW-ACCUMULATION condition code',
            'units': 'code', 'fill': MissingUInt8}
        table.attrs.pres_wx_code = {'name': 'PRESENT-WEATHER-OBSERVATION '+
                'automated atmospheric condition code', 
            'units': 'code', 'fill': MissingUInt8}
        table.attrs.pt_mwx_code = {'name': 'PAST-WEATHER-OBSERVATION manual '+
                'atmospheric condition code', 
            'units': 'code', 'fill': MissingUInt8}
        table.attrs.pt_mwx_pd = {'name': 'PAST-WEATHER-OBSERVATION period '+
                'quantity', 
            'units': 'hour', 'fill': MissingUInt8}
        table.attrs.pt_awx_code = {'name': 'PAST-WEATHER-OBSERVATION automated '+
                'atmospheric condition code', 
            'units': 'code', 'fill': MissingUInt8}
        table.attrs.pt_awx_pd = {'name': 'PAST-WEATHER-OBSERVATION period '+
                'quantity',
            'units': 'hour', 'fill': MissingUInt8}
        table.attrs.rw_vis_dir = {'name': 'RUNWAY-VISUAL-RANGE-OBSERVATION '+
                'direction angle', 
            'units': 'degree', 'fill': MissingInt16}
        table.attrs.rw_code = {'name': 'RUNWAY-VISUAL-RANGE-OBSERVATION runway '+
                'designator-code'}
        table.attrs.rw_vis_dim = {'name': 'RUNWAY-VISUAL-RANGE-OBSERVATION '+
                'visibility dimension', 
            'units': 'm', 'fill': MissingInt16}
        table.attrs.cov_code = {'name': 'SKY-COVER-LAYER coverage code',
            'units': 'code', 'fill': MissingUInt8}
        table.attrs.bs_hi_dim = {'name': 'SKY-COVER-LAYER base height dimension',
            'units': 'm', 'fill': MissingInt32}
        table.attrs.cloud_code = {'name': 'SKY-COVER-LAYER cloud type code',
            'units': 'code', 'fill': MissingUInt8}
        table.attrs.cov_sum_st_code = {'name': 'SKY-COVER-SUMMATION-STATE '+
                'coverage code',
            'units': 'code', 'fill': MissingUInt8}
        table.attrs.cov_sum_code = {'name': 'SKY-COVER-SUMMATION coverage code',
            'units': 'code', 'fill': MissingUInt8}
        table.attrs.cov_sum_st_dim = {'name': 'SKY-COVER-SUMMATION-STATE '+
                'height dimension', 
            'units': 'm', 'fill': MissingInt32}
        table.attrs.cov_sum_char_code = {'name': 'SKY-COVER-SUMMATION-STATE '+
                'characteristic code', 
            'units': 'code', 'fill': MissingUInt8}
        table.attrs.total_cov_code = {'name': 'SKY-CONDITION-OBSERVATION total '+
                'coverage code', 
            'units': 'oktas', 'fill': MissingUInt8}
        table.attrs.total_opa_code = {'name': 'SKY-CONDITION-OBSERVATION total '+
                'opaque coverage code', 
            'units': 'oktas', 'fill': MissingUInt8}
        table.attrs.low_cov_code = {'name': 'SKY-CONDITION-OBSERVATION total '+
                'lowest cloud coverage code', 
            'units': 'oktas', 'fill': MissingUInt8}
        table.attrs.low_cld_gen_code = {'name': 'SKY-CONDITION-OBSERVATION low '+
                'cloud genus code', 
            'units': 'code', 'fill': MissingUInt8}
        table.attrs.low_cld_dim = {'name': 'SKY-CONDITION-OBSERVATION lowest '+
                'base height dimension', 
            'units': 'm', 'fill': MissingInt32}
        table.attrs.mid_cld_gen_code = {'name': 'SKY-CONDITION-OBSERVATION mid '+
                'cloud genus code', 
            'units': 'code', 'fill': MissingUInt8}
        table.attrs.hi_cld_gen_code = {'name': 'SKY-CONDITION-OBSERVATION high '+
                'cloud genus code', 
            'units': 'code', 'fill': MissingUInt8}
        table.attrs.st_cov_code = {'name': 'BELOW-STATION-CLOUD-LAYER coverage '+
                'code',
            'units': 'oktas', 'fill': MissingUInt8}
        table.attrs.st_cld_tp_hi = {'name': 'BELOW-STATION-CLOUD-LAYER top '+
                'height dimension', 
            'units': 'm', 'fill': MissingInt32}
        table.attrs.st_cld_type = {'name': 'BELOW-STATION-CLOUD-LAYER type code',
            'units': 'code', 'fill': MissingUInt8}
        table.attrs.st_cld_tp_code = {'name': 'BELOW-STATION-CLOUD-LAYER top code',
            'units': 'code', 'fill': MissingUInt8}
        table.attrs.sun_dur = {'name': 'SUNSHINE-OBSERVATION duration quantity',
            'units': 'min', 'fill': MissingInt16}
        table.attrs.hail_size = {'name': 'HAIL size', 
            'units': 'cm', 'fill': MissingFloat}
        table.attrs.g2s_code = {'name': 'GROUND-SURFACE-OBSERVATION code',
            'units': 'code', 'fill': MissingUInt8}
        table.attrs.mint_period = {'name': 'GROUND-SURFACE-OBSERVATION minimum '+
                'temperature period quantity', 
            'units': 'hours', 'fill': MissingFloat}
        table.attrs.mint = {'name': 'GROUND-SURFACE-OBSERVATION minimum '+
                'temperature', 
            'units': 'degree Celsius', 'fill': MissingFloat}
        table.attrs.x_tp_period = {'name': 'EXTREME-AIR-TEMPERATURE period '+
                'quantity',
            'units': 'hours', 'fill': MissingFloat}
        table.attrs.x_tp_code = {'name': 'EXTREME-AIR-TEMPERATURE code',
            'units': 'code', 'fill': MissingUInt8}
        table.attrs.x_tp = {'name': 'EXTREME-AIR-TEMPERATURE temperature',
            'units': 'degree Celsius', 'fill': MissingFloat}
        table.attrs.altimeter = {'name': 'ATMOSPHERIC-PRESSURE-OBSERVATION '+
                'altimeter setting rate ', 
            'units': 'hectopascals', 'fill': MissingFloat}
        table.attrs.st_pres = {'name': 'ATMOSPHERIC-PRESSURE-OBSERVATION '+
                'station pressure rate ', 
            'units': 'hectopascals', 'fill': MissingFloat}
        table.attrs.pres_tr = {'name': 'ATMOSPHERIC-PRESSURE-CHANGE tendency '+
                'code',
            'units': 'code', 'fill': MissingUInt8}
        table.attrs.pres_chg_3h = {'name': 'ATMOSPHERIC-PRESSURE-CHANGE three '+
                'hour quantity ', 
            'units': 'hectopascals', 'fill': MissingFloat}
        table.attrs.pres_chg_24h = {'name': 'ATMOSPHERIC-PRESSURE-CHANGE twenty '+
                'four hour quantity ', 
            'units': 'hectopascals', 'fill': MissingFloat}
        table.attrs.isobar_code = {'name': 'GEOPOTENTIAL-HEIGHT-ISOBARIC-LEVEL '+
                'code',
            'units': 'code', 'fill': MissingUInt8}
        table.attrs.isobar_dim = {'name': 'GEOPOTENTIAL-HEIGHT-ISOBARIC-LEVEL '+
                'height dimension', 
            'units': 'gpm', 'fill': MissingInt16}
        table.attrs.wx_vic_code = {'name': 'PRESENT-WEATHER-IN-VICINITY-'+
                'OBSERVATION atmospheric condition code', 
            'units': 'code', 'fill': MissingUInt8}
        table.attrs.pres_wxm_code = {'name': 'PRESENT-WEATHER-OBSERVATION '+
                'manual atmospheric condition code', 
            'units': 'code', 'fill': MissingUInt8}
        table.attrs.sup_wd_code = {'name': 'SUPPLEMENTARY-WIND-OBSERVATION '+
                'type code',
            'units': 'code', 'fill': MissingUInt8}
        table.attrs.sup_wd_prd = {'name': 'SUPPLEMENTARY-WIND-OBSERVATION '+
                'period quantity', 
            'units': 'hour', 'fill': MissingUInt8}
        table.attrs.sup_wd_spd = {'name': 'SUPPLEMENTARY-WIND-OBSERVATION '+
                'speed rate',
            'units': 'm/s', 'fill': MissingFloat}
        table.attrs.wd_gust = {'name': 'WIND-GUST-OBSERVATION speed rate',
            'units': 'm/s', 'fill': MissingFloat}
