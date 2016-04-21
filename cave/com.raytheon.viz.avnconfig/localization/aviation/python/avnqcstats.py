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
#    Name:
#       avnqcstats.py
#       GFS1-NHD:A7966.0000-SCRIPT;1.5
#
#    Status:
#       DELIVERED
#    
#    History:
#       Revision 1.5 (DELIVERED)
#         Created:  29-NOV-2007 09:54:11      OBERFIEL
#           Removed obsolete directory search path
#       
#       Revision 1.4 (DELIVERED)
#         Created:  18-MAY-2006 10:32:32      TROJAN
#           SPR7150: port to HDF5 data format
#       
#       Revision 1.3 (DELIVERED)
#         Created:  07-MAY-2005 11:41:36      OBERFIEL
#           Added Item Header Block
#       
#       Revision 1.2 (DELIVERED)
#         Created:  07-DEC-2004 18:36:12      TROJAN
#           spr 6486
#       
#       Revision 1.1 (APPROVED)
#         Created:  19-AUG-2004 21:09:15      OBERFIEL
#           date and time created 08/19/04 21:09:15 by oberfiel
#
#    Change Document History:
#       1:
#       	Change Document:   GFS1-NHD_SPR_7351
#       	Action Date:       19-MAR-2008 08:14:54
#       	Relationship Type: In Response to
#       	Status:           CLOSED
#       	Title:             AvnFPS: Remove dependency on shared library, climmodule.so
#       
#
#    Purpose:
#	To create monthly climate files for TAF QC function in AvnFPS

import logging, os, sys, stat, time, ConfigParser
import numpy
import pupynere
import h5py
import itertools
import ClimateProcessLogger

#TopDir = os.environ['TOP_DIR']
#sys.path = sys.path[1:]
#sys.path.extend([os.path.join(TopDir, dir) for dir in \
#    ['sitepy', 'py', 'toolpy']])
#import Startup

#_Logger = logging.getLogger(__name__)
_Logger = logging.getLogger(ClimateProcessLogger.CLIMATE_CATEGORY)

# Change this to the path to your climate directory
climoDir = '/home/avarani/workspace/build.cave/static/common/cave/etc/aviation/climate'

# dimension indices 
DD, FF, OBV, INT, PCP, CIG, VSBY = range(7)
#IdsFile = os.path.join('etc', 'ids.cfg')

###############################################################################
fields = {}

class Bunch(object):
    def __init__(self, **kwds):
        self.__dict__ = kwds

    def values(self):
        return self.__dict__.values()
    
PType = Bunch(freezing=1<<0, frozen=1<<1, liquid=1<<2)
PBest = Bunch(drizzle=1<<0, continuous=1<<1, shower=1<<2)
PInt = Bunch(lgt=1<<0, mod=1<<1, hvy=1<<2)

MWxTable = { \
    0: {'str': ''}, \
    1: {'str': ''}, \
    2: {'str': ''}, \
    3: {'str': ''}, \
    4: {'str': 'FU'}, \
    5: {'str': 'HZ'}, \
    6: {'str': 'DU'}, \
    7: {'str': 'DU'}, \
    8: {'str': 'DU'}, \
    9: {'str': 'BLSA'}, \
    10: {'str': 'BR'}, \
    11: {'str': 'BCFG'}, \
    12: {'str': 'MIFG'}, \
    13: {'str': 'TS'}, \
    14: {'str': ''}, \
    15: {'str': ''}, \
    16: {'str': ''}, \
    17: {'str': 'TS'}, \
    18: {'str': ''}, \
    19: {'str': ''}, \
    20: {'str': ''}, \
    21: {'str': ''}, \
    22: {'str': ''}, \
    23: {'str': ''}, \
    24: {'str': ''}, \
    25: {'str': ''}, \
    26: {'str': ''}, \
    27: {'str': ''}, \
    28: {'str': ''}, \
    29: {'str': ''}, \
    30: {'str': 'BLSA'}, \
    31: {'str': 'BLSA'}, \
    32: {'str': 'BLSA'}, \
    33: {'str': 'BLSA'}, \
    34: {'str': 'BLSA'}, \
    35: {'str': 'BLSA'}, \
    36: {'str': 'DRSN'}, \
    37: {'str': 'DRSN'}, \
    38: {'str': 'BLSN'}, \
    39: {'str': 'BLSN'}, \
    40: {'str': ''}, \
    41: {'str': 'BCFG'}, \
    42: {'str': 'FG'}, \
    43: {'str': 'FG'}, \
    44: {'str': 'FG'}, \
    45: {'str': 'FG'}, \
    46: {'str': 'FG'}, \
    47: {'str': 'FG'}, \
    48: {'str': 'FG'}, \
    49: {'str': 'FG'}, \
    50: {'int': PInt.lgt, 'str': 'DZ', \
            'best': PBest.drizzle, 'type': PType.liquid}, \
    51: {'int': PInt.lgt, 'str': 'DZ', \
            'best': PBest.drizzle, 'type': PType.liquid}, \
    52: {'int': PInt.mod, 'str': 'DZ', \
            'best': PBest.drizzle, 'type': PType.liquid}, \
    53: {'int': PInt.mod, 'str': 'DZ', \
            'best': PBest.drizzle, 'type': PType.liquid}, \
    54: {'int': PInt.hvy, 'str': 'DZ', \
            'best': PBest.drizzle, 'type': PType.liquid}, \
    55: {'int': PInt.hvy, 'str': 'DZ', \
            'best': PBest.drizzle, 'type': PType.liquid}, \
    56: {'int': PInt.lgt, 'str': 'FZDZ', \
            'best': PBest.drizzle, 'type': PType.freezing}, \
    57: {'int': PInt.mod|PInt.hvy, 'str': 'FZDZ', \
            'best': PBest.drizzle, 'type': PType.freezing}, \
    58: {'int': PInt.lgt, 'str': 'DZRA', \
            'best': PBest.drizzle, 'type': PType.liquid}, \
    59: {'int': PInt.mod|PInt.hvy, 'str': 'DZRA', \
            'best': PBest.drizzle, 'type': PType.liquid}, \
    60: {'int': PInt.lgt, 'str': 'RA', \
            'best': PBest.continuous, 'type': PType.liquid}, \
    61: {'int': PInt.lgt, 'str': 'RA', \
            'best': PBest.continuous, 'type': PType.liquid}, \
    62: {'int': PInt.mod, 'str': 'RA', \
            'best': PBest.continuous, 'type': PType.liquid}, \
    63: {'int': PInt.mod, 'str': 'RA', \
            'best': PBest.continuous, 'type': PType.liquid}, \
    64: {'int': PInt.hvy, 'str': 'RA', \
            'best': PBest.continuous, 'type': PType.liquid}, \
    65: {'int': PInt.hvy, 'str': 'RA', \
            'best': PBest.continuous, 'type': PType.liquid}, \
    66: {'int': PInt.lgt, 'str': 'FZRA', \
            'best': PBest.continuous, 'type': PType.freezing}, \
    67: {'int': PInt.mod|PInt.hvy, 'str': 'FZRA', \
            'best': PBest.continuous, 'type': PType.freezing}, \
    68: {'int': PInt.lgt, 'str': 'RASN', \
            'best': PBest.continuous, 'type': PType.liquid}, \
    69: {'int': PInt.mod|PInt.hvy, 'str': 'RASN', \
            'best': PBest.continuous, 'type': PType.liquid}, \
    70: {'int': PInt.lgt, 'str': 'SN', \
            'best': PBest.continuous, 'type': PType.frozen}, \
    71: {'int': PInt.lgt, 'str': 'SN', \
            'best': PBest.continuous, 'type': PType.frozen}, \
    72: {'int': PInt.mod, 'str': 'SN', \
            'best': PBest.continuous, 'type': PType.frozen}, \
    73: {'int': PInt.mod, 'str': 'SN', \
            'best': PBest.continuous, 'type': PType.frozen}, \
    74: {'int': PInt.hvy, 'str': 'SN', \
            'best': PBest.continuous, 'type': PType.frozen}, \
    75: {'int': PInt.hvy, 'str': 'SN', \
            'best': PBest.continuous, 'type': PType.frozen}, \
    76: {'int': PInt.lgt, 'str': 'IC', \
            'best': PBest.continuous, 'type': PType.frozen}, \
    77: {'int': PInt.lgt, 'str': 'SG', \
            'best': PBest.continuous, 'type': PType.frozen}, \
    78: {'int': PInt.lgt, 'str': 'IC', \
            'best': PBest.continuous, 'type': PType.frozen}, \
    79: {'int': PInt.lgt, 'str': 'PL', \
            'best': PBest.continuous, 'type': PType.freezing}, \
    80: {'int': PInt.lgt, 'str': 'SHRA', \
            'best': PBest.shower, 'type': PType.liquid}, \
    81: {'int': PInt.mod|PInt.hvy, 'str': 'SHRA', \
            'best': PBest.shower, 'type': PType.liquid}, \
    82: {'int': PInt.hvy, 'str': 'SHRA', \
            'best': PBest.shower, 'type': PType.liquid}, \
    83: {'int': PInt.lgt, 'str': 'SHRASN', \
            'best': PBest.shower, 'type': PType.liquid}, \
    84: {'int': PInt.mod|PInt.hvy, 'str': 'SHRASN', \
            'best': PBest.shower, 'type': PType.liquid}, \
    85: {'int': PInt.lgt, 'str': 'SHSN', \
            'best': PBest.shower, 'type': PType.frozen}, \
    86: {'int': PInt.mod|PInt.hvy, 'str': 'SHSN', \
            'best': PBest.shower, 'type': PType.frozen}, \
    87: {'int': PInt.lgt, 'str': 'SHGS', \
            'best': PBest.shower, 'type': PType.freezing}, \
    88: {'int': PInt.mod|PInt.hvy, 'str': 'SHGS', \
            'best': PBest.shower, 'type': PType.freezing}, \
    89: {'int': PInt.lgt, 'str': 'SHGS', \
            'best': PBest.shower, 'type': PType.freezing}, \
    90: {'int': PInt.mod|PInt.hvy, 'str': 'SHGS', \
            'best': PBest.shower, 'type': PType.freezing}, \
    91: {'int': PInt.lgt, 'str': 'TSRA', \
            'best': PBest.shower, 'type': PType.liquid}, \
    92: {'int': PInt.mod|PInt.hvy, 'str': 'TSRA', \
            'best': PBest.shower, 'type': PType.liquid}, \
    93: {'int': PInt.lgt, 'str': 'TSGS', \
            'best': PBest.shower, 'type': PType.freezing}, \
    94: {'int': PInt.mod|PInt.hvy, 'str': 'TSGS', \
            'best': PBest.shower, 'type': PType.freezing}, \
    95: {'int': PInt.lgt|PInt.mod, 'str': 'TSRA', \
            'best': PBest.shower, 'type': PType.liquid}, \
    96: {'int': PInt.lgt|PInt.mod, 'str': 'TSGS', \
            'best': PBest.shower, 'type': PType.freezing}, \
    97: {'int': PInt.hvy, 'str': 'TSRA', \
            'best': PBest.shower, 'type': PType.liquid}, \
    98: {'int': 0, 'str': 'TS SS', 'best': 0, 'type': 0}, \
    99: {'int': PInt.hvy, 'str': 'TSGR', \
            'best': PBest.shower, 'type': PType.freezing}, \
}

def hd2us_vsby(vsby):
    '''Converts visibility from [m] to statue miles.'''
    if 0 <= vsby <= 100000:
        return vsby/1609.0
    else:
        return -1

def hd2us_cig(cig):
    '''Converts ceiling from [m] to [ft].
Unlimited ceiling is set to 99999'''
    if 0 <= cig <= 12000:
        return cig/0.305
    elif cig < 30000:
        return 99999
    else:
        return -1

def hd2us_wind_speed(speed):
    '''Converts speed from [m/s] to [kt].'''
    if 0 <= speed <= 100:
        return int(speed*3600.0/1852.0+0.5)
    else:
        return -1

def category(v, iterable):
    "Returns smallest index n such that v is less than iterable[n]."
    return sum(itertools.imap(lambda x: x<=v, iterable))

def wind_category(dd, ff, dd_threshold, ff_threshold):
    if dd == 'VRB' or dd == 0:
        return 0, 0
    else:
        numDDcat = len(dd_threshold)
        dd_cat = (category(dd, dd_threshold)%numDDcat) + 1
    ff_cat = category(ff, ff_threshold)
    if ff_cat == 0:
        dd_cat = 0
    return dd_cat, ff_cat

def get_site_cfg():
    cfg = {}
    cfg['alpha'] = 0.3
    cfg['showdetails'] = 0
    cfg['vsby'] = [0.27,1.1,2.9,6.1]
    cfg['cig'] = [205,605,1005,3105,40000]
    cfg['ff'] = [22.5,67.5,112.5,157.5,202.5,247.5,292.5,337.5]
    cfg['dd'] = [45.0,135.0,225.0,315.0]
    return cfg

def createNetCDFFile(cfg, direct):
    path = '%s/%s.%02d.nc' % (direct, cfg['ident'], cfg['month'])
    os.system('/bin/rm -f %s' % path)
    try:
        fh = pupynere.NetCDFFile(path, 'w', time.ctime())
    except IOError:
        msg = 'Cannot create file %s' % path
        print msg
        if _Logger : _Logger.error(msg)
        return None
    fh.title = 'Some hopefully less useless junk'
    fh.ident = cfg['ident']
    fh.month = cfg['month']
    fh.createDimension('dd', len(cfg['dd'])+1)  # cycle, 0 is calm/vrb
    fh.createDimension('ff', len(cfg['ff'])+1)
    fh.createDimension('cig', len(cfg['cig'])+1)
    fh.createDimension('vsby', len(cfg['vsby'])+1)
    fh.createDimension('obv', 8)
    fh.createDimension('int', 4)    # check this
    fh.createDimension('pcp', 4)
    fh.createDimension('total', 1)

    for var in fh.dimensions:
        v = fh.createVariable(var, 'i', (var,))
        v[:] = 0
    all = fh.createVariable('all', 'i', 
        ('dd', 'ff', 'obv', 'int', 'pcp', 'cig', 'vsby'))
    all[:] = 0
    for v in fh.variables:
        fh.variables[v].units = 'count'
    return fh

def getOffset(cfg, row):
    offObv = 0
    for wx in [MWxTable[x] for x in row[fields['pres_wxm_code']] if x<50]:
        if 'HZ' in wx['str']:
            offObv |= 1
        if 'FG' in wx['str'] or 'BR' in wx['str']:
            offObv |= 2
        if 'BL' in wx['str']:
            offObv |= 4
    offInt = 0
    pcp = [MWxTable[x] for x in row[fields['pres_wxm_code']] if 50<=x<=99]
    if pcp:
        i = max([p['int'] for p in pcp])
        if i & PInt.hvy:
            offInt = 3
        elif i & PInt.mod:
            offInt = 2
        elif i & PInt.lgt:
            offInt = 1
    i = 0
    if offInt > 0:
        for wx in pcp:
            if 'SN' in wx['str']:
                i |= 1
            if 'DZ' in wx['str']:
                i |= 2
            if 'RA' in wx['str']:
                i |= 4
    if i == 0:
        offInt = offPcp = 0
    else:
        if i & 1:
            offPcp = 1
        elif i & 2:
            offPcp = 2
        elif i & 4:
            offPcp = 3
    wspeed = hd2us_wind_speed(row[fields['wind_spd']])
    if wspeed < 0:
        return None
    if row[fields['wdir_type']] == 'C' or wspeed == 0:
        wdir = wspeed = 0
    elif row[fields['wdir_type']] == 'V':
        wdir = wspeed = 0
    else:
        wdir = row[fields['wind_dir']]
        if not 0 < wdir <= 360:
            return None
    offDD, offFF = wind_category(wdir, wspeed, cfg['dd'], cfg['ff'])
    if row[fields['vis']] > 100000:
        return ''
    vis = hd2us_vsby(row[fields['vis']])
    offVsby = category(vis, cfg['vsby'])
    cig = hd2us_cig(row[fields['cig']])
    offCig = category(cig, cfg['cig'])
    return offDD, offFF, offObv, offInt, offPcp, offCig, offVsby

def makeStatsFile(cfg, climateDir):
    ncfh = createNetCDFFile(cfg, climateDir)
    if not ncfh:
        return
    ncfhFilename = ncfh.filename
    try :
        os.chmod(ncfhFilename, stat.S_IRUSR | stat.S_IWUSR |stat.S_IRGRP | stat.S_IWGRP | stat.S_IROTH)
        fname = '%s/%s.hd5' % (climateDir, cfg['ident'])
        fh = h5py.File(fname, 'r')
        missed = 0
        table = fh['obs']
        names = table.dtype.names
        for i in range(len(names)):
            fields[names[i]] = i
        tms = time.gmtime(time.mktime((2000, cfg['month'], 15, 0, 0, 0, 0, 0, 0)))
        yday1 = tms.tm_yday-20
        yday2 = tms.tm_yday+20
        yday = table['yday']
        yd1 = (yday1 <= yday)
        yd2 = (yday <= yday2)
    except Exception, e:
        ncfh.close()
        os.remove(ncfhFilename)
        if fh :
            fh.close()
        msg = 'Unable to create file %s' % ncfhFilename
        print msg
        print e
        if _Logger : _Logger.error(msg, e)
        raise e
    
    def time_selector1():
        index = numpy.where(yd1&yd2)[0]
        rows = [table[i] for i in index]
        for o in rows:
            yield o
    
    def time_selector2():
        index = numpy.where(yd1)[0]
        rows = [table[i] for i in index]
        for o in rows:
            yield o
        index = numpy.where(yd2)[0]
        rows = [table[i] for i in index]
        for o in rows:
            yield o
            
    try :
        if yday1 < yday2:
            time_selector = time_selector1
        else:
            time_selector = time_selector2
        for row in time_selector():
            off = getOffset(cfg, row)
            if not off:
                missed += 1
                continue
            ncfh.variables['all'][off] += 1
            ncfh.variables['dd'][off[DD]] += 1
            ncfh.variables['ff'][off[FF]] += 1
            ncfh.variables['obv'][off[OBV]] += 1
            ncfh.variables['int'][off[INT]] += 1
            ncfh.variables['pcp'][off[PCP]] += 1
            ncfh.variables['cig'][off[CIG]] += 1
            ncfh.variables['vsby'][off[VSBY]] += 1
            ncfh.variables['total'][0] += 1
        print 'Processed %d records, skipped %d' % (ncfh.variables['total'][0], missed)
        if _Logger : _Logger.debug('Processed %s %s, %d records, skipped %d',
            cfg['ident'], cfg['month'], ncfh.variables['total'][0], missed)
        print 'Created stats file for %s, month %02d' % (cfg['ident'], cfg['month'])
        if _Logger : _Logger.info('Created stats file for %s, month %02d', 
            cfg['ident'], cfg['month'])
    except Exception, e:
        os.remove(ncfhFilename)
        msg = 'Unable to create file %s' % ncfhFilename
        print e
        if _Logger : _Logger.error(msg, e)
        raise e
    finally:
        fh.close()
        ncfh.close()

def main():
    climateDir = climoDir
    if len(sys.argv) == 4 :
        climateDir = sys.argv[3]
    elif len(sys.argv) != 3:
        print 'Usage:', os.path.basename(sys.argv[0]), 'site-id month [climate-dir]'
        raise SystemExit
    _Logger = None
    genFiles(sys.argv[1], sys.argv[2], climateDir)
    return

def genFiles(siteList, monthList, climateDir):
    totstart = time.time()
    sites = siteList.upper().split(',')
    months = monthList.split(',')
    for ident in sites :
        for month in months :
            try :
                tstart = time.time()
                cfg = get_site_cfg()
                cfg['ident'] = ident
                cfg['month'] = int(month)
                makeStatsFile(cfg, climateDir)
            except Exception, e:
                pass
            finally:
                print 'Elapsed time:', time.time() - tstart
    msg =  'Total Elapsed time: %d' % (time.time() - totstart)
    print msg
    if _Logger : _Logger.debug(msg)
    return

###############################################################################
if __name__ == '__main__':
    main()
