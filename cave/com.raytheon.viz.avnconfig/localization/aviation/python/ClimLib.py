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
#       ClimLib.py
#       GFS1-NHD:A9007.0000-SCRIPT;3
#
#    Status:
#       DELIVERED
#    
#    History:
#       Revision 3 (DELIVERED)
#         Created:  24-MAY-2006 09:05:37      TROJAN
#           spr 7144: ficed weather element dictionary, added VLIFR
#           category
#       
#       Revision 2 (DELIVERED)
#         Created:  16-FEB-2006 14:35:36      TROJAN
#           added check for cache age
#       
#       Revision 1 (APPROVED)
#         Created:  30-JAN-2006 07:51:28      TROJAN
#           stdr 945
#
#    Change Document History:
#       1:
#       	Change Document:   GFS1-NHD_SPR_7144
#       	Action Date:       14-FEB-2007 12:27:26
#       	Relationship Type: In Response to
#       	Status:           CLOSED
#       	Title:             AvnFPS: Incorrect method to specify filtering criteria in Cig/Vis Monthly tool
#       
#
# ClimLib.py
# common code for Climate tools
# George Trojan, SAIC/MDL, November 2005
# last update: 05/24/06

import logging, os, cPickle
import Avn

_Logger = logging.getLogger(__name__)

PType = Avn.Bunch(freezing=1<<0, frozen=1<<1, liquid=1<<2)
PBest = Avn.Bunch(drizzle=1<<0, continuous=1<<1, shower=1<<2)
PInt = Avn.Bunch(lgt=1<<0, mod=1<<1, hvy=1<<2)

PTypeStr = { \
    (PType.freezing, PBest.drizzle): 'FZDZ', \
    (PType.freezing, PBest.continuous): 'FZRA', \
    (PType.freezing, PBest.shower): 'SHPL', \
    (PType.frozen, PBest.drizzle): 'DZSN', \
    (PType.frozen, PBest.continuous): 'SN', \
    (PType.frozen, PBest.shower): 'SHSN', \
    (PType.liquid, PBest.drizzle): 'DZ', \
    (PType.liquid, PBest.continuous): 'RA', \
    (PType.liquid, PBest.shower): 'SHRA', \
    }

# Codes used in pres_wxm_code
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

MOS_obvis = set([5,10]+range(30,36)+[38,39]+range(42,50))

# disjoint
MOS_ptype = {}
for key in PType.values():  
    MOS_ptype[key] = set([x for x in MWxTable if x>=50 and \
        MWxTable[x]['type']&key])

# disjoint
MOS_pbest = {}
for key in PBest.values():  
    MOS_pbest[key] = set([x for x in MWxTable if x>=50 and \
        MWxTable[x]['best']&key])

# not disjoint
MOS_pint = {}
for key in PInt.values():  
    MOS_pint[key] = set([x for x in MWxTable if x>=50 and \
        MWxTable[x]['int']&key])

###############################################################################
# matches GFS MOS
#           1    2    3    4     5     6     7      8
CigCat = [0, 190, 450, 950, 1950, 3050, 6550, 12000, 999999] 
#           1     2     3     4    5    6    7
VisCat = [0, 0.45, 0.95, 1.95, 2.9, 5.5, 6.5, 999.0]
UnltdCig = 22000    # as appears in hd5 file

#def get_cat(value, seq):
#    for n, (lo, hi) in enumerate(Avn.window(seq)):
#        if lo <= value < hi:
#            return n
#    else:
#        raise ValueError('Invalid value: %f' % value)

###############################################################################
# conversion functions
Unlimited = 99999

def hd2us_cig(cig):
    '''Converts ceiling from [m] to [ft].
Unlimited ceiling is set to 99999'''
    if 0 <= cig <= 12000:
        return cig/0.305
    elif cig < 30000:
        return Unlimited
    else:
        return -1

def us2hd_cig(cig, unlimited):
    '''Converts ceiling from [ft] to [m].
Unlimited (99999) ceiling is set to "unlimited", which should be
set to fh.variables['cig'].Unlimited'''
    if cig == Unlimited:
        return unlimited
    else:
        return cig*0.305

def hd2us_vsby(vsby):
    '''Converts visibility from [m] to statue miles.'''
    if 0 <= vsby <= 100000:
        return vsby/1609.0
    else:
        return -1

def us2hd_vsby(vsby):
    '''Converts visibility from statue miles to [m].'''
    return vsby*1609.0

def hd2us_wind_speed(speed):
    '''Converts speed from [m/s] to [kt].'''
    if 0 <= speed <= 100:
        return int(speed*3600.0/1852.0+0.5)
    else:
        return -1

def us2hd_wind_speed(speed):
    '''Converts speed from [kt] to [m/s].'''
    return speed*1852.0/3600.0

# flight category definition (metric)
FlightCats = { \
    0: {'cig': us2hd_cig(190, 22000), 'vis': us2hd_vsby(0.45)}, \
    1: {'cig': us2hd_cig(490, 22000), 'vis': us2hd_vsby(0.95)}, \
    2: {'cig': us2hd_cig(950, 22000), 'vis': us2hd_vsby(2.9)}, \
    3: {'cig': us2hd_cig(3050, 22000), 'vis': us2hd_vsby(5.2)}, \
    }

###############################################################################
class Cache:
    def __init__(self):
        self.home = os.path.join(os.environ['TOP_DIR'], 'data', 'climate', 
            'cache')

    def put(self, id_, key, data):
        dir_ = os.path.join(self.home, id_)
        try:
            if not os.path.isdir(dir_):
                os.mkdir(dir_)
            path = os.path.join(dir_, key)
            cPickle.dump(data, file(path, 'w'), -1)
            return True
        except IOError, e:
            _Logger.exception('put() failed for %s\n%s', path, e)
            return False

    def get(self, id_, key, cpath=None):
        path = os.path.join(self.home, id_, key)
        # compare times, return None if climate file newer than cache
        try:
            if cpath and os.path.getmtime(path) < os.path.getmtime(cpath):
                _Logger.info('climate data file newer than cache')
                return None
            return cPickle.load(file(path))
        except (ValueError, IOError, OSError), e:
            _Logger.info('get() failed for %s\n%s', path, e)
            return None
