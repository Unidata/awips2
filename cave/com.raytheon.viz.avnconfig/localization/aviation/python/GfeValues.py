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
# Methods ported from IFPS2AvnFPS
#  
#    
#     SOFTWARE HISTORY
#    
#    Date            Ticket#       Engineer       Description
#    ------------    ----------    -----------    --------------------------
#    08/12/09                      njensen        Initial Creation.
#    04/23/2-14       #3006        randerso       Fix Wx parsing
# 
#


_vsbyCodes = ['F','ZF','IF','IC','H','BS','BN','K','BD','Y','ZY','VA']

_translateCode = { 'Iso' : 'IS', 'Sct' : 'SC', 'Num' : 'NM', 'Wide': 'WP', 'Ocnl': 'O', 'SChc': 'S',
                       'Chc' : 'C', 'Lkly': 'L',  'Def' : 'D',  'Patchy': 'IS', 'Areas':'SC', 'Frq': 'L',
                       'Brf': 'S', 'Pds': 'C', 'Inter': 'L', '<NoCov>':' ', '<NoInten>':' ', '--':'-',
                       '-':'-', 'm':'m', '+':'+' }


def scalarValue(value):
    try:
        return str(int(round(value)))
    except:
        return "999"


def skyValue(value):
    """Method to return a simple scalar as a string with minor processing"""
    try:
        return str(int((value+5)/10.))
    except:
        return "999"

def windDirValue(value):
    try:                
        return str(int(round(value)/10.))
    except:
            return "999"

def windMagValue(value):
    try:
        return str(int(round(value)))
    except:
        return "999"

def obvisValue(value):
    """Find the first obstruction to vision type and report that"""    
    wxType = '999'
    wxStats = None
    if type(value) is str:
        wxStats = FakeWxKey(value)
    if wxStats is None:
        return wxType
    else:
        for subkey in wxStats:
            if subkey.wxType() in _vsbyCodes:
                wxType = subkey.wxType()
                if wxType == 'F' or wxType == 'ZF' or wxType == 'IF':
                    if subkey.intensity() == '+':
                        wxType = 'F'
                    else:
                        wxType = 'BR'
                break
    return wxType


def wxVal(value, index):
    """Get the Nth weather type.  N is given in argList[0]"""
    wxStats = None
    if type(value) is str:
        wxStats = FakeWxKey(value)
    value = '999'
    if wxStats is None:
        return value

    skip = index
    count = 1
    for subkey in wxStats:
        if subkey.wxType() not in ['<NoWx>','T'] + _vsbyCodes:
            if count == skip:
                value = subkey.wxType()
                break
            else:
                count = count + 1

    return value

def wxValInst(value, index):
    """Get the Nth weather intensity.  N is given in argList[0]"""
    wxStats = None
    if type(value) is str:
        wxStats = FakeWxKey(value)
    value = '999'
    if wxStats is None:
        return value

    skip = index
    count = 1
    for subkey in wxStats:
        if subkey.wxType() not in ['<NoWx>','T'] + _vsbyCodes:
            if count == skip:
                value = _translateCode.get(subkey.intensity(), 'm' )
                break
            else:
                count = count + 1

    return value

def wxValCov(value, index):
    """Get the Nth weather coverage.  N is given in argList[0]"""
    wxStats = None
    if type(value) is str:
        wxStats = FakeWxKey(value)
    value = '999'
    if wxStats is None:
        return value

    skip = index
    count = 1
    for subkey in wxStats:
        if subkey.wxType() not in ['<NoWx>','T'] + _vsbyCodes:
            if count == skip:
                value = _translateCode.get(subkey.coverage(), 'S' )
                break
            else:
                count = count + 1
    return value

def wxTstm(value):
    """Search for any reference to thunderstorms and, if found, return the coverage expected"""
    wxStats = None
    if type(value) is str:
        wxStats = FakeWxKey(value)
    value = '999'
    if wxStats is None:
        return value

    for subkey in wxStats:
        if subkey.wxType() == 'T':
            value = _translateCode.get(subkey.coverage(), 'S' )
            break

    return value

def wxTstmInt(value):
    """Search for any reference to thunderstorms and, if found, return the intensity expected"""
    wxStats = None
    if type(value) is str:
        wxStats = FakeWxKey(value)
    value = '999'
    if wxStats is None:
        return value

    for subkey in wxStats:
        if subkey.wxType() == 'T':
            value = _translateCode.get(subkey.intensity(), 'm' )
            break

    return value


class FakeWxKey:
    
    def __init__(self, value):
        split = value.split('^')
        self.subkeys = []
        for s in split:
            self.subkeys.append(FakeWxSubkey(s))
    
    def __str__(self):
        return "^".join([str(subkey) for subkey in self.subkeys])
        
    def __getitem__(self, key):
        return self.subkeys[key]
    
    def __len__(self):
        return len(self.subkey)
    
    def __iter__(self):
        return self.subkeys.__iter__()


class FakeWxSubkey:
    
    def __init__(self, value):
        split = value.split(':')
        self.cov = split[0]
        self.type = split[1]
        self.inten = split[2]
        self.vis = split[3]
    
    def __str__(self):
        return ":".join([self.cov, self.type, self.inten, self.vis])

    def wxType(self):
        return self.type
    
    def coverage(self):
        return self.cov
    
    def intensity(self):
        return self.inten
    
    def visibility(self):
        return self.vis


    
