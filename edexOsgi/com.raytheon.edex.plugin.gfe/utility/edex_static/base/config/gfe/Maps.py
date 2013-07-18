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
# NOTE: THIS FILE SHOULD NOT BE USER_MODIFIED.  INSTEAD, REFER TO THE
# DOCUMENTATION ON HOW ENTRIES IN THIS FILE MAY BE OVERRIDDEN.  REFER TO
# LOCALMAPS DOCUMENTATION.

# Maps.py - map background definitions for ifpServer

from ShapeTable import ShapeTable

import siteConfig, LogStream, JUtil

BASELINE = getattr(siteConfig, 'BASELINE', 0)

# Following lines extract the CWA (WFO name) from siteConfig
CWA = siteConfig.GFESUITE_SITEID

# Each map is identified by a Python variable.  The ShapeTable
# is first identified as the data source.  The map is then filtered in
# differing ways, or not at all if desired.  The name of the map
# is specified.  The edit area name and edit area group
# are specified, if it is desired to automatically generate edit areas
# from the map. 
#
# NOTE: In AWIPS 2 map backgrounds are created using map bundles. 
# Maps.py is only used for generating edit areas. 
#
#MapNameVariable = ShapeTable('the name of the map table')
#MapNameVariable.filter( -- - - - - filter string - - - - - - )
#MapNameVariable.name = 'the display name of the map' 
#MapNameVariable.editAreaName = 'attribute in ShapeTable to be used to name
#    'editArea'
#MapNameVariable.groupName = 'name of the edit area group'
#

# -------------------------------------------------------------
# Functions for determining name of edit areas
# -------------------------------------------------------------
# FIPS codes
def fips(atts):
    #make sure fips attribute exists and of proper length
    #make sure state attribute exists and of proper length
    if atts.has_key('fips') and len(atts['fips']) == 5 and \
      atts.has_key('state') and len(atts['state']) == 2:
        fips = atts['fips'][-3:]   #last 3 digits from fips code
        s = atts['state'] + "C" + fips  #assemble COC013
        return s
    else:
        return ""  #for no fips in ShapeTable

# Public Zones
def cwazones(atts):
    if atts.has_key('zone') and len(atts['zone']) == 3 and \
      atts.has_key('state') and len(atts['state']) == 2:
        return atts['state'] + "Z" + atts['zone']  #assemble COZ023
    else:
        return ""   #bad attributes

# Fire Wx Zones
def fwxzones(atts):
    if atts.has_key('zone') and len(atts['zone']) == 3 and \
      atts.has_key('state') and len(atts['state']) == 2:
        return atts['state'] + "Z" + atts['zone']  #assemble COZ023
    else:
        return ""

# Marine Zones
def marineZ(atts):
    if atts.has_key('id') and len(atts['id']) == 6:
        return atts['id']
    else:
        return ""

# Offshore Marine Zones
def offshoreZ(atts):
    if atts.has_key('id') and len(atts['id']) == 6:
        return atts['id']
    else:
        return ""

#---------------------------------------------------------------------
# Map Background Filters
#---------------------------------------------------------------------
# filter for public zones.
def publicZoneFilter(atts):
    if not atts.has_key('cwa'):
        return 0
    
    # this CWA (all but AFC site)
    if atts['cwa'] == CWA:
        return 1

    # AFC data - separate out AER/ALU data
    elif atts['cwa'] == 'AFC':
        id = cwazones(atts)
        if CWA == 'AER':
            return id in ['AKZ101', 'AKZ111', 'AKZ121', 'AKZ125', 'AKZ131',
              'AKZ135', 'AKZ141', 'AKZ145', 'AKZ171']

        elif CWA == 'ALU':
            return id in  ['AKZ151', 'AKZ155', 'AKZ161', 'AKZ181',
              'AKZ185', 'AKZ187', 'AKZ191', 'AKZ195']

        elif CWA == 'AICE':
            return 1

    return 0

# filter for fire weather zones.
def firewxZoneFilter(atts):
    if not atts.has_key('cwa'):
        return 0
    
    # this CWA (all but AFC site)
    if atts['cwa'] == CWA:
        return 1

    # AFC data - separate out AER/ALU data
    elif atts['cwa'] == 'AFC':
        id = fwxzones(atts)
        if CWA == 'AER':
            return id in ['AKZ101', 'AKZ111', 'AKZ121', 'AKZ125', 'AKZ131',
              'AKZ135', 'AKZ141', 'AKZ145', 'AKZ171']

        elif CWA == 'ALU':
            return id in ['AKZ151', 'AKZ155', 'AKZ161', 'AKZ181',
              'AKZ185', 'AKZ187', 'AKZ191', 'AKZ195']

        elif CWA == 'AICE':
            return 1

    return 0

# filter for marine zones.
def marineZoneFilter(atts):
    if not atts.has_key('wfo'):
        return 0
    
    # this CWA (all but AFC site)
    if atts['wfo'] == CWA:
        return 1

    # AFC data - separate out AER/ALU data
    elif atts['wfo'] == 'AFC':
        id = marineZ(atts)
        if CWA == 'AER':
            return id in ['PKZ120', 'PKZ121', 'PKZ125', 'PKZ126', 'PKZ127',
              'PKZ128', 'PKZ129', 'PKZ130', 'PKZ132', 'PKZ136', 'PKZ137',
              'PKZ138', 'PKZ140', 'PKZ141']
        elif CWA == 'ALU':
            return id in  ['PKZ150', 'PKZ155', 'PKZ160', 'PKZ165', 'PKZ170',
              'PKZ171', 'PKZ172', 'PKZ175', 'PKZ176', 'PKZ179', 'PKZ180',
              'PKZ185']
        elif CWA == 'AICE':
            return 1

    return 0


# filter for offshore marine zones.
def offshoreZoneFilter(atts):
    if not atts.has_key('wfo'):
        return 0
    
    # this CWA (all but AFC site)
    if atts['wfo'] == CWA:
        return 1

    # AFC data - separate out AER/ALU data
    elif atts['wfo'] == 'AFC':
        id = offshoreZ(atts)
        if CWA == 'AER':
            return id in ['PKZ350']
        elif CWA == 'ALU':
            return id in  ['PKZ410']
        elif CWA == 'AICE':
            return 1

    return 0


#---------------------------------------------------------------------
# Map Background Definitions
#---------------------------------------------------------------------

# CWA Counties
CWAcounties = ShapeTable('county')
CWAcounties.filter(lambda x : x['cwa'][0:3] == CWA or x['cwa'][3:6] == CWA)
CWAcounties.name = 'Counties_' + CWA
CWAcounties.editAreaName = ['state','countyname']
CWAcounties.groupName = 'Counties'

# FIPS for my counties - only include first WFO indicated in CWA field
FIPS = ShapeTable('county')
FIPS.name = 'FIPS_' + CWA
FIPS.filter(lambda x : x['cwa'][0:3] == CWA)
FIPS.editAreaName = fips
FIPS.groupName = 'FIPS_' + CWA

# Unfiltered Counties
Counties = ShapeTable('county')
Counties.name = 'Counties'
Counties.editAreaName = fips
Counties.groupName = 'FIPS'

# CWA Zones
CWAzones = ShapeTable('zone')
CWAzones.filter(publicZoneFilter)
CWAzones.name = 'Zones_' + CWA
CWAzones.editAreaName = cwazones
CWAzones.groupName = 'Zones_' + CWA

# Unfiltered Zones
Zones = ShapeTable('zone')
Zones.name = 'Zones'
Zones.editAreaName = cwazones
Zones.groupName = 'Zones'

# Fire Wx Zones
FWCWAzones = ShapeTable('firewxzones')
FWCWAzones.filter(firewxZoneFilter)
FWCWAzones.name = 'FireWxZones_' + CWA
FWCWAzones.editAreaName = fwxzones
FWCWAzones.groupName = 'FireWxZones_' + CWA

# Unfiltered Fire Wx Zones
FWZones = ShapeTable('firewxzones')
FWZones.name = 'FireWxZones'
FWZones.editAreaName = fwxzones
FWZones.groupName = 'FireWxZones'

# CWAs for all
cwas = ShapeTable('cwa')
cwas.name = 'CWA_all'
cwas.editAreaName = 'wfo'
cwas.groupName = 'WFOs'

# ISC areas for all
isc = ShapeTable('isc')
isc.name = 'ISC_all'
isc.editAreaName = ['ISC','wfo']
isc.groupName = 'ISC'

# Fire Wx AOR for all
fwaor = ShapeTable('firewxaor')
fwaor.name = 'FireWxAOR'
fwaor.editAreaName = ['FireWxAOR', 'cwa']
fwaor.groupName = 'FireWxAOR'

# Marine Zones for CWA
CWAmzones = ShapeTable('marinezones')
CWAmzones.filter(marineZoneFilter)
CWAmzones.name = 'Marine_Zones_' + CWA
CWAmzones.editAreaName = marineZ
CWAmzones.groupName = 'MZones_' + CWA

# Marine Zones (unfiltered)
Mzones = ShapeTable('marinezones')
Mzones.name = "Marine_Zones"
Mzones.editAreaName = marineZ
Mzones.groupName = 'MZones'

# States (unfiltered)
States = ShapeTable('states')
States.name = "States"
States.editAreaName = 'name'
States.groupName = 'States'

# RFC maps
rfc = ShapeTable('rfc')
rfc.name = "RFC"
rfc.editAreaName = ['ISC','site_id']
rfc.groupName = 'ISC'

# Offshore Marine Zones - unfiltered
offshore = ShapeTable('offshore')
offshore.name = "Offshore_Marine_Zones"
offshore.editAreaName = offshoreZ
offshore.groupName = 'OffShoreMZones'

# Offshore Marine Zones - filtered by CWA
offshoreCWA = ShapeTable('offshore')
offshoreCWA.filter(offshoreZoneFilter)
offshoreCWA.name = "Offshore_Marine_Zones_" + CWA
offshoreCWA.editAreaName = offshoreZ
offshoreCWA.groupName = 'OffShoreMZones_' + CWA

# this is a complete listing of all maps
maps = [ CWAcounties, FIPS, Counties, CWAzones, Zones, FWCWAzones, FWZones,
         cwas, isc, fwaor, CWAmzones, Mzones, States, rfc, offshore, offshoreCWA ]

# import the local maps file
if not BASELINE:
    try:
        from localMaps import *
    except ImportError:
        pass

def getMaps():
    from java.util import ArrayList
    jmaps = ArrayList(len(maps))
    for m in maps:
        j = m.toJavaObj()
        jmaps.add(j)
        for k,v in globals().iteritems():
            if v is m:
                j.setInstanceName(k)
                break

    return jmaps

def runFilter(instance, info):
    info = JUtil.javaObjToPyVal(info)
    return bool(globals()[instance].doFilter(info))

def runNamer(instance, info):
    info = JUtil.javaObjToPyVal(info)
    return str(globals()[instance].getEAName(info))  
