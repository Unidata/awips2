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
# This is an absolute override file, indicating that a higher priority version
# of the file will completely replace a lower priority version of the file.
##

##
##  Dictionary that maps VTEC codes to headlines
##  Routines that define environmentally related hazards (phen codes)

#this table contains all allowable vtec phenomena and significance
#codes. The dictionary contains the VTEC phonomena, significance,
#and associated headline.
# Updated in OB9.3 to remove HI/TI hazards.

#these entries are used in serverConfig.py to define the basic set
#of headline keys.

VTECTable = {
    'AF.W' : {'phen': 'AF',
              'sig': 'W',
              'hdln': 'Ashfall Warning'},
    'AF.Y' : {'phen': 'AF',
              'sig': 'Y',
              'hdln': 'Ashfall Advisory'},
    'AQ.Y' : {'phen': 'AQ',
              'sig': 'Y',
              'hdln': 'Air Quality Alert'},
    'AS.O' : {'phen': 'AS',
              'sig': 'O',
              'hdln': 'Air Stagnation Outlook'},
    'AS.Y' : {'phen': 'AS',
              'sig': 'Y',
              'hdln': 'Air Stagnation Advisory'},
    'BH.S' : {'phen': 'BH',
              'sig': 'S',
              'hdln': 'Beach Hazards Statement'},
    'BW.Y' : {'phen': 'BW',
              'sig': 'Y',
              'hdln': 'Brisk Wind Advisory'},
    'BZ.A' : {'phen' : 'BZ',
              'sig' : 'A',
              'hdln' : 'Blizzard Watch'},
    'BZ.W' : {'phen' : 'BZ',
              'sig' : 'W',
              'hdln' : 'Blizzard Warning'},
    'CF.A' : {'phen': 'CF',
              'sig': 'A',
              'hdln': 'Coastal Flood Watch'},
    'CF.W' : {'phen': 'CF',
              'sig': 'W',
              'hdln': 'Coastal Flood Warning'},
    'CF.Y' : {'phen': 'CF',
              'sig': 'Y',
              'hdln': 'Coastal Flood Advisory'},
    'CF.S' : {'phen': 'CF',
              'sig': 'S',
              'hdln': ''},  #No headline for this VTEC
    'DS.W' : { 'phen': 'DS',
              'sig': 'W',
              'hdln': 'Dust Storm Warning'},
    'DS.Y' : {'phen': 'DS',
              'sig': 'Y',
              'hdln': 'Dust Storm Advisory'},
    'DU.W' : {'phen': 'DU',
              'sig': 'W',
              'hdln': 'Blowing Dust Warning'},
    'DU.Y' : {'phen': 'DU',
              'sig': 'Y',
              'hdln': 'Blowing Dust Advisory'},
    'EC.A' : {'phen': 'EC',
              'sig': 'A',
              'hdln': 'Extreme Cold Watch'},
    'EC.W' : {'phen': 'EC',
              'sig': 'W',
              'hdln': 'Extreme Cold Warning'},
    'EH.A' : {'phen': 'EH',
              'sig': 'A',
              'hdln': 'Excessive Heat Watch'},
    'EH.W' : {'phen': 'EH',
              'sig': 'W',
              'hdln': 'Excessive Heat Warning'},
    'EW.W' : {'phen': 'EW',
              'sig': 'W',
              'hdln': 'Excessive Wind Warning'},
    'FA.A' : {'phen': 'FA',
              'sig': 'A',
              'hdln': 'Flood Watch'},
    'FA.W' : {'phen': 'FA',
              'sig': 'W',
              'hdln': 'Areal Flood Warning'},
    'FA.Y' : {'phen': 'FA',
              'sig': 'Y',
              'hdln': 'Areal Flood Advisory'},
    'FF.A' : {'phen': 'FF',
              'sig': 'A',
              'hdln': 'Flash Flood Watch'},
    'FF.W' : {'phen': 'FF',
              'sig': 'W',
              'hdln': 'Flash Flood Warning'},
    'FG.Y' : {'phen': 'FG',
              'sig': 'Y',
              'hdln': 'Dense Fog Advisory'},
    'FL.A' : {'phen': 'FL',
              'sig': 'A',
              'hdln': 'Flood Watch'},
    'FL.W' : {'phen': 'FL',
              'sig': 'W',
              'hdln': 'Flood Warning'},
    'FL.Y' : {'phen': 'FL',
              'sig': 'Y',
              'hdln': 'Flood Advisory'},
    'FR.Y' : {'phen': 'FR',
              'sig': 'Y',
              'hdln': 'Frost Advisory'},
    'FW.A' : {'phen': 'FW',
              'sig': 'A',
              'hdln': 'Fire Weather Watch'},
    'FW.W' : {'phen': 'FW',
              'sig': 'W',
              'hdln': 'Red Flag Warning'},
    'FZ.A' : {'phen': 'FZ',
              'sig': 'A',
              'hdln': 'Freeze Watch'},
    'FZ.W' : {'phen': 'FZ',
              'sig': 'W',
              'hdln': 'Freeze Warning'},
    'GL.A' : {'phen': 'GL',
              'sig': 'A',
              'hdln': 'Gale Watch'},
    'GL.W' : {'phen': 'GL',
              'sig': 'W',
              'hdln': 'Gale Warning'},
    'HF.A' : {'phen': 'HF',
              'sig': 'A',
              'hdln': 'Hurricane Force Wind Watch'},
    'HF.W' : {'phen': 'HF',
              'sig': 'W',
              'hdln': 'Hurricane Force Wind Warning'},
    'HT.Y' : {'phen': 'HT',
              'sig': 'Y',
              'hdln': 'Heat Advisory'},
    'HU.A' : {'phen': 'HU',
              'sig': 'A',
              'hdln': 'Hurricane Watch'},
    'HU.S' : {'phen': 'HU',
              'sig': 'S',
              'hdln': ''},    #No headline for this VTEC
    'HU.W' : {'phen': 'HU',
              'sig': 'W',
              'hdln': 'Hurricane Warning'},
    'HW.A' : {'phen': 'HW',
              'sig': 'A',
              'hdln': 'High Wind Watch'},
    'HW.W' : {'phen': 'HW',
              'sig': 'W',
              'hdln': 'High Wind Warning'},
    'HZ.A' : {'phen': 'HZ',
              'sig': 'A',
              'hdln': 'Hard Freeze Watch'},
    'HZ.W' : {'phen': 'HZ',
              'sig': 'W',
              'hdln': 'Hard Freeze Warning'},
    'IS.W' : {'phen': 'IS',
              'sig': 'W',
              'hdln': 'Ice Storm Warning'},
    'LE.A' : {'phen': 'LE',
              'sig': 'A',
              'hdln': 'Lake Effect Snow Watch'},
    'LE.W' : {'phen': 'LE',
              'sig': 'W',
              'hdln': 'Lake Effect Snow Warning'},
    'LE.Y' : {'phen': 'LE',
              'sig': 'Y',
              'hdln': 'Lake Effect Snow Advisory'},
    'LO.Y' : {'phen': 'LO',
              'sig': 'Y',
              'hdln': 'Low Water Advisory'},
    'LS.A' : {'phen': 'LS',
              'sig': 'A',
              'hdln': 'Lakeshore Flood Watch'},
    'LS.S' : {'phen': 'LS',
              'sig': 'S',
              'hdln': ''},    #No headline for this VTEC
    'LS.W' : {'phen': 'LS',
              'sig': 'W',
              'hdln': 'Lakeshore Flood Warning'},
    'LS.Y' : {'phen': 'LS',
              'sig': 'Y',
              'hdln': 'Lakeshore Flood Advisory'},
    'LW.Y' : {'phen': 'LW',
              'sig': 'Y',
              'hdln': 'Lake Wind Advisory'},
    'MA.S' : {'phen': 'MA',
              'sig': 'S',
              'hdln': ''},    #No headline for this VTEC
    'MA.W' : {'phen': 'MA',
              'sig': 'W',
              'hdln': 'Special Marine Warning'},
    'MF.Y' : {'phen': 'MF',
              'sig': 'Y',
              'hdln': 'Dense Fog Advisory'}, # Marine Fog
    'MH.W' : {'phen': 'MH',
              'sig': 'W',
              'hdln': 'Ashfall Warning'}, # Marine Ashfall
    'MH.Y' : {'phen': 'MH',
              'sig': 'Y',
              'hdln': 'Ashfall Advisory'}, # Marine Ashfall
    'MS.Y' : {'phen': 'MS',
              'sig': 'Y',
              'hdln': 'Dense Smoke Advisory'}, # Marine Smoke
    'RB.Y' : {'phen': 'RB',
              'sig': 'Y',
              'hdln': 'Small Craft Advisory for rough bar'},
    'RP.S' : {'phen': 'RP',
              'sig' : 'S',
              'hdln': 'High Rip Current Risk'}, #DR 21037 change
    'SC.Y' : {'phen': 'SC',
              'sig': 'Y',
              'hdln': 'Small Craft Advisory'},
    'SE.A' : {'phen': 'SE',
              'sig': 'A',
              'hdln': 'Hazardous Seas Watch'},
    'SE.W' : {'phen': 'SE',
              'sig': 'W',
              'hdln': 'Hazardous Seas Warning'},
    'SI.Y' : {'phen': 'SI',
              'sig': 'Y',
              'hdln': 'Small Craft Advisory for winds'},
    'SM.Y' : {'phen': 'SM',
              'sig': 'Y',
              'hdln': 'Dense Smoke Advisory'},
    'SQ.W': {'phen': 'SQ',
             'sig': 'W',
             'hdln': 'Snow Squall Warning'},
    'SR.A' : {'phen': 'SR',
              'sig': 'A',
              'hdln': 'Storm Watch'},
    'SR.W' : {'phen': 'SR',
              'sig': 'W',
              'hdln': 'Storm Warning'},
    'SS.A' : {'phen': 'SS',
              'sig': 'A',
              'hdln': 'Storm Surge Watch'},
    'SS.W' : {'phen': 'SS',
              'sig': 'W',
              'hdln': 'Storm Surge Warning'},
    'SU.W' : {'phen': 'SU',
              'sig': 'W',
              'hdln': 'High Surf Warning'},
    'SU.Y' : {'phen': 'SU',
              'sig': 'Y',
              'hdln': 'High Surf Advisory'},             
    'SV.A' : {'phen': 'SV',
              'sig': 'A',
              'hdln': 'Severe Thunderstorm Watch'},
    'SV.W' : {'phen': 'SV',
              'sig': 'W',
              'hdln': 'Severe Thunderstorm Warning'},
    'SW.Y' : {'phen': 'SW',
              'sig': 'Y',
              'hdln': 'Small Craft Advisory for hazardous seas'},
    'TO.A' : {'phen': 'TO',
              'sig': 'A',
              'hdln': 'Tornado Watch'},
    'TO.W' : {'phen': 'TO',
              'sig': 'W',
              'hdln': 'Tornado Warning'},
    'TR.A' : {'phen': 'TR',
              'sig': 'A',
              'hdln': 'Tropical Storm Watch'},
    'TR.W' : {'phen': 'TR',
              'sig': 'W',
              'hdln': 'Tropical Storm Warning'},
    'TS.A' : {'phen': 'TS',
              'sig': 'A',
              'hdln': 'Tsunami Watch'},
    'TS.W' : {'phen': 'TS',
              'sig': 'W',
              'hdln': 'Tsunami Warning'},
    'TS.Y' : {'phen': 'TS',
              'sig': 'Y',
              'hdln': 'Tsunami Advisory'},
    'TY.A' : {'phen': 'TY',
              'sig': 'A',
              'hdln': 'Typhoon Watch'},
    'TY.W' : {'phen': 'TY',
              'sig': 'W',
              'hdln': 'Typhoon Warning'},
    'UP.A' : {'phen': 'UP',
              'sig': 'A',
              'hdln': 'Heavy Freezing Spray Watch'},
    'UP.W' : {'phen': 'UP',
              'sig': 'W',
              'hdln': 'Heavy Freezing Spray Warning'},
    'UP.Y' : {'phen': 'UP',
              'sig': 'Y',
              'hdln': 'Freezing Spray Advisory'},
    'WC.A' : {'phen': 'WC',
              'sig': 'A',
              'hdln': 'Wind Chill Watch'},
    'WC.W' : {'phen': 'WC',
              'sig': 'W',
              'hdln': 'Wind Chill Warning'},
    'WC.Y' : {'phen': 'WC',
              'sig': 'Y',
              'hdln': 'Wind Chill Advisory'},
    'WI.Y' : {'phen': 'WI',
              'sig': 'Y',
              'hdln': 'Wind Advisory'},
    'WS.A' : {'phen': 'WS',
              'sig': 'A',
              'hdln': 'Winter Storm Watch'},
    'WS.W' : {'phen': 'WS',
              'sig': 'W',
              'hdln': 'Winter Storm Warning'},
    'WW.Y' : {'phen': 'WW',
              'sig': 'Y',
              'hdln': 'Winter Weather Advisory'},
    'ZF.Y' : {'phen': 'ZF',
              'sig': 'Y',
              'hdln': 'Freezing Fog Advisory'},
    'ZR.Y' : {'phen': 'ZR',
              'sig': 'Y',
              'hdln': 'Freezing Rain Advisory'},
    }

#
# Upgrade Hazards Dictionary - upgradeHazardsDict is a dictionary of 
# phen/sig combinations defining upgrades. Each key is the proposed hazard. 
# The associated list are the hazards which are upgraded by the 
# proposed hazard.
#

upgradeHazardsDict = {
'WC.W': ['WC.A', 'WC.Y'], 
'WC.Y': ['WC.A'],
'BZ.W': ['WS.W', 'LE.W', 'ZR.Y', 'LE.Y', 'WW.Y',
         'BZ.A', 'WS.A', 'LE.A'],
'IS.W': ['WS.W', 'LE.W', 'ZR.Y', 'LE.Y', 'WW.Y',
         'BZ.A', 'WS.A', 'LE.A'],
'LE.W': ['ZR.Y', 'LE.Y', 'WW.Y',
         'BZ.A', 'WS.A', 'LE.A'],
'WS.W': ['ZR.Y', 'LE.Y', 'WW.Y',
         'BZ.A', 'WS.A', 'LE.A'],
'ZR.Y': ['BZ.A', 'WS.A', 'LE.A'],
'LE.Y': ['BZ.A', 'WS.A', 'LE.A'],
'WW.Y': ['BZ.A', 'WS.A', 'LE.A'],
'EH.W': ['EH.A', 'HT.Y'],
'HT.Y': ['EH.A'],
'FZ.W': ['FZ.A', 'FR.Y', 'HZ.A'],
'HZ.W': ['FZ.A', 'FR.Y', 'HZ.A'],
'FR.Y': ['FZ.A', 'HZ.A'],
'HW.W': ['DU.Y', 'LW.Y', 'WI.Y', 'HW.A'],
'DU.W': ['DU.Y', 'LW.Y', 'WI.Y', 'HW.A'],
'WI.Y': ['HW.A'],
'EC.W': ['EC.A'],
'FW.W': ['FW.A'],
'CF.W': ['CF.A', 'CF.Y'],
'CF.Y': ['CF.A'],
'LS.W': ['LS.A', 'LS.Y'],
'LS.Y': ['LS.A'],
'BW.Y': ['GL.A', 'SR.A', 'HF.A', 'SE.A'],
'RB.Y': ['GL.A', 'SR.A', 'HF.A', 'SE.A'],
'SC.Y': ['GL.A', 'SR.A', 'HF.A', 'SE.A'],
'SI.Y': ['GL.A', 'SR.A', 'HF.A', 'SE.A'],
'SW.Y': ['SE.A'],
'UP.Y': ['UP.A'],
'HF.W': ['SR.W', 'GL.W', 'SC.Y', 'SW.Y', 'BW.Y', 'SI.Y', 'RB.Y', 'GL.A', 'SR.A', 'HF.A', 'SE.A'],
'SR.W': ['GL.W', 'SC.Y', 'SW.Y', 'BW.Y', 'SI.Y', 'RB.Y', 'GL.A', 'SR.A', 'HF.A', 'SE.A'],
'GL.W': ['SC.Y', 'SW.Y', 'BW.Y', 'SI.Y', 'RB.Y', 'GL.A', 'SR.A', 'HF.A', 'SE.A'],
'SE.W': ['SC.Y', 'RB.Y', 'GL.A', 'SR.A', 'HF.A', 'SE.A'],
'UP.W': ['UP.Y', 'UP.A'],
'SU.W': ['SU.Y'],
'SS.W': ['SS.A'],
'HU.W': ['HU.A', 'TR.W', 'TR.A'],
'HU.A': ['TR.A'],
'TR.W': ['TR.A', 'HU.A', 'TY.A'],
'TY.W': ['TY.A', 'TR.W', 'TR.A'],
'TY.A': ['TR.A'],
'AF.W': ['AF.Y'],
'MH.W': ['MH.Y'],
 }
 
#
# When passed a phen/sig for both the current hazard and the proposed hazard,
# checkForUpgrade returns a 1 if the proposed hazard is an upgrade, otherwise 0
#

def checkForUpgrade(pPhen, pSig, cPhen, cSig):
    proposed = pPhen + "." + pSig
    current = cPhen + "." + cSig
    if proposed in upgradeHazardsDict:
        if current in upgradeHazardsDict[proposed]:
            return 1
        else:
            return 0
    else:
        return 0

#
# Downgrade Hazards Dictionary - downgradeHazardsDict is a dictionary of 
# phen/sig combinations defining downgrades. Each key is the proposed hazard. 
# The associated list are the hazards which are downgraded by the 
# proposed hazard.
#

downgradeHazardsDict = {
'ZR.Y': ['BZ.W', 'LE.W', 'IS.W', 'WS.W'],
'LE.Y': ['BZ.W', 'LE.W', 'IS.W', 'WS.W'],
'WW.Y': ['BZ.W', 'LE.W', 'IS.W', 'WS.W'],
'WC.Y': ['WC.W'],
'WS.W': ['BZ.W', 'IS.W'],
'LE.W': ['BZ.W', 'IS.W'],
'DU.Y': ['DU.W', 'HW.W'],
'LW.Y': ['DU.W', 'HW.W', 'WI.Y'],
'WI.Y': ['DU.W', 'HW.W'],
'HT.Y': ['EH.W'],
'FR.Y': ['FZ.W', 'HZ.W'],
'TR.W': ['HU.W', 'TY.W'],
'UP.Y': ['UP.W'],
'SR.W': ['HF.W'],
'GL.W': ['HF.W', 'SR.W'],
'SC.Y': ['HF.W', 'SR.W', 'GL.W', 'SE.W'],
'SW.Y': ['SE.W'],
'RB.Y': ['HF.W', 'SR.W', 'GL.W', 'SE.W'],
'SU.Y': ['SU.W'],
'BW.Y': ['HF.W', 'SR.W', 'GL.W'],
'SI.Y': ['HF.W', 'SR.W', 'GL.W'],
'LS.Y': ['LS.W'],
'CF.Y': ['CF.W'],
'AF.Y': ['AF.W'],
'MH.Y': ['MH.W'],
}

#
# When passed a phen/sig for both the current hazard and the proposed hazard,
# checkForDowngrade returns a 1 if the proposed hazard is an downgrade, otherwise 0
#

def checkForDowngrade(pPhen, pSig, cPhen, cSig):
    proposed = pPhen + "." + pSig
    current = cPhen + "." + cSig
    if proposed in downgradeHazardsDict:
        if current in downgradeHazardsDict[proposed]:
            return 1
        else:
            return 0
    else:
        return 0
