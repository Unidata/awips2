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
              'hdln': 'ASHFALL WARNING'},
    'AF.Y' : {'phen': 'AF',
              'sig': 'Y',
              'hdln': 'ASHFALL ADVISORY'},
    'AQ.Y' : {'phen': 'AQ',
              'sig': 'Y',
              'hdln': 'AIR QUALITY ALERT'},
    'AS.O' : {'phen': 'AS',
              'sig': 'O',
              'hdln': 'AIR STAGNATION OUTLOOK'},
    'AS.Y' : {'phen': 'AS',
              'sig': 'Y',
              'hdln': 'AIR STAGNATION ADVISORY'},
    'BH.S' : {'phen': 'BH',
              'sig': 'S',
              'hdln': 'BEACH HAZARDS STATEMENT'},
    'BW.Y' : {'phen': 'BW',
              'sig': 'Y',
              'hdln': 'BRISK WIND ADVISORY'},
    'BZ.A' : {'phen' : 'BZ',
              'sig' : 'A',
              'hdln' : 'BLIZZARD WATCH'},
    'BZ.W' : {'phen' : 'BZ',
              'sig' : 'W',
              'hdln' : 'BLIZZARD WARNING'},
    'CF.A' : {'phen': 'CF',
              'sig': 'A',
              'hdln': 'COASTAL FLOOD WATCH'},
    'CF.W' : {'phen': 'CF',
              'sig': 'W',
              'hdln': 'COASTAL FLOOD WARNING'},
    'CF.Y' : {'phen': 'CF',
              'sig': 'Y',
              'hdln': 'COASTAL FLOOD ADVISORY'},
    'CF.S' : {'phen': 'CF',
              'sig': 'S',
              'hdln': ''},  #No headline for this VTEC
    'DS.W' : {'phen': 'DS',
              'sig': 'W',
              'hdln': 'DUST STORM WARNING'},
    'DU.Y' : {'phen': 'DU',
              'sig': 'Y',
              'hdln': 'BLOWING DUST ADVISORY'},
    'EC.A' : {'phen': 'EC',
              'sig': 'A',
              'hdln': 'EXTREME COLD WATCH'},
    'EC.W' : {'phen': 'EC',
              'sig': 'W',
              'hdln': 'EXTREME COLD WARNING'},
    'EH.A' : {'phen': 'EH',
              'sig': 'A',
              'hdln': 'EXCESSIVE HEAT WATCH'},
    'EH.W' : {'phen': 'EH',
              'sig': 'W',
              'hdln': 'EXCESSIVE HEAT WARNING'},
    'EW.W' : {'phen': 'EW',
              'sig': 'W',
              'hdln': 'EXCESSIVE WIND WARNING'},
    'FA.A' : {'phen': 'FA',
              'sig': 'A',
              'hdln': 'FLOOD WATCH'},
    'FA.W' : {'phen': 'FA',
              'sig': 'W',
              'hdln': 'AREAL FLOOD WARNING'},
    'FA.Y' : {'phen': 'FA',
              'sig': 'Y',
              'hdln': 'AREAL FLOOD ADVISORY'},
    'FF.A' : {'phen': 'FF',
              'sig': 'A',
              'hdln': 'FLASH FLOOD WATCH'},
    'FF.W' : {'phen': 'FF',
              'sig': 'W',
              'hdln': 'FLASH FLOOD WARNING'},
    'FG.Y' : {'phen': 'FG',
              'sig': 'Y',
              'hdln': 'DENSE FOG ADVISORY'},
    'FL.A' : {'phen': 'FL',
              'sig': 'A',
              'hdln': 'FLOOD WATCH'},
    'FL.W' : {'phen': 'FL',
              'sig': 'W',
              'hdln': 'FLOOD WARNING'},
    'FL.Y' : {'phen': 'FL',
              'sig': 'Y',
              'hdln': 'FLOOD ADVISORY'},
    'FR.Y' : {'phen': 'FR',
              'sig': 'Y',
              'hdln': 'FROST ADVISORY'},
    'FW.A' : {'phen': 'FW',
              'sig': 'A',
              'hdln': 'FIRE WEATHER WATCH'},
    'FW.W' : {'phen': 'FW',
              'sig': 'W',
              'hdln': 'RED FLAG WARNING'},
    'FZ.A' : {'phen': 'FZ',
              'sig': 'A',
              'hdln': 'FREEZE WATCH'},
    'FZ.W' : {'phen': 'FZ',
              'sig': 'W',
              'hdln': 'FREEZE WARNING'},
    'GL.A' : {'phen': 'GL',
              'sig': 'A',
              'hdln': 'GALE WATCH'},
    'GL.W' : {'phen': 'GL',
              'sig': 'W',
              'hdln': 'GALE WARNING'},
    'HF.A' : {'phen': 'HF',
              'sig': 'A',
              'hdln': 'HURRICANE FORCE WIND WATCH'},
    'HF.W' : {'phen': 'HF',
              'sig': 'W',
              'hdln': 'HURRICANE FORCE WIND WARNING'},
    'HT.Y' : {'phen': 'HT',
              'sig': 'Y',
              'hdln': 'HEAT ADVISORY'},
    'HU.A' : {'phen': 'HU',
              'sig': 'A',
              'hdln': 'HURRICANE WATCH'},
    'HU.S' : {'phen': 'HU',
              'sig': 'S',
              'hdln': ''},    #No headline for this VTEC
    'HU.W' : {'phen': 'HU',
              'sig': 'W',
              'hdln': 'HURRICANE WARNING'},
    'HW.A' : {'phen': 'HW',
              'sig': 'A',
              'hdln': 'HIGH WIND WATCH'},
    'HW.W' : {'phen': 'HW',
              'sig': 'W',
              'hdln': 'HIGH WIND WARNING'},
    'HZ.A' : {'phen': 'HZ',
              'sig': 'A',
              'hdln': 'HARD FREEZE WATCH'},
    'HZ.W' : {'phen': 'HZ',
              'sig': 'W',
              'hdln': 'HARD FREEZE WARNING'},
    'IS.W' : {'phen': 'IS',
              'sig': 'W',
              'hdln': 'ICE STORM WARNING'},
    'LE.A' : {'phen': 'LE',
              'sig': 'A',
              'hdln': 'LAKE EFFECT SNOW WATCH'},
    'LE.W' : {'phen': 'LE',
              'sig': 'W',
              'hdln': 'LAKE EFFECT SNOW WARNING'},
    'LE.Y' : {'phen': 'LE',
              'sig': 'Y',
              'hdln': 'LAKE EFFECT SNOW ADVISORY'},
    'LO.Y' : {'phen': 'LO',
              'sig': 'Y',
              'hdln': 'LOW WATER ADVISORY'},
    'LS.A' : {'phen': 'LS',
              'sig': 'A',
              'hdln': 'LAKESHORE FLOOD WATCH'},
    'LS.S' : {'phen': 'LS',
              'sig': 'S',
              'hdln': ''},    #No headline for this VTEC
    'LS.W' : {'phen': 'LS',
              'sig': 'W',
              'hdln': 'LAKESHORE FLOOD WARNING'},
    'LS.Y' : {'phen': 'LS',
              'sig': 'Y',
              'hdln': 'LAKESHORE FLOOD ADVISORY'},
    'LW.Y' : {'phen': 'LW',
              'sig': 'Y',
              'hdln': 'LAKE WIND ADVISORY'},
    'MA.S' : {'phen': 'MA',
              'sig': 'S',
              'hdln': ''},    #No headline for this VTEC
    'MA.W' : {'phen': 'MA',
              'sig': 'W',
              'hdln': 'SPECIAL MARINE WARNING'},
    'MF.Y' : {'phen': 'MF',
              'sig': 'Y',
              'hdln': 'DENSE FOG ADVISORY'}, # Marine Fog
    'MH.W' : {'phen': 'MH',
              'sig': 'W',
              'hdln': 'ASHFALL WARNING'}, # Marine Ashfall
    'MH.Y' : {'phen': 'MH',
              'sig': 'Y',
              'hdln': 'ASHFALL ADVISORY'}, # Marine Ashfall
    'MS.Y' : {'phen': 'MS',
              'sig': 'Y',
              'hdln': 'DENSE SMOKE ADVISORY'}, # Marine Smoke
    'RB.Y' : {'phen': 'RB',
              'sig': 'Y',
              'hdln': 'SMALL CRAFT ADVISORY FOR ROUGH BAR'},
    'RP.S' : {'phen': 'RP',
              'sig' : 'S',
              'hdln': 'HIGH RIP CURRENT RISK'}, #DR 21037 change
    'SC.Y' : {'phen': 'SC',
              'sig': 'Y',
              'hdln': 'SMALL CRAFT ADVISORY'},
    'SE.A' : {'phen': 'SE',
              'sig': 'A',
              'hdln': 'HAZARDOUS SEAS WATCH'},
    'SE.W' : {'phen': 'SE',
              'sig': 'W',
              'hdln': 'HAZARDOUS SEAS WARNING'},
    'SI.Y' : {'phen': 'SI',
              'sig': 'Y',
              'hdln': 'SMALL CRAFT ADVISORY FOR WINDS'},
    'SM.Y' : {'phen': 'SM',
              'sig': 'Y',
              'hdln': 'DENSE SMOKE ADVISORY'},
    'SR.A' : {'phen': 'SR',
              'sig': 'A',
              'hdln': 'STORM WATCH'},
    'SR.W' : {'phen': 'SR',
              'sig': 'W',
              'hdln': 'STORM WARNING'},
    'SS.A' : {'phen': 'SS',
              'sig': 'A',
              'hdln': 'STORM SURGE WATCH'},
    'SS.W' : {'phen': 'SS',
              'sig': 'W',
              'hdln': 'STORM SURGE WARNING'},
    'SU.W' : {'phen': 'SU',
              'sig': 'W',
              'hdln': 'HIGH SURF WARNING'},
    'SU.Y' : {'phen': 'SU',
              'sig': 'Y',
              'hdln': 'HIGH SURF ADVISORY'},             
    'SV.A' : {'phen': 'SV',
              'sig': 'A',
              'hdln': 'SEVERE THUNDERSTORM WATCH'},
    'SV.W' : {'phen': 'SV',
              'sig': 'W',
              'hdln': 'SEVERE THUNDERSTORM WARNING'},
    'SW.Y' : {'phen': 'SW',
              'sig': 'Y',
              'hdln': 'SMALL CRAFT ADVISORY FOR HAZARDOUS SEAS'},
    'TO.A' : {'phen': 'TO',
              'sig': 'A',
              'hdln': 'TORNADO WATCH'},
    'TO.W' : {'phen': 'TO',
              'sig': 'W',
              'hdln': 'TORNADO WARNING'},
    'TR.A' : {'phen': 'TR',
              'sig': 'A',
              'hdln': 'TROPICAL STORM WATCH'},
    'TR.W' : {'phen': 'TR',
              'sig': 'W',
              'hdln': 'TROPICAL STORM WARNING'},
    'TS.A' : {'phen': 'TS',
              'sig': 'A',
              'hdln': 'TSUNAMI WATCH'},
    'TS.W' : {'phen': 'TS',
              'sig': 'W',
              'hdln': 'TSUNAMI WARNING'},
    'TY.A' : {'phen': 'TY',
              'sig': 'A',
              'hdln': 'TYPHOON WATCH'},
    'TY.W' : {'phen': 'TY',
              'sig': 'W',
              'hdln': 'TYPHOON WARNING'},
    'UP.A' : {'phen': 'UP',
              'sig': 'A',
              'hdln': 'HEAVY FREEZING SPRAY WATCH'},
    'UP.W' : {'phen': 'UP',
              'sig': 'W',
              'hdln': 'HEAVY FREEZING SPRAY WARNING'},
    'UP.Y' : {'phen': 'UP',
              'sig': 'Y',
              'hdln': 'FREEZING SPRAY ADVISORY'},
    'WC.A' : {'phen': 'WC',
              'sig': 'A',
              'hdln': 'WIND CHILL WATCH'},
    'WC.W' : {'phen': 'WC',
              'sig': 'W',
              'hdln': 'WIND CHILL WARNING'},
    'WC.Y' : {'phen': 'WC',
              'sig': 'Y',
              'hdln': 'WIND CHILL ADVISORY'},
    'WI.Y' : {'phen': 'WI',
              'sig': 'Y',
              'hdln': 'WIND ADVISORY'},
    'WS.A' : {'phen': 'WS',
              'sig': 'A',
              'hdln': 'WINTER STORM WATCH'},
    'WS.W' : {'phen': 'WS',
              'sig': 'W',
              'hdln': 'WINTER STORM WARNING'},
    'WW.Y' : {'phen': 'WW',
              'sig': 'Y',
              'hdln': 'WINTER WEATHER ADVISORY'},
    'ZF.Y' : {'phen': 'ZF',
              'sig': 'Y',
              'hdln': 'FREEZING FOG ADVISORY'},
    'ZR.Y' : {'phen': 'ZR',
              'sig': 'Y',
              'hdln': 'FREEZING RAIN ADVISORY'},
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
'BZ.W': ['ZR.Y', 'LE.Y', 'WW.Y',
         'BZ.A', 'WS.A', 'LE.A'],
'IS.W': ['ZR.Y', 'LE.Y', 'WW.Y',
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
'DS.W': ['DU.Y', 'LW.Y', 'WI.Y', 'HW.A'],
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
    if upgradeHazardsDict.has_key(proposed):
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
'DU.Y': ['DS.W', 'HW.W'],
'LW.Y': ['DS.W', 'HW.W', 'WI.Y'],
'WI.Y': ['DS.W', 'HW.W'],
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
    if downgradeHazardsDict.has_key(proposed):
        if current in downgradeHazardsDict[proposed]:
            return 1
        else:
            return 0
    else:
        return 0
