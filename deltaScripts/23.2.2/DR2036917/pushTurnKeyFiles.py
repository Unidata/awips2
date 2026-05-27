#!/awips2/python/bin/python

########################################################################
#
# pushTurnKeyFiles.py -s arg1
#
# Input Arguments:
# arg1 = The site ID pulled from ${AW_SITE_IDENTIFIER} environment
#        variable
#
# Description: There is a hazard simplification effort surrounding
# Non-Precipitation Weather (NPW) heat hazards. This script activates
# the HazSimp heat components inside GFE.
#
# Summary of Changes:
#   - Replace EH.A --> XH.A and EH.W --> XH.W in gfeConfig.py
#   - Replace EH.A --> XH.A and EH.W --> XH.W in the "Non-Precipitation"
#     category in MakeHazardConfig.py
#
########################################################################

import argparse
import glob
import os
import re
import shutil
import sys


def getRegion(wfoID):
    '''
    @summary: Determine the NWS region from the WFO site ID
    @param wfoID: The site ID (e.g., OAX)
    @return: String
    '''
    siteRegionDict = {
        "AR": ["AFG", "AJK", "ALU", "AER", "ACR", "AFC",
               "VRH", "AAWU", "AVAK"],
        "CR": ["ABR", "APX", "ARX", "BIS", "BOU", "CYS",
               "DDC", "DLH", "DMX", "DTX", "DVN", "EAX",
               "FGF", "FSD", "GID", "GJT", "GLD", "GRB",
               "GRR", "ICT", "ILX", "IND", "IWX", "JKL",
               "LBF", "LMK", "LOT", "LSX", "MKX", "MPX",
               "MQT", "OAX", "PAH", "PUB", "RIW", "SGF",
               "TOP", "UNR"],
        "ER": ["AKQ", "ALY", "BGM", "BOX", "BTV", "BUF",
               "CAE", "CAR", "CHS", "CLE", "CTP", "GSP",
               "GYX", "ILM", "ILN", "LWX", "MHX", "OKX",
               "PBZ", "PHI", "RAH", "RLX", "RNK"],
        "PR": ["GUM", "HFO", "PBP", "PPG", "PQW", "PQE"],
        "SR": ["ABQ", "AMA", "BMX", "BRO", "CRP", "EPZ",
               "EWX", "FFC", "FWD", "HGX", "HUN", "JAN",
               "JAX", "KEY", "LCH", "LIX", "LUB", "LZK",
               "MAF", "MEG", "MFL", "MLB", "MOB", "MRX",
               "OHX", "OUN", "SHV", "SJT", "SJU", "TAE",
               "TBW", "TSA"],
        "WR": ["BOI", "BYZ", "EKA", "FGZ", "GGW", "HNX",
               "LKN", "LOX", "MFR", "MSO", "MTR", "OTX",
               "PDT", "PIH", "PQR", "PSR", "REV", "SEW",
               "SGX", "SLC", "STO", "TFX", "TWC", "VEF"]
        }

    nwsRegion = None
    for region in siteRegionDict:
        if wfoID in siteRegionDict[region]:
            nwsRegion = region
            break
    return nwsRegion


def confirmMessage(fileName, filePath):
    '''
    @summary: Solicit input from the user on whether to take action
    on a file in a folder
    @param fileName: The name of the file
    @param filePath: The path to the file
    @return: Boolean
    '''
    answer = ""
    msg = f"Do you want this script to create a {fileName} override at \n{filePath} (Y/N)? "
    while answer not in ["y", "n"]:
      answer = input(msg).lower()
    return answer == "y"


def deletePath(inputPath):
    '''
    @summary: Deletes either a file or a directory
    @param inputPath: An input file or directory
    @return: None
    '''
    if os.path.isdir(inputPath):
        shutil.rmtree(inputPath)
    else:
        os.remove(inputPath)
    if not os.path.exists(inputPath):
        print(f"  Deleted {inputPath}")


def printMessageBreak():
    '''
    @summary: Prints a message break to the screen
    @return: None
    '''
    print("=========================================")

##############
# Main Method
##############
parser = argparse.ArgumentParser()
parser.add_argument("-s", "--site", dest="sitePath", help="AWIPS site ID (e.g., OAX)", required=True)
args = parser.parse_args()

# Get the site ID from the script input
wfoID = args.sitePath.upper()

# Path to the current directory
scriptDirectory = os.path.dirname(os.path.realpath(__file__))

# Path to the cave_static directory
utilityRootDir = os.path.join(os.sep, "awips2", "edex", "data", "utility")
caveStaticRootDir = os.path.join(utilityRootDir, "cave_static")

# Verify the path to cave_static exists
if not os.path.isdir(caveStaticRootDir):
    print(f"{caveStaticRootDir} is not a directory; can't run this delta script")
    sys.exit()

# Get the NWS region from the WFO ID
nwsRegion = getRegion(wfoID)
awipsSetupFile = os.path.join(os.sep, "awips2", "edex", "bin", "setup.env")
if not nwsRegion:
    print(f"No region found for WFO {wfoID}, please check the AW_SITE_IDENTIFIER variable in {awipsSetupFile}.")
    sys.exit()

# Path to the turnkey files
npwHazSimpFolder = os.path.join(scriptDirectory, "GFE_NPW_HazSimp_Heat")

# Script constants
gfeConfigScript = "gfeConfig.py"
makeHazardConfigScript = "MakeHazardConfig.py"

##################################################################
# Make region-level override of gfeConfig.py
##################################################################
printMessageBreak()
regionOverrideDir = os.path.join(caveStaticRootDir, "region", nwsRegion,
                                 "gfe", "userPython", "gfeConfig")
gfeConfigRegionFile = os.path.join(regionOverrideDir, gfeConfigScript)
if not os.path.exists(regionOverrideDir):
    os.makedirs(regionOverrideDir, 0o755, exist_ok=True)
    if not os.path.exists(regionOverrideDir):
        print(f"Could not create region override directory: {regionOverrideDir}")
        sys.exit()
fileToCopy = os.path.join(npwHazSimpFolder, gfeConfigScript)
confirmUpdate = confirmMessage(gfeConfigScript, regionOverrideDir)
if confirmUpdate:
    shutil.copy(fileToCopy, regionOverrideDir)

##################################################################
# Make region-level override of MakeHazardConfig.py
##################################################################
printMessageBreak()
regionOverrideDir = os.path.join(caveStaticRootDir, "region", nwsRegion,
                                 "gfe", "userPython", "utilities")
makeHazardRegionFile = os.path.join(regionOverrideDir, makeHazardConfigScript)
if not os.path.exists(regionOverrideDir):
    os.makedirs(regionOverrideDir, 0o755, exist_ok=True)
    if not os.path.exists(regionOverrideDir):
        print(f"Could not create region override directory: {regionOverrideDir}")
        sys.exit()
fileToCopy = os.path.join(npwHazSimpFolder, makeHazardConfigScript)
confirmUpdate = confirmMessage(makeHazardConfigScript, regionOverrideDir)
if confirmUpdate:
    shutil.copy(fileToCopy, regionOverrideDir)

##################################################################
# Notify user to update any overrides to gfeConfig.py
##################################################################
printMessageBreak()
print("Checking for changes that need to be made to python files in cave_static/*/*/gfe/userPython/gfeConfig/*.py")
gfeConfigOverrides = glob.glob(os.path.join(caveStaticRootDir, "*", "*", "gfe", "userPython", "gfeConfig", "*.py"))
for localOverride in gfeConfigOverrides:
    # Skip newly copied region level override
    if localOverride == gfeConfigRegionFile:
        continue

    printMessageBreak()
    allClear = True
    # Open file and parse lines
    f = open(localOverride, "r")

    # If Hazards_commonValues is found, then throw message to user to check for changes to be made
    for line in f:
        matchesFound = re.search("Hazards_commonValues", line)
        if matchesFound:
            allClear = False
            print(f"You need to verify the following changes have been made to:\n{localOverride}\n",
                    "within the Hazards_commonValues entry:")
            print("1. You need to rename the EH.W/EH.A entries to XH.W/XH.A:\n",
                  "For Example:\n",
                  "Watches|Non-Precipitation|EH.A --> Watches|Non-Precipitation|XH.A\n",
                  "Warnings|Non-Precipitation|EH.W --> Warnings|Non-Precipitation|XH.W")
            break
    # Close out of the file
    f.close()

    if allClear:
       print(f"{localOverride} is compatible\nwith the heat updates...no further action needed.")

##################################################################
# Notify user to update any overrides to MakeHazardConfig.py
##################################################################
printMessageBreak()
print("Checking for overrides to MakeHazardConfig.py")
makeHazardOverrides = glob.glob(os.path.join(caveStaticRootDir, "*", "*", "gfe", "userPython", "utilities", makeHazardConfigScript))
for localOverride in makeHazardOverrides:
    # Skip newly copied region level override
    if localOverride == makeHazardRegionFile:
        continue

    print(f"\nFound {localOverride}...")
    allClear = True

    # Open file and parse lines
    f = open(localOverride, "r")
    lineList = f.read()
    lineList = lineList.split("\n")
    f.close()

    # Walk through each line in the file
    for i in range(len(lineList)):
        line = lineList[i]
        lineNum = i + 1
        for heatHazard in ["EH.A", "EH.W"]:
            if heatHazard in line:
                heatPhen, heatSig = heatHazard.split(".")
                print(f"{heatHazard} found on Line #{lineNum}: This hazardDict entry needs to be renamed to XH.{heatSig}")
                allClear = False


    if allClear:
       print(f"{localOverride} is compatible with\nthe heat updates...no further action needed.")
