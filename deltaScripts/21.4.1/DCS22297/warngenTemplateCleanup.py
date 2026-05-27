#!/bin/python

########################################################################
#
# warngenTemplateCleanup.py
#
# Description: With DCS22297, several old convective WarnGen template
# files have been removed. This script will notify you of any overrides
# that exist and ask if you wish to backup and delete them. This script
# will also update any override to warngen/config.xml.
#
# Backups will be stored in /tmp/DCS_22297_YYYYMMDD_HH where
# YYYYMMDD_HH is the current system time down to the hour.
#
########################################################################

import xml.etree.ElementTree as ET
import os, shutil
from datetime import datetime
from sys import exit, argv
from glob import glob
from getpass import getuser

def confirmMessage(filePath, msgType="Delete"):
    '''
    Method to solicit input from the user
    '''
    answer = ""
    if msgType == "Delete":
        msg = f"Do you wish to backup delete the override(s) at\n{filePath}.* (Y/N)? "
    elif msgType == "Modify":
        msg = f"Do you want the script to backup and update the file located at\n{filePath} (Y/N)? "
    elif msgType == "Rename":
        msg = f"Do you want the script to rename this file to be {filePath} (Y/N)? "
    while answer not in ["y", "n"]:
      answer = input(msg).lower()
    return answer == "y"

def backupFile(inFile, backupDir):
    '''
    Method that will back up a file into the backup directory
    (e.g., /tmp/DCS22297_YYYYMMD_HH)
    '''
    fileDir = os.path.dirname(inFile)
    fileName = os.path.basename(inFile)

    # Get path to relative directory (e.g., common_static/site/OAX/warngen)
    relDir = fileDir.split("/utility/")
    if len(relDir) > 1:
        relDir = relDir[1]
    else:
        print(f"Could not backup {inFile}")
        return False

    # Build relative path in the backup directory
    outDir = os.path.join(backupDir, relDir)
    os.makedirs(outDir, 0o755, exist_ok=True)
    if not os.path.exists(outDir):
        print(f"Could not create output directory for backup: {outDir}")
        return False

    # Backup file to be deleted or modified
    outPath = os.path.join(outDir, fileName)
    shutil.copy(inFile, outPath)

    # Verify file was copied
    if not os.path.exists(outPath):
        print(f"{fileName} was unable to be copied to {outDir}")
        return False

    # Backup occurred successfully
    print(f" Backed up {fileName} to {outDir}")
    return True

def deletePath(inFile):
    '''
    Method to delete a file or path
    '''
    if os.path.isdir(inFile):
        shutil.rmtree(inFile)
    else:
        os.remove(inFile)
    if not os.path.exists(inFile):
        print(f"  Deleted {inFile}")

# Path to the common_static directory
utilityRootDir = "/awips2/edex/data/utility"
commonStaticRootDir = os.path.join(utilityRootDir, "common_static")

# Verify the path to common_static exists
if not os.path.isdir(commonStaticRootDir):
    print(f"{commonStaticRootDir} is not a directory; can't run this delta script")
    exit()

# Verify user is either awips or root
if getuser() != "awips":
    print("This script should be run as the awips user")
    exit()

# Define and create path to the backup directory
currentTime = datetime.now().strftime("%Y%m%d_%H")
backupDir = f"/tmp/DCS22297_{currentTime}"
os.makedirs(backupDir, 0o755, exist_ok=True)
if not os.path.exists(backupDir):
    print(f"Could not create archive directory here: {backupDir}")
    print("Please check write permissions and try script again.")
    exit()
else:
    print(f"Created {backupDir} to store backup files")

# Relative paths to the directories of interest
warnGenFormattersPath = os.path.join("warngen")

# Files to find and delete
warnGenFileHeaders = ["tornadoWarning", "severeThunderstormWarning",
                      "severeWeatherStatement", "specialMarineWarning",
                      "specialMarineWarningFollowup", "significantWeatherAdvisory",
                      "specialWeatherStatement"]

# How old templates maps to new templates
impactMapDict = { "tornadoWarning" : "impactTornadoWarning",
                  "severeThunderstormWarning" : "impactSevereThunderstormWarning",
                  "severeWeatherStatement" : "impactSevereWeatherStatement",
                  "specialMarineWarning" : "impactSpecialMarineWarning",
                  "specialMarineWarningFollowup" : "impactSpecialMarineWarningFollowup",
                  "significantWeatherAdvisory" : "impactSpecialWeatherStatement",
                  "specialWeatherStatement" : "impactSpecialWeatherStatement",
                }

############################
# WarnGen formatter checking
############################
warnGenRootDirs = glob(os.path.join(commonStaticRootDir, "*", "*", warnGenFormattersPath))
for warnGenPath in warnGenRootDirs:

    # Check and move impactSignificantWeatherAdvisory to impactSpecialWeatherStatement
    for ext in [".vm",".xml"]:
        sigPath = os.path.join(warnGenPath,f"impactSignificantWeatherAdvisory{ext}")
        if os.path.exists(sigPath):
            print("=========================================")
            print(f"{sigPath} found...")
            newPath = os.path.join(warnGenPath,f"impactSpecialWeatherStatement{ext}")
            print(f"impactSignificantWeatherAdvisory{ext} was renamed to impactSpecialWeatherStatement{ext} at base-level.")
            confirmRename = confirmMessage(newPath,"Rename")
            if confirmRename:
                shutil.move(sigPath,newPath)

    # Check and delete old .vm/.xml files
    for warnGenHeader in warnGenFileHeaders:
        tmpPath = os.path.join(warnGenPath, warnGenHeader)
        matchingFiles = sorted(glob(tmpPath+".*"))
        if matchingFiles:
            print("=========================================")
            print(f"{warnGenHeader} is deleted at the base-level and is no longer used.")
            confirmDelete = confirmMessage(tmpPath)
            if confirmDelete:
                 for tmpFile in matchingFiles:
                     fileBackup = backupFile(tmpFile, backupDir)
                     if fileBackup:
                         deletePath(tmpFile)
            else:
                numMatchingFiles = len(matchingFiles)
                print(f"{numMatchingFiles} files associated with {warnGenHeader} not deleted")

    # Ask to update config.xml
    warnGenConfigPath = os.path.join(warnGenPath, "config.xml")
    if os.path.exists(warnGenConfigPath):
        print("=========================================")
        print(f"A override of config.xml was found in {warnGenPath}.")
        # Verify if user wants to update config.xml
        confirmUpdate = confirmMessage(warnGenConfigPath, msgType="Modify")
        if confirmUpdate:
            tree = ET.parse(warnGenConfigPath)
            root = tree.getroot()
            fileChanged = False
            for child in root:
                text = child.text
                if child.tag == "defaultTemplate":
                   fileName = text.strip()
                   if fileName in impactMapDict:
                       newFileName = impactMapDict.get(fileName)
                       print(f" No {fileName} vm/xml files exist at the base-level, updating <defaultTemplate> entry to be {newFileName}.")
                       child.text = newFileName
                       fileChanged = True
                # Only focus on the mainWarngenProducts and otherWarngenProducts tags
                if child.tag in ["mainWarngenProducts", "otherWarngenProducts"]:
                    # List out each WarnGen entry (e.g. Flash Flood Warning/flashFloodWarning)
                    entryList = text.split(",")
                    newEntryList = []
                    modifiedEntry = False
                    for entry in entryList:
                        # Remove any excess leading/trailing whitespace
                        entry = entry.strip()
                        # Split up at the forward slash to get the GUI label and configuration file name
                        if "/" in entry:
                            label, fileName = entry.split("/")[0:2]
                            # Remove any excess leading/trailing whitespace
                            fileName = fileName.strip()
                            # If entry lists a base-level file that was removed, do not add it to the new tag text
                            if fileName in impactMapDict:
                                newFileName = impactMapDict.get(fileName)
                                entry = f"{label}/{newFileName}"
                                print(f" No {fileName} vm/xml files exist at the base-level, updating this entry to be {entry}.")
                                modifiedEntry = True
                        # Add entry to be updated in the tag text
                        newEntryList.append(entry)
                    # Update tag in the XML file
                    if modifiedEntry:
                        child.text = ",".join(newEntryList)
                        fileChanged = True
            # If anything was changed, rewrite the file
            if fileChanged:
                # Attempt to backup the file
                fileBackup = backupFile(warnGenConfigPath, backupDir)
                if fileBackup:
                    print(f"Writing updated {warnGenConfigPath}")
                    tree.write(warnGenConfigPath)
            else:
                print(f"No updates needed for {warnGenConfigPath}.")

# Check if any files were backed up, if not, delete the backup folder
backupFiles = glob(os.path.join(backupDir, "*"))
if not backupFiles:
    print("=========================================")
    print(f"No files backed up into {backupDir}, deleting folder.")
    deletePath(backupDir)
print("=========================================")
print(f"{argv[0]} Completed.")
