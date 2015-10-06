#!/usr/bin/env python

import errno
import glob
import logging
import os
import shutil
import subprocess
import xml.etree.ElementTree as ET


BASH_COMMAND = ['bash', '-c']
SOURCE_COMMAND_FORMAT = 'source {} && env'
SETUP_ENV_FILE = '/awips2/edex/bin/setup.env'
USER_DICT_GLOB_PATTERN = '/awips2/edex/data/utility/cave_static/user/*/spelldict'
CAVE_STATIC_SITE_LEVEL_PATH = '/awips2/edex/data/utility/cave_static/site/'
COMMON_STATIC_SITE_LEVEL_PATH = '/awips2/edex/data/utility/common_static/site/'
EDEX_STATIC_SITE_LEVEL_PATH = '/awips2/edex/data/utility/edex_static/site/'
SVC_BU_PROPS_PATH = 'config/gfe/svcbu.properties'
USER_ROLES_PATH = 'roles/userRoles.xml'
NEW_DICT_PATH = 'spellchecker/spelldict.txt'
NEW_ROLE_ID = 'com.raytheon.localization.site/cave_static/spellchecker'


logging.basicConfig(format="%(asctime)s %(levelname)s:  %(message)s", 
                    datefmt="%H:%M:%S", 
                    level=logging.DEBUG)
log = logging.getLogger('migrateUserSpellingDictionaries')


def createDir(dir):
    try:
        os.makedirs(os.path.dirname(dir))
    except OSError as e:
        if e.errno != errno.EEXIST:
            raise

def sourceEnvFileGetVariable(envFile, variable):
    cmdArray = list(BASH_COMMAND)
    cmdArray.append(SOURCE_COMMAND_FORMAT.format(envFile))
    output = subprocess.check_output(cmdArray)
    for line in output.splitlines():
        if variable in line:
            tokens = line.split('=')
            return tokens[1].strip()
    return ''

def getSiteID():
    try:
        return sourceEnvFileGetVariable(SETUP_ENV_FILE, "AW_SITE_IDENTIFIER")
    except subprocess.CalledProcessError as e:
        log.exception("Could not process [%s].", os.path.basename(SETUP_ENV_FILE))
    return ''

def getPrimarySites(awSiteID):
    svcBuPropsFile = os.path.join(EDEX_STATIC_SITE_LEVEL_PATH, awSiteID, SVC_BU_PROPS_PATH)
    if os.path.isfile(svcBuPropsFile):
        siteList = sourceEnvFileGetVariable(svcBuPropsFile, "PRIMARY_SITES")
        if siteList:
            return [site.strip() for site in siteList.split(',')]
    return []

def main():
    log.info("Running delta script for DR 4781: migrating USER spelling dictionaries to SITE.")
    
    userDicts = glob.glob(USER_DICT_GLOB_PATTERN)
    combinedDict = set()
    for userDict in userDicts:
        log.info("Migrating USER-level spelling dictionary [%s].", userDict)
        try:
            with open(userDict, 'r') as f:
                spelling_entries = f.readlines()
                combinedDict = combinedDict.union(spelling_entries)
        except IOError:
            log.warning("Skipping USER-level spelling dictionary [%s].", userDict, exc_info=1)
    
    siteID = getSiteID()
    if not siteID:
        log.error("Could not determine this server's site ID.")
        log.error("Delta script failed.")
        return -1
    
    siteDictFile = os.path.join(CAVE_STATIC_SITE_LEVEL_PATH, siteID, NEW_DICT_PATH)
    log.info("Writing SITE-level spelling dictionary [%s].", siteDictFile)
    try:
        createDir(siteDictFile)
        with open(siteDictFile, 'w') as f:
            f.writelines(sorted(combinedDict))
    except OSError:
        log.exception("Could not create SITE directory for spelling dictionary.")
        log.error("Delta script failed.")
        return -1
    except IOError:
        log.exception("Could not write SITE spelling dictionary.")
        log.error("Delta script failed.")
        return -1
    except Exception:
        log.exception("Unknown error trying to create SITE spelling dictionary.")
        log.error("Delta script failed.")
        return -1
    
    log.info("Updating userRoles.xml.")
    siteUserRoles = os.path.join(COMMON_STATIC_SITE_LEVEL_PATH, siteID, USER_ROLES_PATH)
    if os.path.isfile(siteUserRoles):
        tree = ET.parse(siteUserRoles)
        root = tree.getroot()
        root.set('xmlns:ns2', 'group')
        newPermission = ET.SubElement(root, "permission", {'id':NEW_ROLE_ID})
        users = root.findall("user")
        for user in users:
            if user.get("userId") == "ALL":
                 newUserPermission = ET.SubElement(user, "userPermission")
                 newUserPermission.text = NEW_ROLE_ID
    roughString = ET.tostring(root, 'utf-8')
    finalString = '<?xml version="1.0" encoding="UTF-8" standalone="yes"?>' + '\n' + roughString
    try:
        with open(siteUserRoles, 'w') as f:
            f.write(finalString)
    except IOError:
        log.exception("Could not update SITE-level userRoles.xml.")
        log.error("Delta script failed.")
        return -1
    except Exception:
        log.exception("Unknown error trying to update SITE-level userRoles.xml.")
        log.error("Delta script failed.")
        return -1
    
    gfeSites = []
    try:
        gfeSites = getPrimarySites(siteID)
    except Exception as e:
        log.exception("Could not retrieve PRIMARY_SITES setting.")
        log.error("Delta script failed.")
        return -1
    
    for site in gfeSites:
        gfeSiteFile = os.path.join(CAVE_STATIC_SITE_LEVEL_PATH, site, NEW_DICT_PATH)
        log.info("Writing SITE-level spelling dictionary [%s].", gfeSiteFile)
        try:
            createDir(gfeSiteFile)
            shutil.copy(siteDictFile, gfeSiteFile)
        except OSError:
            log.exception("Could not create SITE directory for spelling dictionary.")
            log.error("Delta script failed.")
            return -1
        except IOError:
            log.exception("Could not write SITE spelling dictionary.")
            log.error("Delta script failed.")
            return -1
        except Exception:
            log.exception("Unknown error trying to create SITE spelling dictionary.")
            log.error("Delta script failed.")
            return -1
    
    log.info("Delta script complete.")

if __name__ == '__main__':
    main()
