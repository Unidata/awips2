#!/usr/bin/env python

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

import glob
import os
import shutil


def forceTextProdRegen():
    oldTextProds = glob.glob('/awips2/edex/data/utility/cave_static/configured/*/gfe/userPython/textProducts/*.py*')
    for script in oldTextProds:
        try:
            os.remove(script)
        except:
            pass
    oldTextUtils = glob.glob('/awips2/edex/data/utility/cave_static/configured/*/gfe/userPython/textUtilities/regular/*.py*')
    for script in oldTextUtils:
        try:
            os.remove(script)
        except:
            pass
    # touch shapefile and template file to force regen of textProducts and all textUtilities
    shapeFile = glob.glob('/awips2/edex/data/utility/edex_static/base/shapefiles/*/*.shp')[0]
    prodTemplate = glob.glob('/awips2/edex/data/utility/edex_static/base/textproducts/templates/product/*.py')[0]
    # passing None as the second arg is equivalent to running touch
    os.utime(shapeFile, None)
    os.utime(prodTemplate, None)
    
def relocateSiteLevelUtils():
    sitePaths = getSubDirs('/awips2/edex/data/utility/cave_static/site')
    for site in sitePaths:
        scripts = glob.glob(os.path.join(site, 'gfe/userPython/textProducts/*.py'))
        for script in scripts:
            if not isTextProduct(script):
                moveToUtilities(script)

def relocateUserLevelUtils():
    userPaths = getSubDirs('/awips2/edex/data/utility/cave_static/user')
    for user in userPaths:
        scripts = glob.glob(os.path.join(user, 'gfe/userPython/textProducts/*.py'))
        for script in scripts:
            if not isTextProduct(script):
                moveToUtilities(script)

def getSubDirs(path):
    return [os.path.join(path, name) for name in os.listdir(path) 
     if os.path.isdir(os.path.join(path, name))]
    
def isTextProduct(path):
    retVal = False
    with open(path, 'r') as f:
        txt = f.read()
        if "class TextProduct" in txt:
            retVal = True
    return retVal

def moveToUtilities(srcPath):
    destPath = srcPath.replace('textProducts', 'textUtilities/regular', 1)
    if not os.path.isdir(os.path.dirname(destPath)):
        os.makedirs(os.path.dirname(destPath))
    shutil.move(srcPath, destPath)
    # make sure any .pyo, .pyc, and .md5 files are not left behind
    garbageFiles = glob.glob(srcPath + "*")
    for file in garbageFiles:
        try:
            os.remove(file)
        except:
            pass

def main():
    forceTextProdRegen()
    relocateSiteLevelUtils()
    relocateUserLevelUtils()

if __name__ == '__main__':
    main()