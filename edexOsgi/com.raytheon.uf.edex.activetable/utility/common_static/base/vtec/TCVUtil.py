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
# Common utilities for sending/receiving TCV files
#
#    
#     SOFTWARE HISTORY
#    
#    Date            Ticket#       Engineer       Description
#    ------------    ----------    -----------    --------------------------
#    12/12/14        4953          randerso       Initial Creation.
# 
##
import glob, os, subprocess, time
import LocalizationSupport

def getCaveStaticSiteDir():
    # this should return /awips2/edex/data/utility/cave_static/site
    
    import siteConfig
    siteID = siteConfig.GFESUITE_SITEID
    
    siteDir = LocalizationSupport.getLocalizationFile(LocalizationSupport.CAVE_STATIC, 
                                                     LocalizationSupport.SITE, 
                                                     siteID, "gfe").getFile()
    siteDir = siteDir.getParentFile().getParentFile().getAbsolutePath()
    return siteDir

def purgeAllCanFiles(logger):
    PURGE_AGE = 30 * 24 * 60 * 60  # 30 days in seconds
    siteDir = getCaveStaticSiteDir()
    
    # purge allCan files older than PURGE_AGE
    purgeTime = time.time() - PURGE_AGE
    for f in glob.iglob(os.path.join(siteDir, "*/gfe/tcvAdvisories/*.allCAN")):
        site = f.replace(siteDir,"").split("/")[1]
        basename = os.path.basename(f)
        try: 
            modTime = os.path.getmtime(f)
            if modTime < purgeTime:
                logger.debug("Purging "+basename+": "+time.strftime("%Y%m%d %H%M%S", time.gmtime(modTime)))
                LocalizationSupport.deleteFile(LocalizationSupport.CAVE_STATIC, 
                                               LocalizationSupport.SITE, site, 
                                               "gfe/tcvAdvisories/" + basename)
        except:
            logger.exception("Unable to delete "+f)
    
def packageTCVFiles(siteList, fileName, logger):
    siteDir = getCaveStaticSiteDir()
    cmd = "cd " + siteDir + "; tar cvzf " + fileName
    
    for siteID in siteList:
        tcvDir = os.path.join(siteID, "gfe", "tcvAdvisories")

        for fileType in ["*.json", "*.allCAN"]:
            path = os.path.join(tcvDir, fileType)
            if len(glob.glob(os.path.join(siteDir, path))) > 0:
                cmd += " " + path
        
    logger.debug("cmd: '" + cmd + "'")
    subprocess.check_call([cmd], shell=True)
    