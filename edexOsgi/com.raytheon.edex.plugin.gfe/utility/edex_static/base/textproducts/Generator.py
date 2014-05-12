
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

"""Generate site specific text products.

This script is run at install time to customize a set of the text products 
for a given site.

SOFTWARE HISTORY
Date            Ticket#        Engineer    Description
------------    ----------     ----------- --------------------------
Jun 23, 2008    1180           jelkins     Initial creation
Jul 08, 2008    1222           jelkins     Modified for use within Java
Jul 09, 2008    1222           jelkins     Split command line loader from class
Jul 24, 2012    #944           dgilling    Refactored to support separate
                                           generation of products and utilities.
Sep 07, 2012    #1150          dgilling    Ensure all necessary dirs get created.   
May 12, 2014    2536           bclement    renamed text plugin to include uf in name

@author: jelkins
"""
__version__ = "1.0"

import errno
import os
from os.path import basename
from os.path import join
from os.path import dirname
from com.raytheon.uf.common.serialization import SerializationUtil
from com.raytheon.uf.common.localization import LocalizationFile
from com.raytheon.uf.common.localization import PathManagerFactory
from com.raytheon.uf.common.localization import LocalizationContext_LocalizationType as LocalizationType
from com.raytheon.uf.common.localization import LocalizationContext_LocalizationLevel as LocalizationLevel
from java.lang import System

SCRIPT_DIR = dirname(__file__)

# ---- Standard Paths ----------------------------------------------

LIBRARY_DIR    = join(SCRIPT_DIR , "library")
TEMPLATE_DIR   = join(SCRIPT_DIR , "templates")
PREFERENCE_DIR = join(SCRIPT_DIR , "preferences")

# ---- "Import" Preferences and Library Files -------------------------------
from sys import path
path.append(join(LIBRARY_DIR,"../"))
path.append(join(PREFERENCE_DIR,"../"))

from library.SiteInfo import SiteInfo as SITE_INFO
from preferences.configureTextProducts import NWSProducts as NWS_PRODUCTS

from os.path import basename
from os.path import dirname
from os.path import abspath
from os.path import join

# ---- Setup Logging ----------------------------------------------------------
LOG_CONF  = join(SCRIPT_DIR,"preferences","logging.conf")

import logging.config 
logging.config.fileConfig(LOG_CONF)

LOG = logging.getLogger("Generator")

# List of protected files
fileList = []

#Installation information for product formatters.
#Directories to Process, src/dest
ProcessDirectories = [
  {
     'src': "textproducts/templates/product",
     'dest': "textProducts"
  },
  {
     'src': "textproducts/templates/utility",
     'dest': "textUtilities/regular"
  },
  ]


class Generator():
    """Generates site specific text products from base template files.
    
    This class handles the substituting and renaming of template files.
    """
    
    def __init__(self):
        """Class constructor"""
        self.__destination = None
        self.__siteId = None
        
    def setSiteId(self,siteId):
        """Set the site ID
        
        Checks if the given site ID is valid and sets the site ID.
        
        @param siteId: site ID
        @type siteId: string
        
        @raise LookupError: when the site ID is invalid
        """
        if siteId in SITE_INFO.keys():
            self.__siteId = siteId
        else:
            raise LookupError, ' unknown WFO: ' + siteId

    def setDestination(self, value):
        """Set this generator's output directory
        
        Verifies the directory exists and is writable
        
        @param value: this value should be a fully qualified path
        @type value: string
        
        @raise IOError: when the directory does not exist or is not writable
        """        
        
        try:
            os.makedirs(value, 0755)
        except OSError, e:
            if e.errno != errno.EEXIST:
                LOG.warn("%s: '%s'" % (e.strerror,e.filename))
            
        self.__destination = value


    def getSiteId(self):
        """Get the site ID
        
        @return: the site ID
        @rtype: string
        """
        return self.__siteId

    def getDestination(self):
        """The directory into which the generated files are placed
        
        @return: the directory in which the generated files are placed
        @rtype: string
        """
        return self.__destination
        
    def create(self, siteId, destinationDir):
        """Create text products"""
        LOG.debug("Configuring Text Products Begin........")
        
        try:
            self.setSiteId(siteId)
        except LookupError:
            LOG.warning(siteId + "is not a known WFO. Skipping text formatter configuration.")
            return
            
        self.setDestination(destinationDir)
        
        self.__delete()
        self.__createPilDictionary(self.__siteId)
        
        created = 0
        for dirInfo in ProcessDirectories:
            created += self.__create(dirInfo['src'], dirInfo['dest'])
        LOG.info("%d text products created" % created)
        LOG.debug("Configuration of Text Products Finish")
    
    def delete(self):
        """Delete text products"""
        
        LOG.debug("Deleting Text Products Begin........")
        deleted = self.__delete()
        
        LOG.info("%d text products deleted" % deleted)
        
        # ---- Delete Empty Directory -----------------------------------------
        
        for dirInfo in ProcessDirectories:
            try:
                os.rmdir(os.path.join(self.getDestination(), dirInfo['dest']))
            except OSError, description: 
                LOG.warn("unable to remove directory (%s)" % description)
                pass
        
    def info(self):
        """Text product information for this site"""
        
        LOG.debug("PIL Information for %s Begin......" % self.__siteId)
        pils = self.__createPilDictionary(self.__siteId)
        
        self.__printPilDictionary(pils)
        
        LOG.info("%d total PILs found" % len(pils))
        
        LOG.debug("PIL Information for %s End" % self.__siteId)
        
    def allinfo(self):
        """Text product information for all sites in this generator's a2a file
        """
        found = 0
        
        LOG.debug("PIL Information for all sites Begin.......")
        
        for site in SITE_INFO.keys():
            LOG.info("--------------------------------------------")
            LOG.info("%s %s %s" % (site, 
                                   SITE_INFO[site]['fullStationID'], 
                                   SITE_INFO[site]['wfoCityState']))
            pils = self.__createPilDictionary(site)
            self.__printPilDictionary(pils)
            found += len(pils)
        
        LOG.info("%d total PILs found" % found)
        LOG.debug("PIL Information for all sites End")

    # ---- Private Methods --------------------------------------------------

    class __Substitutor():
        """Substitute values inside file contents and filenames
        
        This class contains all methods capable of performing the substitution.
        """
        
        def __init__(self,product,pilInfo,multiPilFlag,siteID):
            """Class constructor
            
            Initialize the class
            
            @param product: product in which substitution is performed
            @type product: string
            
            @param pilInfo: PIL information for the product
            @type pilInfo: dictionary
            
            @param multiPilFlag: indicates there is more than one Pil
            @type multiPilFlag: boolean
            
            @type siteID: string 
            """
            self.__product  = product
            self.__pilInfo  = pilInfo
            self.__multiPilFlag = multiPilFlag
            self.__siteID = siteID

        
        def substituteKeywords(self, subDict, textData):
            """Replace all instances of the find: replace pairs in the text
            
            Given a dictionary of keywords and their values, along with text 
            data, performs the string substitution and returns the updated text.
            
            @param subDict: words to find (keys) and what to replace (value) 
                            them with
            @type subDict: dictionary
            
            @param textData: text in which to find and replace values
            @type textData: string 
            
            @return: the updated text
            @rtype: string
            """
            from copy import deepcopy
            from string import replace
            
            txt = deepcopy(textData)
            for k in subDict.keys():
                txt = replace(txt, k, subDict[k])            
            return txt
        
        def replaceText(self,contents):
            """Replace the contents of the template with the correct values
            
            Build the substitution dictionary and perform substitution
            
            @param contents: the template contents
            @type contents: string
            
            @return: the contents with all template values substituted
            @rtype: string
            """
            
            siteid = self.__siteID
            product = self.__product
            multiPilFlag = self.__multiPilFlag
            pilInfo = self.__pilInfo
            
            from preferences.configureTextProducts import ProductToStandardMapping
            
            subDict = {}
            subDict['<site>'] = siteid.strip()
            subDict['<region>'] = SITE_INFO[siteid]['region'].strip()
            subDict['<wfoCityState>'] = SITE_INFO[siteid]['wfoCityState'].strip()
            subDict['<wfoCity>'] = SITE_INFO[siteid]['wfoCity'].strip()
            subDict['<fullStationID>'] = SITE_INFO[siteid]['fullStationID'].strip()
            subDict['<state>'] = SITE_INFO[siteid]['state'].strip()
            if product is not None:
                subDict['<product>'] = product.strip()
                if ProductToStandardMapping.has_key(product):
                    subDict['<standard>'] = ProductToStandardMapping[product].strip()
                else:
                    subDict['<standard>'] = product.strip()
            if pilInfo is not None:
                for k in pilInfo.keys():
                    subDict['<' + k + '>'] = pilInfo[k].strip()
            if pilInfo is not None and pilInfo.has_key("pil") and multiPilFlag:
                subDict['<MultiPil>'] = pilInfo["pil"][3:6].strip() #pil=nnnxxx, want xxx
            else:
                subDict['_<MultiPil>'] = ""   #no multiple pils
        
            return self.substituteKeywords(subDict, contents)
        
        def replaceFilename(self,fileName):
            """Replace any template variables in the filename with correct 
            values
            
            @param fileName: the filename for this product
            @type fileName: string
            
            @return: the updated filename
            @rtype: string
            """
            
            siteid = self.__siteID
            product = self.__product
            multiPilFlag = self.__multiPilFlag
            pilInfo = self.__pilInfo
            
            subDict = {}
            subDict['Site'] = siteid.strip()
            subDict['Region'] = SITE_INFO[siteid]['region'].strip()
            if product is not None:
                subDict['Product'] = product.strip()
            if pilInfo is not None and pilInfo.has_key("pil") and multiPilFlag:
                subDict['MultiPil'] = pilInfo["pil"][3:6].strip()  #xxx of nnnxxx
            else:
                subDict['_MultiPil'] = ""   #no pil information, remove entry
            
            return self.substituteKeywords(subDict,fileName)
    
    def __getTemplateFiles(self, srcDir):
        """Get a list of all template files
        
        @return: a list of all template files
        @rtype: list
        """
        
        pathMgr = PathManagerFactory.getPathManager()
        edexStaticBase = pathMgr.getContext(LocalizationType.EDEX_STATIC, LocalizationLevel.BASE)
        templateFiles = pathMgr.listFiles(edexStaticBase, srcDir, None, False, True)
        return templateFiles
    
    def __printPilDictionary(self, pillist):
        """Prints the PIL dictionary information
        
        @param pillist: site specific PIL information
        @type pillist: dictionary
        """
        
        pils = pillist.keys()
        pils.sort()
        LOG.info("PIL List: (product) (pil, wmoid, site)")
        for p in pils:
            LOG.info("%s %s" % (p,pillist[p]))
    
    def __createPilDictionary(self, siteid):
        """Update the SITE_INFO with a PIL dictionary
        
        Read the a2a data from the database, create PIL information, and add the information
        to the SITE_INFO dictionary.
        
        @param site: the site for which PIL information is created
        @type site: string
        
        @return: the PILS found for the site
        @rtype: dictionary
        """
        
        siteD = SITE_INFO[siteid]
        stationID4 = siteD['fullStationID']
        
        from com.raytheon.uf.edex.plugin.text.dao import AfosToAwipsDao
        
        dao = AfosToAwipsDao()
        response = dao.lookupAfosId(stationID4, None, None)
        errMsg = response.getErrorMessage()
        if errMsg is not None and "" != errMsg.strip():
            raise Exception(errMsg)

        siteTable = []
        # idList is a Java list of non-String objects.
        # use the Java iterator 
        idListIt = response.getIdList().iterator()
        while( idListIt.hasNext() ):
            item = idListIt.next()
            pil = item.getAfosid()
            wmoid = item.getWmottaaii()
            site4 = item.getWmocccc()
            if pil[6:] == "AER" and siteid == "ALU" or \
              pil[6:] == "ALU" and siteid == "AER":  #ignore other AFC domain
                continue
            if pil[3:6] == "OFF" and pil[6:] in ['N11','N12','N13','N14']:
                continue
            siteTable.append((pil, wmoid, site4))
            
        # get the pil list for each product
        pillist = {}
        for nnn in NWS_PRODUCTS:
            found = 0
            pilEntries = []
            for pil, wmoid, site4 in siteTable:
                if pil[3:6] == nnn:
                    pilEntries.append((pil, wmoid, site4))
    
            pillist[nnn] = pilEntries
            d = []  #list of entries for this particular pil
            for pil, wmoid, site4 in pilEntries:
                e = {}
                e['wmoID'] = wmoid
                e['pil'] = pil[3:]
                e['textdbPil'] = pil
                e['awipsWANPil'] = site4 + pil[3:]
                d.append(e)
            siteD[nnn] = d #store the pil dictionary back into the SITE_INFO
    
        return pillist

    def __getProductFromFilename(self,regularNameBase):
        """get product from filename
        
        Looks for exact matches in the DirectFileToProductMapping dictionary,
        which contains only those names that you can't determine the
        product id from the name.  If name begins with Product, then returns
        the set of products based on the TemplatedProducts dictionary.
        
        @param regularNameBase: the filename without the extension
        @type regularNameBase: string
        
        @return a set of products based on the TemplatedProducts dictionary
        @rtype: list of strings
        """
        import types, string
        from preferences.configureTextProducts import DirectFileToProductMapping
        from preferences.configureTextProducts import TemplatedProducts
        
        if regularNameBase == "NONE":
            return ["NONE"]
    
        #in the direct mapping dictionary?
        if DirectFileToProductMapping.has_key(regularNameBase):
            v = DirectFileToProductMapping[regularNameBase]
            if type(v) is types.StringType:
                return [v]
            else:
                return v
    
        #look for Product in the filename
        index = string.find(regularNameBase, "Product")
        if index == 0:
            if TemplatedProducts.has_key(regularNameBase):
                return TemplatedProducts[regularNameBase]
            else:
                return None
    
        #attempt to extract out the PIL based on the filename. Look for
        #3 uppercase letters, separated by an underscore.  This entry must
        #exist in the NWSProducts directory.
        sections = string.split(regularNameBase, "_")
        for sec in sections:
            if len(sec) == 3 and sec in NWS_PRODUCTS:
                return [sec]
    
        #got here, don't know what this product is
        LOG.error("Unknown Product: %s" % regularNameBase)
        return None

    def __create(self, srcDir, destDir):
        """Build the formatters for this site
        
        Substitute the appropriate values into the templates.  Name and place
        the generated file into the correct destination.
        
        @return the number of products that were created
        @rtype: number
        """
        LOG.debug("Processing Formatter Templates.......")
        
        try:
            os.makedirs(join(self.getDestination(), destDir))
        except OSError, e:
            if e.errno != errno.EEXIST:
                LOG.error("%s: '%s'" % (e.strerror,e.filename))
                return 0
        
        siteid = self.__siteId
        productsWritten = 0
        
        # ---- Gather a list of all template Files --------------------------
        
        templateFiles = self.__getTemplateFiles(srcDir)
        
        # ---- Process the files ---------------------------------------------
        
        import stat, string
        for lf in templateFiles:

            fileName = str(lf.getFile().getAbsolutePath())
            isProtected = lf.isProtected()
            
            #skip svn files
            if string.find(fileName,".svn-base") != -1:
                continue
            
            #decode the filename
            regularName = basename(fileName)
            index = string.find(regularName, '.')
            if index != -1:
                regNameExt = regularName[index:]
                regNameBase = regularName[0:index]
            else:
                regNameExt = ""
                regNameBase = regularName
        
            #read the source
            try:                
                fd = open(fileName)
                fileContents = fd.read()
                fd.close()
            except IOError, (num,info):
                LOG.warn("Unable to read template (%s): %s" % (info,
                                                               basename(fileName)))
                continue
        
            #get source permissions
            perms = os.stat(fileName)[stat.ST_MODE] & 0777
        
            LOG.debug("=========================================" + \
              "=======================")
            LOG.debug("file: %s" % fileName[len(SCRIPT_DIR):])
            LOG.debug("regularNameBase: %s" % regNameBase)
            LOG.debug("RegNameExt: %s" % regNameExt)
            #determine products, for templated could be more than 1
            products = self.__getProductFromFilename(regNameBase)
            if products is None:
                continue
            LOG.debug("PRODUCTS: %s" % products)
            # ---- Process the products ------------------------------------
        
            for product in products:
                LOG.debug("     --------------")
                LOG.debug("     PRODUCT: %s" % product)
        
                # get the pil information
                pilNames = self.__getProductFromFilename(product)  #indirect
                LOG.debug("     PRODUCT PIL: %s" % pilNames)

                if pilNames is None or len(pilNames) == 0:
                    continue
        
                # extract out the pil information from the dictionary
                if SITE_INFO[siteid].has_key(pilNames[0]):
                    pils = SITE_INFO[siteid][pilNames[0]]
                else:
                    #set pils to empty list if none defined
                    pils = [{'awipsWANPil': 'kssscccnnn',
                             'textdbPil': 'cccnnnxxx',
                             'pil': 'nnnxxx', 'wmoID': 'wmoidd'}]
                LOG.debug("     PILS: %s" % pils)
        
                # ---- Process each PIL ---------------------------------------
        
                for pilInfo in pils:
                    multiple = len(pils) > 1
                    
                    # ---- Perform Substitutions -----------------------------
                                        
                    substitutor = self.__Substitutor(product,pilInfo,multiple,
                                                                 self.__siteId)
                                                            
                    newFileContents = substitutor.replaceText(fileContents)
                    destName     = substitutor.replaceFilename(regNameBase)                    
                    
                    # ---- Output to File -----------------------------------
                    
                    destFilename = join(self.getDestination(), destDir, destName) + regNameExt
                    LOG.debug("       ---> %s" % destFilename)
        
                    try:
                        os.chmod(destFilename, 0644)
                        os.remove(destFilename)
                        LOG.debug("""       Replacing the existing file: %s
                        
                        The file may already exist due to any of the following 
                        reasons:
                        
                        1. Multiple PILS exist but the template filename does
                           not have 'MultiPil' in its name.
                        2. Multiple Products exist but the template filename
                           does not have 'Product' in its name.
                        3. The destination directory was not previously cleared.
                        """ % destName)
                        productsWritten = productsWritten - 1
                    except:
                        pass
        
                    try:
                        fd = open(destFilename, 'w', 0644)
                        fd.write(newFileContents)
                        fd.close()
                        os.chmod(destFilename, perms)
                        productsWritten = productsWritten + 1
                        
                        # Set protected if file is read only
                        isWritable = os.access(destFilename, os.W_OK)
                        
                        if isProtected:
                            self.addProtection(destFilename)                         
                        
                    except IOError, (number, description):
                        LOG.warn("       %s" % description)
        
        # write out the protected file
        
                        
        return productsWritten
    
    def addProtection(self, filename): 
        # cleanup the filename
        parts = filename.split("configured/"+self.__siteId)
        newName = "CONFIGURED:cave_static" + parts[1]
 
        fileList.append(newName)
        
    def getProtectedFiles(self):
            return fileList
            
    def __delete(self):
        """Delete all pre-existing text templates
        
        @return the number of pre-existing products that were deleted
        @rtype: number
        """
        
        productsRemoved = 0
        
        import string
        import glob
        import os
        
        LOG.debug("     Deleting Existing Baseline Templates Begin........")
    
        from preferences.configureTextProducts import templateProds
    
        #make a list of all known products
        allProducts = []
        for p in NWS_PRODUCTS:
            allProducts.append(p)
        for p in templateProds:
            allProducts.append(p)
            
        for dirInfo in ProcessDirectories:
            templateFiles = self.__getTemplateFiles(dirInfo['src'])
    
            #determine potential files, based on the template files
            for lf in templateFiles:
                tf = str(lf.getFile().getAbsolutePath())
    
                LOG.debug("Template= %s" % basename(tf))
                mname = basename(tf)
                globExpressions = []
    
                #wildcard the Site
                mname = string.replace(mname, "Site", "???")   #always 3 chars
    
                #wildcard the Region
                mname = string.replace(mname, "Region", "??")  #always 2 chars
    
                #wildcard the _MultiPil
                wcards = []
                if string.find(mname, "_MultiPil") != -1:
                    wcards.append(string.replace(mname, "_MultiPil", ""))
                    wcards.append(string.replace(mname, "_MultiPil", "_?"))
                    wcards.append(string.replace(mname, "_MultiPil", "_??"))
                    wcards.append(string.replace(mname, "_MultiPil", "_???"))
                else:
                    wcards.append(mname)
    
                #wildcard the Product
                if string.find(mname, "Product") == 0:
                    for wc in wcards:
                        for prd in allProducts:
                            ge = prd + wc[7:]   #Product is first 7 characters
                            globExpressions.append(ge)
    
                #simple case - Product does not need to be expanded
                else:
                    for wc in wcards:
                        globExpressions.append(wc)
    
                for g in globExpressions:
                    searchString = join(self.getDestination(), dirInfo['dest'], g)
                    delfiles = glob.glob(searchString)
                    
                    for fn in delfiles:
                        #delete any existing file
                        try:
                            os.chmod(fn, 0644)
                            os.remove(fn)
                            LOG.debug("   DEL---> %s" % fn)
                            productsRemoved += 1
                        except:
                            pass
    
        LOG.debug("     Deleting Existing Baseline Templates Finished........")
        
        return productsRemoved
                        