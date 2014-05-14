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
# VTEC Partner configuration
#
#    
#     SOFTWARE HISTORY
#    
#    Date            Ticket#       Engineer       Description
#    ------------    ----------    -----------    --------------------------
#    06/11/13        #2083         randerso       Fixed getISCSites to look in configured
#    02/20/2014      #2824         randerso       Added log message when no localVTECPartners file is found

#VTEC_Partners.py - configuration file to control filtering and merging
#of VTEC active table.

# The following list is a set of AWIPS WAN IDs (typically WFO IDs) which
# identifies the locations from which we will request their active VTEC
# table for merge purposes.  These are 3-letter WFO IDs, e.g., BOU.
VTEC_TABLE_REQUEST_SITES = []

# Name of site identifier for SPC and TCV bulletins. 4-characters.
VTEC_SPC_SITE = ['KWNS']
VTEC_TPC_SITE = ['KNHC']

# The following list is a set of office identifiers which is used
# in the ingestAT/MergeVTEC software to filter out records from offices
# that aren't in our area of responsibility.  Our site and the spc site
# are automatically added to this list in the software.  These are 4-character
# site identifiers, e.g., KBOU.
VTEC_MERGE_SITES = []

# The following list is a set of office identifiers which is used
# in the VTECDecoder software to filter out records from offices
# that aren't in our area of responsibility.  Our site and the spc site
# are automatically added to this list in the software.   These are
# 4-letter WFO ids, e.g., KBOU.
VTEC_DECODER_SITES = []

# The following list is a set of product categories (e.g., ZFP, WOU) that
# when decoded, the text for each segment is captured.  The text is not
# normally needed to be captured except for warning-style products.
VTEC_CAPTURE_TEXT_CATEGORIES = ['WSW', 'NPW', 'RFW', 'FFA', 'CFW', 'MWS', 'HLS', 'MWW']

# Remapping of product pils.  This is required for certain VTEC events
# if a hazard is created in one pil and then updated or cancelled in another
# pil.  Example is the TO.W (Tornado Warning), which is issued in the TOR
# pil but updated/cancelled in the SVS pil.  In order for the table matching
# purging to work correctly, the pils in the followup products must be
# mapped back to the original.  This is *only* for use for non-GFE/GHG
# issued products since the GHG logic uses the pil information to calculate
# VTEC. This is a dictionary of (phen,sig,pil) with the remapped pil as the
# value.
VTEC_MAPPED_PILS = {
  ('TO','W','SVS'): 'TOR', ('SV','W','SVS'): 'SVR', ('FF','W','FFS'): 'FFW',
  ('FL','W','FLS'): 'FLW', ('MA','W','MWS'): 'SMW', ('EW','W','SVS'): 'EWW',
  ('FA','W','FLS'): 'FLW',
  }

# Frequency of requests for table updates in seconds
#VTEC_REMOTE_TABLE_FETCH_TIME = 60*60   #normal operations
VTEC_REMOTE_TABLE_FETCH_TIME = 0   #disable

# Purge parameter for backups of the active table in hours
VTEC_BACKUP_TABLE_PURGE_TIME = 168*4

# Allow my site to respond to VTEC table requests
#VTEC_RESPOND_TO_TABLE_REQUESTS = 1   #normal operations (non-RPP)
VTEC_RESPOND_TO_TABLE_REQUESTS = 0   #disable

#----------------------------------------
#DO NOT CHANGE ANY CODE BELOW THIS NOTICE
#----------------------------------------
# Default configuration based on list of primary/secondary backup sites
# Format: disabled WFO site:  (1stBackup, 2ndBackup)
# Note: You must have at least 2 entries for each site.
BackupDict = {
 'ABR': ('FSD', 'BIS'),
 'ABQ': ('EPZ', 'AMA'),
 'AER': ('AJK', 'AFG'),
 'AFG': ('AER', 'ALU', 'AJK'),
 'AJK': ('AER', 'ALU', 'AFG'),
 'AKQ': ('MHX', 'RAH'),
 'ALU': ('AJK', 'AFG'),
 'ALY': ('BTV', 'BGM'),
 'AMA': ('LUB', 'ABQ'),
 'APX': ('MQT', 'DTX'),
 'ARX': ('DMX', 'DVN'),
 'BGM': ('CTP', 'ALY'),
 'BIS': ('FGF', 'ABR'),
 'BMX': ('FFC', 'HUN'),
 'BOI': ('PIH', 'PDT'),
 'BOU': ('PUB', 'CYS'),
 'BOX': ('OKX', 'GYX'),
 'BRO': ('LCH', 'EWX'),
 'BTV': ('ALY', 'CAR'),
 'BUF': ('CLE', 'PBZ'),
 'BYZ': ('GGW', 'RIW'),
 'CAE': ('GSP', 'CHS'),
 'CAR': ('GYX', 'BTV'),
 'CHS': ('ILM', 'CAE'),
 'CLE': ('BUF', 'ILN'),
 'CRP': ('EWX', 'HGX'),
 'CTP': ('BGM', 'LWX'),
 'CYS': ('RIW', 'UNR'),
 'DDC': ('GLD', 'ICT'),
 'DLH': ('MPX', 'FGF'),
 'DMX': ('DVN', 'OAX'),
 'DTX': ('GRR', 'APX'),
 'DVN': ('ARX', 'DMX'),
 'EAX': ('SGF', 'TOP'),
 'EKA': ('MFR', 'MTR'),
 'EPZ': ('ABQ', 'MAF'),
 'EWX': ('CRP', 'BRO'),
 'FFC': ('BMX', 'MRX'),
 'FGF': ('BIS', 'DLH'),
 'FGZ': ('VEF', 'PSR'),
 'FSD': ('ABR', 'MPX'),
 'FWD': ('SHV', 'OUN'),
 'GGW': ('BYZ', 'TFX'),
 'GID': ('OAX', 'LBF'),
 'GJT': ('SLC', 'BOU'),
 'GLD': ('DDC', 'PUB'),
 'GRB': ('MKX', 'MQT'),
 'GRR': ('DTX', 'IWX'),
 'GSP': ('CAE', 'RNK'),
 'GUM': ('HFO', 'HFO'),
 'GYX': ('CAR', 'BOX'),
 'HFO': ('GUM', 'MTR'),
 'HGX': ('LCH', 'CRP'),
 'HNX': ('STO', 'SGX'),
 'HUN': ('JAN', 'BMX'),
 'ICT': ('TOP', 'DDC'),
 'ILM': ('CHS', 'MHX'),
 'ILN': ('JKL', 'CLE', 'IND', 'LMK'),
 'ILX': ('LOT', 'LSX'),
 'IND': ('IWX', 'LMK'),
 'IWX': ('IND', 'GRR'),
 'JAN': ('HUN', 'SHV'),
 'JAX': ('TAE', 'CHS'),
 'JKL': ('ILN', 'RLX', 'LMK'),
 'KEY': ('MFL', 'TBW'),
 'LBF': ('UNR', 'GID'),
 'LCH': ('HGX', 'LIX'),
 'LIX': ('MOB', 'LCH'),
 'LKN': ('REV', 'BOI'),
 'LMK': ('PAH', 'IND'),
 'LOT': ('ILX', 'MKX'),
 'LOX': ('SGX', 'MTR'),
 'LSX': ('EAX', 'ILX'),
 'LUB': ('AMA', 'SJT'),
 'LWX': ('PHI', 'CTP'),
 'LZK': ('MEG', 'TSA'),
 'MAF': ('SJT', 'EPZ'),
 'MEG': ('LZK', 'OHX'),
 'MFL': ('MLB', 'TBW'),
 'MFR': ('EKA', 'MTR'),
 'MHX': ('AKQ', 'ILM'),
 'MKX': ('GRB', 'LOT'),
 'MLB': ('TBW', 'SJU'),
 'MOB': ('LIX', 'TAE'),
 'MPX': ('DLH', 'ARX'),
 'MQT': ('APX', 'GRB'),
 'MRX': ('OHX', 'FFC'),
 'MSO': ('TFX', 'GGW'),
 'MTR': ('LOX', 'EKA'),
 'OAX': ('GID', 'FSD'),
 'OHX': ('MRX', 'MEG'),
 'OKX': ('BOX', 'PHI'),
 'OTX': ('PDT', 'MSO'),
 'OUN': ('TSA', 'FWD'),
 'PAH': ('LMK', 'SGF'),
 'PBZ': ('RLX', 'BUF'),
 'PDT': ('OTX', 'SEW'),
 'PHI': ('LWX', 'OKX'),
 'PIH': ('BOI', 'SLC'),
 'PQR': ('SEW', 'MFR'),
 'PSR': ('TWC', 'VEF'),
 'PUB': ('BOU', 'GLD'),
 'RAH': ('RNK', 'AKQ'),
 'REV': ('LKN', 'STO'),
 'RIW': ('CYS', 'BYZ'),
 'RLX': ('PBZ', 'JKL'),
 'RNK': ('RAH', 'GSP'),
 'SEW': ('PQR', 'MFR'),
 'SGF': ('LSX', 'PAH'),
 'SGX': ('LOX', 'MTR'),
 'SHV': ('FWD', 'JAN'),
 'SJT': ('MAF', 'LUB'),
 'SJU': ('MFL', 'MLB'),
 'SLC': ('GJT', 'PIH'),
 'STO': ('HNX', 'REV'),
 'TAE': ('JAX', 'MOB'),
 'TBW': ('MLB', 'MFL'),
 'TFX': ('MSO', 'OTX'),
 'TOP': ('ICT', 'EAX'),
 'TSA': ('OUN', 'LZK'),
 'TWC': ('PSR', 'FGZ'),
 'UNR': ('LBF', 'CYS'),
 'VEF': ('FGZ', 'PSR'),
 }

#convert 3 letter to 4 letter site ids
def get4ID(id):
    if id in ['SJU']:
        return "TJSJ"
    elif id in ['AFG', 'AJK', 'HFO', 'GUM']:
        return "P" + id
    elif id in ['AER', 'ALU']:
        return "PAFC"
    else:
        return "K" + id

# returns list of ISC sites within my defined domain. We use
# the generated ISC edit areas to determine this.  Returned names
# are 3-letter identifiers.
def getISCSites():
    import glob, os.path
    from com.raytheon.uf.common.localization import PathManagerFactory
    from com.raytheon.uf.common.localization import LocalizationContext
    from com.raytheon.uf.common.localization import LocalizationContext_LocalizationType as LocalizationType
    from com.raytheon.uf.common.localization import LocalizationContext_LocalizationLevel as LocalizationLevel
    pathMgr = PathManagerFactory.getPathManager()
    commonStaticCfg = pathMgr.getContext(LocalizationType.COMMON_STATIC, LocalizationLevel.CONFIGURED)
    commonStaticCfg.setContextName(siteConfig.GFESUITE_SITEID)
    eaDir = pathMgr.getFile(commonStaticCfg, "gfe/editAreas").getCanonicalPath()
    files = glob.glob(eaDir + "/ISC_???.xml")
    sites = []
    WFOSites = BackupDict.keys()
    for f in files:
        fb = os.path.basename(f)
        if len(fb) == 11 and fb[0:4] == "ISC_":
            site = fb[4:7]
            #ensure it is a known WFO site
            if site in WFOSites:
                sites.append(site)
    #remove our own site
    if siteConfig.GFESUITE_SITEID in sites:
        del sites[sites.index(siteConfig.GFESUITE_SITEID)]
    return sites

# returns list of site ids given the host siteID (3letters), letter3 is
# 1 for returning 3-letter ids, 0 for 4-letter ids. Our own site id
# is not returned. The passed in iscSites is the list of surrounding
# sites based on the isc domains.
def siteList(siteID, letter3, iscSites):
    ids = []
    # add in primary and secondary backup sites, if this WFO fails
    if BackupDict.has_key(siteID):
        for e in BackupDict[siteID]:
            ids.append(e)
    # add in failed site for which this WFO serves as primary or
    # secondary backup sites
    for k in BackupDict.keys():
        if siteID in BackupDict[k]:
            if k not in ids:
                ids.append(k)

    # add in the isc sites if not already in the list
    for site in iscSites:
        if site not in ids:
            ids.append(site)

    if letter3:
        return ids
    else:
        #add in the 4th letter ("T", "K", "P")
        for x in xrange(len(ids)):
            ids[x] = get4ID(ids[x])
    return ids

#auto-configure
try:
    import siteConfig
    iscSites = getISCSites()
    VTEC_TABLE_REQUEST_SITES = siteList(siteConfig.GFESUITE_SITEID, 1, [])
    VTEC_MERGE_SITES = siteList(siteConfig.GFESUITE_SITEID, 0, iscSites)
    VTEC_DECODER_SITES = siteList(siteConfig.GFESUITE_SITEID, 0, iscSites)
except:
    pass

#allow overrides
try:
    from localVTECPartners import *
except ImportError:
    import LogStream
    LogStream.logEvent("No localVTECPartners file found, using baseline settings.");
