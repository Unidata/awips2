#
# Name: XmlTafConfig.py
# Purpose: To provide US NWS TAF TAC->IWXXM encoder with configurable settings
#
# Author: Mark Oberfield
# Organization: NOAA/NWS/OSTI/Meteorological Development Laboratory
# Contact Info: Mark.Oberfield@noaa.gov
#
# Date: 5 April 2019
#
# SOFTWARE HISTORY
#
# Date          Ticket#  Engineer        Description
# ------------- -------- --------------- --------------------------
# May 15, 2019  20693    mgamazaychikov  Initial Creation
# Jan 29, 2020  21611    mgamazaychikov  Upgrade to version 3.4;
#                                        add parameters for command line option-argument
#                                        pairs for msg_send command
#
import os.path

# IWXXM versioning
_iwxxm = '3.0'
_release = '3.0'
#
IWXXM_URI = 'http://icao.int/iwxxm/%s' % _iwxxm
IWXXM_URL = 'https://schemas.wmo.int/iwxxm/%s/iwxxm.xsd' % _release
#
# IWXXM-US versioning
_us_iwxxm = '3.0'
_us_release = '3.0'
#
# IWXXM-US URI and URL
IWXXM_US_URI = 'http://www.weather.gov/iwxxm-us/%s' % _us_iwxxm
IWXXM_US_URL = 'https://nws.weather.gov/schemas/iwxxm-us/%s/taf.xsd' % _us_release
#
# US Code Registry for Meteorological Services
OFCM_CODE_REGISTRY_URL = 'https://codes.nws.noaa.gov'
#
# NWSI-10-813 Release
NWSI10813_INSERTION = False
NWSI10813_URL = 'https://www.nws.noaa.gov/directives/sym/pd01008013curr.pdf'
NWSI10813_TITLE = 'NWS Instruction 10-813, Aviation Weather Services, Terminal Aerodrome Forecasts'
#
# Code Registry URL for US TAF
US_TAF_CODE_REGISTRY_URL = '%s/NWSI-10-813/AmendableTAFParameter' % OFCM_CODE_REGISTRY_URL
#
# How many elements in the aerodrome's ARP geo-location, either 2 or 3. Shall be set
# to two (2) for the indefinite future.
#
srsDimension = '2'
srsName = 'http://www.opengis.net/def/crs/EPSG/0/4326'
#
# If srsDimensions is equal to 3, then vertical datum must be set correctly for the elevation used
#
# Allowed values are: 'EGM_96', 'AHD', 'NAVD88', or string matching regular expression pattern
# 'OTHER:(\w|_){1,58}'
#
verticalDatum = 'EGM_96'
#
# Elevation value unit of measure (UOM). Either 'FT' or 'M' or string matching regular expression
# pattern OTHER:(\w|_){1,58}
#
elevationUOM = 'M'
#
# Path to file containing codes obtained from WMO Code Registry in
# RDF/XML format
#
CodesFilePath = os.path.join(os.path.abspath(os.path.dirname(__file__)),'../data/4678.rdf')
#
# URLs to miscellaneous WMO Code Registry tables and entries
#
# NIL reasons
NIL_NOOPRSIG_URL = 'http://codes.wmo.int/common/nil/nothingOfOperationalSignificance'
NIL_NA_URL = 'http://codes.wmo.int/common/nil/inapplicable'
NIL_MSSG_URL = 'http://codes.wmo.int/common/nil/missing'
#
CLDCVR_URL =   'http://codes.wmo.int/49-2/CloudAmountReportedAtAerodrome/'
CUMULONIMBUS = 'http://codes.wmo.int/49-2/SigConvectiveCloudType/CB'
TWRNGCUMULUS = 'http://codes.wmo.int/49-2/SigConvectiveCloudType/TCU'
#
# Bit masks
Weather = 1 << 0
CloudAmt = 1 << 1
#
# xlink:title attributes are optional in IWXXM XML documents. TITLES determines
# whether they are displayed.
#
# If no xlink:title attributes (with rare exceptions) are wanted in IWXXM XML documents,
# set TITLES to 0 (zero). Otherwise, set bits appropriately.
#
TITLES = 0
#
# If xlink:titles are to appear in the document, set preferred language. English, 'en', is
# the default if the desired language is not found in the WMO Code Registry.
#
PreferredLanguageForTitles = 'en'
#
# Titles, if desired, for cloud cover amount
CldCvr = {'CLR': 'Sky clear within limits', 'SKC': 'Sky clear', 'FEW': 'Few',
          'SCT': 'Scattered', 'BKN': 'Broken', 'OVC': 'Overcast'}
#
# Message Handling System configuration
MHS_CODE = '-c134'
MHS_SUBJECT = '-sAvnFPS IWXXM Product'
MHS_ADDRESSEE = '-aNWSTG'
MHS_PRIORITY = '-p0'
MHS_OUTPUT_FULL_PATH_DIR = '/awips2/edex/data/outgoing'
