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
# configureTextProducts.py preferences
#
# SOFTWARE HISTORY
#    
# Date            Ticket#       Engineer       Description
# ------------    ----------    -----------    --------------------------
# 11/10/14        #4953         randerso       Added Hazard_TCV to templateProds
# 09/08/2015      #4847         randerso       Restore Hazard_HLS to templateProds
# 09/28/2016      19293         randerso       Added productsPerDomain dictionary
# 10/31/2016      25946         randerso       Changed to keep Hazard_TCVNHC from 
#                                              overwriting Hazard_TCV
# 01/19/2017       6096         randerso       Added FWFNHC to producsPerDomain for NH1, NH2
#
##

# ---- NWS Products --------------------------------------------------------
# contains a list of pil category values to automatically
# determine pils information. Only these products will be considered.

#Mapping from product name to standard name, for string substitution.
#keys are product name, value is standard name.  Only need to put in
#entries when the <product> and <standard> are different.   Code assumes
#if entry not present that they are the same.  This is only used for
#templated products and used to know which 'standard' file to import.
ProductToStandardMapping = {
  'ZFP': 'AreaFcst',
  'SAF': 'AreaFcst',
  'FWS': 'FWF',
  'AFM': 'PFM',
  }

#Mapping from filename to product name, mainly for string substitution, in
#the filename "Product" and also inside product files <product>.
#Keys are filename, value is product name (in the filename). This table
#is only needed for those filenames that don't contain the product id
#clearly shown and separated by an underscore character.
DirectFileToProductMapping = {
  'AreaFcst': 'ZFP',
  'FWFTable': 'FWF',
  'FWFTable_Site_MultiPil_Definition': 'FWF',
  'GenericReport': 'NONE',
  'GenericHazards': 'NONE',
  'CivilEmerg': 'NONE',
  'FIPS_EA_Site_MultiPil_Definition': 'WCN',
  'PublicMarine_EA_Site_MultiPil_Definition': 'AFD',
  'PublicMarineFireWx_EA_Site_MultiPil_Definition': 'AFD',
  'FireWxZones_EA_Site_MultiPil_Definition': 'RFW',
  'MarineZones_EA_Site_MultiPil_Definition': 'MWW',
  'PublicZones_EA_Site_MultiPil_Definition': ['NPW','WSW','CFW','FFA','AQA'],
  'Hazard_TCV': 'TCV',
  'Hazard_HLS': 'HLS',
  'Hazard_TCVNHC_MultiPil': 'TCV'
  }

#NWS Products - contains a list of pil category values to automatically
#determine pils information. Only these products will be considered.
NWSProducts = ['ADR', 'AFD', 'AFM', 'AQA', 'AVA', 'AVW', 'CAE', 'CCF', 'CDW', 'CEM',
               'CFW', 'CWF', 'EQR', 'EQW', 'ESF', 'EVI', 'FFA', 'FRW', 'FWF',
               'FWM', 'FWS', 'GLF', 'HLS', 'HMW', 'HWO', 'LAE', 'LEW', 'MWS',
               'MVF', 'NOW', 'NPW', 'NSH', 'NUW', 'OFF', 'PFM', 'PNS', 'RFD',
               'RFW', 'RHW', 'SAF', 'SFT', 'SPS', 'SPW', 'SRF', 'TCV', 'TOE', 
               'VOW', 'WCN', 'WSW', 'ZFP', 'MWW']


productsPerDomain = {
    'ONA':  ['OFFN01', 'OFFN02', 'OFFN03', 'MIMATN'],
    'ONP':  ['OFFN07', 'OFFN08', 'OFFN09', 'MIMPAC'],
    'NH1':  ['HSFEP2', 'HSFEP3', 'FWFNHC', 'FWSNHC', 'TWDEP', 
             'OFFPZ7', 'OFFPZ8'],
    'NH2':  ['OFFN04', 'OFFN05', 'OFFN06', 'OFFN20', 'OFFN21', 'OFFNT3',               
             'OFFNT4', 'MIMATS', 'HSFAT2', 'FWFNHC', 'FWSNHC', 'TWDAT'],
    'NHA':  ['TCVAT1', 'TCVAT2', 'TCVAT3', 'TCVAT4', 'TCVAT5'],
}



#Templated files. Named with "Product" in them, will be replaced with the
#actual product name. Dictionary contains the template filename, list
#contains the products to be generated (e.g., AFM). These products
#follow the Baseline, Region, Site technique.
templateProds= ['AFM', 'ZFP', 'CCF', 'CWF', 'CWF_Pacific', 'FWF', 'HLS',
                'FWFTable', 'FWM', 'GLF', 'MVF', 'NSH', 'PFM', 'SFT', 'SRF', 'OFF', 'AFD',
                'Hazard_TCV', 'Hazard_HLS']
templateProdsWsaf= ['AFM', 'ZFP', 'CCF', 'CWF', 'CWF_Pacific', 'FWF', 'HLS',
  'FWFTable', 'FWM', 'GLF', 'MVF', 'NSH', 'PFM', 'SFT', 'SRF', 'OFF', 'AFD', 'SAF',
  'FWS', 'Hazard_TCV', 'Hazard_HLS']
templateProds_minus_HLS = ['AFM', 'ZFP', 'CCF', 'CWF', 'CWF_Pacific', 'FWF',
                'FWFTable', 'FWM', 'GLF', 'MVF', 'NSH', 'PFM', 'SFT', 'SRF', 'OFF', 'AFD']
TemplatedProducts = {
  'Product_Site_MultiPil': templateProds,
  'Product_Site_MultiPil_Baseline': templateProds_minus_HLS,
  'Product_Site_MultiPil_Region': templateProds_minus_HLS,
  'Product_Region_Overrides': templateProdsWsaf,
  'Product_Site_Overrides': templateProdsWsaf,
  }

