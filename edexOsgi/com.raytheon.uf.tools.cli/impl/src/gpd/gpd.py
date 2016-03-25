##
# This script is used to query Generic Point Data database from EDEX.
# 
# 5/22/2013 Chin J. Chen
##
import io
import logging
import sys
import time
import GpdCliRequestHandler
from awips import UsageArgumentParser
from dynamicserialize.dstypes.gov.noaa.nws.ncep.common.dataplugin.gpd.query import GenericPointDataReqMsg
from dynamicserialize.dstypes.java.util import Date

logger = None
parser = None
USAGE_MESSAGE = \
"""
    <To query product information in XML format, result saved at optional filePath >
    gpd qix --p prodName --f filePath                       
    <To query and list product information in XML format >
    gpd qixl --p prodName       
    <To query a product (all stations) in XML format, optional version number, result saved at optional filePath > 
    gpd qpx --p prodName --t referenceTime [--f filePath --v versionNum]
    <To query and list a product (all stations) in XML format, optional version number > 
    gpd qpxl --p prodName --t referenceTime [--v versionNum]
    <To query a station product (single station) in XML format,optional version number, result saved at optional filePath > 
    gpd qsx --p prodName --t referenceTime --id stationId [--f filePath --v versionNum]
    <To query and list a station product (single station) in XML format, optional version number > 
    gpd qsxl --p prodName --t referenceTime --id stationId [--v versionNum]
    <To query a moving product in XML format, optional version number result saved at optional filePath >  
    gpd qmx --p prodName --t referenceTime --slat latitude --slon longitude  [--f filePath --v versionNum]
    <To query and list a moving product in XML format,optional version number >  
    gpd qmxl --p prodName --t referenceTime --slat latitude --slon longitude  [--v versionNum]
    <To query product information in Gempak format, result saved at filePath >
    <To store product XML file at filePath to EDEX server database>
    gpd spx --f filePath
    <To store product GEMPAK file at filePath to EDEX server database>
    gpd spg --f filePath --p prodName [--v versionNum]
    <To query product information in Gempak format  result saved at filePath>
    gpd qig --p prodName --f filePath                       
    <To query and list product information in Gempak format >
    gpd qigl --p prodName       
    <To query a product (all stations) in Gempak format, optional version number, result saved at filePath > 
    gpd qpg --p prodName --t referenceTime [--f filePath --v versionNum]
    <To query and list a product (all stations) in Gempak format, optional version number > 
    gpd qpgl --p prodName --t referenceTime [--v versionNum]
    <To query a station product (single station) in Gempak format,optional version number, result saved at optional filePath > 
    gpd qsg --p prodName --t referenceTime --id stationId [--f filePath --v versionNum]
    <To query and list a station product (single station) in Gempak format, optional version number > 
    gpd qsgl --p prodName --t referenceTime --id stationId [--v versionNum]
    <To query a moving product in Gempak format, optional version number result saved at optional filePath >  
    gpd qmg --p prodName --t referenceTime --slat latitude --slon longitude  [--f filePath --v versionNum]
    <To query and list a moving product in Gempak format, optional version number >  
    gpd qmgl --p prodName --t referenceTime --slat latitude --slon longitude  [--v versionNum]
    <To purge expired products from DB and HDF5>
    gpd pe
    <To purge all products from DB and HDF5>
    gpd pa
    <To print this usage>
    gpd u
    #######
    NOTE: referenceTime string format yyyy-mm-dd HH:MM:SS, for example "2013-05-21 20:30:00"
          filePath - required when saving Gempak table or XML file
          filePath - optional when querying, if not provided, result will be saved at current directory 
          versionNum - optional, if not provided, latest version is returned
    #######
"""
SUBCOMMAND_MESSAGE = \
"""
    qix: To query product information, result saved at optional filePath
    qixl: To query and list product information 
    qpx: To query a product (all stations), result saved at optional filePath
    qpxl: To query and list a product (all stations)
    qsx: To query a station product, result saved at optional filePath  
    qsxl: To query and list a station product 
    qmx: To query a moving product, result saved at optional filePath 
    qmxl: To query and print a moving product 
    spx: To store product XML file to EDEX server database 
    spg: To store product GEMPAk table file to EDEX server database 
    qig: To query product information, result saved at optional filePath
    qigl: To query and list product information 
    qpg: To query a product (all stations), result saved at optional filePath
    qpgl: To query and list a product (all stations)
    qsg: To query a station product, result saved at optional filePath  
    qsgl: To query and list a station product 
    qmg: To query a moving product, result saved at optional filePath 
    qmgl: To query and print a moving product 
    pe: To purge expired products
    pa: To purge all products
    u: To print usage
"""
def __initLogger():
    global logger
    logger = logging.getLogger("gpd")
    logger.setLevel(logging.DEBUG)
    ch = logging.StreamHandler()
    ch.setLevel(logging.INFO)
    # Uncomment line below to enable debug-level logging
    ch.setLevel(logging.DEBUG)
    formatter = logging.Formatter("\n%(asctime)s %(name)s %(levelname)s:  %(message)s", "%H:%M:%S")
    ch.setFormatter(formatter)
    logger.addHandler(ch)

def __parseCommandLine():
    global parser
    parser = UsageArgumentParser.UsageArgumentParser(prog='gpd',description="Query or store point data database from EDEX.")#, usage=USAGE_MESSAGE)
    #parser.add_argument("-u", action="help",
     #                help="show usage")

    subparsers = parser.add_subparsers(help=USAGE_MESSAGE)
    
    #to print usage
    parser_usage = subparsers.add_parser('u')   
    parser_usage.set_defaults(func=__printUsage)
    
    #To update/replace product information
    #parser_saveinfo = subparsers.add_parser('si')       
    #parser_saveinfo.add_argument("--f", dest="filePath", action="store",required=True,
    #                  help=":target file path for return product")
    #parser_saveinfo.set_defaults(func=__saveProdInfo)

    #To save a GPD product in XML format
    parser_saveprod_xml = subparsers.add_parser('spx')       
    parser_saveprod_xml.add_argument("--f", dest="filePath", action="store",required=True,
                      help=":target file path for return product")
    parser_saveprod_xml.set_defaults(func=__saveProdXml)
    
    #To save a GPD product in GEMPAk table format
    parser_saveprod_gempak = subparsers.add_parser('spg')       
    parser_saveprod_gempak.add_argument("--f", dest="filePath", action="store",required=True,
                      help=":target file path for return product")
    parser_saveprod_gempak.add_argument("--p", dest="prodName", action="store",required=True,
                      help=":name of a Generic Point Data product")   
    parser_saveprod_gempak.add_argument("--l", dest="maxNumLevel", action="store",required=True,
                      help=":max number of level for product")   
    parser_saveprod_gempak.add_argument("--v", dest="versionNum", action="store",
                      help=":product version")
    parser_saveprod_gempak.set_defaults(func=__saveProdGempak)

    #To query product information
    parser_infoXml = subparsers.add_parser('qix')       
    parser_infoXml.add_argument("--p", dest="prodName", action="store",required=True,
                      help=":name of a Generic Point Data product")   
    parser_infoXml.add_argument("--f", dest="filePath", action="store",
                      help=":target file path for return product")
    parser_infoXml.set_defaults(func=__getProdInfoXml)
    
    parser_info_printXml = subparsers.add_parser('qixl')       
    parser_info_printXml.add_argument("--p", dest="prodName", action="store",required=True,
                      help=":name of a Generic Point Data product")         
    #parser_info_printXml.add_argument("--f", dest="filePath", action="store",
    #                  help=":target file path for return product")
    parser_info_printXml.set_defaults(func=__getPrintProdInfoXml)
    
    parser_infoGempak = subparsers.add_parser('qig')       
    parser_infoGempak.add_argument("--p", dest="prodName", action="store",required=True,
                      help=":name of a Generic Point Data product")   
    parser_infoGempak.add_argument("--f", dest="filePath", action="store",
                      help=":target file path for return product")
    parser_infoGempak.set_defaults(func=__getProdInfoGempak)
    
    parser_info_printGempak = subparsers.add_parser('qigl')       
    parser_info_printGempak.add_argument("--p", dest="prodName", action="store",required=True,
                      help=":name of a Generic Point Data product")         
    parser_info_printGempak.set_defaults(func=__getPrintProdInfoGempak)
    
    #To query a station product (single station)
    parser_stnProdXml = subparsers.add_parser('qsx') 
    parser_stnProdXml.add_argument("--p", dest="prodName", action="store",required=True,
                      help=":name of a Generic Point Data product")   
    parser_stnProdXml.add_argument("--t", dest="refTime", action="store",required=True,
                      help=":reference time of a product")
    parser_stnProdXml.add_argument("--id", dest="stnId", action="store",required=True,
                      help=":station id of a product")
    parser_stnProdXml.add_argument("--f", dest="filePath", action="store",
                      help=":target file path for return product")
    parser_stnProdXml.add_argument("--v", dest="versionNum", action="store",
                      help=":product version")
    parser_stnProdXml.set_defaults(func=__getStnProdXml)
    
    parser_stnProdXml_print = subparsers.add_parser('qsxl') 
    parser_stnProdXml_print.add_argument("--p", dest="prodName", action="store",required=True,
                      help=":name of a Generic Point Data product")   
    parser_stnProdXml_print.add_argument("--t", dest="refTime", action="store",required=True,
                      help=":reference time of a product")
    parser_stnProdXml_print.add_argument("--id", dest="stnId", action="store",required=True,
                      help=":station id of a product")
    parser_stnProdXml_print.add_argument("--v", dest="versionNum", action="store",
                      help=":product version")
    parser_stnProdXml_print.set_defaults(func=__getPrintStnProdXml)
    
    
    parser_stnProdGempak = subparsers.add_parser('qsg') 
    parser_stnProdGempak.add_argument("--p", dest="prodName", action="store",required=True,
                      help=":name of a Generic Point Data product")   
    parser_stnProdGempak.add_argument("--t", dest="refTime", action="store",required=True,
                      help=":reference time of a product")
    parser_stnProdGempak.add_argument("--id", dest="stnId", action="store",required=True,
                      help=":station id of a product")
    parser_stnProdGempak.add_argument("--f", dest="filePath", action="store",
                      help=":target file path for return product")
    parser_stnProdGempak.add_argument("--v", dest="versionNum", action="store",
                      help=":product version")
    parser_stnProdGempak.set_defaults(func=__getStnProdGempak)
    
    parser_stnProdGempak_print = subparsers.add_parser('qsgl') 
    parser_stnProdGempak_print.add_argument("--p", dest="prodName", action="store",required=True,
                      help=":name of a Generic Point Data product")   
    parser_stnProdGempak_print.add_argument("--t", dest="refTime", action="store",required=True,
                      help=":reference time of a product")
    parser_stnProdGempak_print.add_argument("--id", dest="stnId", action="store",required=True,
                      help=":station id of a product")
    parser_stnProdGempak_print.add_argument("--v", dest="versionNum", action="store",
                      help=":product version")
    parser_stnProdGempak_print.set_defaults(func=__getPrintStnProdGempak)
    
    #To query a moving product
    parser_movingProdXml = subparsers.add_parser('qmx') 
    parser_movingProdXml.add_argument("--p", dest="prodName", action="store",required=True,
                      help=":name of a Generic Point Data product")   
    parser_movingProdXml.add_argument("--t", dest="refTime", action="store",required=True,
                      help=":reference time of a product")
    parser_movingProdXml.add_argument("--slat", dest="slat", type=float,  action="store",required=True,
                      help=":latitude of a moving product")
    parser_movingProdXml.add_argument("--slon", dest="slon", type=float,  action="store",required=True,
                      help=":longitude of a moving product")
    parser_movingProdXml.add_argument("--f", dest="filePath", action="store",
                      help=":target file path for return product")
    parser_movingProdXml.add_argument("--v", dest="versionNum", action="store",
                      help=":product version")
    parser_movingProdXml.set_defaults(func=__getMovingProdXml)
    
    parser_movingProdXml_print = subparsers.add_parser('qmxl') 
    parser_movingProdXml_print.add_argument("--p", dest="prodName", action="store",required=True,
                      help=":name of a Generic Point Data product")   
    parser_movingProdXml_print.add_argument("--t", dest="refTime", action="store",required=True,
                      help=":reference time of a product")
    parser_movingProdXml_print.add_argument("--slat", dest="slat", type=float,  action="store",required=True,
                      help=":latitude of a moving product")
    parser_movingProdXml_print.add_argument("--slon", dest="slon", type=float,  action="store",required=True,
                      help=":longitude of a moving product")
    parser_movingProdXml_print.add_argument("--v", dest="versionNum", action="store",
                      help=":product version")
    parser_movingProdXml_print.set_defaults(func=__getPrintMovingProdXml)
    
    
    parser_movingProdGempak = subparsers.add_parser('qmg') 
    parser_movingProdGempak.add_argument("--p", dest="prodName", action="store",required=True,
                      help=":name of a Generic Point Data product")   
    parser_movingProdGempak.add_argument("--t", dest="refTime", action="store",required=True,
                      help=":reference time of a product")
    parser_movingProdGempak.add_argument("--slat", dest="slat", type=float,  action="store",required=True,
                      help=":latitude of a moving product")
    parser_movingProdGempak.add_argument("--slon", dest="slon", type=float,  action="store",required=True,
                      help=":longitude of a moving product")
    parser_movingProdGempak.add_argument("--f", dest="filePath", action="store",
                      help=":target file path for return product")
    parser_movingProdGempak.add_argument("--v", dest="versionNum", action="store",
                      help=":product version")
    parser_movingProdGempak.set_defaults(func=__getMovingProdGempak)
    
    parser_movingProdGempak_print = subparsers.add_parser('qmgl') 
    parser_movingProdGempak_print.add_argument("--p", dest="prodName", action="store",required=True,
                      help=":name of a Generic Point Data product")   
    parser_movingProdGempak_print.add_argument("--t", dest="refTime", action="store",required=True,
                      help=":reference time of a product")
    parser_movingProdGempak_print.add_argument("--slat", dest="slat", type=float,  action="store",required=True,
                      help=":latitude of a moving product")
    parser_movingProdGempak_print.add_argument("--slon", dest="slon", type=float,  action="store",required=True,
                      help=":longitude of a moving product")
    parser_movingProdGempak_print.add_argument("--v", dest="versionNum", action="store",
                      help=":product version")
    parser_movingProdGempak_print.set_defaults(func=__getPrintMovingProdGempak)
    

    #To query a product
    parser_prodXml = subparsers.add_parser('qpx') 
    parser_prodXml.add_argument("--p", dest="prodName", action="store",required=True,
                      help=":name of a Generic Point Data product")   
    parser_prodXml.add_argument("--t", dest="refTime", action="store",required=True,
                      help=":reference time of a product")
    parser_prodXml.add_argument("--f", dest="filePath", action="store",
                      help=":target file path for return product")
    parser_prodXml.add_argument("--v", dest="versionNum", action="store",
                      help=":product version")
    parser_prodXml.set_defaults(func=__getProdXml)
    
    
    parser_prodGempak = subparsers.add_parser('qpg') 
    parser_prodGempak.add_argument("--p", dest="prodName", action="store",required=True,
                      help=":name of a Generic Point Data product")   
    parser_prodGempak.add_argument("--t", dest="refTime", action="store",required=True,
                      help=":reference time of a product")
    parser_prodGempak.add_argument("--f", dest="filePath", action="store",
                      help=":target file path for return product")
    parser_prodGempak.add_argument("--v", dest="versionNum", action="store",
                      help=":product version")
    parser_prodGempak.set_defaults(func=__getProdGempak)
    
    parser_prodXml_print = subparsers.add_parser('qpxl') 
    parser_prodXml_print.add_argument("--p", dest="prodName", action="store",required=True,
                      help=":name of a Generic Point Data product")   
    parser_prodXml_print.add_argument("--t", dest="refTime", action="store",required=True,
                      help=":reference time of a product")
    parser_prodXml_print.add_argument("--v", dest="versionNum", action="store",
                      help=":product version")
    
    parser_prodXml_print.set_defaults(func=__getPrintProdXml)
    
    parser_prodGempak_print = subparsers.add_parser('qpgl') 
    parser_prodGempak_print.add_argument("--p", dest="prodName", action="store",required=True,
                      help=":name of a Generic Point Data product")   
    parser_prodGempak_print.add_argument("--t", dest="refTime", action="store",required=True,
                      help=":reference time of a product")
    parser_prodGempak_print.add_argument("--v", dest="versionNum", action="store",
                      help=":product version")
    
    parser_prodGempak_print.set_defaults(func=__getPrintProdGempak)
    

    '''
    #parser_purge_prod = subparsers.add_parser('pp') 
    #parser_purge_prod.add_argument("--p", dest="prodName", action="store",required=True,
    #                  help=":name of a Generic Point Data product")   
    #parser_purge_prod.add_argument("--t", dest="refTime", action="store",
    #                  help=":reference time of a product")
    #parser_purge_prod.add_argument("--all", dest="all", action="store",
    #                  help=":yes")
    #parser_purge_prod.set_defaults(func=__purgeProd)
    '''
    
    parser_purge_expired = subparsers.add_parser('pe') 
    parser_purge_expired.set_defaults(func=__purgeExpired)

    parser_purge_all = subparsers.add_parser('pa') 
    parser_purge_all.set_defaults(func=__purgeAll)
    
    options = parser.parse_args()

    #logger.debug("Command-line arguments: " + str(options))
    return options

def __convertTimeToDate(refTime):
    struct_time = time.strptime(refTime, "%Y-%m-%d %H:%M:%S")
    #print "returned tuple: %s " % struct_time
    #print "timeZone " + str(time.timezone) + " altimezone " + str(time.altzone)
    milsec = (time.mktime(struct_time)-time.altzone) * 1000 
    date= Date()
    date.setTime(milsec)
    #print "time in msec: %s" % str(date.getTime())
    return date

def __createFilenameStringXml(name, refTime):
    filename = name + '@'+ refTime.replace(" ","@")+".xml"
    return filename

def __createFilenameStringGempak(name, refTime):
    filename = name + '@'+ refTime.replace(" ","@")+".gempak"
    return filename

def __getPrintProdInfoXml(options):
    req= GpdCliRequestHandler.GpdCliRequestHandler()
    prodInfo = req.getGpdProdInfo(options.prodName, "GET_GPD_PRODUCT_INFO_XML")
    if(prodInfo != None):      
        print(prodInfo)
    else:
        print("Query failed!")
    
def __getProdInfoXml(options):
    #if  options.prodName == None:
    #    parser.print_help()
    #    return None
    req= GpdCliRequestHandler.GpdCliRequestHandler()
    prodInfo = req.getGpdProdInfo(options.prodName,"GET_GPD_PRODUCT_INFO_XML")
    if(prodInfo != None):
        if(options.filePath == None):
            f = open(options.prodName+"Info.xml",'w')
        else:
            f = open(options.filePath,'w')
        f.write(prodInfo)
        return prodInfo
    else:
        print("Query failed!")
        return None

def __getPrintProdInfoGempak(options):
    req= GpdCliRequestHandler.GpdCliRequestHandler()
    prodInfo = req.getGpdProdInfo(options.prodName, "GET_GPD_PRODUCT_INFO_GEMPAK_TBL")
    if(prodInfo != None):      
        print(prodInfo)
    else:
        print("Query failed!")
    
def __getProdInfoGempak(options):
    #if  options.prodName == None:
    #    parser.print_help()
    #    return None
    req= GpdCliRequestHandler.GpdCliRequestHandler()
    prodInfo = req.getGpdProdInfo(options.prodName,"GET_GPD_PRODUCT_INFO_GEMPAK_TBL")
    if(prodInfo != None):
        if(options.filePath == None):
            f = open(options.prodName+"Info.gempak",'w')
        else:
            f = open(options.filePath,'w')
        f.write(prodInfo)
        return prodInfo
    else:
        print("Query failed!")
        return None

    #===========================================================================
    # print ("GPD Report::::::::::::::") 
    # print str("Report Name: "+ prodInfo.getName())
    # print str("Master Level Name = " + prodInfo.getMasterLevel().getName())
    # print str("Master Level Description = " + prodInfo.getMasterLevel().getDescription())
    # print str("Master Level Type = " + prodInfo.getMasterLevel().getType())
    # print str("Master Level Unit String = " + prodInfo.getMasterLevel().getUnitString())
    # print ("Max NUmber of Level = " + str(prodInfo.getMaxNumberOfLevel()))
    # print ("Parameters:")
    # parmLst =prodInfo.getParameterLst()
    # i = 1
    # for parm in parmLst:
    #    print ("Parameter "+ str(i)+ ": Abbreviation="+ str(parm.getAbbreviation())+ " Unit="+str(parm.getUnitString())+ " Name =" + str(parm.getName()))
    #    i=i+1
    #===========================================================================

def __getStnProdXml(options):            
    #if  options.prodName == None or options.refTime == None or options.stnId == None:
    #    parser.print_help()
    #    return None
    date = __convertTimeToDate(options.refTime)
    req= GpdCliRequestHandler.GpdCliRequestHandler()
    
    if(options.versionNum == None):
        stnProd = req.getGpdStationProduct(options.stnId, date,options.prodName,"GET_GPD_STATION_PRODUCT_XML")
    else:
        stnProd = req.getGpdStationProduct(options.stnId, date,options.prodName, "GET_GPD_STATION_PRODUCT_XML", True, int(options.versionNum))
    if(stnProd!=None):
        if(options.filePath == None):
            filename = __createFilenameStringXml((options.prodName+'@'+options.stnId),options.refTime)
        else:
            filename = options.filePath        
        f = open(filename,'w')
        f.write(stnProd)
        return stnProd
    else:
        print("Query failed!")
        return None
def __getPrintStnProdXml(options):       
    date = __convertTimeToDate(options.refTime)
    req= GpdCliRequestHandler.GpdCliRequestHandler()
    
    if(options.versionNum == None):
        stnProd = req.getGpdStationProduct(options.stnId, date,options.prodName,"GET_GPD_STATION_PRODUCT_XML")
    else:
        stnProd = req.getGpdStationProduct(options.stnId, date,options.prodName, "GET_GPD_STATION_PRODUCT_XML",True, int(options.versionNum))
    if(stnProd != None):      
        print(stnProd)
    else:
        print("Query failed!")

def __getStnProdGempak(options):            
    #if  options.prodName == None or options.refTime == None or options.stnId == None:
    #    parser.print_help()
    #    return None
    date = __convertTimeToDate(options.refTime)
    req= GpdCliRequestHandler.GpdCliRequestHandler()
    
    if(options.versionNum == None):
        stnProd = req.getGpdStationProduct(options.stnId, date,options.prodName,"GET_GPD_STATION_PRODUCT_GEMPAK_TBL")
    else:
        stnProd = req.getGpdStationProduct(options.stnId, date,options.prodName, "GET_GPD_STATION_PRODUCT_GEMPAK_TBL", True, int(options.versionNum))
    if(stnProd!=None):
        if(options.filePath == None):
            filename = __createFilenameStringGempak((options.prodName+'@'+options.stnId),options.refTime)
        else:
            filename = options.filePath        
        f = open(filename,'w')
        f.write(stnProd)
        return stnProd
    else:
        print("Query failed!")
        return None
def __getPrintStnProdGempak(options):       
    date = __convertTimeToDate(options.refTime)
    req= GpdCliRequestHandler.GpdCliRequestHandler()
    
    if(options.versionNum == None):
        stnProd = req.getGpdStationProduct(options.stnId, date,options.prodName,"GET_GPD_STATION_PRODUCT_GEMPAK_TBL")
    else:
        stnProd = req.getGpdStationProduct(options.stnId, date,options.prodName, "GET_GPD_STATION_PRODUCT_GEMPAK_TBL",True, int(options.versionNum))
    if(stnProd != None):      
        print(stnProd)
    else:
        print("Query failed!")

def __getMovingProdXml(options):            
    #if  options.prodName == None or options.refTime == None or options.slat == None or options.slon == None:
    #    parser.print_help()
    #    return None
    date = __convertTimeToDate(options.refTime)
    req= GpdCliRequestHandler.GpdCliRequestHandler()
    if(options.versionNum == None):
        stnProd = req.getGpdMovingProduct(options.slat, options.slon, date,options.prodName,"GET_GPD_MOVING_PRODUCT_XML")
    else:
        stnProd = req.getGpdMovingProduct(options.slat, options.slon, date,options.prodName, "GET_GPD_MOVING_PRODUCT_XML",True, int(options.versionNum))
    if(stnProd!=None):
        if(options.filePath == None):
            filename = __createFilenameStringXml((options.prodName+'@'+str(options.slat)+'#'+str(options.slon)),options.refTime)        
        else:
            filename = options.filePath                
        f = open(filename,'w')
        f.write(stnProd)
        return stnProd
    else:
        print("Query failed!")
        return None
def __getPrintMovingProdXml(options):
    date = __convertTimeToDate(options.refTime)
    req= GpdCliRequestHandler.GpdCliRequestHandler()
    if(options.versionNum == None):
        stnProd = req.getGpdMovingProduct(options.slat, options.slon, date,options.prodName,"GET_GPD_MOVING_PRODUCT_XML")
    else:
        stnProd = req.getGpdMovingProduct(options.slat, options.slon, date,options.prodName, "GET_GPD_MOVING_PRODUCT_XML", True, int(options.versionNum))
    if(stnProd != None):      
        print(stnProd)
    else:
        print("Query failed!")
 
def __getMovingProdGempak(options):            
    #if  options.prodName == None or options.refTime == None or options.slat == None or options.slon == None:
    #    parser.print_help()
    #    return None
    date = __convertTimeToDate(options.refTime)
    req= GpdCliRequestHandler.GpdCliRequestHandler()
    if(options.versionNum == None):
        stnProd = req.getGpdMovingProduct(options.slat, options.slon, date,options.prodName,"GET_GPD_MOVING_PRODUCT_GEMPAK_TBL")
    else:
        stnProd = req.getGpdMovingProduct(options.slat, options.slon, date,options.prodName, "GET_GPD_MOVING_PRODUCT_GEMPAK_TBL",True, int(options.versionNum))
    if(stnProd!=None):
        if(options.filePath == None):
            filename = __createFilenameStringGempak((options.prodName+'@'+str(options.slat)+'#'+str(options.slon)),options.refTime)        
        else:
            filename = options.filePath                
        f = open(filename,'w')
        f.write(stnProd)
        return stnProd
    else:
        print("Query failed!")
        return None
def __getPrintMovingProdGempak(options):
    date = __convertTimeToDate(options.refTime)
    req= GpdCliRequestHandler.GpdCliRequestHandler()
    if(options.versionNum == None):
        stnProd = req.getGpdMovingProduct(options.slat, options.slon, date,options.prodName,"GET_GPD_MOVING_PRODUCT_GEMPAK_TBL")
    else:
        stnProd = req.getGpdMovingProduct(options.slat, options.slon, date,options.prodName, "GET_GPD_MOVING_PRODUCT_GEMPAK_TBL", True, int(options.versionNum))
    if(stnProd != None):      
        print(stnProd)
    else:
        print("Query failed!")
   
def __getProdXml(options):            
    #if  options.prodName == None or options.refTime == None:
    #    parser.print_help()
    #    return None
    date = __convertTimeToDate(options.refTime)
    req= GpdCliRequestHandler.GpdCliRequestHandler()
    if(options.versionNum == None):
        prod = req.getGpdProduct( date,options.prodName,"GET_GPD_PRODUCT_XML")
    else:
        prod = req.getGpdProduct( date,options.prodName,"GET_GPD_PRODUCT_XML", True, int(options.versionNum))
    if(prod != None):
        if(options.filePath == None):
            filename = __createFilenameStringXml(options.prodName,options.refTime)
        else:
            filename = options.filePath        
        f = open(filename,'w')
        f.write(prod)
        return prod
    else:
        print("Query failed!")
        return None

def __getPrintProdXml(options): 
    date = __convertTimeToDate(options.refTime)
    req= GpdCliRequestHandler.GpdCliRequestHandler()
    if(options.versionNum == None):
        prod = req.getGpdProduct( date,options.prodName,"GET_GPD_PRODUCT_XML")
    else:
        prod = req.getGpdProduct( date,options.prodName,"GET_GPD_PRODUCT_XML", True, int(options.versionNum))
    if(prod != None):      
        print(prod)
    else:
        print("Query failed!")
  
def __getProdGempak(options):            
    #if  options.prodName == None or options.refTime == None:
    #    parser.print_help()
    #    return None
    date = __convertTimeToDate(options.refTime)
    req= GpdCliRequestHandler.GpdCliRequestHandler()
    if(options.versionNum == None):
        prod = req.getGpdProduct( date,options.prodName,"GET_GPD_PRODUCT_GEMPAK_TBL")
    else:
        prod = req.getGpdProduct( date,options.prodName,"GET_GPD_PRODUCT_GEMPAK_TBL", True, int(options.versionNum))
    if(prod != None):
        if(options.filePath == None):
            filename = __createFilenameStringGempak(options.prodName,options.refTime)
        else:
            filename = options.filePath        
        f = open(filename,'w')
        f.write(prod)
        return prod
    else:
        print("Query failed!")
        return None

def __getPrintProdGempak(options): 
    date = __convertTimeToDate(options.refTime)
    req= GpdCliRequestHandler.GpdCliRequestHandler()
    if(options.versionNum == None):
        prod = req.getGpdProduct( date,options.prodName,"GET_GPD_PRODUCT_GEMPAK_TBL")
    else:
        prod = req.getGpdProduct( date,options.prodName,"GET_GPD_PRODUCT_GEMPAK_TBL", True, int(options.versionNum))
    if(prod != None):      
        print(prod)
    else:
        print("Query failed!")
     
#def __purgeProd(options):            
#    req= GpdCliRequestHandler.GpdCliRequestHandler()
#    req.purgeGpdProd(options.prodName,options.refTime, option.all)
    
def __purgeExpired(options):            
    req= GpdCliRequestHandler.GpdCliRequestHandler()
    req.purgeGpdExpired()

def __purgeAll(options):            
    req= GpdCliRequestHandler.GpdCliRequestHandler()
    req.purgeGpdAll()
 
def __printUsage(options):
    print(USAGE_MESSAGE)
    

def __saveProdXml(options):
    # read in XML from input file
    xmlfile = io.open(options.filePath, 'rb')
    gpdXML = xmlfile.read()
    xmlfile.close()
 
    #  Store GPD report to EDEX
    req= GpdCliRequestHandler.GpdCliRequestHandler()
    result = req.storeGpdXmlProduct(gpdXML)
    if result==None:
        print("Store action failed!")
    else:
        print(result)

def __saveProdGempak(options):
    # read in XML from input file
    gpkfile = io.open(options.filePath, 'rb')
    gpdGempak = gpkfile.read()
    gpkfile.close()
 
    #  Store GPD report to EDEX
    req= GpdCliRequestHandler.GpdCliRequestHandler()
    maxNumLevel = options.maxNumLevel
    
    if(options.versionNum == None):
        result = req.storeGpdGempakProduct(gpdGempak,options.prodName,int(maxNumLevel),0)
    else:
        result = req.storeGpdGempakProduct(gpdGempak,options.prodName,int(maxNumLevel),int(options.versionNum))
    
    if result==None:
        print("Store action failed!")
    else:
        print(result)

def main():
    __initLogger()
    #logger.info("Starting Query report.")
    options = __parseCommandLine()
    #vars(options)
    options.func(options)
    #logger.info("queried report name: " + options.prodName)
    
    return 0

if __name__ == '__main__':
    main()
