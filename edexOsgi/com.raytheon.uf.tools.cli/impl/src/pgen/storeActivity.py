##
# This script is used to store a PGEN Activity to EDEX.
# 
# Users can override the default EDEX server and port name by specifying them
# in the $DEFAULT_HOST and $DEFAULT_PORT shell environment variables.
# 
##
import io
import logging
import xml.etree.ElementTree as ET

from ufpy import UsageArgumentParser
from dynamicserialize.dstypes.gov.noaa.nws.ncep.common.dataplugin.pgen import ActivityInfo
import ProductStorer

logger = None
def __initLogger():
    global logger
    logger = logging.getLogger("storeActivity")
    logger.setLevel(logging.DEBUG)
    ch = logging.StreamHandler()
    ch.setLevel(logging.INFO)
    # Uncomment line below to enable debug-level logging
    ch.setLevel(logging.DEBUG)
    formatter = logging.Formatter("%(asctime)s %(name)s %(levelname)s:  %(message)s", "%H:%M:%S")
    ch.setFormatter(formatter)
    logger.addHandler(ch)

def __parseCommandLine():
    parser = UsageArgumentParser.UsageArgumentParser(prog='storeActivity',description="Store a PGEN Activity to EDEX.")
    #bgroup = parser.add_argument_group(title='batch',description='For running in scripts and/or batch mode.')

    #parser.add_argument("-i", action="store", dest="infoFile",
    #                  help="Activity Information used to store Activity",
    #                  required=True, metavar="infoFile")
    parser.add_argument("-t", action="store", dest="activityType",
                      help="Activity Type",
                      required=False, metavar="type")
    parser.add_argument("-st", action="store", dest="activitySubtype",
                      help="Activity Subtype",
                      required=False, metavar="subtype")
    parser.add_argument("-n", action="store", dest="activityName",
                      help="Activity Name",
                      required=False, metavar="name")
    parser.add_argument("-s", action="store", dest="site",
                      help="site identifier",
                      required=False, metavar="site")
    parser.add_argument("-f", action="store", dest="forecaster",
                      help="forecaster",
                      required=False, metavar="forecaster")
    parser.add_argument("filename",
                      help="File containing XML Activity to be stored",
                      metavar="filename")
    options = parser.parse_args()

    logger.debug("Command-line arguments: " + str(options))
    return options

#
# create an ActivityInfo object from command line input and
# activityXML. If no input was found from command line, the 
# info in activityXML will be used.
#
def __getActivityInfo(xml, options):

    ainfo = ActivityInfo()
    
    tree = ET.fromstring(xml)
    product = tree.find('Product')
    
    # strip the path from file and use it as activityLabel
    
    fullname = options.filename
    lastslash = fullname.rfind("/")
    filename = fullname
    if ( lastslash >=0 ):
        filename = fullname[lastslash+1:]
    
    ainfo.setActivityLabel(filename)
        
    if ( options.forecaster != None ):
        ainfo.setForecaster(options.forecaster)
    else:
        ainfo.setForecaster(product.attrib['forecaster'])

    if ( options.site != None ):
        ainfo.setSite(options.site)
    else:
        ainfo.setSite(product.attrib['center'])
    
    # set activity type and subtype
    if ( options.activityType != None ):        
        ainfo.setActivityType(options.activityType)
        
        if ( options.activitySubtype != None ): 
            ainfo.setActivitySubtype(options.activitySubtype)
        else:
            ainfo.setActivitySubtype("")             
    else:        
        if (product.attrib['type'].find("(") < 0 ):
            ainfo.setActivityType( product.attrib['type'] )
            ainfo.setActivitySubtype( "" )
        else:
            fulltyp = product.attrib['type']
            start = fulltyp.find("(")
            end = fulltyp.find(")")
            ainfo.setActivityType( fulltyp[0:start] )
            ainfo.setActivitySubtype( fulltyp[start+1:end] )
                            
    # set activityName
    if ( options.activityName != None ):
        ainfo.setActivityName(options.activityName)
    else:
        if ( options.activityType != None ):
            aname = options.activityType;
            if ( options.activitySubtype != None ): 
                aname = aname + "(" + options.activitySubtype + ")"        
            ainfo.setActivityName( aname )        
        else:
            ainfo.setActivityName( product.attrib['name'] )
                  
    return ainfo

#  Update Product tag attributes with options given on command line
def __updateXML(xml, options):
    tree = ET.fromstring(xml)
    product = tree.find('Product')
    if options.activityName != None:
        product.attrib['name'] = options.activityName
                     
    if options.activityType != None:
        if options.activitySubtype != None:
            ntype = options.activityType + '(' + options.activitySubtype + ')'
            product.attrib['type'] = ntype
        else:
            product.attrib['type'] = options.activityType
    
    if options.filename != None:
        product.attrib['outputFile'] = options.filename
    
    if options.forecaster != None:
        product.attrib['forecaster'] = options.forecaster
    
    if options.site != None:
        product.attrib['center'] = options.site
    
    return ET.tostring(tree)

def main():
    __initLogger()
    logger.info("Starting storeActivity.")
    options = __parseCommandLine()
 
    # read in XML from input file
    actfile = io.open(options.filename, 'rb')
    activityXML = actfile.read()
    actfile.close()
 
    # generate an activityInfo object and update XML with options 
    # from command line   
    actinfo = __getActivityInfo(activityXML, options)
    activityXML = __updateXML(activityXML, options)

    #  Store Activity to EDEX
    ps = ProductStorer.ProductStorer(actinfo, activityXML)
    dataURI = ps.storeActivity()

    logger.info("Activity stored with dataURI: " + dataURI)
    logger.info("storeActivity is complete.")

 
if __name__ == '__main__':
    main()