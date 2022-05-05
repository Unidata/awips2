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

# This is a utility provided to convert old style .svg syntax to something that the new plot rendering
# code expects.
# The code basically scans the .svg file and removes certain attributes and moves them to a plotparamters_$plugin file
#

import xml.etree.ElementTree as ET
import sys, shutil, os, time
from os import path
from datetime import datetime

svgNS = "{http://www.w3.org/2000/svg}"
NEW_LINE="\n\n"
markerPluginElement = {'displayName':'Marker', 'param':'Marker', 'displayType': 'MARKER', "svgClass":"marker", 'sampleValue':' '}

markerSVGElement = {'id':'marker', 'plotParam':'Marker', 'class': 'marker', 'x':'-20px', 'y':'-20px'}
styleElement = {'type':'text/css'}
markerFont = "    @font-face { font-family: \"MarkerSymbolFont\";\n" + "              src: url(MarkerSymbols.svg#MarkerSymbols); }\n"

valid_modes = ["TEXT", "BARB", "TABLE", "RANGE", "ARROW", "ARROWUV", "MARKER"]

############################################
class _CommentedTreeBuilder ( ET.XMLTreeBuilder ):
    def __init__ ( self, html = 0, target = None ):
        ET.XMLTreeBuilder.__init__( self, html, target )
        self._parser.CommentHandler = self.handle_comment
    
    def handle_comment ( self, data ):
        self._target.start( ET.Comment, {} )
        self._target.data( data )
        self._target.end( ET.Comment )


############################################
# process the CDATA section. The xmltree doesn't handle CDATA correctly.
# It removes them during the parsing process. So we replace them with a <CDATA> tag and reverse it when
# we are done
def processCDATA(fName, postProcess=False):

    with open(fName) as f:
        file_string = f.read()
 
    if postProcess is False:
        file_string = file_string.replace("<![CDATA[", "<CDATA>")
        file_string = file_string.replace("]]>", "</CDATA>")
    else:
        file_string = file_string.replace("<CDATA>", "<![CDATA[")
        file_string = file_string.replace("</CDATA>", "]]>")
        file_string = file_string.replace("#NEWCDATA#", getDefaultCDATAText() )

    with open(fName, 'w') as f:
        f.write(file_string)

############################################
def getLicenseText():
    licenseText = '''
        This_software_was_developed_and_/_or_modified_by_Raytheon_Company,
        pursuant_to_Contract_DG133W-05-CQ-1067_with_the_US_Government.
        
        U.S._EXPORT_CONTROLLED_TECHNICAL_DATA
        This_software_product_contains_export-restricted_data_whose
        export/transfer/disclosure_is_restricted_by_U.S._law._Dissemination
        to_non-U.S._persons_whether_in_the_United_States_or_abroad_requires
        an_export_license_or_other_authorization.
        
        Contractor_Name:________Raytheon_Company
        Contractor_Address:_____6825_Pine_Street,_Suite_340
        ________________________Mail_Stop_B8
        ________________________Omaha,_NE_68106
        ________________________402.291.0100
        
        See_the_AWIPS_II_Master_Rights_File_("Master_Rights_File.pdf")_for
        further_licensing_information.
    -->
    <!--
        This is an absolute override file, indicating that a higher priority
        version of the file will completely replace a lower priority version
        of the file.
    '''
    return licenseText
############################################
def getDefaultCDATAText():
    cText = '''
            <![CDATA[
             @font-face { font-family: "WindSymbolFont";
              src: url(WindSymbols.svg#WindSymbols); }
             @font-face { font-family: "StandardFont";
              src: url(Standard.svg#Standard); }
             @font-face { font-family: "SpecialSymbolFont";
              src: url(SpecialSymbols.svg#SpecialSymbols); }
             @font-face { font-family: "MarkerSymbolFont";
              src: url(MarkerSymbols.svg#MarkerSymbols); }
            ]]>
    '''
    return cText
############################################
def getSampleValue(plot):
    val = plot.text
    commaCnt = plot.attrib["plotParam"].count(",")
    if commaCnt > 0:
        val = '0'
        for i in range(1, commaCnt+1):
           val = val + "," + str(i)
    return val


############################################
# creates plotParameters_'+pluginName file

def createPlotParamPluginXML(plotParamDefsArray, pluginFileName, pluginName):
    # Add the default market element
    plotParamDefsArray.append(markerPluginElement)
    root = ET.Element('plotParameterDefinitions')
    root.tail=NEW_LINE
    plugin = ET.SubElement(root, 'plugin')
    plugin.text=pluginName
    plugin.tail=NEW_LINE

    # We need to set a "displayName" in the xml file which is a derived
    # from the parameter name

    for element in plotParamDefsArray:
        line = ET.SubElement(root, 'plotParameterDefinition', element)
        line.tail=NEW_LINE
        if element['displayType'] == 'BARB' and "," in element['param']:
           paramFields =  element['param'].split(',')
           paramDisplayFields = element['displayName'].split(',')
           
           #create the Wind Dir
           windDirPluginElement =  { 'displayType': 'ARROWUV', 'sampleValue':'16,17'}
           windDirPluginElement["displayName"] = paramDisplayFields[1]
           windDirPluginElement["param"] = paramFields[0]+','+paramFields[1]
           if "unit" in element:
              windDirPluginElement["unit"] = element["unit"]   
           line = ET.SubElement(root, 'plotParameterDefinition', windDirPluginElement)
           line.tail=NEW_LINE
           
           ## Create the BARB and Gust only if there are 3 fields to begin with 
           # Example: plotParam="windSpeed,windDir,windGust"
           
           # If there are only 2, create only the wind dir component. (that is the only additional parameter that can be "derived"
           # from 2).
           # Example: plotParam="wSp,WD"
           
           if(len(paramFields) == 3):
              windBarbPluginElement = { 'displayType': 'BARB', 'sampleValue':'16,17'}
              windBarbPluginElement["displayName"] = paramDisplayFields[0]+','+paramDisplayFields[1]
              windBarbPluginElement["param"] = paramFields[0]+','+paramFields[1]
              if "unit" in element:
                 windBarbPluginElement["unit"] = element["unit"]
              line = ET.SubElement(root, 'plotParameterDefinition', windBarbPluginElement)
              line.tail=NEW_LINE
            
              windGustPluginElement = { 'displayType': 'TEXT', 'sampleValue':'8'}
              windGustPluginElement["displayName"] = paramDisplayFields[2]
              windGustPluginElement["param"] = paramFields[2]
              if "unit" in element:
                 windGustPluginElement["unit"] = element["unit"]
              line = ET.SubElement(root, 'plotParameterDefinition', windGustPluginElement)
              line.tail=NEW_LINE    
    tree = ET.ElementTree(root)
    tree.write(pluginFileName, encoding='UTF-8', method='xml')


############################################
# updates plotParameters_'+pluginName file

def updatePlotParamPluginXML(plotParamDefsArray, pluginFileName):


    ctb = _CommentedTreeBuilder()
    tree = ET.parse(pluginFileName, ctb)
    root = tree.getroot()
    existingDefs = root.findall('plotParameterDefinition')

    # We need to be careful that the display names are unique.
    # If there are multiple, starting the second one, they will be named $name-1, $name-2 etc
    existingDisplayNames = []
    for plotDef in existingDefs:
        existingDisplayNames.append(plotDef.attrib['displayName'])

    for element in plotParamDefsArray:
        orgDisplayName = element['displayName']
        displayName = element['displayName']
        i = 1
        while displayName in existingDisplayNames:
            displayName = orgDisplayName + '-' + str(i)
            i += 1
        element['displayName'] = displayName
        line = ET.SubElement(root, 'plotParameterDefinition', element)
        line.tail = NEW_LINE
        existingDisplayNames.append(displayName)
    tree = ET.ElementTree(root)
    tree.write(pluginFileName, encoding='UTF-8', method='xml')

###################### print usage 
def print_usage():
    print ("Usage: python convertSVG.py plugin svgFile")
    sys.exit (1)
###################### main body
def main():
    global logger 

    if len (sys.argv) != 3 :
        print_usage()
    if(sys.argv[1] == '-h' or sys.argv[1] == '-help'):
        print_usage()


    pluginName = sys.argv[1]
    svgFileName = sys.argv[2]
    if path.exists(svgFileName):
        # check that the plugin directory exists here
        src = path.realpath(svgFileName)
        svgDir = path.dirname(path.realpath(src))
        pluginNameStripped = pluginName
        if "/" in pluginName:
        	pluginNameStripped = pluginNameStripped.partition("/")[2]
        if not path.isdir(path.join(svgDir, pluginNameStripped)):
            print "Unable to find plugin directory:" + path.join(svgDir, pluginNameStripped)
            sys.exit (1)
        dst = src + "-" + datetime.fromtimestamp(time.time()).strftime('%Y-%m-%d-%H:%M:%S')
        shutil.copy(src, dst)
        pluginFileName = path.join(path.join(svgDir, pluginNameStripped), "plotParameters_"+pluginNameStripped+".xml")
    else:
        print ("Unable to open svg file: " + svgFileName)
        sys.exit (1)

    ET.register_namespace("", 'http://www.w3.org/2000/svg')
    ET.register_namespace("xlink", "http://www.w3.org/1999/xlink")

# new_root is used to hold the licensing and override text. In the original .svg file, this info sits outside
# of any root tag and hence not parsed. The meaningful xml contents of the .svg file are then attached to the new root

    new_root = ET.Element(None)

# Add css processing instructions
    pi = ET.PI("xml-stylesheet", "type='text/css' href='newplots.css'")
    pi.tail = NEW_LINE
    new_root.append(pi)

# Add licensing text and other comments
    licenseText = ET.Comment(getLicenseText())
    licenseText.tail = NEW_LINE
    new_root.insert(0, licenseText)

# Preprocess the file to handle CDATA sections

    processCDATA(svgFileName)
#
    ctb = _CommentedTreeBuilder()
    tree = ET.parse(svgFileName, ctb)
    root = tree.getroot()
    root.set("xmlns", "http://www.w3.org/2000/svg")
    root.set("xmlns:xlink", "http://www.w3.org/1999/xlink")
    root.set("plugin", pluginName)
    
    new_root.append(root)

#These are the parameters that we expect to find in the .svg file
    plotParams = ('plotMode', 'plotParam', 'plotFormat', 'plotUnit', 'plotSymbol', 'plotTrim', 'plotFunctionTable', 'plotLookupTable', 'plotIndex', 'class')

#The names in .svg and plotParameters_{$plugin} file doesn't match.
#We need a small mapping dictionary

    mappedParamNames = {'plotMode':'displayType', 
            'plotParam':'param',
            'plotFormat':'format',
            'plotUnit':'unit', 
            'plotSymbol':'symbol', 
            'plotTrim':'trim', 
            'plotFunctionTable':'functionTable', 
            'plotLookupTable' : 'lookupTable',
            'plotIndex': 'index',
            'class':'svgClass'}
    
    hasStyle = False
    
    for child in root:

    # The heavy lifting is under the "symbol" element
        for symbol in child.findall(svgNS + 'symbol'):
            plotParamDefsArray = []
    # Scan and find all symbol/text and symbol/g
            plotFields = symbol.findall(svgNS + 'text')
            plotFields.extend(symbol.findall(svgNS + 'g'))

            for plotField in plotFields:
                plotParam = plotField.attrib["plotParam"]
                plotParamDef = {'param':plotParam}

# We need to keep only "plotParam" (and special cases plotMode=SAMPLE and plotMode=NONE) field in the SVG. 
# All others are removed and moved to plotParameters_{$plugin} file
# Also note that the "unit", if any, is added to the plotParam and plotparam
# becomes "plotParam + ($unit)" in the modified .svg file.
# Some modes are treated with care!

                for param in plotParams:
                    if param in plotField.attrib and param != 'plotParam':
                        deleteParam = True
                        if param == 'plotMode':
                            mode = plotField.attrib[param]
                            if mode.lower() == 'sample':
                                plotField.attrib['plotMode'] = 'SAMPLE'
                                plotParamDef['displayType'] = 'TEXT'
                                deleteParam = False
                            elif mode.lower() == 'null':
                                plotField.attrib['plotMode'] = 'NONE'
                                plotParamDef['displayType'] = 'TEXT'
                                deleteParam = False
                            elif mode.lower() == 'recursive_translation':
                                plotParamDef['displayType'] = 'TABLE'
                                plotParamDef['recursiveLookup'] = 'true'
                            elif mode.upper() in valid_modes:
                                plotParamDef['displayType'] = mode.upper()
                            else:
                                plotParamDef['displayType'] = 'TEXT'
                        else:
                            plotParamDef[mappedParamNames[param]] = plotField.attrib[param]
                        if deleteParam:
                            del plotField.attrib[param]
                    # if you have index or units, change the poltParam name in the .svg (which maps to the displayname in .xml
                    # it is possible for multiple indices pointing to the same base parameter
                    elif param == 'plotParam':
                        if 'plotIndex' in plotField.attrib:
                            plotField.attrib[param] = plotField.attrib[param] + '-' + plotField.attrib['plotIndex']
                        if 'plotUnit' in plotField.attrib:
                            plotField.attrib[param] = plotField.attrib[param] + ' (' + plotField.attrib['plotUnit'] + ')' 
                        plotParamDef['displayName'] = plotField.attrib[param]
                plotParamDef['sampleValue'] = getSampleValue(plotField)
                plotParamDefsArray.append(plotParamDef)

    # Add the default Marker element
            line = ET.SubElement(symbol, 'text', markerSVGElement)
            line.tail=NEW_LINE
            line.text=' '
            
        
   # Now, add the markerFont in the style section
        for script in child.findall(svgNS + 'style'):
            hasStyle = True
        # Scan and find all script/cdata
            for cdata in script.findall(svgNS + 'CDATA'):
                if "MarkerSymbols" not in cdata.text:
                    cdata.text = cdata.text + markerFont
        
    
    # there is no style element in the original. Add a default
    if not hasStyle:
        for defs in root.findall(svgNS + 'defs'):
    
            style = ET.SubElement(defs, 'style', styleElement)
            style.text = '#NEWCDATA#'
            style.tail=NEW_LINE
        
    newTree = ET.ElementTree(new_root)
    newTree.write(svgFileName, encoding='UTF-8', method='xml')

#Now change the CDATA xml tags to <![CDATA[ 
    processCDATA(svgFileName, postProcess=True)

# Now create/update the plotparameters_{$plugin} file (Eg: plotParameters_obs.xml)"
    if path.exists(pluginFileName):
        updatePlotParamPluginXML(plotParamDefsArray, pluginFileName)
    else:
        createPlotParamPluginXML(plotParamDefsArray, pluginFileName, pluginName)

######################
if __name__ == "__main__":
    main()
