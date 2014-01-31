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

##
# Converts A1 CDL files to A2-formatted gridParamInfo XML files.
#  
#    
#     SOFTWARE HISTORY
#    
#    Date            Ticket#       Engineer       Description
#    ------------    ----------    -----------    --------------------------
#    08/30/12        #1117         dgilling       Initial Creation.
#    01/08/14        #2657         dgilling       Use new unified grid tags,
#                                                 code cleanup.
#
##

import errno
import glob
import logging
import os
import re
import subprocess
import sys
import xml.dom.minidom as minidom
import xml.etree.ElementTree as ET

from ufpy import UsageArgumentParser

try:
    # dev environment
    from Scientific.IO import NetCDF
except:
    # runtime we don't have the whole scientific package
    import NetCDF


logger = None


def main():
    __initLogger()
    
    logger.info("Starting convertCDL2XML...")
    
    with open(os.devnull, 'w') as DEVNULL:
        try:
            subprocess.check_call("type ncgen", stdout=DEVNULL, stderr=subprocess.STDOUT, shell=True)
        except subprocess.CalledProcessError:
            logger.error("This script requires the ncgen program to run. Please install before running this script again.")
            sys.exit(-1)
    
    userOptions = __parseArgs()
    logger.debug("Command-line args: " + str(userOptions))
    
    outputDir = os.path.realpath(userOptions.outputDir)
    try:
        os.makedirs(outputDir)
    except OSError as e:
        if e.errno != errno.EEXIST:
            logger.exception("Could not create output directory " + userOptions.outputDir)
            sys.exit(-1)
            
    for file in userOptions.cdlFiles:
        __convertFile(file, outputDir)
        
    logger.info("All files converted and saved to [" + outputDir + "].")


def __initLogger():
    logging.basicConfig(format="%(asctime)s %(name)s %(levelname)s:  %(message)s",
                        datefmt="%H:%M:%S",
                        level=logging.INFO)    
    global logger
    logger = logging.getLogger("convertCDL2XML")

    
def __parseArgs():
    parser = UsageArgumentParser.UsageArgumentParser(conflict_handler="resolve")
    parser.add_argument("-f", action="append", dest="inputFiles",
                      help="file to convert. Multiple files can be specified by providing multiple -f flags or specifying a directory that contains CDL files.", 
                      required=True, metavar="cdlFile")
    parser.add_argument("-o", action="store", dest="outputDir", 
                      help="directory to write XML files to (defaults to ./convertedCDL)",
                      default="./convertedCDL", metavar="outputDir")
    options = parser.parse_args()
    
    cdlFiles = []
    for file in options.inputFiles:
        if os.path.isfile(file) and os.path.splitext(file)[1] == '.cdl':
            cdlFiles.append(file)
        elif os.path.isdir(file):
            searchString = os.path.join(file, "*.cdl")
            newCdlFiles = glob.glob(searchString)
            cdlFiles.extend([os.path.realpath(cdlFile) for cdlFile in newCdlFiles])
        else:
            parser.error("Invalid -f argument specified: " + file)
    setattr(options, 'cdlFiles', cdlFiles)
    
    return options

def __convertFile(cdlFile, outputDir):
    logger.info("Processing CDL file [" + cdlFile + "].")
    
    # convert to netcdf
    cdfFile = __convertToCdf(cdlFile, outputDir)
    if cdfFile is None:
        logger.warning("Could not convert CDL file [" + cdlFile + "] to temporary CDF file. Skipping...")
        return
    
    # read data
    cdfData = __parseCdfFile(cdfFile)
    if not len(cdfData):
        logger.warning("Could not read parameter info from CDL file [" + cdlFile + "]. Skipping...")
        return
    
    # delete cdf file
    try:
        os.remove(cdfFile)
    except (OSError, IOError):
        logger.exception("Could not delete temporary NetCDF file.")
    
    # create XML structure
    xmlStruct = __createXML(cdfData)
    
    # write nicely formatted XML
    roughString = ET.tostring(xmlStruct, 'utf-8')
    reparsed = minidom.parseString(roughString)
    middleString = reparsed.toprettyxml(encoding='UTF-8', indent='  ')
    text_re = re.compile('>\n\s+([^<>\s].*?)\n\s+</', re.DOTALL)    
    prettyString = text_re.sub('>\g<1></', middleString)
    
    # write to disk
    fileNameParts = os.path.splitext(os.path.basename(cdlFile))
    outputFile = os.path.join(outputDir, fileNameParts[0] + '.xml')
    with open(outputFile, 'w') as outFile:
        outFile.write(prettyString)

def __convertToCdf(cdlFile, outputDir):
    outFile = os.path.join(outputDir, 'tmpFile.nc')
    cmd = ['ncgen', '-x', '-o', outFile, cdlFile]
    try:
        subprocess.check_call(cmd)
    except subprocess.CalledProcessError:
        logger.error("Could not create temporary CDF file for [" + cdlFile + "].")
        return None
    return outFile

def __parseCdfFile(cdfFile):
    valueMap = {}
    parmNames = []
    
    try:
        cdfObj = NetCDF.NetCDFFile(cdfFile, 'r')
        
        # getting times
        # ported from A1 D2DFile.getTimes()
        try:
            timesVar = cdfObj.variables['valtimeMINUSreftime']
            if timesVar.typecode() != 'i':
                raise ValueError
            # converting from numpy array to native list
            times = [val for val in timesVar.getValue()]
            times.sort()
            valueMap['valtimeMINUSreftime'] = times
        except AttributeError:
            logger.exception("Missing valtimeMINUSreftime variable.")
            return {}
        except ValueError:
            logger.exception("Invalid valtimeMINUSreftime variable.")
            return {}
        
        # getting parms
        # ported from A1 D2DFile.getNames()
        for varName in cdfObj.variables.keys():
            var = cdfObj.variables[varName]
            if var.typecode() != 'f':
                continue
            if ' ' in varName:
                continue
            if 'x' not in var.dimensions or 'y' not in var.dimensions:
                continue
            attMap = __getParmAtts(var, varName, cdfObj)
            valueMap[varName] = attMap
            parmNames.append(varName)
    finally:
        if cdfObj is not None:
            cdfObj.close()
    
    valueMap['parmNames'] = parmNames
    return valueMap

def __getParmAtts(ncVar, varName, ncFile):
    attrMap = {'short_name': varName}
    
    try:
        attrMap['units'] = getattr(ncVar, 'units')
    except AttributeError:
        logger.debug("[units] not found for " + varName)
    
    try:
        attrMap['udunits'] = getattr(ncVar, 'udunits')
    except AttributeError:
        logger.debug("[udunits] not found for " + varName)
    
    try:
        attrMap['long_name'] = getattr(ncVar, 'long_name')
    except AttributeError:
        logger.debug("[long_name] not found for " + varName)
        
    try:
        attrMap['uiname'] = getattr(ncVar, 'uiname')
    except AttributeError:
        logger.debug("[uiname] not found for " + varName)
        
    try:
        validRange = getattr(ncVar, 'valid_range')
        attrMap['valid_range'] = (float(validRange[0]), float(validRange[1]))
    except AttributeError:
        if varName not in ('staticTopo', 'staticSpacing', 'staticCoriolis'):
            logger.debug("[valid_range] not found for " + varName)
            
    try:
        fillValue = getattr(ncVar, '_FillValue')
        attrMap['fillValue'] = float(fillValue[0])
    except AttributeError:
        logger.debug("[_FillValue] not found for " + varName)
        
    try:
        n3d = getattr(ncVar, '_n3D')
        attrMap['n3D'] = int(n3d[0])
    except AttributeError:
        logger.debug("[_n3D] not found for " + varName)
        
    try:
        attrMap['levelsDesc'] = getattr(ncVar, 'levels')
    except AttributeError:
        logger.debug("[levels] not found for " + varName)
        
    try:
        rawLevels = ncFile.variables[varName + "Levels"]
        parmLevels = []
        for charString in rawLevels.getValue():
            levelString = ""
            for char in charString:
                if char.isalnum():
                    levelString += char
            if len(levelString):
                parmLevels.append(levelString)
        attrMap['levels'] = parmLevels
    except KeyError:
        logger.debug("[" + varName + "Levels] not found in NetCDF file.")
    
    return attrMap

def __createXML(cdfData):
    root = ET.Element('gridParamInfo', {'xmlns:ns2': 'group'})

    fcstTimes = ET.SubElement(root, 'valtimeMINUSreftime')
    for time in cdfData['valtimeMINUSreftime']:
        child = ET.SubElement(fcstTimes, 'fcst')
        child.text = str(time)

    for parm in cdfData['parmNames']:
        atts = {'xsi:type': "parameterInfo",
                'xmlns:xsi': "http://www.w3.org/2001/XMLSchema-instance"}
        parmRoot = ET.SubElement(root, 'gridParameterInfo', atts)
        for key in ['short_name', 'long_name', 'units', 'udunits', 'uiname', 'valid_range', 'fillValue', 'n3D', 'levelsDesc', 'levels']:
            if key in cdfData[parm]:
                if key == 'valid_range':
                    child = ET.SubElement(parmRoot, 'valid_range')
                    child.text = str(cdfData[parm]['valid_range'][0])
                    child = ET.SubElement(parmRoot, 'valid_range')
                    child.text = str(cdfData[parm]['valid_range'][1])
                elif key == 'levels':
                    levelRoot = ET.SubElement(parmRoot, 'levels')
                    for level in cdfData[parm]['levels']:
                        child = ET.SubElement(levelRoot, 'level')
                        child.text = str(level)
                else:
                    child = ET.SubElement(parmRoot, key)
                    child.text = str(cdfData[parm][key])

    return root

if __name__ == '__main__':
    main()
