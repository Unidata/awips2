#!/awips2/python/bin/python
# Update plotResourceData lowerLimit from -9999.0 to -9998.0
# This script should be run as user AWIPS from dx3 with no command line arguments to update 
# any D2D procedures stored in localization.
# 
# The script may also be run by the field to update saved displays/perspectives outside localization
#
# When run by the field 1 or more files or directories containing save displays/perspectives 
# should be provided as command line arguments

import glob
import getpass
import socket
import logging
import shutil
import sys
import re
import os

import xml.dom.minidom as minidom
import xml.etree.ElementTree as ET
from xml.etree.ElementTree import ParseError
from xml.parsers.expat import ErrorString
from xml.parsers.expat import errors

SITE_PATHS = ["/awips2/edex/data/utility/cave_static/*/*/procedures",
              "/awips2/edex/data/utility/common_static/*/*/perspectives",
             ]

LOWER_LIMIT_KEY = "lowerLimit"
LOWER_LIMIT = -9998.0

logging.basicConfig(format='%(asctime)-15s %(levelname)s:  %(message)s', 
                    datefmt='%Y-%m-%d %H:%M:%S',
                    level=logging.DEBUG)
log = logging.getLogger("updateLowerLimit.py")

class CommentedTreeBuilder(ET.TreeBuilder):
    def __init__(self, *args, **kwargs):
        super(CommentedTreeBuilder, self).__init__(*args, **kwargs)

    def comment(self, data):
        self.start(ET.Comment, {})
        self.data(data)
        self.end(ET.Comment)

def processDir(directory):
    log.info("Processing directory: %s", directory)
    for root, directories, files in os.walk(directory, topdown=False):
        for filePath in files:
            if str(filePath).endswith(".xml"):
                fullPath = os.path.join(root, filePath)
                updateFile(fullPath)
                    
        for directory in directories:
            fullPath = os.path.join(root, directory)
            processDir(fullPath)

def updateFile(path):
    changed = False
    try:
        parser = ET.XMLParser(target=CommentedTreeBuilder())
        tree = ET.parse(path, parser)
        root = tree.getroot()
    except ParseError as e:
            if ErrorString(e.code) == errors.XML_ERROR_JUNK_AFTER_DOC_ELEMENT:
                log.info("Skipping xml fragment: %s", path)
            elif ErrorString(e.code) == errors.XML_ERROR_NO_ELEMENTS:
                log.info("Skipping empty xml document: %s", path)
            else:
                log.error("Error parsing file: %s \"%s\"", path, ErrorString(e.code))
            return 1
        
    except Exception:
        log.exception("Unable to parse XML file: %s", path)
        return 1

    for resourceData in root.iter("resourceData"):
        # ensure we have a plotResourceData
        isPlotResourceData = False
        for key, value in resourceData.attrib.items():
            if key.endswith("}type") and value == "plotResourceData":
                isPlotResourceData = True
                break
            
        # if we have plotResourceData
        if isPlotResourceData:
            # if lowerLimt less than LOWER_LIMIT replace it with LOWER_LIMIT
            if LOWER_LIMIT_KEY in resourceData.attrib:
                value = resourceData.attrib[LOWER_LIMIT_KEY]
                if float(value) < LOWER_LIMIT:
                    resourceData.attrib[LOWER_LIMIT_KEY] = str(LOWER_LIMIT)
                    changed = True

    if changed:
        log.info("Updating path: %s", path)
        shutil.copy(path, path+".bak")
        pretty_xml = minidom.parseString(ET.tostring(root, 'utf-8')).toprettyxml(indent=' '*4, encoding='UTF-8')
        pretty_xml = '\n'.join([line for line in pretty_xml.split('\n') if line.strip()])
        text_re = re.compile('>\n\s+([^<>\s].*?)\n\s+</', re.DOTALL)
        pretty_xml = text_re.sub('>\g<1></', pretty_xml)
        
        try:
            with open(path, 'w') as outFile:
                outFile.write(pretty_xml)
        except Exception:
            log.exception("Unable to update %s", path)
            return 1
    return 0
        
def processArgs():
    status = 0
    searchPaths = SITE_PATHS
    if len(sys.argv) > 1:
        searchPaths = sys.argv[1:]
    else:
        # No paths on command line 
        # verify we are running on dx3 as user awips 
        userName = getpass.getuser()
        hostName = socket.gethostname()
        log.info("Running %s as user: %s on host: %s", sys.argv[0], userName, hostName)
        if userName != "awips" or not hostName.startswith("dx3"):
            log.error("To update files under the localization tree you must run as user awips on dx3")
            return -1

    for sp in searchPaths:
        paths = glob.glob(sp)
        for path in paths:
            if os.path.isdir(path):
                processDir(path)
            elif os.path.isfile(path):
                updateFile(path)

    
    return status
        
def main():
    log.info("RODO DR #6894 update plotResourceData lowerLimit to -9998.0")
    status = processArgs()
    if status == 0:
        log.info("RODO DR #6894: Success.")
    else:
        log.warning("RODO DR #6894: Errors encountered.")
        
    return status

if __name__ == '__main__':
    sys.exit(main())
