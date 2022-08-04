#! /awips2/python/bin/python
#
# Update MONITOR textBox configurations
# 
# usage updateMonitorTextBox.py path...
# where path can be either an xml file or directory of xml files containing
# alertViz configurations to be updated.
# 
# If run with no parameters it will process all xml files under any directory listed
# in CONFIG_PATHS below
#

import glob
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

CONFIG_PATHS = ["/awips2/edex/data/utility/cave_static/*/*/alertViz/configurations"]

logging.basicConfig(format='%(asctime)-15s %(levelname)s:  %(message)s', 
                    datefmt='%Y-%m-%d %H:%M:%S',
                    level=logging.DEBUG)
log = logging.getLogger("updateAlertVizActions.py")

class CommentedTreeBuilder(ET.TreeBuilder):
    def __init__(self, *args, **kwargs):
        super(CommentedTreeBuilder, self).__init__(*args, **kwargs)

    def comment(self, data):
        self.start(ET.Comment, {})
        self.data(data)
        self.end(ET.Comment)

def processDir(directory):
    status = 0
    log.info("Processing directory: %s", directory)
    for root, directories, files in os.walk(directory, topdown=False):
        for filePath in files:
            if str(filePath).endswith(".xml"):
                fullPath = os.path.join(root, filePath)
                status |= updateFile(fullPath)
                    
        for directory in directories:
            fullPath = os.path.join(root, directory)
            status |= processDir(fullPath)
    return status

def updateFile(path):
    if (path.endswith("/alertViz/AlertVizForced.xml")):
        return 0
        
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
        log.error("Unable to parse XML path: %s", path)
        return 1

    if root.tag != "alertConfiguration":
        log.warning("Not an alertConfiguration xml file: %s", path)
        return 0
        
    for metaData in root.iter("category"):
        if metaData.attrib["categoryName"] == "MONITOR":
            if metaData.attrib["textBox"] != "0": 
                metaData.attrib["textBox"] = "0"
                changed = True

    if changed:
        log.info("Updating file: %s", path)
        backupPath = path+".DR7659.bak"
        if not os.path.exists(backupPath):
            shutil.copy(path, backupPath)
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

def main():
    log.info("Running delta script for RODO DR 7659 (O&M DR 12512)...")
    status = 0
    
    searchPaths = CONFIG_PATHS
    if len(sys.argv) > 1:
        searchPaths = sys.argv[1:]
    for sp in searchPaths:
        paths = glob.glob(sp)
        for path in paths:
            if os.path.isdir(path):
                status |= processDir(path)
            elif os.path.isfile(path):
                status |= updateFile(path)


    if status:
        log.error("delta script for RODO DR 7659 (O&M DR 12512) complete with errors")    
    else:
        log.info("delta script for RODO DR 7659 (O&M DR 12512) complete")    
    return status

if __name__ == '__main__':
    sys.exit(main())
