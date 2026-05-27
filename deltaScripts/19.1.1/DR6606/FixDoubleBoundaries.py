#! /awips2/python/bin/python
#
# Replace the mapdata.world constraint to work with the new world_admin0
# shape file
# 
# usage FixDoubleBoundaries.py path...
# where path can be either an xml file or directory of xml files containing
# bundles/procedures to be updated.
# 
# If run with no parameters it will process all xml under any directory listed
# in SITE_PATHS below
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

SITE_PATHS = ["/awips2/edex/data/utility/cave_static/*/*/bundles/maps",
              "/awips2/edex/data/utility/cave_static/*/*/bundles/scales",
              "/awips2/edex/data/utility/cave_static/*/*/procedures"
             ]
OLD_CONSTRAINT = "NAME NOT IN ('CANADA', 'MEXICO', 'UNITED STATES')"
NEW_CONSTRAINT = "upper(name) NOT IN ('CANADA', 'MEXICO', 'UNITED STATES') AND first_coun != 'Y'"

logging.basicConfig(format='%(asctime)-15s %(levelname)s:  %(message)s', 
                    datefmt='%Y-%m-%d %H:%M:%S',
                    level=logging.DEBUG)
log = logging.getLogger("FixDoubleBoundaries.py")

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
                log.warning("Skipping xml fragment: %s", path)
            elif ErrorString(e.code) == errors.XML_ERROR_NO_ELEMENTS:
                log.warning("Skipping empty xml document: %s", path)
            else:
                log.error("Error parsing file: %s \"%s\"", path, ErrorString(e.code))
            return 1
        
    except Exception:
        log.error("Unable to parse XML path: %s", fole)
        traceback.print_exc()
        return 1

    for resourceData in root.iter("resourceData"):
        for attr in resourceData.attrib.values():
            if attr != "dbMapResourceData":
                continue
            if resourceData.find("table").text.lower() == "mapdata.world":
                constraint = resourceData.find("constraint")
                if constraint is not None:
                    current_constraint = re.sub(r'\s+', ' ', constraint.text).strip().upper()
                    if current_constraint == OLD_CONSTRAINT:
                        constraint.text = NEW_CONSTRAINT
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
        
def main():
    status = 0
    searchPaths = SITE_PATHS
    if len(sys.argv) > 1:
        searchPaths = sys.argv[1:]
    for sp in searchPaths:
        paths = glob.glob(sp)
        for path in paths:
            if os.path.isdir(path):
                processDir(path)
            elif os.path.isfile(path):
                updateFile(path)

    
    return status

if __name__ == '__main__':
    sys.exit(main())
