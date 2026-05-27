#!/awips2/python/bin/python
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

CONFIG_PATHS = ["/awips2/edex/data/utility/common_static/*/*/styleRules/*.xml",
                "/awips2/edex/data/utility/cave_static/*/*/ncep/styleRules/*.xml"
                ]

logging.basicConfig(format='%(asctime)-15s %(levelname)s:  %(message)s', 
                    datefmt='%Y-%m-%d %H:%M:%S',
                    level=logging.DEBUG)
log = logging.getLogger("replaceStylePrefs.py")

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
    return status

def updateFile(path):
    logging.debug("Processing file: %s", path)
        
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

    if root.tag != "styleRuleset":
        log.info("Not a styleRule xml file: %s", path)
        return 0
    
    for styleRule in root.iterfind("styleRule"):
        for imageStyle in styleRule.iterfind("imageStyle"):
            samplePrefs = imageStyle.find("samplePrefs")
            if samplePrefs is not None:
                logging.debug("found samplePrefs")
                changed = True
                formatString = samplePrefs.find("formatString")
                numericFormat = samplePrefs
                numericFormat.tag = "numericFormat"
                if formatString is not None:
                    logging.debug("formatString: %s", ET.tostring(formatString, 'utf-8'))
                    numericFormat.remove(formatString)
                    count = int(formatString.text)
                    logging.debug("count: %d", count)
                    pattern = "0"
                    if count > 0:
                        pattern += "." + "0"* count
                    logging.debug("pattern: %s", pattern)
                    ET.SubElement(numericFormat, "pattern").text = pattern
                logging.debug("numericFormat: %s", ET.tostring(numericFormat, 'utf-8'))

    if changed:
        log.info("Updating file: %s", path)
        backupPath = path+".DR8145.bak"
        if not os.path.exists(backupPath):
            shutil.copy(path, backupPath)
        pretty_xml = minidom.parseString(ET.tostring(root, 'utf-8')).toprettyxml(indent=' '*4, encoding='UTF-8')
        pretty_xml = '\n'.join([line for line in pretty_xml.split('\n') if line.strip()])
        text_re = re.compile('>\n\s+([^<>\s].*?)\n\s+</', re.DOTALL)
        pretty_xml = text_re.sub('>\g<1></', pretty_xml)
        
        rootTag = "<"+root.tag+">"
        header = readHeaderComments(path, rootTag)
        rootPos =  pretty_xml.find(rootTag)
        try:
            with open(path, 'w') as outFile:
                outFile.write(header)
                outFile.write(pretty_xml[rootPos:])
        except Exception:
            log.exception("Unable to update %s", path)
            return 1
    return 0

def readHeaderComments(path, rootTag):
    with open(path, 'r') as inFile:
        orig = inFile.read()
        
    pos = orig.find(rootTag)
    return orig[0:pos]

def main():
    log.info("Running delta script for RODO DR 8145...")
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
        log.error("delta script for RODO 8145 complete with errors")    
    else:
        log.info("delta script for RODO 8145 complete")    
    return status

if __name__ == '__main__':
    sys.exit(main())
