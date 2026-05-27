#! /awips2/python/bin/python
#
# Search all site level warngen xml files and replace all $warngenCWAFilter
# EQUALS or LIKE constraints with %$warngenCWAFilter% LIKE constraint
#

import glob
import logging
import re
import shutil
import sys

import xml.dom.minidom as minidom
import xml.etree.ElementTree as ET
from xml.etree.ElementTree import ParseError

from xml.parsers.expat import ErrorString
from xml.parsers.expat import errors

SITE_WARNGEN_PATH = "/awips2/edex/data/utility/common_static/site/*/warngen/*.xml"

logging.basicConfig(format='%(asctime)-15s %(levelname)s:  %(message)s', 
                    datefmt='%Y-%m-%d %H:%M:%S',
                    level=logging.DEBUG)
log = logging.getLogger("UpdateWarngenCWAFilters.py")

class CommentedTreeBuilder(ET.TreeBuilder):
    def __init__(self, *args, **kwargs):
        super(CommentedTreeBuilder, self).__init__(*args, **kwargs)

    def comment(self, data):
        self.start(ET.Comment, {})
        self.data(data)
        self.end(ET.Comment)
        
def main():
    log.info("Running delta script for DR #7130")
    status = 0
    paths = glob.glob(SITE_WARNGEN_PATH)
    for path in paths:
        log.info("Processing file: %s", path)
        changed = False
        root = None
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
            continue
            
        except Exception:
            log.error("Unable to parse XML file: %s", path)
            traceback.print_exc()
            continue

        if root.tag != "warngenConfig":
            log.warning("Not a warngenConfig xml file: %s", path)
            continue
        
        for pointSource in root.iterfind("pointSource"):
            if pointSource.attrib["variable"] == "riverdrainages":
                for filter in pointSource.iterfind("filter"):
                    for mapping in filter.iterfind("mapping"):
                        if mapping.attrib["key"] == "cwa":
                            for constraint in mapping.iterfind("constraint"):
                                if constraint.attrib["constraintValue"] =="$warngenCWAFilter" and \
                                   constraint.attrib["constraintType"] in ["LIKE", "EQUALS"]:
                                    
                                    constraint.attrib["constraintValue"] = "%$warngenCWAFilter%"
                                    constraint.attrib["constraintType"] = "LIKE"
                                    changed = True
        
        if changed:
            log.info("Updating file: %s", path)
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
                status = 1
                

    if status:
        log.error("Delta script complete with errors")
    else:
        log.info("Delta script complete")
        
    return status

if __name__ == '__main__':
    sys.exit(main())
