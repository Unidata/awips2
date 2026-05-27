#!/awips2/python/bin/python
import glob
import logging
import os
import re
import shutil
import sys
from xml.etree.ElementTree import ParseError
from xml.parsers.expat import ErrorString
from xml.parsers.expat import errors

import xml.dom.minidom as minidom
import xml.etree.ElementTree as ET

DR_NUMBER = "8334"

CONFIG_PATHS = ["/awips2/edex/data/utility/*/*/*/archiver/purger/*.xml"]

logging.basicConfig(format='%(asctime)-15s %(levelname)s:  %(message)s',
                    datefmt='%Y-%m-%d %H:%M:%S',
                    level=logging.DEBUG)
log = logging.getLogger("updateDataStoreRetention.py")


class CommentedTreeBuilder(ET.TreeBuilder):

    def __init__(self, *args, **kwargs):
        super(CommentedTreeBuilder, self).__init__(*args, **kwargs)

    def comment(self, data):
        self.start(ET.Comment, {})
        self.data(data)
        self.end(ET.Comment)


def processDir(directory):
    status = 0
    log.info(f"Processing directory: {directory}")
    for root, directories, files in os.walk(directory, topdown=False):
        for filePath in files:
            if str(filePath).endswith(".xml"):
                fullPath = os.path.join(root, filePath)
                status |= updateFile(fullPath)
    return status


def updateFile(path):
    rootTagStr = "archive"

    logging.debug(f"Processing file: {path}")
    rootTag = f"<{rootTagStr}>"
    header, xml = readHeaderComments(path, rootTag)

    changed = False
    try:
        parser = ET.XMLParser(target=CommentedTreeBuilder())
        root = ET.XML(xml, parser)
    except ParseError as e:
            if ErrorString(e.code) == errors.XML_ERROR_JUNK_AFTER_DOC_ELEMENT:
                log.info(f"Skipping xml fragment: {path}")
            elif ErrorString(e.code) == errors.XML_ERROR_NO_ELEMENTS:
                log.info(f"Skipping empty xml document: {path}")
            else:
                log.error(f"Error parsing file: {path} \"{ErrorString(e.code)}\"")
            return 1

    except Exception:
        log.exception(f"Unable to parse XML path: {path}")
        return 1

    if root.tag != rootTagStr:
        log.info(f"Not a {rootTagStr} xml file: {path}")
        return 0

    rootDir = root.find("rootDir")
    if rootDir is not None and not rootDir.text.startswith("/data_store"):
        log.info(f"Not for /data_store: {path}")
        return 0

    defaultRetention = root.find("defaultRetentionHours")
    if defaultRetention is not None:
        hours = int(defaultRetention.text)
        if hours > 12:
            changed = True
            defaultRetention.text = "12"

    for category in root.iterfind("category"):
        selectedRetention = category.find("selectedRetentionHours")
        hours = int(selectedRetention.text)
        if hours > 12:
            changed = True
            selectedRetention.text = "12"

    if changed:
        log.info(f"Updating file: {path}")
        backupPath = path + f".DR{DR_NUMBER}.bak"
        if not os.path.exists(backupPath):
            shutil.copy(path, backupPath)
        pretty_xml = minidom.parseString(ET.tostring(root, 'utf-8')).toprettyxml(indent=' ' * 4, encoding='UTF-8').decode()
        pretty_xml = '\n'.join([line for line in pretty_xml.split('\n') if line.strip()])
        text_re = re.compile('>\n\s+([^<>\s].*?)\n\s+</', re.DOTALL)
        pretty_xml = text_re.sub('>\g<1></', pretty_xml)

        rootPos = pretty_xml.find(rootTag)
        try:
            with open(path, 'w') as outFile:
                outFile.write(header)
                outFile.write(pretty_xml[rootPos:])
        except Exception:
            log.exception(f"Unable to update {path}")
            return 1
    return 0


def readHeaderComments(path, rootTag):
    with open(path, 'r') as inFile:
        orig = inFile.read()

    pos = orig.find(rootTag)
    start = orig.find("<!--")
    while start > 0 and start < pos:
        end = orig.find("-->", start + 4)
        if end < 0:
            # unclosed comment
            raise Exception("File contains unclosed comment")

        if end > pos:
            pos = orig.find(rootTag, end + 3)
        start = orig.find("<!--", end + 3)
    return orig[0:pos], orig[pos:]


def main():
    log.info(f"Running delta script for RODO DR {DR_NUMBER}...")
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
        log.error(f"delta script for RODO {DR_NUMBER} complete with errors")
    else:
        log.info(f"delta script for RODO {DR_NUMBER} complete")
    return status


if __name__ == '__main__':
    sys.exit(main())
