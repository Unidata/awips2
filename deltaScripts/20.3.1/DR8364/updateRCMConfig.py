#!/awips2/python/bin/python3
#
# This script must be run on the host where edex_rcm is installed.
#

import copy
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


DR_NUMBER = "8364_8469"

CONFIG_PATHS = ["/awips2/rcm/data/config/persist/config.xml"
                ]

logging.basicConfig(format='%(asctime)-15s %(levelname)s:  %(message)s', 
                    datefmt='%Y-%m-%d %H:%M:%S',
                    level=logging.INFO)
log = logging.getLogger("updateRCMConfig.py")

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

def updateFile(path):
    log.info("Processing file: %s", path)
        
    rootTagStr = "config"
    rootTag = f"<{rootTagStr}>"
    header, xml = readHeaderComments(path, rootTag)

    try:
        parser = ET.XMLParser(target=CommentedTreeBuilder())
        root = ET.XML(xml, parser)
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

    if root.tag != rootTagStr:
        log.info(f"Not a {rootTagStr} xml file: {path}")
        return 0

    originalEndpontConfigElement = None
    endpointConfigElement = root.find("endpointConfig")
    if endpointConfigElement is None:
        log.warn("endpointConfig element not found, is this really an RCM config file?")
        return 1

    originalEndpontConfigElement = copy.deepcopy(endpointConfigElement)
    
    # set up default values for JMS configuration
    hostname = "localhost"
    port = "5672"
    vhost = "edex"
    servicePort = "8180"
    
    # if connectionURL element is present, 
    # parse out existing values for hostname, port, vhost, and remove it
    connectionURLElement = endpointConfigElement.find("connectionURL")
    if connectionURLElement is not None:
        url = connectionURLElement.text
        match = re.compile("amqp://[^:]+:[^@]+@/(?P<vhost>[^?]+)\?brokerlist='tcp://(?P<hostname>[^:]+):(?P<port>\d+)'.*").match(url)
        if match:
            hostname = match.group("hostname")
            port = match.group("port")
            vhost = match.group("vhost")
            endpointConfigElement.remove(connectionURLElement)
        else:
            log.error(f"Unable to parse url: {url}\nFile not updated.")

    # if host element is present from earlier errant delta script,
    # change the tag to hostname
    hostElement = endpointConfigElement.find("host")
    if hostElement is not None:
        hostElement.tag = "hostname"
        hostnameElement = hostElement

    # if hostname element is missing add it
    hostnameElement = endpointConfigElement.find("hostname")
    if hostnameElement is None:
        hostnameElement = ET.SubElement(endpointConfigElement, "hostname") 
        hostnameElement.text = hostname

    # if port is element missing add it
    portElement = endpointConfigElement.find("port")
    if portElement is None:
        portElement = ET.SubElement(endpointConfigElement, "port")
        portElement.text = port

    # if vhost element is missing add it
    vhostElement = endpointConfigElement.find("vhost")
    if vhostElement is None:
        vhostElement = ET.SubElement(endpointConfigElement, "vhost") 
        vhostElement.text = vhost

    # if servicePort element is missing add it
    servicePortElement = endpointConfigElement.find("servicePort")
    if servicePortElement is None:
        servicePortElement = ET.SubElement(endpointConfigElement, "servicePort")
        servicePortElement.text = servicePort

    if ET.tostring(endpointConfigElement) != ET.tostring(originalEndpontConfigElement):
        log.info("Updating file: %s", path)
        backupPath = f"{path}.DR{DR_NUMBER}.bak"
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
            log.exception("Unable to update %s", path)
            return 1
    return 0

def main():
    log.info(f"Running delta script for RODO DR {DR_NUMBER}...")
    status = 0
    
    searchPaths = CONFIG_PATHS
    if len(sys.argv) > 1:
        searchPaths = sys.argv[1:]
    log.debug("searchPaths: %s", searchPaths)
    for sp in searchPaths:
        paths = glob.glob(sp)
        log.debug("paths: %s", paths)
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
