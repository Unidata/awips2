##
##

##
# This is a base file that is not intended to be overridden.
##



import xml
from xml.etree import ElementTree
from xml.etree.ElementTree import Element, SubElement
import IrtAccess

def extractServerInfo(xmlIncoming):
    sources = []
    irt = IrtAccess.IrtAccess("")
    xmlTree = ElementTree.ElementTree(ElementTree.XML(xmlIncoming))
    sourceE = xmlTree.find('source')
    for addressE in sourceE.getchildren():
        sourceServer = irt.decodeXMLAddress(addressE)
        if sourceServer is None:
            continue
        sources.append(irt.printServerInfo(sourceServer))
    return sources

    