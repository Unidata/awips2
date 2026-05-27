#!/usr/bin/env python
# DR 5348 - This script will replace references to us_county.lpi in a bundle or procedure with
# references to the new county_names view in the maps database

import sys
import xml.etree.ElementTree as ET

xsitype = '{http://www.w3.org/2001/XMLSchema-instance}type'

def upgradeBundle(inputFile, outputFile):
    tree = ET.parse(inputFile)
    root = tree.getroot()
    iterpath = 'bundles/bundle/displayList/displays/descriptor/resource'
    if root.tag == 'bundle':
        iterpath = 'displayList/displays/descriptor/resource'

    if checkResources(root, iterpath):
        with open(outputFile, 'w') as out:   
            tree.write(out, encoding='UTF-8', xml_declaration=True)

def checkResources(element, iterPath):
    updated = False
    for resource in element.iterfind(iterPath):
        resourceData = resource.find('resourceData') 
        type = resourceData.get(xsitype)
        if type == 'mapResourceGroupData':
            updated |= checkResources(resourceData, 'resource')
        elif type == 'lpiResourceData':
            filename = resourceData.find('filename')
            if filename.text == 'us_county.lpi':
                convertResource(resource)
                updated |= True
    
    return updated

def convertResource(resource):
    # convert resourceData to dpPointMapResourceData
    resourceData = resource.find('resourceData')
    resourceData.set(xsitype,'dbPointMapResourceData')
    resourceData.remove(resourceData.find('filename'))
    table = ET.SubElement(resourceData, 'table')
    table.text = 'mapdata.county_names'

    # remove the outlineCapability, densityCapability, and labelableCapability
    loadProperties = resource.find('loadProperties')
    capabilities = loadProperties.find('capabilities')
    for capability in capabilities.findall('capability'):
        if capability.get(xsitype) in ['outlineCapability', 'densityCapability', 'labelableCapability']:
            capabilities.remove(capability)
    
    # set density to max so all county names are displayed
    densityCapability = ET.SubElement(capabilities, 'capability')
    densityCapability.set(xsitype, 'densityCapability')
    densityCapability.set('density', '9999')
    
    labelableCapability = ET.SubElement(capabilities, 'capability')
    labelableCapability.set(xsitype, 'labelableCapability')
    labelableCapability.set('labelField', 'name')
    
    # set pdProps to only display county names when counties are displayed in States/County Boundaries
    properties = resource.find('properties')
    if properties == None:
        properties = ET.SubElement(resource, 'properties')
        properties.set('isSystemResource', 'false') 
        properties.set('isBlinking', 'false') 
        properties.set('isMapLayer', 'true') 
        properties.set('isHoverOn', 'false')
        properties.set('isVisible', 'true')

    pdProps = properties.find('pdProps')
    if pdProps  == None:
        pdProps = ET.SubElement(properties, 'pdProps')
        pdProps.set('minDisplayWidth', '0')
    pdProps.set('maxDisplayWidth', '750000')

if __name__ == '__main__':
    if len(sys.argv) < 3:
        print "Usage:\n", sys.argv[0], "inputFile outputFile"
        sys.exit(1)
    
    upgradeBundle(sys.argv[1], sys.argv[2])