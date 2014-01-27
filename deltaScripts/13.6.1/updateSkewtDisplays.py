#!/usr/bin/python
# This script will update any saved displays which use older skewT displays to
# use Nsharp.
#
# This update only needs to be run if there are saved displays being stored
# outside of localization, for procedures saved in localization,
# updateSkewtProcedures.sh will automatically call this.


import sys
import xml.etree.ElementTree as ET

xsitype = '{http://www.w3.org/2001/XMLSchema-instance}type'

def upgradeBundle(bundleFile):
    tree = ET.parse(bundleFile)
    root = tree.getroot()
    iterpath = 'bundles/bundle/displayList/displays'
    if root.tag == 'bundle':
        iterpath = 'displayList/displays'
    for display in root.iterfind(iterpath):
        if display.get(xsitype) == "skewtDisplay":
            plugins = getPlugins(display)
            nsharp = False
            varheight = False
            for plugin in plugins:
                nsharp |= isNsharp(plugin)
                varheight |= isHodoVarHeight(plugin)
            if varheight and nsharp:
                # This will cause the bundle to continue loading old sounding,
                # this is not a big problem until that is deleted
                print 'Cannot convert bundle with both var height hodo and nsharp'
            elif varheight:
                convertDisplayToHodoVarHeight(display)
            elif nsharp:
                 convertDisplayToNsharp(display)
        elif display.get(xsitype) == "d2DNSharpDisplay":
            display.set(xsitype,'nsharpSkewTPaneDisplay')
            descriptor = display.find('descriptor')
            descriptor.set(xsitype,'nsharpSkewTPaneDescriptor')
    tree.write(bundleFile)
    
def getPlugins(xmlDisplay):
    plugins = set()
    for resourceData in xmlDisplay.iterfind('descriptor/resource/resourceData'):
        plugin = getPluginName(resourceData)
        if plugin is not None:
            plugins.add(plugin)
    return plugins;

def getPluginName(resourceData):
    if resourceData.get(xsitype) == 'gribSoundingSkewTResourceData':
        return 'grib'
    elif resourceData.get(xsitype) == 'skewTResourceData':
        return getConstraintValue(resourceData, 'pluginName')
    return None

def getConstraintValue(resourceData, key):
    for mapping in resourceData.iterfind('metadataMap/mapping'):
        if(mapping.get('key') == key):
            return mapping.find('constraint').get('constraintValue')
    return None

def isNsharp(plugin):
   return plugin == 'grib' or plugin == 'bufrua' or\
          plugin == 'goessounding' or plugin == 'poessounding' or\
          plugin == 'acarssounding' or plugin == 'modelsounding' 
          
def isHodoVarHeight(plugin):
    return plugin == 'radar' or plugin == 'profiler'

def convertDisplayToNsharp(xmlDisplay):
    xmlDisplay.set(xsitype,'nsharpSkewTPaneDisplay')
    descriptor = xmlDisplay.find('descriptor')
    descriptor.set(xsitype,'nsharpSkewTPaneDescriptor')
    toRemove = []
    for resource in descriptor.iterfind('resource'):
        resourceData = resource.find('resourceData')
        type = resourceData.get(xsitype)
        if type == 'skewTBkgResourceData':
            toRemove.append(resource)
        elif type == 'gribSoundingSkewTResourceData':
            convertResourceToNsharp(resource)
        elif type == 'skewTResourceData':
            convertResourceToNsharp(resource)
        else:
            print "Removing unrecognized resource of type: " + type
            toRemove.append(resource)
    for resource in toRemove:
        descriptor.remove(resource)

def convertResourceToNsharp(xmlResource):
    resourceData = xmlResource.find('resourceData')
    plugin = getPluginName(resourceData)
    if(plugin == 'grib'):
        resourceData.set(xsitype,'gribNSharpResourceData')
        resourceData.set('soundingType', getConstraintValue(resourceData, 'info.datasetId'))
        resourceData.set('pointName', 'Point' + resourceData.get('point'))
        resourceData.attrib.pop('point')
    elif(plugin == 'bufrua'):
        resourceData.set(xsitype,'bufruaNSharpResourceData')
        resourceData.set('soundingType', 'BUFRUA')
    elif(plugin == 'modelsounding'):
        reportType = getConstraintValue(resourceData, 'reportType')
        if reportType == 'ETA':
            reportType = 'NAM'
        resourceData.set('soundingType', reportType + 'SND')
        resourceData.set(xsitype,'mdlSndNSharpResourceData')
    elif(plugin == 'goessounding'):
        resourceData.set('soundingType', 'GOES')
        resourceData.set(xsitype,'goesSndNSharpResourceData')
    elif(plugin == 'poessounding'):
        resourceData.set('soundingType', 'POES')
        resourceData.set(xsitype,'poesSndNSharpResourceData')
    elif(plugin == 'acarssounding'):
        resourceData.set('soundingType', 'MDCRS')
        resourceData.set(xsitype,'acarsSndNSharpResourceData')
    loadProperties = xmlResource.find('loadProperties')
    if loadProperties is not None:
        # since nsharp doesn't use any capabilities just drop them all.
        capabilities = loadProperties.find('capabilities')
        if capabilities is not None:
            loadProperties.remove(capabilities)

def convertDisplayToHodoVarHeight(xmlDisplay):
    xmlDisplay.set(xsitype,'varHeightRenderableDisplay')
    xmlDisplay.set('tabTitle','Var vs height : Log 1050-150')
    descriptor = xmlDisplay.find('descriptor')
    descriptor.set(xsitype,'varHeightHodoDescriptor')
    toRemove = []
    for resource in descriptor.iterfind('resource'):
        resourceData = resource.find('resourceData')
        pluginName = getPluginName(resourceData)
        type = resourceData.get(xsitype)
        if type == 'skewTBkgResourceData':
            toRemove.append(resource)
        elif type == 'skewTResourceData':
            resourceData.set(xsitype,'varHeightResourceData')
            resourceData.set('parameter','Wind')
            resourceData.set('parameterName','Wind')
            if pluginName == 'radar':
                ET.SubElement(resourceData, 'source').text = 'VWP'
            else:
                ET.SubElement(resourceData, 'source').text = pluginName
        else:
            print "Removing unrecognized resource of type: " + type
            descriptor.remove(resource)
    for resource in toRemove:
        descriptor.remove(resource)
        
    heightScale = ET.SubElement(descriptor, 'heightScale')
    heightScale.set('unit','MILLIBARS')
    heightScale.set('name','Log 1050-150')
    heightScale.set('minVal','1050.0')
    heightScale.set('maxVal','150.0')
    heightScale.set('parameter','P')
    heightScale.set('parameterUnit','hPa')
    heightScale.set('scale','LOG')
    heightScale.set('heightType','PRESSURE')
    ET.SubElement(heightScale, 'labels').text = '1000,850,700,500,400,300,250,200,150'
    
    gridGeometry = descriptor.find('gridGeometry')
    gridGeometry.set('rangeX','0 999')
    gridGeometry.set('rangeY','0 999')
    gridGeometry.set('envelopeMinX','0.0')
    gridGeometry.set('envelopeMaxX','1000.0')
    gridGeometry.set('envelopeMinY','0.0')
    gridGeometry.set('envelopeMaxY','1000.0')

    

if __name__ == '__main__':
    for arg in sys.argv[1:]:
        upgradeBundle(arg)