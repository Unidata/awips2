#!/usr/bin/env python
# This script will update any saved displays or procedures with the old Topo file name
#
# This update only needs to be run if there are saved displays being stored
# outside of localization, for procedures saved in localization,
# updateTopoFile.sh will automatically call this.


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
        if display.get(xsitype) == "d2DMapRenderableDisplay":
            for resourceData in display.iterfind('descriptor/resource/resourceData'):
                if resourceData.get(xsitype) == 'topoResourceData':
                    for topoFile in resourceData.iterfind('topoFile'):
                        if topoFile.text == 'srtm30.hdf':
                            topoFile.text = 'defaultTopo.h5'
    tree.write(bundleFile)
    
if __name__ == '__main__':
    for arg in sys.argv[1:]:
        upgradeBundle(arg)