##
# This software was developed and / or modified by Raytheon Company,
# pursuant to Contract DG133W-05-CQ-1067 with the US Government.
# 
# U.S. EXPORT CONTROLLED TECHNICAL DATA
# This software product contains export-restricted data whose
# export/transfer/disclosure is restricted by U.S. law. Dissemination
# to non-U.S. persons whether in the United States or abroad requires
# an export license or other authorization.
# 
# Contractor Name:        Raytheon Company
# Contractor Address:     6825 Pine Street, Suite 340
#                         Mail Stop B8
#                         Omaha, NE 68106
#                         402.291.0100
# 
# See the AWIPS II Master Rights File ("Master Rights File.pdf") for
# further licensing information.
##
from pupynere import NetCDFFile
import sys
import xml.etree.ElementTree as ET 

typeMap = { 'd' : 'DOUBLE', 'f' : 'FLOAT', 
            'i' : 'INT', 'c' : 'CHAR',
            's' : 'STRING', 'h' : 'INT' }

excludeVars = ('time', 'validTimeList', 'nStaticIds', 'inventory', 'globalInventory', 'firstInBin', 'lastInBin', 'lastRecord', 'staticIds','prevRecord', 'firstOverflow', 'isOverflow')


def main():
    fileName = sys.argv[1]

    ncFile = NetCDFFile(fileName, 'r')
    description = ET.Element('pointDataDescription')

    variableKeys = ncFile.variables.keys()
    variableKeys.sort()
    for variableName in variableKeys:
       if variableName in excludeVars:
          continue
       variableObject = ncFile.variables[variableName]
       varElement = ET.SubElement(description, 'parameter')
       varElement.set('name', variableName) 
       typeCode = variableObject.typecode()
       numDims = len(variableObject.shape)
       if numDims > 1 and typeCode is 'c':
          typeCode = 's' 
          numDims = numDims - 1
       varElement.set('type', typeMap[typeCode])
       varElement.set('numDims', str(numDims))

       if 'units' in variableObject._attributes: 
          unit = variableObject._attributes['units']
          varElement.set('unit', unit)
       if '_FillValue' in variableObject._attributes:
          fillValue = variableObject._attributes['_FillValue']
          # Disabled for now: 
          # if fillValue:
          #   varElement.set('fillValue',str(long(fillValue)))
    indentElement(description)
    print ET.tostring(description, 'UTF-8')

def indentElement(elem, level=0):
    """Used for pretty printing XML."""
    i = "\n" + level*"  "
    if len(elem):
        if not elem.text or not elem.text.strip():
            elem.text = i + "  "
        for e in elem:
            indentElement(e, level+1)
            if not e.tail or not e.tail.strip():
                e.tail = i + "  "
        if not e.tail or not e.tail.strip():
            e.tail = i
    else:
        if level and (not elem.tail or not elem.tail.strip()):
            elem.tail = i


if __name__ == "__main__":
   main()  
