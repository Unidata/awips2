#!/usr/bin/env python
# This script will replace attribute values for XML elements that match an xpath

import sys
import xml.etree.ElementTree as ET

if len(sys.argv) != 6:
    print "Usage: %s [xml inputfile file] [output file name] [xpath] [attribute name] [replacement value]" % (sys.argv[0])
    sys.exit(1)

print "Parsing XML file at " + sys.argv[1]
tree = ET.parse(sys.argv[1])
root = tree.getroot()

matches = root.findall(sys.argv[3])

if len(matches) < 1:
    print "No matches found, exiting"
    sys.exit(0)

attribute = sys.argv[4]
replValue = sys.argv[5]

for match in matches:
    if attribute in match.attrib:
        print "Replacing attribute '%s': old value '%s', new value '%s'" % \
            (attribute, match.attrib[attribute], replValue)
        match.attrib[attribute] = replValue

print "Writing results to file at " + sys.argv[2]
tree.write(sys.argv[2])
print "Done"
