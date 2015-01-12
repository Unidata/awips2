#!/usr/bin/env python
# DR3708 utility for updating collaboration config to support blacklists
# this script is not intended to be run standalone
# see shell script in parent directory

import sys
import xml.etree.ElementTree as ET

CONFIG_TAG="config"
LIST_TYPE_ATTRIBUTE="listType"
WHITELIST_ATTRIB_VALUE="WHITELIST"
SUB_SITE_TAG="subscribedSites"
LIST_ENTRY_TAG="listEntry"

if len(sys.argv) != 2:
    sys.stderr.write("Usage: %s [xml inputfile file]" % (sys.argv[0]))
    sys.exit(1)

tree = ET.parse(sys.argv[1])
root = tree.getroot()

matches = root.findall(CONFIG_TAG)

if len(matches) < 1:
    sys.stderr.write("No matches found, exiting\n")
    sys.exit(0)

for match in matches:
    if LIST_TYPE_ATTRIBUTE not in match.attrib:
        match.attrib[LIST_TYPE_ATTRIBUTE] = WHITELIST_ATTRIB_VALUE
        subSites = match.findall(SUB_SITE_TAG)
        for subSite in subSites :
            lt = ET.SubElement(match, LIST_ENTRY_TAG)
            lt.text = subSite.text
            match.remove(subSite)

tree.write(sys.stdout)
