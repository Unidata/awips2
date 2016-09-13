#!/usr/bin/env python
# Delta script to insert productId tag into warngen .vm and .xml files
# Original files will be backed up in *.backup[n]
# 

import glob
import os
import re
import shutil

SITE_XML_PATH = "/awips2/edex/data/utility/common_static/site/*/warngen/*.xml"
BASE_TEMPLATE_DIR = "/awips2/edex/data/utility/common_static/base/warngen"

PID_PATTERN = re.compile("\n\$\{WMOId\} \$\{vtecOffice\} 000000 \$\{BBBId\}\n([A-Z]{3})\$\{siteId\}\n", re.MULTILINE)

PRODUCT_ID_PATTERN = re.compile("\n[ \t]*<productId>([A-Z]{3})</productId>", re.MULTILINE)

PHENSIG_COMMENT_PATTERN = re.compile("\n([ \t]*)<!-- Phensigs:", re.MULTILINE)
PHENSIG_TAG_PATTERN = re.compile("\n([ \t]*)<phensigs>", re.MULTILINE)


PRODUCT_ID = "{0}<!-- Product ID: nnn id of the product -->\n{0}<productId>{1}</productId>\n\n"

def __backupFile(orig):
    """create a unique backup file name from orig"""

    suffix = ".backup"
    backup = orig + suffix
    count = 0
    while os.path.exists(backup):
        count += 1
        backup = orig + suffix + str(count)
    
    return backup

def main():
    
    for siteXml in glob.glob(SITE_XML_PATH):
        with open(siteXml, "r") as xmlFile:
            siteXmlContents = xmlFile.read()

        if "<warngenConfig>" not in siteXmlContents:
            # not a template configuration file
            continue

        match = PRODUCT_ID_PATTERN.search(siteXmlContents)
        if match is not None:
            # xml already contains productId
            continue
        
        baseXml = os.path.join(BASE_TEMPLATE_DIR, os.path.basename(siteXml))
        siteTemplate = siteXml.replace(".xml", ".vm")

        pid = None
        # try to get productId from base xml file
        if os.path.exists(baseXml):
            with open(baseXml, "r") as xmlFile:
                baseXmlContents = xmlFile.read()
                
            match = PRODUCT_ID_PATTERN.search(baseXmlContents)
            if match is not None:
                pid = match.group(1)
        
        if pid is None:
            # try to get the productId from the site vm template file
            # should only be necessary for a non-baseline template
            if os.path.exists(siteTemplate):
                template = siteTemplate
                
                with open(template, "r") as templateFile:
                    templateContents = templateFile.read()
                    
                match = PID_PATTERN.search(templateContents)
                if match is not None:
                    pid = match.group(1)

        if pid is None:
            print "ERROR: Unable to determine productId for", siteXml
            continue

        # insert product id tag into the xml file before the phensig tag
        match = PHENSIG_COMMENT_PATTERN.search(siteXmlContents)
        if match is None:
            match = PHENSIG_TAG_PATTERN.search(siteXmlContents)
        
        if match is None:
            print "ERROR: Unable to insert productId into", siteXml
            continue
        
        print "Adding productId",pid, "to", siteXml
        indent = match.group(1)
        siteXmlContents = siteXmlContents[:match.start(1)] + PRODUCT_ID.format(indent, pid) + siteXmlContents[match.start(1):]
            
        shutil.copy2(siteXml, __backupFile(siteXml))
        with open(siteXml, "w") as xmlFile:
            xmlFile.write(siteXmlContents)
            
        # if siteTemplate exists attempt to insert productId
        if os.path.exists(siteTemplate):
            print "Referencing productId in", siteTemplate

            with open(siteTemplate, "r") as templateFile:
                templateContents = templateFile.read()

            match = PID_PATTERN.search(templateContents)
            while match is not None:
                templateContents = templateContents[:match.start(1)] + "${productId}" + templateContents[match.end(1):]
                match = PID_PATTERN.search(templateContents)
            
            shutil.copy2(siteTemplate, __backupFile(siteTemplate))
            with open(siteTemplate, "w") as templateFile:
                templateFile.write(templateContents)
            

if __name__ == "__main__":
    main()
