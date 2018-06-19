#!/awips2/python/bin/python2

##
# DR 6252 - This script will update any found site-level overrides to 
# cave_static/base/textws/gui/QualityControlCfg.xml with new configuration
# entries requires to support the DSW and SQW WarnGen products.
##

import logging
import glob
import re
import sys
import xml.dom.minidom as minidom
import xml.etree.ElementTree as ET


logging.basicConfig(format='%(asctime)-15s %(levelname)s:  %(message)s', 
                    datefmt='%Y-%m-%d %H:%M:%S',
                    level=logging.INFO)
log = logging.getLogger("updateQualityControlCfg.py")

QC_CONFIG_PATH = "/awips2/edex/data/utility/cave_static/site/*/textws/gui/QualityControlCfg.xml"



def main():
    log.info("Starting delta script for DR #6252: updating QualityControlCfg.xml...")
    
    for file in glob.iglob(QC_CONFIG_PATH):
        log.info("Updating file [%s]...", file)
        
        tree = ET.parse(file)
        root = tree.getroot()
        
        product_map = root.find("productTypeMap")
        if product_map is not None:
            new_item = ET.SubElement(product_map, "item")
            ET.SubElement(new_item, "key").text = "DSW"
            ET.SubElement(new_item, "value").text = "Dust Storm Warning"
            
            new_item = ET.SubElement(product_map, "item")
            ET.SubElement(new_item, "key").text = "SQW"
            ET.SubElement(new_item, "value").text = "Snow Squall Warning"
            
            new_item = ET.SubElement(product_map, "item")
            ET.SubElement(new_item, "key").text = "DS.W"
            ET.SubElement(new_item, "value").text = "Dust Storm Warning"
            
            new_item = ET.SubElement(product_map, "item")
            ET.SubElement(new_item, "key").text = "DS.Y"
            ET.SubElement(new_item, "value").text = "Dust Advisory"
            
            new_item = ET.SubElement(product_map, "item")
            ET.SubElement(new_item, "key").text = "SQ.W"
            ET.SubElement(new_item, "value").text = "Snow Squall Warning"
        else:
            log.error("Could not find productTypeMap in file [%s].", file)
            log.error("Exiting.")
            sys.exit(-1)
             
        nnn_ident = root.find("nnnOfIdent")
        if nnn_ident is not None:
            new_item = ET.SubElement(nnn_ident, "item")
            ET.SubElement(new_item, "key").text = "DS.W"
            ET.SubElement(new_item, "value").text = "DSW"
            
            new_item = ET.SubElement(nnn_ident, "item")
            ET.SubElement(new_item, "key").text = "DS.Y"
            ET.SubElement(new_item, "value").text = "DSW"
            
            new_item = ET.SubElement(nnn_ident, "item")
            ET.SubElement(new_item, "key").text = "SQ.W"
            ET.SubElement(new_item, "value").text = "SQW"
        else:
            log.error("Could not find nnnOfIdent in file [%s].", file)
            log.error("Exiting.")
            sys.exit(-1)
            
        followup_nnn = root.find("followupNNN")
        if followup_nnn is not None:
            new_item = ET.SubElement(followup_nnn, "item")
            ET.SubElement(new_item, "key").text = "DS.W"
            ET.SubElement(new_item, "value").text = "DSW"
            
            new_item = ET.SubElement(followup_nnn, "item")
            ET.SubElement(new_item, "key").text = "DS.Y"
            ET.SubElement(new_item, "value").text = "DSW"
            
            new_item = ET.SubElement(followup_nnn, "item")
            ET.SubElement(new_item, "key").text = "SQ.W"
            ET.SubElement(new_item, "value").text = "SQW"
        else:
            log.error("Could not find followupNNN in file [%s].", file)
            log.error("Exiting.")
            sys.exit(-1)
        
        bullet_map = root.find("bulletTypeMap")
        if bullet_map is not None:
            new_item = ET.SubElement(bullet_map, "item")
            ET.SubElement(new_item, "key").text = "DS.Y"
            ET.SubElement(new_item, "value").text = "Dust Advisory"
        else:
            log.error("Could not find bulletTypeMap in file [%s].", file)
            log.error("Exiting.")
            sys.exit(-1)

        # Cleanup junk new-lines and extraneous spaces from output
        pretty_xml = minidom.parseString(ET.tostring(root, 'utf-8')).toprettyxml(indent=' '*4, encoding='UTF-8')
        pretty_xml = '\n'.join([line for line in pretty_xml.split('\n') if line.strip()])
        text_re = re.compile('>\n\s+([^<>\s].*?)\n\s+</', re.DOTALL)
        pretty_xml = text_re.sub('>\g<1></', pretty_xml)
        
        # because of how the SegmentedNNN tags are defined, if we used ET to
        # insert these new entries they would just be placed at the end of the
        # file and not with the block of existing entries
        try:
            index = pretty_xml.rindex('</SegmentedNNN>')
            index += len('</SegmentedNNN>') + 1
            pretty_xml = pretty_xml[:index] + '    <SegmentedNNN>DSW</SegmentedNNN>\n' +\
                        '    <SegmentedNNN>SQW</SegmentedNNN>\n' + pretty_xml[index:]
        except ValueError:
            log.error("Could not find SegmentedNNN in file [%s].", file)
            log.error("Exiting.")
            sys.exit(-1)
        
        with open (file, 'w') as out_file:
            out_file.write(pretty_xml)

    log.info("Delta script complete.")



if __name__ == '__main__':
    main()
