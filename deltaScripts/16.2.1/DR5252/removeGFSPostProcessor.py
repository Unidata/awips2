#!/awips2/python/bin/python
# AWIPS2 #5252
# Looks for configurations with GFSPostProcessor, and if found, removes that processor.
# If GFSPostProcessor is the only processor defined, the whole postProcessedModel is removed,
# with a comment stating so.


import xml.etree.ElementTree as ET
import sys

# http://effbot.org/zone/element-pi.htm
# Custom XMLTreeBuilder to preserve comments when parsing
class CommentParser(ET.XMLTreeBuilder):
    def __init__(self):
        ET.XMLTreeBuilder.__init__(self)
        # assumes ElementTree 1.2.X
        self._parser.CommentHandler = self.handle_comment
        self._parser.ProcessingInstructionHandler = self.handle_pi

    def close(self):
        return ET.XMLTreeBuilder.close(self)

    def handle_comment(self, data):
        self._target.start(ET.Comment, {})
        self._target.data(data)
        self._target.end(ET.Comment)

    def handle_pi(self, target, data):
        self._target.start(ET.PI, {})
        self._target.data(target + " " + data)
        self._target.end(ET.PI)

    def parse(source):
        return ET.parse(source, CommentParser())

if len(sys.argv) != 3: # this script is sys.argv[0]
    print "Error. Expected two file names as arguments. Received " + str(sys.argv[1:])
    sys.exit(1)

tree = ET.parse(sys.argv[1], CommentParser())

if not tree:
    print "Could not parse " + sys.argv[1]
    sys.exit(1)

root = tree.getroot()
if root.tag != "postProcessedModels":
    print "Root element is not postProcessedModels. Found " + root.tag
    sys.exit(1)

i=0
for ppm in root: 
    # only care about postProcessedModel tags, but want the index to insert a
    # comment when a whole block postProcessedModel block is removed.
    if ppm.tag == "postProcessedModel":
        prev = ppm
        for pn in ppm:
            #only care about processorName tags
            if pn.tag == "processorName" and pn.text.find("GFSPostProcessor") >= 0:
                ppm.remove(pn)
                prev.tail = pn.tail
            else:
                prev = pn
        if len(ppm.findall("processorName")) == 0:
            # if this postProcessedModel doesn't have any configured processors, remove it.
            root.remove(ppm)
            modelName = ppm.findtext("modelName", default=" A postProcessedModel block").replace("--","- -").strip()
            comment = ET.Comment(" " + modelName + " was removed because its only processor, GFSPostProcessor, is OBE. ")
            comment.tail = ppm.tail
            root.insert(i, comment)
    i+=1
                
tree.write(sys.argv[2])



