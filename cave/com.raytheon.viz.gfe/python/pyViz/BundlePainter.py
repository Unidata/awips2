##
##

import VizPainter

from com.raytheon.uf.viz.core.procedures import Bundle
from java.io import File

#
# Base class for painting Viz bundles from python
#  
#    
#     SOFTWARE HISTORY
#    
#    Date            Ticket#       Engineer       Description
#    ------------    ----------    -----------    --------------------------
#    04/16/09                      njensen        Initial Creation.
#    
# 
#


class BundlePainter(VizPainter.VizPainter):
    
    def __init__(self, bundlePath, imageWidth=400.0, imageHeight=400.0):
        f = File(bundlePath)
        b = Bundle.unmarshalBundle(f, None)
        displays = b.getDisplays()
        if len(displays) != 1:
            raise RuntimeError, 'BundlePainter only supports bundles with one display'
        VizPainter.VizPainter.__init__(self, displays[0], imageWidth, imageHeight)
        
        # time match to the first resource with times we find
        resourceList = self.getDescriptor().getResourceList()
        size = resourceList.size()
        timeMatchBasis = None        
        for x in range(size):
            rp = resourceList.get(x)
            rsc = rp.getResource()
            if rsc is None:
                rp.instantiateResource(self.getDescriptor())
                rsc = rp.getResource()
                rsc.init(self.getTarget())
            if len(rsc.getDataTimes()) > 0 and timeMatchBasis is None:
                timeMatchBasis = rsc
        
        if timeMatchBasis:    
            self.getDescriptor().getTimeMatcher().changeTimeMatchBasis(timeMatchBasis)
            self.getDescriptor().getTimeMatcher().redoTimeMatching(self.getDescriptor())                        
    
    def __del__(self):
        VizPainter.VizPainter.__del__(self)
    
                    