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
    
                    