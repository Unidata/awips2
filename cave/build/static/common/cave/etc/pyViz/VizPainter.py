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

from com.raytheon.uf.viz.core import GraphicsFactory
from com.raytheon.uf.viz.core.drawables import PaintProperties
from com.raytheon.viz.core.gl import GLTargetProxy

#
# Base class for Viz painting from python
#  
#    
#     SOFTWARE HISTORY
#    
#    Date            Ticket#       Engineer       Description
#    ------------    ----------    -----------    --------------------------
#    04/01/09                      njensen        Initial Creation.
#    
# 
#

SUPPORTED_FORMATS = ('.png', '.jpg', '.gif')


class VizPainter():
    
    def __init__(self, renderableDisplay, imageWidth=None, imageHeight=None):
        self.display = renderableDisplay        
        width = float(self.display.getWorldWidth())
        height = float(self.display.getWorldHeight())
        extent = GraphicsFactory.getGraphicsAdapter().constructExtent(0.0, width, 0.0, height)
        self.display.setExtent(extent)        
        
        if imageWidth is None:
            imageWidth = width
        if imageHeight is None:
            imageHeight = height
                
        self.target = GLTargetProxy.constructOffScreenTarget(imageWidth, imageHeight)
        self.target.init()
        self.display.setup(self.target)
    
    def __del__(self):
        resources = self.getDescriptor().getResourceList()
        size = resources.size()
        for i in range(size):
            try:
                rsc = resources.get(i).getResource()
                rsc.dispose()
            except:
                pass
        self.target.dispose()
    
    def getDescriptor(self):
        return self.display.getDescriptor()
    
    def getTarget(self):
        return self.target
    
    def addVizResource(self, vizResource):
        desc = self.getDescriptor()
        vizResource.setDescriptor(desc)
        vizResource.init(self.target)
        desc.getResourceList().add(vizResource)
    
    def paint(self, time, canvas=None, backgroundColor=None):
        if type(time) is str:
            from com.raytheon.uf.common.time import DataTime
            time = DataTime(time)
        self._changeTime(time)
        
        alpha = 1.0
        zoom = 1.0
        view = self.display.getView()
        if canvas is None:
            from org.eclipse.swt.graphics import Rectangle
            canvas = Rectangle(0, 0, int(self.target.getWidth()), int(self.target.getHeight()))
        framesInfo = self.display.getDescriptor().getFramesInfo()
        props = PaintProperties(alpha, zoom, view, canvas, False, framesInfo)
        
        if backgroundColor is not None and type(backgroundColor) is str:
            from com.raytheon.uf.viz.core import RGBColors
            backgroundColor = RGBColors.getRGBColor(backgroundColor)                            
            
        # requires multiple passes to paint everything                                            
        paint = True        
        while paint:            
            self.target.beginFrame(self.display.getView(), True)
            if backgroundColor is not None:
                self.target.setBackgroundColor(backgroundColor)
            self.display.paint(self.target, props)
            self.target.endFrame()
            paint = self.target.isNeedsRefresh()        
    
    def outputImage(self, bufferedImage, filename):
        if not filename.endswith(SUPPORTED_FORMATS):
            filename += SUPPORTED_FORMATS[0]
        imageType = filename[-3:]        
        from java.io import ByteArrayOutputStream, File
        from javax.imageio import ImageIO
        bytes = ByteArrayOutputStream()
        ImageIO.write(bufferedImage, imageType, bytes)
        f = File(filename)
        from com.raytheon.uf.common.util import FileUtil
        FileUtil.bytes2File(bytes.toByteArray(), f)
    
    def _changeTime(self, time):
        index = -1
        times = self.getDescriptor().getFrames()
        for i in range(len(times)):
            if times[i].equals(time):
                index = i
                break        
        if index > -1:            
            self.getDescriptor().setFrame(index)
    
                    