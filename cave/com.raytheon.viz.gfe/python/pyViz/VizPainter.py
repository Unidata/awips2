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
#
# Base class for Viz painting from python
#
#
# SOFTWARE HISTORY
#
# Date          Ticket#  Engineer  Description
# ------------- -------- --------- --------------------------------------------
# Apr 01, 2009           njensen   Initial Creation.
# Aug 20, 2012  1077     randerso  Fixed backgroundColor setting
# Apr 16, 2014  3039     njensen   Ensure correct ResourceList.add() is used
# Jan 19, 2017  5987     randerso  Fix after constructExtent method was removed
# Mar 15, 2018  6967     randerso  Wait for resources to be painted
#
##
import time

from com.raytheon.uf.viz.core.drawables import PaintProperties, PaintStatus
from com.raytheon.viz.core.gl import GLTargetProxy
from com.raytheon.uf.viz.core.rsc import ResourceProperties
from com.raytheon.uf.viz.core import PixelExtent
SUPPORTED_FORMATS = ('.png', '.jpg', '.gif')

PAINT_TIMEOUT = 10  # seconds

class VizPainter():

    def __init__(self, renderableDisplay, imageWidth=None, imageHeight=None, backgroundColor=None):
        self.display = renderableDisplay
        width = float(self.display.getWorldWidth())
        height = float(self.display.getWorldHeight())
        extent = PixelExtent(0.0, width, 0.0, height)
        self.display.setExtent(extent)

        if backgroundColor is not None and type(backgroundColor) is str:
            from com.raytheon.uf.viz.core import RGBColors
            backgroundColor = RGBColors.getRGBColor(backgroundColor)
            self.display.setBackgroundColor(backgroundColor)

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
        desc.getResourceList().add(vizResource, ResourceProperties())

    def paint(self, frameTime, canvas=None):
        if type(frameTime) is str:
            from com.raytheon.uf.common.time import DataTime
            frameTime = DataTime(frameTime)
        self._changeTime(frameTime)

        alpha = 1.0
        zoom = 1.0
        view = self.display.getView()
        if canvas is None:
            from org.eclipse.swt.graphics import Rectangle
            canvas = Rectangle(0, 0, int(self.target.getWidth()), int(self.target.getHeight()))
        framesInfo = self.display.getDescriptor().getFramesInfo()
        props = PaintProperties(alpha, zoom, view, canvas, False, framesInfo)

        # requires multiple passes to paint everything
        timeout = time.time() + PAINT_TIMEOUT
        paint = True
        while paint:
            if time.time() > timeout:
                break

            self.target.beginFrame(self.display.getView(), True)
            self.display.paint(self.target, props)
            self.target.endFrame()
            paint = self.target.isNeedsRefresh()
            if paint:
                continue

            # check if each resource is painted
            for pair in self.display.getDescriptor().getResourceList():
                rsc = pair.getResource()
                if rsc:
                    if not PaintStatus.PAINTED.equals(rsc.getPaintStatus()):
                        paint = True
                        break

    def outputImage(self, bufferedImage, filename):
        if not filename.endswith(SUPPORTED_FORMATS):
            filename += SUPPORTED_FORMATS[0]
        imageType = filename[-3:]
        from java.io import ByteArrayOutputStream, File
        from javax.imageio import ImageIO
        byteStream = ByteArrayOutputStream()
        ImageIO.write(bufferedImage, imageType, byteStream)
        f = File(filename)
        from com.raytheon.uf.common.util import FileUtil
        FileUtil.bytes2File(byteStream.toByteArray(), f)

    def _changeTime(self, time):
        index = -1
        times = self.getDescriptor().getFrames()
        for i in range(len(times)):
            if times[i].equals(time):
                index = i
                break
        if index > -1:
            self.getDescriptor().setFrame(index)


