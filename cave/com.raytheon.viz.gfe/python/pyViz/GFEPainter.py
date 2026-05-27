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
# GFE Painter for painting GFE data from scripts
#
#
# SOFTWARE HISTORY
#
# Date          Ticket#  Engineer  Description
# ------------- -------- --------- --------------------------------------------
# Apr 01, 2009           njensen   Initial Creation.
# Aug 20, 2012  1077     randerso  Fixed backgroundColor setting
# Sep 13, 2012  1147     dgilling  Allow map labels to be disabled.
# Nov 06, 2012  15406    ryu       Correction for computing domain from mask
# Sep 12, 2013  2033     dgilling  Change how logo files are accessed.
# Apr 25, 2015  4952     njensen   Updated for new JEP API
# Jan 19, 2017  5987     randerso  Fix after java FontAdapter class was removed
# Mar 15, 2018  6967     randerso  Improved error handling in exceptional cases
#
##

import os

from com.raytheon.uf.viz.core import RGBColors
from com.raytheon.uf.viz.core.map import MapDescriptor
from com.raytheon.uf.viz.core.rsc.capabilities import ColorableCapability, \
    OutlineCapability, LabelableCapability, MagnificationCapability, ColorMapCapability
from com.raytheon.viz.core import ColorUtil
from com.raytheon.viz.gfe.core import DataManagerOffscreenFactory, GFEMapRenderableDisplay
from com.raytheon.viz.gfe.ifpimage import GfeImageUtil, ImageLegendResource
from com.raytheon.viz.gfe.rsc import GFEResource, GFESystemResource
from com.raytheon.viz.gfe.core.parm import ParmDisplayAttributes
EditorType = ParmDisplayAttributes.EditorType
VisMode = ParmDisplayAttributes.VisMode
VisualizationType = ParmDisplayAttributes.VisualizationType
from com.raytheon.uf.common.dataplugin.gfe.reference import ReferenceID

from java.lang import Double
from java.lang import Integer
from javax.imageio import ImageIO
from java.io import File

import LogStream
import VizPainter

class GFEPainter(VizPainter.VizPainter):

    def __init__(self, imageWidth=None, imageHeight=None, expandLeft=25.0, expandRight=25.0, expandTop=25.0, expandBottom=25.0, mask=None, wholeDomain=0, bgColor=None):
        # Create empty display and data manager for display
        display = GFEMapRenderableDisplay()
        self.dataMgr = DataManagerOffscreenFactory.getInstance(display)
        self.refId = None
        envelope = None
        gloc = self.dataMgr.getParmManager().compositeGridLocation()
        if mask is not None:
            from com.raytheon.uf.common.dataplugin.gfe.reference import ReferenceData
            CoordinateType = ReferenceData.CoordinateType
            self.refId = ReferenceID(mask)
            refSet = self.dataMgr.getRefManager().loadRefSet(self.refId)
            if refSet is None:
                LogStream.logProblem("Edit area %s not found" % mask)
            else:
                if wholeDomain == 0:
                    envelope = refSet.overallDomain(CoordinateType.GRID)
        if imageWidth is not None:
            imageWidth = Integer(int(imageWidth))
        if imageHeight is not None:
            imageHeight = Integer(int(imageHeight))
        geom = GfeImageUtil.getLocationGeometry(gloc, envelope, imageWidth, imageHeight, expandLeft / 100.0, expandRight / 100.0, expandTop / 100.0, expandBottom / 100.0)

        # Create descriptor for display
        desc = MapDescriptor(geom)
        display.setDescriptor(desc)
        VizPainter.VizPainter.__init__(self, display, backgroundColor=bgColor)

        gfeSystem = GFESystemResource(self.dataMgr)
        self.addVizResource(gfeSystem)
        desc.getResourceList().getProperties(gfeSystem).setSystemResource(True)
        self.primaryRsc = None


    def __del__(self):
        VizPainter.VizPainter.__del__(self)

    def setupLegend(self, localTime=False, snapshotTime=False, snapshot='', descriptiveName='SHORT', duration='', start='', end='', override=None, lang=''):
        legend = ImageLegendResource(self.dataMgr)
        legend.setLocalTime(localTime)
        legend.setSnapshotTime(snapshotTime)
        legend.setSnapshotFormat(snapshot)
        legend.setDescriptiveName(descriptiveName)
        legend.setDurationFormat(duration)
        legend.setStartFormat(start)
        legend.setEndFormat(end)
        legend.setLanguage(lang)
        if override:
            for parm in override:
                legend.setColorOverride(parm, override[parm])
        self.addVizResource(legend)
        self.getDescriptor().getResourceList().getProperties(legend).setSystemResource(True)

    def enableColorbar(self):
        from com.raytheon.viz.gfe.rsc.colorbar import GFEColorbarResource
        colorBar = GFEColorbarResource(self.dataMgr)
        self.addVizResource(colorBar)
        self.getDescriptor().getResourceList().getProperties(colorBar).setSystemResource(True)

    def __makeGFEResource(self, parm):
        parm.getParmState().setPickUpValue(None)
        gfeRsc = GFEResource(parm, self.dataMgr)
        self.addVizResource(gfeRsc)
        if not parm.getDisplayAttributes().getBaseColor():
            parm.getDisplayAttributes().setBaseColor(ColorUtil.getNewColor(self.getDescriptor()))
        return gfeRsc

    def addGfeResource(self, parm, colormap=None, colorMin=None, colorMax=None, smooth=False, color=None, lineWidth=None):
        gfeRsc = self.__makeGFEResource(parm)
#        jvisType = VisualizationType.valueOf('IMAGE')
#        jset = HashSet()
#        jset.add(jvisType)
#        parm.getDisplayAttributes().setVisualizationType(EDITOR, IMAGE, jset)
#        parm.getDisplayAttributes().setVisMode(IMAGE)
        if self.refId is not None:
            parm.getDisplayAttributes().setDisplayMask(self.refId)
        self.primaryRsc = gfeRsc
        params = gfeRsc.getCapability(ColorMapCapability).getColorMapParameters()
        if colormap is not None:
            from com.raytheon.uf.viz.core.drawables import ColorMapLoader
            try:
                params.setColorMap(ColorMapLoader.loadColorMap(colormap))
            except:
                LogStream.logProblem("Error loading colormap %s:" % colormap, LogStream.exc())
        if colorMax is not None and colorMin is not None:
            params.setDataMin(colorMin)
            params.setColorMapMin(colorMin)
            params.setDataMax(colorMax)
            params.setColorMapMax(colorMax)
        if smooth:
            from com.raytheon.uf.viz.core.rsc.capabilities import ImagingCapability
            gfeRsc.getCapability(ImagingCapability).setInterpolationState(True)
        if color is None:
            color = ColorUtil.getNewColor(self.getDescriptor())
        else:
            color = RGBColors.getRGBColor(color)
        gfeRsc.getCapability(ColorableCapability).setColor(color)
        if lineWidth is not None:
            gfeRsc.getCapability(OutlineCapability).setOutlineWidth(lineWidth)

    def addMapBackground(self, mapName, color=None, lineWidth=None,
                         linePattern=None, xOffset=None, yOffset=None,
                         labelAttribute=None, fontOffset=None):
        from com.raytheon.uf.viz.core.maps import MapManager
        rscPair = MapManager.getInstance(self.getDescriptor()).loadMapByBundleName(mapName)
        if not rscPair:
            # map not found, logged by MapManager
            return

        rsc = rscPair.getResource()

        if color is not None:
            rsc.getCapability(ColorableCapability).setColor(RGBColors.getRGBColor(color))
        if lineWidth is not None:
            rsc.getCapability(OutlineCapability).setOutlineWidth(lineWidth)
        if linePattern is not None:
            rsc.getCapability(OutlineCapability).setLineStyle(linePattern)
        if xOffset is not None:
            rsc.getCapability(LabelableCapability).setxOffset(xOffset)
        if yOffset is not None:
            rsc.getCapability(LabelableCapability).setyOffset(yOffset)
        rsc.getCapability(LabelableCapability).setLabelField(labelAttribute)
        if fontOffset is not None:
            mag = Double(1.26 ** fontOffset)
            rsc.getCapability(MagnificationCapability).setMagnification(mag)

    def getDataManager(self):
        return self.dataMgr

    def getAWTFont(self, font):
        from java.awt import Font
        from com.raytheon.uf.viz.core.drawables import IFont

        style = Font.PLAIN
        if font.getStyle() != None:
            for s in font.getStyle():
                if s == IFont.Style.BOLD:
                    style = Font.BOLD | style
                elif s == IFont.Style.ITALIC:
                    style = Font.ITALIC | style

        awtfont = Font(font.getFontName(), style, int(font.getFontSize()))
        return awtfont

    def outputFiles(self, filename, attachLogo=False, logoText=None):
        rendered = self.getTarget().screenshot()
        if attachLogo:
            from java.awt.image import BufferedImage
            noaa = File(os.path.join(os.path.dirname(os.path.realpath(__file__)), 'logos/noaalogo2.png'))
            nws = File(os.path.join(os.path.dirname(os.path.realpath(__file__)), 'logos/nwslogo.png'))
            noaaImage = ImageIO.read(noaa)
            nwsImage = ImageIO.read(nws)
            height = rendered.getHeight() + noaaImage.getHeight()
            finalBuf = BufferedImage(rendered.getWidth(), height, BufferedImage.TYPE_INT_ARGB)
            graphics = finalBuf.createGraphics()
            graphics.drawImage(rendered, 0, 0, None)
            graphics.drawImage(noaaImage, 0, rendered.getHeight(), None)
            graphics.fillRect(noaaImage.getWidth(), rendered.getHeight(), rendered.getWidth() - noaaImage.getWidth() - nwsImage.getWidth(), rendered.getHeight())
            if logoText is not None:
                from java.awt import Color
                graphics.setColor(Color.BLACK)
                graphics.setFont(self.getAWTFont(self.getTarget().getDefaultFont()))
                fm = graphics.getFontMetrics()
                textBounds = fm.getStringBounds(logoText, graphics)
                graphics.drawString(logoText, int((rendered.getWidth() - textBounds.getWidth()) / 2), \
                                    int(rendered.getHeight() + (noaaImage.getHeight() / 2) + textBounds.getHeight() / 2))
            graphics.drawImage(nwsImage, finalBuf.getWidth() - nwsImage.getWidth(), rendered.getHeight(), None)
            finalBuf.flush()
            self.outputImage(finalBuf, filename)
        else:
            self.outputImage(rendered, filename)
