/**
 * This software was developed and / or modified by Raytheon Company,
 * pursuant to Contract DG133W-05-CQ-1067 with the US Government.
 * 
 * U.S. EXPORT CONTROLLED TECHNICAL DATA
 * This software product contains export-restricted data whose
 * export/transfer/disclosure is restricted by U.S. law. Dissemination
 * to non-U.S. persons whether in the United States or abroad requires
 * an export license or other authorization.
 * 
 * Contractor Name:        Raytheon Company
 * Contractor Address:     6825 Pine Street, Suite 340
 *                         Mail Stop B8
 *                         Omaha, NE 68106
 *                         402.291.0100
 * 
 * See the AWIPS II Master Rights File ("Master Rights File.pdf") for
 * further licensing information.
 **/
package com.raytheon.viz.radar.ui.xy;

import java.awt.Font;
import java.awt.Rectangle;
import java.lang.reflect.Method;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.List;

import org.eclipse.swt.graphics.RGB;

import com.raytheon.uf.common.colormap.Color;
import com.raytheon.uf.common.colormap.IColorMap;
import com.raytheon.uf.common.dataplugin.radar.RadarRecord;
import com.raytheon.uf.common.dataplugin.radar.level3.Layer;
import com.raytheon.uf.common.dataplugin.radar.level3.LinkedVector;
import com.raytheon.uf.common.dataplugin.radar.level3.LinkedVectorPacket;
import com.raytheon.uf.common.dataplugin.radar.level3.SymbologyPacket;
import com.raytheon.uf.common.dataplugin.radar.level3.TextSymbolPacket;
import com.raytheon.uf.common.dataplugin.radar.level3.UnlinkedVector;
import com.raytheon.uf.common.dataplugin.radar.level3.UnlinkedVectorPacket;
import com.raytheon.uf.common.dataplugin.radar.level3.WindBarbPacket;
import com.raytheon.uf.common.dataplugin.radar.level3.WindBarbPacket.WindBarbPoint;
import com.raytheon.uf.common.time.DataTime;
import com.raytheon.uf.viz.core.DrawableString;
import com.raytheon.uf.viz.core.IGraphicsTarget;
import com.raytheon.uf.viz.core.IGraphicsTarget.HorizontalAlignment;
import com.raytheon.uf.viz.core.IGraphicsTarget.VerticalAlignment;
import com.raytheon.uf.viz.core.PixelCoverage;
import com.raytheon.uf.viz.core.drawables.ColorMapParameters;
import com.raytheon.uf.viz.core.drawables.IFont;
import com.raytheon.uf.viz.core.drawables.IFont.Style;
import com.raytheon.uf.viz.core.drawables.IImage;
import com.raytheon.uf.viz.core.drawables.PaintProperties;
import com.raytheon.uf.viz.core.drawables.ext.colormap.IColormappedImageExtension;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.rsc.LoadProperties;
import com.raytheon.uf.viz.core.rsc.capabilities.ColorMapCapability;
import com.raytheon.uf.viz.core.rsc.capabilities.ImagingCapability;
import com.raytheon.uf.viz.core.rsc.capabilities.MagnificationCapability;
import com.raytheon.viz.awipstools.capabilities.RangeRingsOverlayCapability;
import com.raytheon.viz.core.contours.util.VectorGraphicsRenderable;
import com.raytheon.viz.radar.RadarHelper;
import com.raytheon.viz.radar.VizRadarRecord;
import com.raytheon.viz.radar.interrogators.IRadarInterrogator;
import com.raytheon.viz.radar.rsc.RadarImageResource;
import com.raytheon.viz.radar.rsc.RadarResourceData;
import com.vividsolutions.jts.geom.Coordinate;

/**
 * Handles radar data that are not on a map or graph
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Mar 16, 2009            askripsk     Initial creation
 * Jul 26, 2010 #3723      bkowal       Now implements the magnification
 *                                      capability.
 * 
 * </pre>
 * 
 * @author askripsk
 * @version 1.0
 */

public class RadarXYResource extends RadarImageResource<RadarXYDescriptor> {

    protected HashMap<Coordinate, String> screenStringMap = new HashMap<Coordinate, String>();

    protected List<UnlinkedVector> unlinkedLines = new ArrayList<UnlinkedVector>();

    protected List<LinkedVector> linkedLines = new ArrayList<LinkedVector>();

    protected List<WindBarbPoint> points = new ArrayList<WindBarbPoint>();

    protected int xOffsetNWP = 0;

    protected int yOffsetNWP = 80;

    protected double scalar = 1.6;

    protected IFont font;

    protected boolean initPlotObjects = true;

    public RadarXYResource(RadarResourceData resourceData,
            LoadProperties loadProperties, IRadarInterrogator interrogator)
            throws VizException {
        super(resourceData, loadProperties, interrogator);
        getCapability(MagnificationCapability.class);

        getCapabilities().removeCapability(ImagingCapability.class);
        getCapabilities().removeCapability(RangeRingsOverlayCapability.class);

        this.dataTimes = new ArrayList<DataTime>();
    }

    @Override
    protected void disposeInternal() {
        super.disposeInternal();
        if (font != null) {
            font.dispose();
            font = null;
        }
    }

    @Override
    protected void paintInternal(IGraphicsTarget target,
            PaintProperties paintProps) throws VizException {

        float magnification = getCapability(MagnificationCapability.class)
                .getMagnification().floatValue();

        if (this.font == null) {
            // Anything larger than 10 leads to text overlap, to go larger we
            // will need to place the text smarter
            this.font = target.initializeFont(Font.MONOSPACED, 10,
                    new Style[] { Style.BOLD });
        }
        if (paintProps.getDataTime() == null) {
            return;
        }
        this.font.setMagnification(magnification);

        RadarRecord radarRecord = getRadarRecord(paintProps.getDataTime());
        if (radarRecord == null) {
            issueRefresh();
            return;
        }

        if (initPlotObjects || !paintProps.getDataTime().equals(displayedDate)) {
            displayedDate = paintProps.getDataTime();
            populatePlotObjects(radarRecord, target);
            getColorMapParameters(target, radarRecord);
            // Even though populatePlotObjects sets this to false,
            // getColorMapParameters updates the color amp and sets this to
            // false so set it back to true here
            initPlotObjects = false;
        }

        if (radarRecord.getRawData() != null) {
            paintRadar(target, paintProps);
        }

        paintLines(target);
        paintText(target);
        paintPoints(target, paintProps);
        // target.drawRect(new PixelExtent(left, right, upper, lower),
        // getCapability(ColorableCapability.class).getColor(), 1.0f, 1.0);

    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.viz.radar.rsc.RadarImageResource#buildCoverage(com.raytheon
     * .uf.viz.core.IGraphicsTarget,
     * com.raytheon.viz.radar.RadarTimeRecord.RadarTiltRecord)
     */
    @Override
    public PixelCoverage buildCoverage(IGraphicsTarget target,
            VizRadarRecord radarRecord) throws VizException {
        double width = radarRecord.getNumBins();
        double height = radarRecord.getNumRadials();
        double xScale = radarRecord.getXscale();
        double yScale = radarRecord.getYscale();
        int iStart = radarRecord.getIstart();
        int jStart = radarRecord.getJstart();

        width *= xScale;
        height *= yScale;

        width *= scalar;
        height *= scalar;

        double upper = (yOffsetNWP + jStart) * scalar;
        double lower = upper + height;
        double left = (xOffsetNWP + iStart) * scalar;
        double right = left + width;

        return new PixelCoverage(new Coordinate(left, upper), new Coordinate(
                left, lower), new Coordinate(right, lower), new Coordinate(
                right, upper));
    }

    private void paintLines(IGraphicsTarget target) throws VizException {

        float magnification = getCapability(MagnificationCapability.class)
                .getMagnification().floatValue();

        // Paint unlinked lines
        for (UnlinkedVector currVec : this.unlinkedLines) {
            target.drawLine((currVec.i1 + xOffsetNWP) * scalar,
                    (currVec.j1 + yOffsetNWP) * scalar, 0,
                    (currVec.i2 + xOffsetNWP) * scalar,
                    (currVec.j2 + yOffsetNWP) * scalar, 0,
                    getVectorColor(currVec), 1 * magnification);
        }

        // Paint linked lines
        for (LinkedVector currVec : this.linkedLines) {
            target.drawLine((currVec.i1 + xOffsetNWP) * scalar,
                    (currVec.j1 + yOffsetNWP) * scalar, 0,
                    (currVec.i2 + xOffsetNWP) * scalar,
                    (currVec.j2 + yOffsetNWP) * scalar, 0,
                    getVectorColor(currVec), 1 * magnification);
        }
    }

    private void paintPoints(IGraphicsTarget target, PaintProperties paintProps)
            throws VizException {
        float magnification = getCapability(MagnificationCapability.class)
                .getMagnification().floatValue();

        double ratio = (paintProps.getView().getExtent().getWidth() / paintProps
                .getCanvasBounds().width) * magnification;

        VectorGraphicsRenderable[] renderables = new VectorGraphicsRenderable[6];
        for (int i = 0; i < renderables.length; i++) {
            renderables[i] = new VectorGraphicsRenderable(descriptor, target,
                    64, 1.0);
            renderables[i].setLineWidth(magnification);
        }
        renderables[0].setColor(RadarHelper.WHITE);
        renderables[1].setColor(RadarHelper.GREEN);
        renderables[2].setColor(RadarHelper.YELLOW);
        renderables[3].setColor(RadarHelper.BRIGHT_RED);
        renderables[4].setColor(RadarHelper.LIGHT_BLUE);
        renderables[5].setColor(RadarHelper.MED_PURPLE);
        for (WindBarbPoint point : points) {
            int index = point.getColorValue();
            if (index >= renderables.length || index < 0) {
                index = 0;
            }
            Coordinate plotLoc = new Coordinate(point.i, point.j);
            plotLoc.x += xOffsetNWP;
            plotLoc.y += yOffsetNWP;
            plotLoc.y -= 2;
            plotLoc.x *= scalar;
            plotLoc.y *= scalar;
            renderables[index].paintBarb(plotLoc, 64 * ratio,
                    point.getWindBarbSpd(),
                    Math.toRadians(point.getWindBarbDir()));
        }
        for (VectorGraphicsRenderable renderable : renderables) {
            renderable.paint(target);
            renderable.dispose();
        }

    }

    private void paintText(IGraphicsTarget target) throws VizException {
        // Paint Text
        List<DrawableString> strings = new ArrayList<DrawableString>();
        for (Coordinate c : this.screenStringMap.keySet()) {
            DrawableString string = new DrawableString(
                    this.screenStringMap.get(c), null);
            string.font = font;
            string.setCoordinates((c.x + xOffsetNWP) * scalar,
                    (c.y + yOffsetNWP) * scalar, 0.0);
            string.horizontalAlignment = HorizontalAlignment.LEFT;
            string.verticallAlignment = VerticalAlignment.TOP;
            strings.add(string);
        }
        target.drawStrings(strings);
    }

    /**
     * Gets the color level from the vector and retrieves the cooresponding
     * color from the color bar. Returns white if the color level is invalid.
     * 
     * @param <T>
     * @param currVec
     *            Vector to get the color level from
     * @return The color from the color bar or white if the color level is
     *         invalid
     */
    private <T> RGB getVectorColor(T currVec) {
        RGB lineRGB = null;
        Color lineColor;
        int colorNumber = -1;

        // Get the color level from the vector via the "getTheColor()" method
        try {
            Method getTheColor = currVec.getClass().getMethod("getTheColor");

            colorNumber = (Integer) getTheColor.invoke(currVec);
        } catch (Exception e) {
            // TODO Auto-generated catch block
            e.printStackTrace();
        }

        IColorMap colorMap = this.getCapability(ColorMapCapability.class)
                .getColorMapParameters().getColorMap();
        if (colorNumber != -1 && colorNumber < colorMap.getColors().size()) {
            // Get the color from the color bar's colors
            lineColor = colorMap.getColors().get(colorNumber * 16);
            lineRGB = new RGB((int) (lineColor.getRed() * 255f),
                    (int) (lineColor.getGreen() * 255f),
                    (int) (lineColor.getBlue() * 255f));
        } else {
            // Default to white
            lineRGB = new RGB(0xFF, 0xFF, 0xFF);
        }

        return lineRGB;
    }

    protected void populatePlotObjects(RadarRecord radarRecord,
            IGraphicsTarget target) throws VizException {
        this.screenStringMap.clear();
        linkedLines.clear();
        unlinkedLines.clear();
        points.clear();
        for (Layer currLayer : radarRecord.getSymbologyBlock().getLayers()) {
            for (SymbologyPacket currPacket : currLayer.getPackets()) {
                if (currPacket instanceof TextSymbolPacket) {
                    TextSymbolPacket tsp = (TextSymbolPacket) currPacket;
                    this.screenStringMap.put(
                            new Coordinate(tsp.getI(), tsp.getJ()),
                            tsp.getTheText());
                } else if (currPacket instanceof WindBarbPacket) {
                    WindBarbPacket pk = (WindBarbPacket) currPacket;
                    points.addAll(Arrays.asList(pk.getPoints()));
                } else if (currPacket instanceof UnlinkedVectorPacket) {
                    UnlinkedVectorPacket pk = (UnlinkedVectorPacket) currPacket;

                    this.unlinkedLines.addAll(pk.getVectors());
                } else if (currPacket instanceof LinkedVectorPacket) {
                    LinkedVectorPacket pk = (LinkedVectorPacket) currPacket;

                    this.linkedLines.addAll(pk.getVectors());
                } else {
                    System.out.println("Need: " + currPacket.getClass());
                }
            }
        }
        initPlotObjects = false;
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.viz.core.rsc.IResourceDataChanged#resourceChanged(com
     * .raytheon.uf.viz.core.rsc.IResourceDataChanged.ChangeType,
     * java.lang.Object)
     */
    @Override
    public void resourceChanged(ChangeType type, Object object) {
        super.resourceChanged(type, object);
        initPlotObjects = true;
        super.issueRefresh();
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.viz.radar.rsc.RadarImageResource#toImageData(com.raytheon
     * .uf.viz.core.IGraphicsTarget,
     * com.raytheon.uf.viz.core.drawables.ColorMapParameters,
     * com.raytheon.uf.common.dataplugin.radar.RadarRecord, java.awt.Rectangle)
     */
    @Override
    protected IImage createImage(IGraphicsTarget target,
            ColorMapParameters params, RadarRecord record, Rectangle rect)
            throws VizException {
        byte[] table = createConversionTable(params, record);
        return target.getExtension(IColormappedImageExtension.class)
                .initializeRaster(
                        new RadarXYDataRetrievalAdapter(record, table, rect),
                        params);
    }

    protected static class RadarXYDataRetrievalAdapter extends
            RadarImageDataRetrievalAdapter {

        public RadarXYDataRetrievalAdapter(RadarRecord record, byte[] table,
                Rectangle rect) {
            super(record, table, rect);
        }

        @Override
        public byte[] convertData() {
            int width = record.getNumRadials();
            int height = record.getNumBins();
            byte[] imageData = new byte[width * height];
            byte[] rawData = record.getRawData();
            int count = 0;
            for (int i = rawData.length - 1; i >= 0; --i) {
                imageData[count] = table[rawData[i] & 0xFF];
                count++;
            }
            return imageData;
        }

    }

}
