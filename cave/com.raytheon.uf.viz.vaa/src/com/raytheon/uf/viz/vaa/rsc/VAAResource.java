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
package com.raytheon.uf.viz.vaa.rsc;

import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.Map;

import org.eclipse.swt.graphics.RGB;
import org.eclipse.swt.graphics.Rectangle;
import org.opengis.referencing.crs.CoordinateReferenceSystem;

import com.raytheon.uf.common.dataplugin.PluginDataObject;
import com.raytheon.uf.common.dataplugin.vaa.VAARecord;
import com.raytheon.uf.common.dataplugin.vaa.VAASubPart;
import com.raytheon.uf.common.geospatial.ReferencedCoordinate;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.common.time.DataTime;
import com.raytheon.uf.viz.core.DrawableString;
import com.raytheon.uf.viz.core.IExtent;
import com.raytheon.uf.viz.core.IGraphicsTarget;
import com.raytheon.uf.viz.core.IGraphicsTarget.VerticalAlignment;
import com.raytheon.uf.viz.core.PixelCoverage;
import com.raytheon.uf.viz.core.PixelExtent;
import com.raytheon.uf.viz.core.drawables.IFont;
import com.raytheon.uf.viz.core.drawables.IFont.Style;
import com.raytheon.uf.viz.core.drawables.IImage;
import com.raytheon.uf.viz.core.drawables.IWireframeShape;
import com.raytheon.uf.viz.core.drawables.PaintProperties;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.map.MapDescriptor;
import com.raytheon.uf.viz.core.point.display.SymbolLoader;
import com.raytheon.uf.viz.core.rsc.AbstractVizResource;
import com.raytheon.uf.viz.core.rsc.IResourceDataChanged.ChangeType;
import com.raytheon.uf.viz.core.rsc.LoadProperties;
import com.raytheon.uf.viz.core.rsc.capabilities.ColorableCapability;
import com.raytheon.uf.viz.core.rsc.capabilities.MagnificationCapability;
import com.raytheon.uf.viz.vaa.util.CommonUtil;
import org.locationtech.jts.geom.Coordinate;
import org.locationtech.jts.geom.Envelope;
import org.locationtech.jts.geom.Point;
import org.locationtech.jts.geom.Polygon;

/**
 * Resource for Volcanic Ash Advisories data
 *
 * <pre>
 *
 * SOFTWARE HISTORY
 * 
 * Date          Ticket#  Engineer  Description
 * ------------- -------- --------- --------------------------------------------
 * Nov 23, 2009  3268     jsanchez  Initial creation
 * Jun 06, 2014  2061     bsteffen  Remove unneccessary imports
 * Jul 29, 2014  3465     mapeters  Updated deprecated drawString() calls.
 * Nov 05, 2015  5070     randerso  Adjust font sizes for dpi scaling
 * Sep 23, 2016  5887     mapeters  Added shapeMap to reuse wireframe shapes
 *                                  across paint() calls
 * Sep 27, 2016  5887     tgurney   Don't draw volcanoes outside clipping pane
 * Nov 28, 2017  5863     bsteffen  Change dataTimes to a NavigableSet
 * 
 * </pre>
 *
 * @author jsanchez
 */
public class VAAResource
        extends AbstractVizResource<VAAResourceData, MapDescriptor> {

    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(VAAResource.class);

    private static final String ANAL00 = "ANAL00";

    private static final String AREA = "AREA";

    private static final char VOLCANO_SYMBOL = '\u00b1';

    private static final double inset = 150.0;

    private final Map<VAARecord, IWireframeShape> shapeMap = new HashMap<>();

    private final Map<DataTime, Collection<VAARecord>> recordsToParse = new HashMap<>();

    protected DataTime displayedDataTime;

    private SymbolLoader symbolLoader;

    private double screenToWorldRatio;

    private double plotWidth;

    private double actualPlotWidth;

    private IFont font;

    protected VAAResource(VAAResourceData resourceData,
            LoadProperties loadProperties) {
        super(resourceData, loadProperties, false);
    }

    @Override
    public String getName() {
        return "Volcanic Ash Advisory";
    }

    @Override
    public String inspect(ReferencedCoordinate coord) throws VizException {
        StringBuilder returnValue = new StringBuilder();

        if (this.displayedDataTime != null) {
            Collection<VAARecord> records = null;
            if (!this.recordsToParse.containsKey(this.displayedDataTime)) {
                return "NO DATA";
            } else {
                records = this.recordsToParse.get(this.displayedDataTime);
            }

            Coordinate latlon = null;
            try {
                latlon = coord.asLatLon();
            } catch (Exception e) {
                statusHandler.handle(Priority.PROBLEM,
                        "Error transforming coordinate", e);
            }
            double[] ml = { latlon.x, latlon.y };
            double[] pml = descriptor.worldToPixel(ml);
            double scaleValue = this.plotWidth / 2.0 / this.screenToWorldRatio;
            PixelExtent interrogationExtent = new PixelExtent(
                    pml[0] - scaleValue, pml[0] + scaleValue,
                    pml[1] - scaleValue, pml[1] + scaleValue);
            Envelope llExtent = descriptor.pixelToWorld(interrogationExtent);

            for (VAARecord record : records) {
                for (VAASubPart subPart : record.getSubParts()) {
                    if (subPart.getSubText() != null
                            && subPart.getSubText().equals(ANAL00)
                            && subPart.getShapeType().equals(AREA)) {
                        Polygon llPolygon = CommonUtil.getPolygon(record,
                                subPart.getSubText());
                        Polygon pixelPoly = CommonUtil.worldToPixel(llPolygon,
                                descriptor);
                        Point point;

                        try {
                            point = pixelPoly.getFactory().createPoint(coord
                                    .asPixel(descriptor.getGridGeometry()));
                        } catch (Exception e) {
                            throw new VizException(
                                    "Error inspecting Volcanic Ash Advisories",
                                    e);
                        }

                        if (pixelPoly.contains(point)) {
                            returnValue.setLength(0);
                            if (record.getAnal00Hr() != null) {
                                returnValue.append(record.getAnal00Hr());
                            }
                        }
                    }
                }

                if (!llExtent.contains(record.getLongitude(),
                        record.getLatitude())) {
                    continue;
                } else {
                    if (returnValue.length() > 0) {
                        returnValue.append("\n \n");
                    }
                    returnValue.append(record.getMessage());
                }
            }
        }

        return returnValue.length() > 0 ? returnValue.toString() : "NO DATA";
    }

    /**
     * Adds a new record to this resource
     *
     * @param record
     */
    protected void addRecord(VAARecord record) {
        DataTime dataTime = record.getDataTime();
        Collection<VAARecord> toParse = recordsToParse.get(dataTime);
        if (toParse == null) {
            dataTimes.add(dataTime);
            toParse = new ArrayList<>();
            recordsToParse.put(dataTime, toParse);
        }
        toParse.add(record);
    }

    @Override
    public void remove(DataTime dataTime) {
        super.remove(dataTime);

        Collection<VAARecord> toParse = recordsToParse.remove(dataTime);
        if (toParse != null) {
            for (VAARecord record : toParse) {
                synchronized (shapeMap) {
                    IWireframeShape shape = shapeMap.remove(record);
                    if (shape != null) {
                        shape.dispose();
                    }
                }
            }
        }
    }

    @Override
    protected void paintInternal(IGraphicsTarget target,
            PaintProperties paintProps) throws VizException {
        DataTime curDataTime = paintProps.getDataTime();
        if (curDataTime == null) {
            this.displayedDataTime = null;
            return;
        }

        this.screenToWorldRatio = paintProps.getCanvasBounds().width
                / paintProps.getView().getExtent().getWidth();

        Collection<VAARecord> toParse = recordsToParse.get(curDataTime);

        if (toParse != null && !toParse.isEmpty()) {
            for (VAARecord record : toParse) {
                paint(target, paintProps, record);
            }
        }

        this.displayedDataTime = curDataTime;
    }

    private void paint(IGraphicsTarget target, PaintProperties paintProps,
            VAARecord record) throws VizException {
        String volcanoName = getVolcanoName(record.getStationId());
        RGB color = getCapability(ColorableCapability.class).getColor();
        IExtent extent = paintProps.getView().getExtent();
        double scale[] = getScale(paintProps);
        double[] loc = descriptor.worldToPixel(
                new double[] { record.getLongitude(), record.getLatitude() });

        double maxX = extent.getMaxX();
        double minX = extent.getMinX();
        double maxY = extent.getMaxY();
        double minY = extent.getMinY();

        maxX = maxX < 0 ? 0 : maxX;
        maxX = maxX > 19_999 ? 19_999 : maxX;
        minX = minX < 0 ? 0 : minX;
        minX = minX > 19_999 ? 19_999 : minX;
        maxY = maxY < 0 ? 0 : maxY;
        maxY = maxY > 19_999 ? 19_999 : maxY;
        minY = minY < 0 ? 0 : minY;
        minY = minY > 19_999 ? 19_999 : minY;

        PixelExtent correctedExtent = new PixelExtent(minX, maxX, minY, maxY);
        if (correctedExtent.contains(loc)
                && paintProps.getClippingPane().contains(loc)) {
            IImage image = symbolLoader.getImage(target, color, VOLCANO_SYMBOL);
            if (image != null) {
                Coordinate ul = new Coordinate(loc[0], loc[1] - 6 * scale[1]);
                Coordinate ur = new Coordinate(loc[0] + 12 * scale[0], ul.y);
                Coordinate lr = new Coordinate(ur.x, loc[1] + 6 * scale[1]);
                Coordinate ll = new Coordinate(loc[0], lr.y);
                PixelCoverage imageExtent = new PixelCoverage(ul, ur, lr, ll);
                loc[0] += 12 * scale[0];

                target.drawRaster(image, imageExtent, paintProps);
                DrawableString string = new DrawableString(volcanoName, color);
                string.font = font;
                string.setCoordinates(loc[0] + 5 * scale[0], loc[1]);
                string.verticallAlignment = VerticalAlignment.MIDDLE;
                target.drawStrings(string);
            }

            IWireframeShape shape;
            synchronized (shapeMap) {
                shape = shapeMap.get(record);
                if (shape == null) {
                    for (VAASubPart subPart : record.getSubParts()) {
                        String subText = subPart.getSubText();
                        if (subText != null && subText.equals(ANAL00)) {
                            if (shape == null) {
                                // Only create it if we find an ANAL00 subPart
                                shape = target.createWireframeShape(false,
                                        descriptor);
                                shapeMap.put(record, shape);
                            }
                            Coordinate[] coordinates = CommonUtil
                                    .getCoordinates(subPart.getLocations());
                            shape.addLineSegment(coordinates);
                        }
                    }
                }
            }

            if (shape != null) {
                target.drawWireframeShape(shape, color, 0.5f);
            }
        } else {
            target.clearClippingPlane();
            double x = paintProps.getView().getExtent().getMinX();
            double y = correctedExtent.getMaxY()
                    - inset * paintProps.getZoomLevel();
            String message = "Volcano " + volcanoName + " is off-screen";
            DrawableString string = new DrawableString(message, color);
            string.font = font;
            string.setCoordinates(x, y);
            string.verticallAlignment = VerticalAlignment.MIDDLE;
            target.drawStrings(string);
            target.setupClippingPlane(paintProps.getClippingPane());
        }
    }

    @Override
    public void resourceDataChanged(ChangeType type, Object object) {
        if (type == ChangeType.DATA_UPDATE) {
            PluginDataObject[] pdo = (PluginDataObject[]) object;
            for (PluginDataObject p : pdo) {
                if (p instanceof VAARecord) {
                    addRecord((VAARecord) p);
                }
            }
        } else if (type == ChangeType.CAPABILITY) {
            if (object instanceof MagnificationCapability) {
                int mag = ((MagnificationCapability) object).getMagnification()
                        .intValue();
                double newPlotWidth = actualPlotWidth * mag;
                if (plotWidth != newPlotWidth) {
                    plotWidth = newPlotWidth;
                }
            }
        }
        issueRefresh();
    }

    @Override
    protected void disposeInternal() {
        if (font != null) {
            font.dispose();
            font = null;
        }

        disposeShapes();
    }

    private void disposeShapes() {
        synchronized (shapeMap) {
            for (IWireframeShape shape : shapeMap.values()) {
                shape.dispose();
            }
            shapeMap.clear();
        }
    }

    @Override
    protected void initInternal(IGraphicsTarget target) throws VizException {
        if (font != null) {
            font.dispose();
        }
        this.actualPlotWidth = 25;
        int mag = getCapability(MagnificationCapability.class)
                .getMagnification().intValue();
        this.plotWidth = actualPlotWidth * mag;
        this.font = target.initializeFont("Monospace", 9,
                new Style[] { Style.ITALIC });
        this.symbolLoader = new SymbolLoader();
    }

    /**
     * Determine the appropriate scale to use on both axis for constant sized
     * objects
     *
     * @param paintProps
     * @return
     */
    private double[] getScale(PaintProperties paintProps) {
        IExtent extent = paintProps.getView().getExtent();
        Rectangle canvasBounds = paintProps.getCanvasBounds();
        double[] scale = new double[2];
        scale[0] = extent.getWidth() / canvasBounds.width;
        scale[1] = extent.getHeight() / canvasBounds.height;
        return scale;
    }

    /**
     * Parses the volcano name from the stationId
     *
     * @param stationId
     * @return volcano name
     */
    private String getVolcanoName(String stationId) {
        StringBuilder returnValue = new StringBuilder();
        String parts[] = stationId.split(" ");
        for (String str : parts) {
            if (str.length() > 0 && str.charAt(0) >= 'A'
                    && str.charAt(0) <= 'Z') {
                if (str.length() > 0) {
                    str += " ";
                }
                returnValue.append(str);
            }
        }
        return returnValue.toString();
    }

    @Override
    public void project(CoordinateReferenceSystem crs) throws VizException {
        disposeShapes();
        issueRefresh();
    }
}
