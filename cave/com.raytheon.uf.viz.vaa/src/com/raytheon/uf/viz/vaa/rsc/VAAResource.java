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
import java.util.Collections;
import java.util.HashMap;
import java.util.Map;

import org.eclipse.swt.graphics.RGB;
import org.eclipse.swt.graphics.Rectangle;

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
import com.raytheon.uf.viz.core.rsc.AbstractVizResource;
import com.raytheon.uf.viz.core.rsc.IResourceDataChanged;
import com.raytheon.uf.viz.core.rsc.LoadProperties;
import com.raytheon.uf.viz.core.rsc.capabilities.ColorableCapability;
import com.raytheon.uf.viz.core.rsc.capabilities.MagnificationCapability;
import com.raytheon.uf.viz.vaa.util.CommonUtil;
import com.raytheon.viz.pointdata.util.SymbolLoader;
import com.vividsolutions.jts.geom.Coordinate;
import com.vividsolutions.jts.geom.Envelope;
import com.vividsolutions.jts.geom.Point;
import com.vividsolutions.jts.geom.Polygon;

/**
 * Resource for Volcanic Ash Advisories data
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date          Ticket#  Engineer    Description
 * ------------- -------- ----------- --------------------------
 * Nov 23, 2009  3268     jsanchez    Initial creation
 * Jun 06, 2014  2061     bsteffen    Remove unneccessary imports
 * Jul 29, 2014  3465     mapeters    Updated deprecated drawString() calls.
 * 
 * </pre>
 * 
 * @author jsanchez
 * @version 1.0
 */
public class VAAResource extends
        AbstractVizResource<VAAResourceData, MapDescriptor> {
    private static final transient IUFStatusHandler statusHandler = UFStatus.getHandler(VAAResource.class);

    protected DataTime displayedDataTime;

    private SymbolLoader symbolLoader;

    private double screenToWorldRatio;

    private double plotWidth;

    private double actualPlotWidth;

    private IFont font;

    private static final String ANAL00 = "ANAL00";

    private final static String AREA = "AREA";

    private static final char VOLCANO_SYMBOL = '\u00b1';

    private static final double inset = 150.0;

    private Map<DataTime, Collection<VAARecord>> recordsToParse = new HashMap<DataTime, Collection<VAARecord>>();

    protected VAAResource(VAAResourceData resourceData,
            LoadProperties loadProperties) {
        super(resourceData, loadProperties);
        resourceData.addChangeListener(new IResourceDataChanged() {
            @Override
            public void resourceChanged(ChangeType type, Object object) {
                if (type == ChangeType.DATA_UPDATE) {
                    PluginDataObject[] pdo = (PluginDataObject[]) object;
                    for (PluginDataObject p : pdo) {
                        if (p instanceof VAARecord) {
                            addRecord((VAARecord) p);
                        }
                    }
                } else if (type == ChangeType.CAPABILITY) {
                    if (object instanceof MagnificationCapability) {
                        int mag = ((MagnificationCapability) object)
                                .getMagnification().intValue();
                        double newPlotWidth = actualPlotWidth * mag;
                        if (plotWidth != newPlotWidth) {
                            plotWidth = newPlotWidth;
                        }
                    }
                }
                issueRefresh();
            }
        });
        this.dataTimes = new ArrayList<DataTime>();
    }

    @Override
    public String getName() {
        return "Volcanic Ash Advisory";
    }

    @Override
    public String inspect(ReferencedCoordinate coord) throws VizException {
        String returnValue = "";

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
            double scaleValue = (this.plotWidth / 2.0)
                    / this.screenToWorldRatio;
            PixelExtent interrogationExtent = new PixelExtent(pml[0]
                    - scaleValue, pml[0] + scaleValue, pml[1] - scaleValue,
                    pml[1] + scaleValue);
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
                            point = pixelPoly.getFactory()
                                    .createPoint(
                                            coord.asPixel(descriptor
                                                    .getGridGeometry()));
                        } catch (Exception e) {
                            throw new VizException(
                                    "Error inspecting Volcanic Ash Advisories",
                                    e);
                        }

                        if (pixelPoly.contains(point)) {
                            returnValue = record.getAnal00Hr() != null ? record
                                    .getAnal00Hr() : "";
                        }
                    }
                }

                if (!llExtent.contains(record.getLongitude(),
                        record.getLatitude())) {
                    continue;
                } else {
                    if (returnValue.length() > 0) {
                        returnValue += "\n \n";
                    }
                    returnValue += record.getMessage();
                }
            }
        }

        return returnValue.length() > 0 ? returnValue : "NO DATA";
    }

    /**
     * Adds a new record to this resource
     * 
     * @param obj
     */
    protected void addRecord(VAARecord obj) {
        DataTime dataTime = obj.getDataTime();
        Collection<VAARecord> toParse = recordsToParse.get(dataTime);
        if (toParse == null) {
            dataTimes.add(dataTime);
            Collections.sort(this.dataTimes);
            toParse = new ArrayList<VAARecord>();
            recordsToParse.put(dataTime, toParse);
        }
        toParse.add(obj);
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.viz.core.rsc.AbstractVizResource#paintInternal(com.raytheon
     * .uf.viz.core.IGraphicsTarget,
     * com.raytheon.uf.viz.core.drawables.PaintProperties)
     */
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

        if (toParse != null && toParse.size() > 0) {
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
        double[] loc = descriptor.worldToPixel(new double[] {
                record.getLongitude(), record.getLatitude() });

        double maxX = extent.getMaxX();
        double minX = extent.getMinX();
        double maxY = extent.getMaxY();
        double minY = extent.getMinY();

        maxX = maxX < 0 ? 0 : maxX;
        maxX = maxX > 19999 ? 19999 : maxX;
        minX = minX < 0 ? 0 : minX;
        minX = minX > 19999 ? 19999 : minX;
        maxY = maxY < 0 ? 0 : maxY;
        maxY = maxY > 19999 ? 19999 : maxY;
        minY = minY < 0 ? 0 : minY;
        minY = minY > 19999 ? 19999 : minY;

        PixelExtent correctedExtent = new PixelExtent(minX, maxX, minY, maxY);
        target.clearClippingPlane();
        if (correctedExtent.contains(loc)) {
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
                string.setCoordinates(loc[0] + 30, loc[1]);
                string.verticallAlignment = VerticalAlignment.MIDDLE;
                target.drawStrings(string);
            }

            for (VAASubPart subPart : record.getSubParts()) {
                if (subPart.getSubText() != null
                        && subPart.getSubText().equals(ANAL00)) {
                    Coordinate[] coordinates = CommonUtil
                            .getCoordinates(subPart.getLocations());
                    IWireframeShape shape = target.createWireframeShape(false,
                            descriptor);
                    shape.addLineSegment(coordinates);
                    target.drawWireframeShape(shape, color, 0.5f);
                    shape.dispose();
                }
            }
        } else {
            double x = correctedExtent.getMinX();
            double y = correctedExtent.getMaxY()
                    - (inset * paintProps.getZoomLevel());
            String message = "Volcano " + volcanoName + " is off-screen";
            DrawableString string = new DrawableString(message, color);
            string.font = font;
            string.setCoordinates(x, y);
            string.verticallAlignment = VerticalAlignment.MIDDLE;
            target.drawStrings(string);
        }
        target.setupClippingPlane(paintProps.getView().getExtent());

    }

    @Override
    protected void disposeInternal() {
        if (font != null) {
            font.dispose();
            font = null;
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
        this.font = target.initializeFont("Monospace", 11,
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
        String returnValue = "";
        String parts[] = stationId.split(" ");
        for (String str : parts) {
            if (str.length() > 0 && str.charAt(0) >= 'A'
                    && str.charAt(0) <= 'Z') {
                if (str.length() > 0) {
                    str += " ";
                }
                returnValue += str;
            }
        }
        return returnValue;
    }
}
