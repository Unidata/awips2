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
package com.raytheon.uf.viz.aviation.advisory.rsc;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.eclipse.swt.graphics.RGB;
import org.eclipse.swt.graphics.Rectangle;
import org.opengis.referencing.crs.CoordinateReferenceSystem;

import com.raytheon.uf.common.dataplugin.PluginDataObject;
import com.raytheon.uf.common.geospatial.ReferencedCoordinate;
import com.raytheon.uf.common.time.DataTime;
import com.raytheon.uf.viz.aviation.advisory.AdvisoryRecord;
import com.raytheon.uf.viz.aviation.advisory.AdvisoryRecord.AdvisoryResourceType;
import com.raytheon.uf.viz.core.DrawableString;
import com.raytheon.uf.viz.core.IExtent;
import com.raytheon.uf.viz.core.IGraphicsTarget;
import com.raytheon.uf.viz.core.IGraphicsTarget.HorizontalAlignment;
import com.raytheon.uf.viz.core.IGraphicsTarget.LineStyle;
import com.raytheon.uf.viz.core.IGraphicsTarget.TextStyle;
import com.raytheon.uf.viz.core.IGraphicsTarget.VerticalAlignment;
import com.raytheon.uf.viz.core.PixelCoverage;
import com.raytheon.uf.viz.core.drawables.IFont;
import com.raytheon.uf.viz.core.drawables.IFont.Style;
import com.raytheon.uf.viz.core.drawables.IImage;
import com.raytheon.uf.viz.core.drawables.IWireframeShape;
import com.raytheon.uf.viz.core.drawables.PaintProperties;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.map.MapDescriptor;
import com.raytheon.uf.viz.core.point.display.SymbolLoader;
import com.raytheon.uf.viz.core.rsc.AbstractVizResource;
import com.raytheon.uf.viz.core.rsc.IResourceDataChanged;
import com.raytheon.uf.viz.core.rsc.LoadProperties;
import com.raytheon.uf.viz.core.rsc.capabilities.ColorableCapability;
import com.raytheon.uf.viz.core.rsc.capabilities.MagnificationCapability;
import com.raytheon.uf.viz.core.rsc.capabilities.OutlineCapability;
import org.locationtech.jts.geom.Coordinate;
import org.locationtech.jts.geom.GeometryFactory;
import org.locationtech.jts.geom.Point;
import org.locationtech.jts.geom.Polygon;

/**
 * 
 * Resource for any Aviation Data that is represented as an outline with a
 * single label
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date          Ticket#  Engineer  Description
 * ------------- -------- --------- --------------------------------------------
 * Oct 01, 2009           bsteffen  Initial creation
 * Jun 10, 2011  9744     cjeanbap  Added Magnification, Outline, and Density
 *                                  capability.
 * Jun 13, 2011  9758     cjeanbap  Set colorString of AdvisoryResourceData.
 * Aug 14, 2014  3523     mapeters  Updated deprecated {@link
 *                                  DrawableString#textStyle} assignments.
 * Jul 07, 2015  10352    byin      Added SymbolLoader to plot symbols
 * Nov 05, 2015  5070     randerso  Adjust font sizes for dpi scaling
 * Nov 28, 2017  5863     bsteffen  Change dataTimes to a NavigableSet
 * Feb 22, 2018  6629     njensen   Removed special color handling
 * 
 * </pre>
 * 
 * @author bsteffen
 */
public class AdvisoryResource
        extends AbstractVizResource<AdvisoryResourceData, MapDescriptor> {

    protected static final String NO_DATA = "No Data Available";

    protected Map<DataTime, Collection<AdvisoryRecord>> records = new HashMap<>();

    protected DataTime displayedDataTime;

    protected IFont font;

    private IWireframeShape mainShape;

    private IWireframeShape dottedShape;

    private IWireframeShape dashedShape;

    private Coordinate nonStandardInspectCoordinate;

    private float magnification;

    private SymbolLoader symbolLoader;

    protected AdvisoryResource(AdvisoryResourceData resourceData,
            LoadProperties loadProperties) {
        super(resourceData, loadProperties, false);
        resourceData.addChangeListener(new IResourceDataChanged() {
            @Override
            public void resourceChanged(ChangeType type, Object object) {
                if (type == ChangeType.DATA_UPDATE) {
                    PluginDataObject[] pdo = (PluginDataObject[]) object;
                    for (PluginDataObject p : pdo) {
                        addRecord(p);
                    }
                }
                issueRefresh();
            }
        });
    }

    @Override
    public String getName() {
        return resourceData.getName();
    }

    /**
     * Adds a new record to this resource
     * 
     * @param obj
     */
    protected void addRecord(PluginDataObject obj) {
        Collection<AdvisoryRecord> parsedRecords = resourceData.getDataAdapter()
                .convertRecord(obj);
        if (parsedRecords != null && !parsedRecords.isEmpty()) {
            clearShapes();
            DataTime dataTime = obj.getDataTime();
            if (this.resourceData.getBinOffset() != null) {
                // long timeInMillis = dataTime.getRefTime().getTime();
                // timeInMillis = this.resourceData.getBinOffset()
                // .getNormalizedTime(timeInMillis);
                // timeInMillis -=
                // this.resourceData.getBinOffset().virtualOffset * 1000;
                // dataTime = new DataTime(new Date(timeInMillis));
                dataTime = this.resourceData.getBinOffset()
                        .getNormalizedTime(dataTime);

            }
            Collection<AdvisoryRecord> oldParse = records.get(dataTime);
            if (oldParse == null) {
                dataTimes.add(dataTime);
                oldParse = new ArrayList<>();
                records.put(dataTime, oldParse);
            }
            oldParse.addAll(parsedRecords);
        }
    }

    @Override
    protected void disposeInternal() {
        clearShapes();
        if (font != null) {
            font.dispose();
            font = null;
        }
    }

    protected void clearShapes() {
        if (mainShape != null) {
            mainShape.dispose();
            mainShape = null;
        }
        if (dottedShape != null) {
            dottedShape.dispose();
            dottedShape = null;
        }
        if (dashedShape != null) {
            dashedShape.dispose();
            dashedShape = null;
        }

    }

    @Override
    protected void initInternal(IGraphicsTarget target) throws VizException {
        if (font != null) {
            font.dispose();
        }
        this.font = target.initializeFont("Monospace", 8, new Style[] {});
        this.symbolLoader = new SymbolLoader();
    }

    @Override
    protected void paintInternal(IGraphicsTarget target,
            PaintProperties paintProps) throws VizException {
        RGB color = getCapability(ColorableCapability.class).getColor();
        DataTime curDataTime = paintProps.getDataTime();
        if (curDataTime == null || records.get(curDataTime) == null) {
            this.displayedDataTime = null;
            clearShapes();

            return;
        } else if (!curDataTime.equals(displayedDataTime)) {
            clearShapes();

        }

        this.displayedDataTime = curDataTime;
        double scale[] = getScale(paintProps);

        if (mainShape == null || dottedShape == null || dashedShape == null) {
            clearShapes();
            mainShape = target.createWireframeShape(false, descriptor);
            dottedShape = target.createWireframeShape(false, descriptor);
            dashedShape = target.createWireframeShape(false, descriptor);
            for (AdvisoryRecord record : records.get(curDataTime)) {
                if (record.getType() == AdvisoryResourceType.AREA) {
                    mainShape.addLineSegment(
                            record.getPolygon().getCoordinates());
                    if (record.getLabel() != null
                            || !record.getLabel().isEmpty()) {
                        double[] pixelLoc = descriptor.worldToPixel(
                                new double[] { record.getLabelLoc().x,
                                        record.getLabelLoc().y });
                        mainShape.addLabel(record.getLabel(), pixelLoc);
                    }
                } else if (record.getType() == AdvisoryResourceType.LINE) {
                    mainShape.addLineSegment(record.getLine());
                    dashedShape.addLineSegment(
                            record.getPolygon().getCoordinates());
                    if (record.getLabel() != null
                            || !record.getLabel().isEmpty()) {
                        double[] pixelLoc = descriptor.worldToPixel(
                                new double[] { record.getLabelLoc().x,
                                        record.getLabelLoc().y });
                        dashedShape.addLabel(record.getLabel(), pixelLoc);
                    }
                } else if (record.getType() == AdvisoryResourceType.ISOL) {
                    dottedShape.addLineSegment(
                            record.getPolygon().getCoordinates());
                    if (record.getLabel() != null
                            || !record.getLabel().isEmpty()) {
                        double[] pixelLoc = descriptor.worldToPixel(
                                new double[] { record.getLabelLoc().x,
                                        record.getLabelLoc().y });
                        dottedShape.addLabel(record.getLabel(), pixelLoc);
                    }
                } else {
                    double[] pixelLoc = descriptor.worldToPixel(new double[] {
                            record.getLabelLoc().x, record.getLabelLoc().y });
                    font.setMagnification(magnification);

                    DrawableString dStrings = new DrawableString(
                            record.getLabel(), color);
                    dStrings.font = font;
                    dStrings.setCoordinates(pixelLoc[0], pixelLoc[1]);
                    dStrings.addTextStyle(TextStyle.DROP_SHADOW);
                    dStrings.horizontalAlignment = HorizontalAlignment.LEFT;
                    dStrings.verticallAlignment = VerticalAlignment.TOP;
                    target.drawStrings(dStrings);
                }
            }
            mainShape.compile();
            dottedShape.compile();
            dashedShape.compile();
        } else {
            mainShape.clearLabels();
            dashedShape.clearLabels();
            dottedShape.clearLabels();

            List<DrawableString> strings = new ArrayList<>();
            for (AdvisoryRecord record : records.get(curDataTime)) {
                if (record.getType() == AdvisoryResourceType.AREA) {
                    font.setMagnification(magnification);
                    double[] pixelLoc = descriptor.worldToPixel(new double[] {
                            record.getLabelLoc().x, record.getLabelLoc().y });
                    String[] labels = record.getLabel().split("\n");
                    RGB[] colors = new RGB[labels.length];
                    Arrays.fill(colors, color);

                    double x = pixelLoc[0];
                    double y = pixelLoc[1];

                    if (record.getLabelSymbolId() != 0) {
                        IImage symbolImg = symbolLoader.getImage(target, color,
                                record.getLabelSymbolId());

                        Coordinate ul = new Coordinate(x, y);
                        Coordinate ur = new Coordinate(x + 12 * scale[0], y);
                        Coordinate lr = new Coordinate(ur.x, y + 12 * scale[1]);
                        Coordinate ll = new Coordinate(x, lr.y);
                        PixelCoverage extent = new PixelCoverage(ul, ur, lr,
                                ll);
                        target.drawRaster(symbolImg, extent, paintProps);
                    }

                    DrawableString dStrings = new DrawableString(labels,
                            colors);
                    dStrings.font = font;
                    dStrings.setCoordinates(pixelLoc[0], pixelLoc[1]);
                    dStrings.addTextStyle(TextStyle.DROP_SHADOW);
                    dStrings.horizontalAlignment = HorizontalAlignment.CENTER;
                    dStrings.verticallAlignment = VerticalAlignment.MIDDLE;
                    strings.add(dStrings);
                } else if (record.getType() == AdvisoryResourceType.TEXT) {
                    double[] pixelLoc = descriptor.worldToPixel(new double[] {
                            record.getLabelLoc().x, record.getLabelLoc().y });
                    font.setMagnification(magnification);

                    DrawableString dStrings = new DrawableString(
                            record.getLabel(), color);
                    dStrings.font = font;
                    dStrings.setCoordinates(pixelLoc[0], pixelLoc[1]);
                    dStrings.addTextStyle(TextStyle.DROP_SHADOW);
                    dStrings.horizontalAlignment = HorizontalAlignment.LEFT;
                    dStrings.verticallAlignment = VerticalAlignment.TOP;
                    strings.add(dStrings);
                }
            }
            target.drawStrings(strings);
        }
        // LineStyle lineStyle = resourceData.getDataAdapter().getLineStyle();
        // float lineWidth = resourceData.getDataAdapter().getLineWidth();
        LineStyle lineStyle = getCapability(OutlineCapability.class)
                .getLineStyle();
        float lineWidth = getCapability(OutlineCapability.class)
                .getOutlineWidth();
        target.drawWireframeShape(mainShape, color, lineWidth, lineStyle);
        target.drawWireframeShape(dottedShape, color, lineWidth,
                LineStyle.DOTTED);
        target.drawWireframeShape(dashedShape, color, lineWidth,
                LineStyle.DASHED);

        magnification = (float) (1
                * getCapability(MagnificationCapability.class)
                        .getMagnification());

        // Questionable
        // double density = getCapability(DensityCapability.class).getDensity();

        if (nonStandardInspectCoordinate != null) {
            paintNonstandardInspect(target, paintProps);
        }

    }

    private void paintNonstandardInspect(IGraphicsTarget target,
            PaintProperties paintProps) throws VizException {
        if (records.get(displayedDataTime) == null) {
            return;
        }
        double zoomLevel = paintProps.getZoomLevel();
        for (AdvisoryRecord record : records.get(displayedDataTime)) {
            if (record.getType() == AdvisoryResourceType.TEXT) {
                continue;
            }
            Coordinate[] coords = record.getPolygon().getCoordinates();
            if (coords.length < 1) {
                continue;
            }
            double[] firstPixel = descriptor
                    .worldToPixel(new double[] { coords[0].x, coords[0].y });
            double dx = firstPixel[0] - nonStandardInspectCoordinate.x;
            double dy = firstPixel[1] - nonStandardInspectCoordinate.y;
            double distance = (dx * dx + dy * dy) / zoomLevel;
            if (distance < 500 && record.getInspectString() != null) {
                RGB color = getCapability(ColorableCapability.class).getColor();
                String[] nonStandardInspectStrings = record.getInspectString()
                        .split("\n");
                IExtent extent = paintProps.getView().getExtent();
                target.clearClippingPlane();
                RGB[] colors = new RGB[nonStandardInspectStrings.length];
                Arrays.fill(colors, color);
                double xLoc = extent.getMinX() + (50 * zoomLevel);
                double yLoc = extent.getMinY() + (100 * zoomLevel);
                font.setMagnification(magnification);

                DrawableString dStrings = new DrawableString(
                        nonStandardInspectStrings, colors);
                dStrings.font = null;
                dStrings.setCoordinates(xLoc, yLoc);
                dStrings.addTextStyle(TextStyle.BLANKED);
                dStrings.horizontalAlignment = HorizontalAlignment.LEFT;
                dStrings.verticallAlignment = VerticalAlignment.TOP;
                target.drawStrings(dStrings);
                return;
            }
        }
        nonStandardInspectCoordinate = null;

    }

    @Override
    public String inspect(ReferencedCoordinate refCoord) throws VizException {

        Collection<AdvisoryRecord> curRecords = records
                .get(this.displayedDataTime);
        if (curRecords == null) {
            return "";
        }
        try {
            if (resourceData.isEnableNonstandardInspect()) {
                nonStandardInspectCoordinate = refCoord
                        .asPixel(descriptor.getGridGeometry());
                return "";
            }
            GeometryFactory factory = new GeometryFactory();

            for (AdvisoryRecord record : curRecords) {
                Polygon polygon = record.getPolygon();
                if (polygon == null) {
                    continue;
                }
                Point point = factory.createPoint(refCoord.asLatLon());
                if (polygon.contains(point)) {
                    return record.getInspectString();
                }
            }
        } catch (Exception e) {
            throw new VizException("Error inspecting aviation advisory data",
                    e);
        }
        return "";

    }

    @Override
    public void project(CoordinateReferenceSystem crs) throws VizException {
        clearShapes();
    }

    private double[] getScale(PaintProperties paintProps) {
        IExtent extent = paintProps.getView().getExtent();
        Rectangle canvasBounds = paintProps.getCanvasBounds();
        double[] scale = new double[2];
        scale[0] = extent.getWidth() / canvasBounds.width;
        scale[1] = extent.getHeight() / canvasBounds.height;
        return scale;
    }

}
