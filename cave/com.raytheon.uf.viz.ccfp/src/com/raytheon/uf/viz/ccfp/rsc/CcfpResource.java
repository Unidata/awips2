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
package com.raytheon.uf.viz.ccfp.rsc;

import java.awt.geom.Rectangle2D;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.HashMap;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Map;

import org.eclipse.swt.graphics.RGB;
import org.eclipse.swt.graphics.Rectangle;
import org.opengis.referencing.crs.CoordinateReferenceSystem;

import com.raytheon.edex.plugin.ccfp.CcfpRecord;
import com.raytheon.uf.common.dataplugin.PluginDataObject;
import com.raytheon.uf.common.geospatial.ReferencedCoordinate;
import com.raytheon.uf.common.time.DataTime;
import com.raytheon.uf.viz.core.IExtent;
import com.raytheon.uf.viz.core.IGraphicsTarget;
import com.raytheon.uf.viz.core.IGraphicsTarget.HorizontalAlignment;
import com.raytheon.uf.viz.core.IGraphicsTarget.LineStyle;
import com.raytheon.uf.viz.core.IGraphicsTarget.TextStyle;
import com.raytheon.uf.viz.core.IGraphicsTarget.VerticalAlignment;
import com.raytheon.uf.viz.core.drawables.IWireframeShape;
import com.raytheon.uf.viz.core.drawables.PaintProperties;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.map.MapDescriptor;
import com.raytheon.uf.viz.core.rsc.AbstractVizResource;
import com.raytheon.uf.viz.core.rsc.IResourceDataChanged;
import com.raytheon.uf.viz.core.rsc.LoadProperties;
import com.raytheon.uf.viz.core.rsc.capabilities.ColorableCapability;
import com.vividsolutions.jts.geom.Coordinate;
import com.vividsolutions.jts.geom.Geometry;
import com.vividsolutions.jts.geom.Point;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;

/**
 * 
 * Resource for CCFP data
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Sep 22, 2009 3072       bsteffen     Initial creation
 * Aug 23, 2012  1096     njensen      Fixed memory leaks
 * Dec 20, 2012 DCS 135    tk          Changes for CCFP 2010 and 2012 TIN's
 * 
 * </pre>
 * 
 * @author bsteffen
 * @version 1.0
 */
public class CcfpResource extends
        AbstractVizResource<CcfpResourceData, MapDescriptor> {

    private static final transient IUFStatusHandler statusHandler = UFStatus
    .getHandler(CcfpResource.class);

    private static final String[] coverageValues = { "", "75-100%", "40-74%",
    "25-39%" };

    private static final String[] confValues = { "", "HIGH", "", "LOW" };

    private static final String[] growthValues = { "", "+", "NC", "-" };

    private static final String[] topsValues = { "", "400", "390", "340",
    "290" };

    private static final String[] resourceTypes = { "", "Solid Coverage",
            "Medium Coverage", "Sparse Coverage", "Line" };

    // This class holds cached shapes to avoid recalculating everything all
    // the time
    private class DisplayFrame {
        Collection<CcfpRecord> records = new ArrayList<CcfpRecord>();

        IWireframeShape dottedPolygons;

        IWireframeShape solidPolygons;

        // Separate from solidPolygon because it needs to be redrawn when zooms
        // occur
        IWireframeShape zoomDependentShapes;

        // When extent != lastExtent we will redo zommDependentShapes
        IExtent lastExtent;

        protected void dispose() {
            if (dottedPolygons != null) {
                dottedPolygons.dispose();
                dottedPolygons = null;
            }
            if (solidPolygons != null) {
                solidPolygons.dispose();
                solidPolygons = null;
            }
            if (zoomDependentShapes != null) {
                zoomDependentShapes.dispose();
                zoomDependentShapes = null;
            }
        }

        protected void createShapes(IGraphicsTarget target) {
            dispose();
            dottedPolygons = target.createWireframeShape(false, descriptor);
            solidPolygons = target.createWireframeShape(false, descriptor);
            zoomDependentShapes = target
                    .createWireframeShape(false, descriptor);
        }

    }

    // Place to store records that have not yet been stored
    private Map<DataTime, Collection<CcfpRecord>> unprocessedRecords = new HashMap<DataTime, Collection<CcfpRecord>>();

    private Map<DataTime, DisplayFrame> frames = new HashMap<DataTime, DisplayFrame>();

    private DataTime displayedDataTime;

    protected CcfpResource(CcfpResourceData resourceData,
            LoadProperties loadProperties) {
        super(resourceData, loadProperties);
        resourceData.addChangeListener(new IResourceDataChanged() {
            @Override
            public void resourceChanged(ChangeType type, Object object) {
                if (type == ChangeType.DATA_UPDATE) {
                    PluginDataObject[] pdo = (PluginDataObject[]) object;
                    for (PluginDataObject p : pdo) {
                        if (p instanceof CcfpRecord) {
                            addRecord((CcfpRecord) p);
                        }
                    }
                }
                issueRefresh();
            }
        });
        this.dataTimes = new ArrayList<DataTime>();

    }

    @Override
    protected void disposeInternal() {
        // Make sure we ditch all the shapes before we go
        disposeFrames();
    }

    private void disposeFrames() {
        synchronized (frames) {
            for (DisplayFrame frame : frames.values()) {
                frame.dispose();
            }
        }
    }

    @Override
    public DataTime[] getDataTimes() {
        if (this.dataTimes == null) {
            return new DataTime[0];
        }
        return this.dataTimes.toArray(new DataTime[this.dataTimes.size()]);
    }

    @Override
    protected void initInternal(IGraphicsTarget target) throws VizException {
    }

    @Override
    public String inspect(ReferencedCoordinate coord) throws VizException {
        DisplayFrame frame = null;
        synchronized (frames) {
            frame = frames.get(this.displayedDataTime);
        }
        if (frame == null) {
            return "";
        }
        StringBuilder res = new StringBuilder();
        for (CcfpRecord record : frame.records) {
            // Check if we have an area we are rendering
            if (isPaintingArea(record)) {
                try {
                    Geometry geom = record.getLocation().getGeometry();
                    Coordinate latLon = coord.asLatLon();
                    Point point = geom.getFactory().createPoint(latLon);
                    if (geom.contains(point)) {
                        String[] data = getFormattedData(record);
                        for (String s : data) {
                            res.append(s);
                            res.append(" ");
                        }
                        res.append("\n");
                    }
                } catch (Exception e) {
                    throw new VizException("Error interogating CCFP data", e);
                }
            }
        }
        return res.toString();
    }

    /**
     * process all records for the displayedDataTime
     * 
     * @param target
     * @param paintProps
     * @throws VizException
     */
    private void updateRecords(IGraphicsTarget target,
            PaintProperties paintProps) throws VizException {
        DisplayFrame frame = null;
        synchronized (frames) {
            frame = frames.get(this.displayedDataTime);
            if (frame == null) {
                frame = new DisplayFrame();
                frames.put(this.displayedDataTime, frame);
            }
        }

        // Add all the new Records
        Collection<CcfpRecord> newRecords = null;
        synchronized (unprocessedRecords) {
            newRecords = unprocessedRecords.get(this.displayedDataTime);
        }
        for (CcfpRecord record : newRecords) {
            // If we need to draw anything for this record then keep it
            if (isPaintingArea(record) || isPaintingMovement(record)
                    || isPaintingText(record) || isPaintingLine(record)) {
                frame.records.add(record);
            }
        }
        newRecords.clear();
        // Get some new shapes
        frame.createShapes(target);
        // Update each record
        for (CcfpRecord record : frame.records) {
            if (isPaintingArea(record)) {
                preparePolygon(record, frame);
            }
            if (isPaintingMovement(record)) {
                prepareArrow(record, frame, target, paintProps);
            }
            if (isPaintingText(record)) {
                prepareTextBox(record, frame, target, paintProps);
            }
            if (isPaintingLine(record)) {
                prepareLine(record, frame);
            }
        }
    }

    @Override
    protected void paintInternal(IGraphicsTarget target,
            PaintProperties paintProps) throws VizException {

        this.displayedDataTime = paintProps.getDataTime();

        // First check to see if we need to process new data
        Collection<CcfpRecord> unprocessed = null;
        synchronized (unprocessedRecords) {
            unprocessed = unprocessedRecords.get(this.displayedDataTime);
        }
        if (unprocessed != null && unprocessed.size() > 0) {
            updateRecords(target, paintProps);
        }

        // Hopefully we now have some data to display, if not bail
        DisplayFrame frame = null;
        synchronized (frames) {
            frame = frames.get(this.displayedDataTime);
        }
        if (frame == null) {
            this.displayedDataTime = null;
            return;
        }

        RGB color = getCapability(ColorableCapability.class).getColor();

        // Draw the polygons from cache
        if (frame.dottedPolygons != null) {
            target.drawWireframeShape(frame.dottedPolygons, color, 1.5f,
                    LineStyle.DASHED);
        }

        if (frame.solidPolygons != null) {
            target.drawWireframeShape(frame.solidPolygons, color, 1.5f,
                    LineStyle.SOLID);
        }

        // If extents changed, update (but don't draw) zoom dependent shapes
        if (!paintProps.getView().getExtent().equals(frame.lastExtent)) {
            if (frame.zoomDependentShapes != null) {
                frame.zoomDependentShapes.dispose();
            }
            frame.zoomDependentShapes = target.createWireframeShape(false,
                    descriptor);
            for (CcfpRecord record : frame.records) {
                if (isPaintingText(record)) {
                    prepareTextBox(record, frame, target, paintProps);
                }
                if (isPaintingMovement(record)) {
                    prepareArrow(record, frame, target, paintProps);
                }
            }
            frame.lastExtent = paintProps.getView().getExtent();
        }

        // Draw the text
        for (CcfpRecord record : frame.records) {
            if (isPaintingText(record)) {
                paintText(record, target);
            }
        }

        // Finally draw the zoomDependentShapes
        if (frame.zoomDependentShapes != null) {
            target.drawWireframeShape(frame.zoomDependentShapes, color, 1.0f,
                    LineStyle.SOLID);
        }
    }

    /**
     * Paint the text onto the target
     * 
     * @param record
     * @param target
     * @throws VizException
     */
    private void paintText(CcfpRecord record, IGraphicsTarget target)
            throws VizException {
        RGB color = getCapability(ColorableCapability.class).getColor();
        String[] lines = getFormattedData(record);

        // This point should be the center left
        double[] pt = descriptor.worldToPixel(new double[] {
                record.getBoxLong(), record.getBoxLat() });
        // Draw the text
        target.drawStrings(null, lines, pt[0], pt[1], pt[2], TextStyle.BLANKED,
                new RGB[] { color, color, color, color },
                HorizontalAlignment.LEFT, VerticalAlignment.MIDDLE);
    }

    /**
     * Add an arrow to the current frame
     * 
     * @param record
     * @param frame
     * @param target
     * @param paintProps
     * @throws VizException
     */
    private void prepareArrow(CcfpRecord record, DisplayFrame frame,
            IGraphicsTarget target, PaintProperties paintProps)
            throws VizException {
        Integer dir = record.getDirection();
        Integer spd = record.getSpeed();
        double[] scale = getScale(paintProps);
        Coordinate centerll = findPointForDirection(record.getLocation()
                .getGeometry(), dir);
        // Change from north at 0 degrees to x axis at zero degrees
        dir = (dir + 90);
        double[] center = descriptor.worldToPixel(new double[] { centerll.x,
                centerll.y });
        double[] txt = calculateRotation(center, spd + 10, dir, scale, target);
        double[] end = calculateRotation(center, spd, dir, scale, target);
        double[] point1 = calculateRotation(end, spd * 0.2, dir + 225, scale,
                target);
        double[] point2 = calculateRotation(end, spd * 0.2, dir + 135, scale,
                target);
        // This is the arrow Body
        frame.zoomDependentShapes
                .addLineSegment(new double[][] { center, end });
        // This is the arrow head
        frame.zoomDependentShapes.addLineSegment(new double[][] { point1, end,
                point2 });
        // Add a label to the arrow
        frame.zoomDependentShapes.addLabel(spd.toString(), txt);
    }

    /**
     * Add a box that will be drawn around the text to the current frame
     * 
     * @param record
     * @param frame
     * @param target
     * @param paintProps
     * @throws VizException
     */
    private void prepareTextBox(CcfpRecord record, DisplayFrame frame,
            IGraphicsTarget target, PaintProperties paintProps)
            throws VizException {

        double[] scale = getScale(paintProps);

        // Determine the dimensions of the text lines;
        String[] lines = getFormattedData(record);
        double maxWidth = 0;
        double height = 0;
        for (String line : lines) {
            Rectangle2D rect = target.getStringBounds(null, line);
            if (rect.getWidth() > maxWidth) {
                maxWidth = rect.getWidth();
                height = rect.getHeight() + 3;
            }
        }
        // This point should be the center left on the box
        double[] pt = descriptor.worldToPixel(new double[] {
                record.getBoxLong(), record.getBoxLat() });
        // Calculate the corners of the text box
        double x1 = pt[0] - 3 * scale[0];
        double x2 = pt[0] + (maxWidth + 3) * scale[0];
        double y1 = pt[1] + (height * 2 + 3) * scale[1];
        double y2 = pt[1] - (height * 2 + 3) * scale[1];
        // Add the box to our shape
        double[][] box = { { x1, y1 }, { x1, y2 }, { x2, y2 }, { x2, y1 },
                { x1, y1 } };
        frame.zoomDependentShapes.addLineSegment(box);

        // Calcualte the center of the box and the polygon
        Coordinate polyCenterLL = record.getLocation().getGeometry()
                .getCentroid().getCoordinate();
        double[] polyCenter = descriptor.worldToPixel(new double[] {
                polyCenterLL.x, polyCenterLL.y });
        double[] boxCenter = new double[] { (x1 + x2) / 2, (y1 + y2) / 2 };
        // Determine which edge it is closest too and assign the closest Point
        // accordingly
        double[] centerDiff = new double[] { (boxCenter[0] - polyCenter[0]),
                (boxCenter[1] - polyCenter[1]) };
        double[] closestBox = new double[2];
        if (Math.abs(centerDiff[0]) < Math.abs(centerDiff[1])) {
            closestBox[0] = boxCenter[0];
            if (centerDiff[1] > 0) {
                closestBox[1] = y2;
            } else {
                closestBox[1] = y1;
            }
        } else {
            closestBox[1] = boxCenter[1];
            if (centerDiff[0] > 0) {
                closestBox[0] = x1;
            } else {
                closestBox[0] = x2;
            }
        }

        // Calculate the closest point or midpoint on the polygon to our box
        Coordinate[] polygon = record.getLocation().getGeometry()
                .getCoordinates();
        double bestDiff = Double.MAX_VALUE;
        double[] closestPoly = polyCenter;
        Coordinate lastPoint = polygon[polygon.length - 1];
        for (Coordinate curPoint : polygon) {
            Coordinate thisPoint = curPoint;
            for (int j = 0; j < 2; ++j) {
                // First, test this vertex
                double[] polyPixel = descriptor.worldToPixel(new double[] {
                        thisPoint.x, thisPoint.y, thisPoint.z });
                double diffX = polyPixel[0] - closestBox[0];
                double diffY = polyPixel[1] - closestBox[1];
                double diff = diffX * diffX + diffY * diffY;
                if (diff < bestDiff) {
                    bestDiff = diff;
                    closestPoly = polyPixel;
                }
                // Second, test the midpoint between this vertex and the
                // previous one.
                thisPoint = new Coordinate((curPoint.x + lastPoint.x) / 2,
                        (curPoint.y + lastPoint.y) / 2);
            }
            lastPoint = curPoint;
        }
        // Finally through the line into our shape
        frame.zoomDependentShapes.addLineSegment(new double[][] { closestBox,
                closestPoly });
    }

    /**
     * Get a polygon from a record and add it to the frame
     * 
     * @param record
     * @param frame
     * @throws VizException
     */
    private void preparePolygon(CcfpRecord record, DisplayFrame frame)
            throws VizException {
        IWireframeShape shape = frame.solidPolygons;
        if (record.getConf() == 3) { // low
            shape = frame.dottedPolygons;
        }
        Geometry geom = record.getLocation().getGeometry();
        shape.addLineSegment(geom.getCoordinates());
    }

    /**
     * Get the line from a record and add it to the frame
     * 
     * @param record
     * @param frame
     * @throws VizException
     */
    private void prepareLine(CcfpRecord record, DisplayFrame frame)
            throws VizException {
        Geometry geom = record.getLocation().getGeometry();
        if (record.getCoverage() == 2) {
            frame.dottedPolygons.addLineSegment(geom.getCoordinates());
        }
        else {
            frame.solidPolygons.addLineSegment(geom.getCoordinates());
        }

    }

    /**
     * Adds a new record to this resource
     * 
     * @param obj
     */
    protected void addRecord(CcfpRecord obj) {
        DataTime dataTime = obj.getDataTime();
        if (resourceData.durationMatches(dataTime)) {
            Collection<CcfpRecord> records = null;
            boolean brandNew = false;
            synchronized (unprocessedRecords) {
                records = unprocessedRecords.get(dataTime);
                if (records == null) {
                    records = new LinkedHashSet<CcfpRecord>();
                    unprocessedRecords.put(dataTime, records);
                    brandNew = true;
                }
            }
            if (brandNew) {
                this.dataTimes.add(dataTime);
                Collections.sort(this.dataTimes);
            }

            records.add(obj);
        }
    }

    @Override
    public String getName() {
        int coverage = this.resourceData.getCoverageFilter();
        int validDuration = resourceData.getValidDuration() / 3600;
        return validDuration + " Hour CCFP " + resourceTypes[coverage];
    }

    /**
     * Get the commonly used output strings for this record
     * 
     * @param record
     * @return
     */
    private String[] getFormattedData(CcfpRecord record) {
        String[] lines = new String[4];

        int tops = record.getTops();
        if (tops < topsValues.length) {
    		lines[0] = "TOPS: " + topsValues[tops];
    	} else {
    		lines[0] = "TOPS: ";
    		statusHandler.handle(Priority.EVENTA,
                    "Problem interogating CCFP data: tops value out of range");
    	}
        
        int gwth = record.getGrowth();
        if (gwth < growthValues.length) {
    		lines[1] = "GWTH: " + growthValues[gwth];
    	} else {
    		 lines[1] = "GWTH: ";
    		 statusHandler.handle(Priority.EVENTA,
             "Problem interogating CCFP data: growth value out of range");
    	}
        
        int conf = record.getConf();
        if (conf < confValues.length) {
    		lines[2] = "CONF: " + confValues[conf];
    	} else {
    		lines[2] = "CONF: ";
    		statusHandler.handle(Priority.EVENTA,
            "Problem interogating CCFP data: confidence value out of range");
    	}
        
        int cvrg = record.getCoverage();
        if (cvrg < coverageValues.length) {
    		lines[3] = "CVRG: " + coverageValues[cvrg];
    	} else {
    		lines[3] = "CVRG: ";
    		statusHandler.handle(Priority.EVENTA,
            "Problem interogating CCFP data: coverage value out of range");
    	}
        
        return lines;
    }

    /**
     * determine if this resource will paint a polygon for this record
     * 
     * @param record
     * @return
     */
    private boolean isPaintingArea(CcfpRecord record) {
        return record.getProducttype().equals("AREA")
                && resourceData.getCoverageFilter() == record.getCoverage()
                && resourceData.isDisplayArea();
    }

    /**
     * determine if this resource will paint a line for this record
     * 
     * @param record
     * @return
     */
    private boolean isPaintingLine(CcfpRecord record) {
        return record.getProducttype().equals("LINE")
                && resourceData.getCoverageFilter() == 4
                && resourceData.isDisplayArea();
    }

    /**
     * determine if this resource will paint text for this record
     * 
     * @param record
     * @return
     */
    private boolean isPaintingText(CcfpRecord record) {
        return record.getProducttype().equals("AREA")
                && resourceData.getCoverageFilter() == record.getCoverage()
                && resourceData.isDisplayText();
    }

    /**
     * determine if this resource will paint movement arrow for this record
     * 
     * @param record
     * @return
     */
    private boolean isPaintingMovement(CcfpRecord record) {
        return record.getProducttype().equals("AREA")
                && resourceData.getCoverageFilter() == record.getCoverage()
                && resourceData.isDisplayMovement() && record.getSpeed() != 0;
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
     * Find a point on the geometry on the outer edge of direction
     * 
     * @param geometry
     * @param direction
     * @return
     */
    private Coordinate findPointForDirection(Geometry geometry, int direction) {

        // Convert cw degrees from north to ccw radians from postive X axis
        double targetDir = (360 - direction + 90) * Math.PI / 180;
        Coordinate[] polygon = geometry.getCoordinates();
        Coordinate centroid = geometry.getCentroid().getCoordinate();
        if (polygon.length < 2) {
            return centroid;
        }
        double bestDiff = 2 * Math.PI;
        Coordinate bestPoint = centroid;

        Coordinate lastPoint = polygon[polygon.length - 1];

        for (Coordinate curPoint : polygon) {
            Coordinate thisPoint = curPoint;
            for (int j = 0; j < 2; ++j) {
                // First, test this vertex
                double angle = Math.atan2(centroid.y - thisPoint.y, thisPoint.x
                        - centroid.x);
                double diff = Math.abs(angle - targetDir);
                while (diff >= 2 * Math.PI)
                    diff -= 2 * Math.PI;
                if (diff > Math.PI)
                    diff = 2 * Math.PI - diff;

                if (diff < bestDiff) {
                    bestDiff = diff;
                    bestPoint = thisPoint;
                }
                // Second, test the midpoint between this vertex and the
                // previous one.
                thisPoint = new Coordinate((curPoint.x + lastPoint.x) / 2,
                        (curPoint.y + lastPoint.y) / 2);
            }
            lastPoint = curPoint;
        }
        return bestPoint;

    }

    /**
     * Used to calculate the rotation around a point maintaining a consistent
     * size across zoom levels
     * 
     * @param center
     * @param radius
     * @param angle
     * @param scale
     * @param target
     * @return
     * @throws VizException
     */
    private double[] calculateRotation(double[] center, double radius,
            double angle, double[] scale, IGraphicsTarget target)
            throws VizException {
        double[] point = target.getPointOnCircle(center[0], center[1],
                center[2], radius, angle);
        point[0] = center[0] + (center[0] - point[0]) * scale[0];
        point[1] = center[1] + (center[1] - point[1]) * scale[1];
        return point;
    }

    @Override
    public void project(CoordinateReferenceSystem crs) throws VizException {
        synchronized (frames) {
            disposeFrames();
            // add as unprocessed to make sure frames created
            for (DataTime time : frames.keySet()) {
                DisplayFrame frame = frames.get(time);
                if (frame != null) {
                    List<CcfpRecord> copyList = new ArrayList<CcfpRecord>(
                            frame.records);
                    synchronized (unprocessedRecords) {
                        unprocessedRecords.put(time, copyList);
                    }
                }
            }
        }
    }

    @Override
    public void remove(DataTime time) {
        super.remove(time);
        Collection<CcfpRecord> notNeeded = null;
        synchronized (unprocessedRecords) {
            notNeeded = unprocessedRecords.remove(time);
        }
        if (notNeeded != null) {
            notNeeded.clear();
        }

        DisplayFrame frame = null;
        synchronized (frames) {
            frame = frames.remove(time);
        }
        if (frame != null) {
            frame.dispose();
        }
    }

}
