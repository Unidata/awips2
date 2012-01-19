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
package com.raytheon.uf.viz.ncwf.rsc;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.HashMap;
import java.util.Map;

import org.opengis.referencing.crs.CoordinateReferenceSystem;

import com.raytheon.uf.common.dataplugin.ncwf.BUFRncwf;
import com.raytheon.uf.common.dataquery.requests.RequestConstraint;
import com.raytheon.uf.common.dataquery.requests.RequestConstraint.ConstraintType;
import com.raytheon.uf.common.pointdata.PointDataContainer;
import com.raytheon.uf.common.pointdata.PointDataView;
import com.raytheon.uf.common.time.DataTime;
import com.raytheon.uf.viz.core.IGraphicsTarget;
import com.raytheon.uf.viz.core.drawables.IWireframeShape;
import com.raytheon.uf.viz.core.drawables.PaintProperties;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.rsc.LoadProperties;
import com.raytheon.uf.viz.core.rsc.capabilities.ColorableCapability;
import com.raytheon.viz.core.rsc.jts.JTSCompiler;
import com.raytheon.viz.pointdata.PointDataRequest;
import com.vividsolutions.jts.geom.Coordinate;
import com.vividsolutions.jts.geom.Geometry;
import com.vividsolutions.jts.geom.GeometryFactory;

/**
 * A resource responsible for drawing the polygon component of ncwf data
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Sep 16, 2009            bsteffen     Initial creation
 * 
 * </pre>
 * 
 * @author bsteffen
 * @version 1.0
 */
public class NcwfPolygonResource extends
        AbstractNcwfResource<NcwfPolygonResourceData> {

    // Default Paramerers for a forecast
    private static final String latParam = "alat";

    private static final String lonParam = "alon";

    private static final String lenParam = "num_of_vertices";

    private static final String[] parameters = { latParam, lonParam, lenParam };

    private class NcwfPolygonFrame {

        // Records which still needs to be parsed
        Collection<BUFRncwf> recordsToParse = new ArrayList<BUFRncwf>();

        // the polygons
        Collection<Coordinate[]> outlines = new ArrayList<Coordinate[]>();

        // A shape compiled from outlines, needs to be rebuilt if new data is
        // added
        IWireframeShape shape;

        public NcwfPolygonFrame() {
        }

    }

    private Map<DataTime, NcwfPolygonFrame> frames = new HashMap<DataTime, NcwfPolygonFrame>();

    protected NcwfPolygonResource(NcwfPolygonResourceData resourceData,
            LoadProperties loadProperties) {
        super(resourceData, loadProperties);
        this.resourceName = resourceData.getName();
    }

    @Override
    protected void disposeInternal() {
        disposeShapes();
    }

    protected void disposeShapes() {
        for (NcwfPolygonFrame frame : frames.values()) {
            if (frame.shape != null) {
                frame.shape.dispose();
                frame.shape = null;
            }
        }
    }

    @Override
    protected void initInternal(IGraphicsTarget target) throws VizException {
    }

    @Override
    protected void paintInternal(IGraphicsTarget target,
            PaintProperties paintProps) throws VizException {

        DataTime displayedDataTime = paintProps.getDataTime();

        NcwfPolygonFrame activeFrame = frames.get(displayedDataTime);

        if (displayedDataTime == null || activeFrame == null) {
            return;
        }

        IWireframeShape shape = activeFrame.shape;

        // If anything gets updated then blow away the shape in cache
        if (updateRecords(activeFrame) && shape != null) {
            shape.dispose();
            activeFrame.shape = null;
            shape = null;
        }

        if (shape == null) {
            shape = target.createWireframeShape(false, descriptor);
            JTSCompiler compiler = new JTSCompiler(null, shape, descriptor);
            Collection<Coordinate[]> curOutlines = activeFrame.outlines;

            if (curOutlines == null) {
                return;
            }
            GeometryFactory factory = new GeometryFactory();
            for (Coordinate[] boundary : curOutlines) {
                compiler.handle((Geometry) factory.createLinearRing(boundary)
                        .clone());
            }
            shape.compile();
            activeFrame.shape = shape;
        }
        target.drawWireframeShape(shape,
                getCapability(ColorableCapability.class).getColor(), 1.5f);
    }

    /**
     * 
     * @return true if anything was changed
     */
    private boolean updateRecords(NcwfPolygonFrame activeFrame)
            throws VizException {
        Collection<BUFRncwf> records = activeFrame.recordsToParse;
        // Return if there is nothing to do
        if (records == null || records.size() == 0) {
            return false;
        }

        RequestConstraint constraint = new RequestConstraint();

        constraint.setConstraintType(ConstraintType.IN);

        DataTime dataTime = null;

        for (BUFRncwf r : records) {
            constraint.addToConstraintValueList(r.getDataURI());
            dataTime = r.getDataTime();
        }
        Map<String, RequestConstraint> constraints = new HashMap<String, RequestConstraint>();
        constraints.put("dataURI", constraint);
        PointDataContainer pdc = PointDataRequest.requestPointDataAllLevels(
                dataTime, resourceData.getMetadataMap().get("pluginName")
                        .getConstraintValue(), parameters, null, constraints);

        for (int uriCounter = 0; uriCounter < pdc.getAllocatedSz(); uriCounter++) {
            PointDataView pdv = pdc.readRandom(uriCounter);
            int num_of_vertices = pdv.getInt(lenParam);
            Number[] lat = pdv.getNumberAllLevels(latParam);
            Number[] lon = pdv.getNumberAllLevels(lonParam);
            // load the coordinates into an array
            Coordinate[] boundary = new Coordinate[num_of_vertices];
            for (int i = 0; i < num_of_vertices; i++) {
                boundary[i] = new Coordinate(lon[i].doubleValue(),
                        lat[i].doubleValue());
            }
            // Add the outline under the correct datatime
            activeFrame.outlines.add(boundary);

            // clear the parse list
            activeFrame.recordsToParse.clear();
        }

        return true;

    }

    protected void addRecord(BUFRncwf obj) {

        DataTime time = calculateBinTime(obj.getDataTime());

        // If there is no frame for this time then make one.
        if (!frames.containsKey(time)) {
            frames.put(time, new NcwfPolygonFrame());
            this.dataTimes.add(time);
            Collections.sort(this.dataTimes);
        }
        // Add this record to the frame
        this.frames.get(time).recordsToParse.add(obj);
    }

    /**
     * For a given DataTime, calculate which bin to place it in
     * 
     * @param time
     *            the original dataTime
     * @return the binned DataT return new DataTime(new Date(adjustedTime)); ime
     */
    private DataTime calculateBinTime(DataTime time) {
        return this.resourceData.getBinOffset().getNormalizedTime(time);
    }

    @Override
    public void project(CoordinateReferenceSystem crs) throws VizException {
        disposeShapes();
    }
}
