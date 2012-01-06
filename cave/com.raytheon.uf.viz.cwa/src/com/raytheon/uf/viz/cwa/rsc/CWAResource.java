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
package com.raytheon.uf.viz.cwa.rsc;

import java.util.ArrayList;
import java.util.Comparator;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.opengis.referencing.crs.CoordinateReferenceSystem;

import com.raytheon.uf.common.dataplugin.PluginDataObject;
import com.raytheon.uf.common.dataplugin.cwa.CWARecord;
import com.raytheon.uf.common.dataquery.requests.RequestConstraint;
import com.raytheon.uf.common.dataquery.requests.RequestConstraint.ConstraintType;
import com.raytheon.uf.common.geospatial.ReferencedCoordinate;
import com.raytheon.uf.common.pointdata.PointDataContainer;
import com.raytheon.uf.common.pointdata.PointDataView;
import com.raytheon.uf.common.time.DataTime;
import com.raytheon.uf.viz.core.DrawableString;
import com.raytheon.uf.viz.core.IGraphicsTarget;
import com.raytheon.uf.viz.core.drawables.IFont;
import com.raytheon.uf.viz.core.drawables.IFont.Style;
import com.raytheon.uf.viz.core.drawables.IRenderable;
import com.raytheon.uf.viz.core.drawables.IWireframeShape;
import com.raytheon.uf.viz.core.drawables.PaintProperties;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.map.MapDescriptor;
import com.raytheon.uf.viz.core.rsc.AbstractVizResource;
import com.raytheon.uf.viz.core.rsc.IResourceDataChanged;
import com.raytheon.uf.viz.core.rsc.LoadProperties;
import com.raytheon.uf.viz.core.rsc.capabilities.ColorableCapability;
import com.raytheon.uf.viz.core.rsc.capabilities.MagnificationCapability;
import com.raytheon.uf.viz.core.rsc.capabilities.OutlineCapability;
import com.raytheon.viz.pointdata.PointDataRequest;
import com.vividsolutions.jts.geom.Coordinate;
import com.vividsolutions.jts.geom.GeometryFactory;
import com.vividsolutions.jts.geom.LinearRing;
import com.vividsolutions.jts.geom.Point;
import com.vividsolutions.jts.geom.Polygon;

/**
 * Resource for Center Weather Advisory
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Feb 4, 2010             jsanchez     Initial creation
 * Jun 10,2011  9744       cjeanbap     Added Magnification, Outline, and Density
 *                                      compabilities.
 * </pre>
 * 
 * @author jsanchez
 * @version 1.0
 */
public class CWAResource extends
        AbstractVizResource<CWAResourceData, MapDescriptor> implements
        IResourceDataChanged {

    protected DataTime displayedDataTime;

    private Map<DataTime, CWAFrame> frameMap;

    private static final String LATS = "latitudes";

    private static final String LONS = "longitudes";

    private static final String EVENT_ID = "eventId";

    private static final String DIMENSION = "dimension";

    private static final String TEXT = "text";

    private static final String NUM_OF_POINTS = "numOfPoints";

    private static final String CWA_NAME = "Conus Center Weather Advisory";

    private IFont font;

    private class CWAFrame implements IRenderable {
        private DataTime time;

        private PointDataContainer pdc;

        private IWireframeShape wfs;

        private List<DrawableString> strings;

        private List<CWARecord> recordsToParse;

        private CWAFrame(DataTime time) {
            this.time = time;
            strings = new ArrayList<DrawableString>();
            recordsToParse = new ArrayList<CWARecord>();
        }

        /*
         * (non-Javadoc)
         * 
         * @see
         * com.raytheon.uf.viz.core.drawables.IRenderable#paint(com.raytheon
         * .uf.viz.core.IGraphicsTarget,
         * com.raytheon.uf.viz.core.drawables.PaintProperties)
         */
        @Override
        public void paint(IGraphicsTarget target, PaintProperties paintProps)
                throws VizException {
            synchronized (recordsToParse) {
                if (wfs == null) {
                    updateFrame(target, paintProps);
                }
            }

            float magnification = getCapability(MagnificationCapability.class)
                    .getMagnification().floatValue();
            font.setMagnification(magnification);

            target.drawWireframeShape(wfs,
                    getCapability(ColorableCapability.class).getColor(),
                    getCapability(OutlineCapability.class).getOutlineWidth(),
                    getCapability(OutlineCapability.class).getLineStyle());

            for (DrawableString string : strings) {
                string.setText(string.getText(),
                        getCapability(ColorableCapability.class).getColor());
            }
            target.drawStrings(strings);
        }

        public void addRecord(CWARecord record) {
            synchronized (recordsToParse) {
                recordsToParse.add(record);
            }
        }

        /**
         * @param target
         * @param paintProps
         */
        private void updateFrame(IGraphicsTarget target,
                PaintProperties paintProps) throws VizException {
            RequestConstraint constraint = new RequestConstraint();
            Map<String, RequestConstraint> constraints = new HashMap<String, RequestConstraint>();
            PointDataContainer pdc;

            for (CWARecord record : recordsToParse) {
                constraint.setConstraintType(ConstraintType.IN);
                constraint.addToConstraintValueList(record.getDataURI());
            }
            constraints.put("dataURI", constraint);
            // Request the point data
            pdc = PointDataRequest.requestPointDataAllLevels(time, resourceData
                    .getMetadataMap().get("pluginName").getConstraintValue(),
                    getParameters(), null, constraints);

            if (this.pdc == null) {
                this.pdc = pdc;
            } else {
                this.pdc.combine(pdc);
                this.pdc.setCurrentSz(this.pdc.getAllocatedSz());
            }

            if (wfs != null) {
                wfs.dispose();
            }
            strings.clear();

            wfs = target.createWireframeShape(false, descriptor);
            for (int uriCounter = 0; uriCounter < pdc.getAllocatedSz(); uriCounter++) {
                PointDataView pdv = pdc.readRandom(uriCounter);

                Coordinate rightMost = new Coordinate(-9999, 0);
                int numOfPoints = pdv.getNumber(NUM_OF_POINTS).intValue();
                Number[] latitudes = pdv.getNumberAllLevels(LATS);
                Number[] longitudes = pdv.getNumberAllLevels(LONS);
                Coordinate[] coordinates = new Coordinate[numOfPoints];
                for (int i = 0; i < numOfPoints; i++) {
                    if (longitudes[i].floatValue() > rightMost.x) {
                        rightMost = new Coordinate(longitudes[i].floatValue(),
                                latitudes[i].floatValue());
                    }
                    coordinates[i] = new Coordinate(longitudes[i].floatValue(),
                            latitudes[i].floatValue());
                }

                if (!coordinates[numOfPoints - 1].equals(coordinates[0])) {
                    Coordinate[] temp = new Coordinate[numOfPoints + 1];
                    for (int j = 0; j < numOfPoints; j++) {
                        temp[j] = coordinates[j];
                    }
                    temp[numOfPoints] = temp[0];
                    coordinates = temp;
                }

                wfs.addLineSegment(coordinates);

                if (numOfPoints > 0) {
                    String eventId = pdv.getString(EVENT_ID);
                    DrawableString string = new DrawableString(eventId,
                            getCapability(ColorableCapability.class).getColor());
                    string.font = font;
                    double[] loc = descriptor.worldToPixel(new double[] {
                            rightMost.x, rightMost.y });
                    string.setCoordinates(loc[0], loc[1]);
                    strings.add(string);
                }
            }
            wfs.compile();
        }

        public void dispose() {
            if (wfs != null) {
                wfs.dispose();
            }
        }
    }

    protected CWAResource(CWAResourceData resourceData,
            LoadProperties loadProperties) {
        super(resourceData, loadProperties);
        frameMap = new HashMap<DataTime, CWAResource.CWAFrame>();
        this.dataTimes = new ArrayList<DataTime>();
    }

    @Override
    public String getName() {
        return CWA_NAME;
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
        displayedDataTime = paintProps.getDataTime();
        if (displayedDataTime == null) {
            return;
        }

        synchronized (frameMap) {
            CWAFrame frame = frameMap.get(displayedDataTime);
            if (frame != null) {
                frame.paint(target, paintProps);
            }
        }
    }

    @Override
    protected void disposeInternal() {
        if (font != null) {
            font.dispose();
            font = null;
        }

        synchronized (frameMap) {
            for (CWAFrame frame : frameMap.values()) {
                frame.dispose();
            }
            frameMap.clear();
        }
    }

    @Override
    protected void initInternal(IGraphicsTarget target) throws VizException {
        this.font = target.initializeFont("Monospace", 11,
                new Style[] { Style.ITALIC });
    }

    @Override
    public String inspect(ReferencedCoordinate coord) throws VizException {
        CWAFrame frame = frameMap.get(displayedDataTime);
        if (frame == null || frame.pdc == null) {
            return "NO DATA";
        }

        for (int uriCounter = 0; uriCounter < frame.pdc.getAllocatedSz(); uriCounter++) {
            PointDataView pdv = frame.pdc.readRandom(uriCounter);

            Coordinate rightMost = new Coordinate(-9999, 0);
            int numOfPoints = pdv.getNumber(NUM_OF_POINTS).intValue();
            Number[] latitudes = pdv.getNumberAllLevels(LATS);
            Number[] longitudes = pdv.getNumberAllLevels(LONS);
            Coordinate[] coordinates = new Coordinate[numOfPoints];
            for (int i = 0; i < numOfPoints; i++) {
                if (longitudes[i].floatValue() > rightMost.x) {
                    rightMost = new Coordinate(longitudes[i].floatValue(),
                            latitudes[i].floatValue());
                }
                coordinates[i] = new Coordinate(longitudes[i].floatValue(),
                        latitudes[i].floatValue());
            }

            if (!coordinates[numOfPoints - 1].equals(coordinates[0])) {
                Coordinate[] temp = new Coordinate[numOfPoints + 1];
                for (int j = 0; j < numOfPoints; j++) {
                    temp[j] = coordinates[j];
                }
                temp[numOfPoints] = temp[0];
                coordinates = temp;
            }

            // GeometryFactory factory = new GeometryFactory();
            // LinearRing ring = factory.createLinearRing(coordinates);
            // Polygon llPolygon = factory.createPolygon(ring, null);
            // Coordinate[] llCoords = llPolygon.getCoordinates();
            Coordinate[] pixelCoords = new Coordinate[coordinates.length];
            for (int i = 0; i < coordinates.length; i++) {
                double[] pixelCoord = descriptor.worldToPixel(new double[] {
                        coordinates[i].x, coordinates[i].y });
                pixelCoords[i] = new Coordinate(pixelCoord[0], pixelCoord[1]);
            }
            GeometryFactory factory2 = new GeometryFactory();
            LinearRing ring2 = factory2.createLinearRing(pixelCoords);

            Polygon pixelPoly = factory2.createPolygon(ring2, null);
            Point point;
            try {
                point = pixelPoly.getFactory().createPoint(
                        coord.asPixel(descriptor.getGridGeometry()));
            } catch (Exception e) {
                throw new VizException(
                        "Error inspecting Center Weather Advisory", e);
            }

            if (pixelPoly.contains(point)) {
                String returnString = pdv.getString(TEXT).split("=")[0];
                return returnString;
            }
        }
        return "NO DATA";
    }

    @Override
    public void remove(DataTime dataTime) {
        super.remove(dataTime);
        synchronized (frameMap) {
            final CWAFrame frame = frameMap.remove(dataTime);
            if (frame != null) {
                frame.dispose();
            }
        }
    }

    /**
     * Adds a new record to this resource
     * 
     * @param obj
     */
    protected void addRecord(CWARecord obj) {
        synchronized (frameMap) {
            DataTime dataTime = obj.getDataTime();
            CWAFrame frame = frameMap.get(dataTime);
            if (frame == null) {
                frame = new CWAFrame(dataTime);
                frameMap.put(dataTime, frame);
            }
            frame.addRecord(obj);
        }
    }

    private String[] getParameters() {
        return new String[] { LATS, LONS, EVENT_ID, DIMENSION, TEXT,
                NUM_OF_POINTS };
    }

    public class CoordinateComparator implements Comparator<Coordinate> {

        @Override
        public int compare(Coordinate o1, Coordinate o2) {
            int result = Double.compare(o1.x, o2.x);
            if (result == 0) {
                result = Double.compare(o1.y, o2.y);
            }
            return result;
        }
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
        if (type == ChangeType.DATA_UPDATE) {
            PluginDataObject[] pdo = (PluginDataObject[]) object;
            for (PluginDataObject p : pdo) {
                if (p instanceof CWARecord) {
                    addRecord((CWARecord) p);
                }
            }
        }
        issueRefresh();
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.viz.core.rsc.AbstractVizResource#project(org.opengis.
     * referencing.crs.CoordinateReferenceSystem)
     */
    @Override
    public void project(CoordinateReferenceSystem crs) throws VizException {
        frameMap.get(displayedDataTime).wfs.dispose();
        frameMap.get(displayedDataTime).wfs = null;
    }

}
