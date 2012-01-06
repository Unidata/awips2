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
package com.raytheon.uf.viz.core.maps.rsc;

import java.awt.geom.Rectangle2D;
import java.math.BigDecimal;
import java.util.ArrayList;
import java.util.List;
import java.util.concurrent.ArrayBlockingQueue;

import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.core.runtime.jobs.Job;
import org.eclipse.swt.graphics.RGB;
import org.geotools.geometry.jts.ReferencedEnvelope;

import com.raytheon.uf.common.geospatial.MapUtil;
import com.raytheon.uf.common.geospatial.ReferencedCoordinate;
import com.raytheon.uf.common.pointdata.vadriver.VA_Advanced;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.viz.core.DrawableString;
import com.raytheon.uf.viz.core.IGraphicsTarget;
import com.raytheon.uf.viz.core.IGraphicsTarget.HorizontalAlignment;
import com.raytheon.uf.viz.core.IGraphicsTarget.PointStyle;
import com.raytheon.uf.viz.core.IGraphicsTarget.TextStyle;
import com.raytheon.uf.viz.core.IGraphicsTarget.VerticalAlignment;
import com.raytheon.uf.viz.core.PixelExtent;
import com.raytheon.uf.viz.core.catalog.DirectDbQuery.QueryLanguage;
import com.raytheon.uf.viz.core.drawables.PaintProperties;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.map.MapDescriptor;
import com.raytheon.uf.viz.core.maps.rsc.AbstractDbMapResourceData.ColumnDefinition;
import com.raytheon.uf.viz.core.rsc.LoadProperties;
import com.raytheon.uf.viz.core.rsc.capabilities.ColorableCapability;
import com.raytheon.uf.viz.core.rsc.capabilities.DensityCapability;
import com.raytheon.uf.viz.core.rsc.capabilities.LabelableCapability;
import com.raytheon.uf.viz.core.rsc.capabilities.MagnificationCapability;
import com.raytheon.uf.viz.core.rsc.capabilities.PointCapability;
import com.vividsolutions.jts.geom.Coordinate;
import com.vividsolutions.jts.geom.Envelope;
import com.vividsolutions.jts.geom.Geometry;
import com.vividsolutions.jts.geom.Point;
import com.vividsolutions.jts.io.WKBReader;

/**
 * Databased map resource for point data
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Mar 19, 2009            randerso     Initial creation
 * 
 * </pre>
 * 
 * @author randerso
 * @version 1.0
 */
public class DbPointMapResource extends
        AbstractDbMapResource<DbPointMapResourceData, MapDescriptor> {
    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(DbPointMapResource.class);

    private class LabelNode {
        private final String label;

        private final ReferencedCoordinate location;

        private Coordinate screenLoc = null;

        private double distance;

        private int goodness;

        LabelNode(String label, Point c) {
            this.label = label;
            this.location = new ReferencedCoordinate(c.getCoordinate());
            try {
                screenLoc = location.asPixel(descriptor.getGridGeometry());
            } catch (Exception e) {
                statusHandler.handle(
                        Priority.PROBLEM,
                        "Error converting lat/lon to screen space: "
                                + e.getLocalizedMessage(), e);
            }
        }

        /**
         * @return the location
         */
        public ReferencedCoordinate getLocation() {
            return location;
        }

        /**
         * @return the distance
         */
        public double getDistance() {
            return distance;
        }

        /**
         * @return the screen projected coordinate
         */
        public Coordinate getScreenLocation() {
            return screenLoc;
        }

        /**
         * @param distance
         *            the distance to set
         */
        public void setDistance(double distance) {
            this.distance = distance;
        }

        /**
         * @return the goodness
         */
        public int getGoodness() {
            return goodness;
        }

        /**
         * @param goodness
         *            the goodness to set
         */
        public void setGoodness(int goodness) {
            this.goodness = goodness;
        }
    }

    private class MapQueryJob extends Job {

        private static final int QUEUE_LIMIT = 1;

        private class Request {
            DbPointMapResource rsc;

            boolean labeled;

            boolean useGoodness;

            String query;

            Request(DbPointMapResource rsc, String query, boolean labeled,
                    boolean useGoodness) {
                this.rsc = rsc;
                this.query = query;
                this.labeled = labeled;
                this.useGoodness = useGoodness;
            }

        }

        public class Result {

            public List<LabelNode> labels;

            public boolean failed;

            public Throwable cause;

            private Result() {
                this.failed = true;
            }
        }

        private ArrayBlockingQueue<Request> requestQueue = new ArrayBlockingQueue<Request>(
                QUEUE_LIMIT);

        private ArrayBlockingQueue<Result> resultQueue = new ArrayBlockingQueue<Result>(
                QUEUE_LIMIT);

        private boolean canceled;

        public MapQueryJob() {
            super("Retrieving map...");
        }

        public void request(IGraphicsTarget target, DbPointMapResource rsc,
                String query, boolean labeled, boolean useGoodness) {
            if (requestQueue.size() == QUEUE_LIMIT) {
                requestQueue.poll();
            }
            requestQueue.add(new Request(rsc, query, labeled, useGoodness));

            this.cancel();
            this.schedule();
        }

        public Result getLatestResult() {
            return resultQueue.poll();
        }

        /*
         * (non-Javadoc)
         * 
         * @seeorg.eclipse.core.runtime.jobs.Job#run(org.eclipse.core.runtime.
         * IProgressMonitor)
         */
        @Override
        protected IStatus run(IProgressMonitor monitor) {
            Request req = requestQueue.poll();
            while (req != null) {
                Result result = new Result();
                try {
                    System.out.println(req.query);
                    long t0 = System.currentTimeMillis();
                    List<Object[]> results = MapQueryCache.executeQuery(
                            req.query, "maps", QueryLanguage.SQL);

                    long t1 = System.currentTimeMillis();
                    System.out.println("Maps DB query took: " + (t1 - t0)
                            + "ms");

                    List<LabelNode> newLabels = new ArrayList<LabelNode>();

                    WKBReader wkbReader = new WKBReader();
                    for (Object[] r : results) {
                        if (canceled) {
                            canceled = false;
                            result = null;
                            // System.out.println("MapQueryJob Canceled.");
                            return Status.CANCEL_STATUS;
                        }
                        int i = 0;
                        Geometry g = null;
                        if (r[i] instanceof byte[]) {
                            byte[] wkb = (byte[]) r[i++];
                            g = wkbReader.read(wkb);
                        } else {
                            statusHandler.handle(Priority.ERROR,
                                    "Expected byte[] received "
                                            + r[i].getClass().getName() + ": "
                                            + r[i].toString() + "\n  query=\""
                                            + req.query + "\"");
                        }

                        if (g != null) {
                            String label = "";
                            if (req.labeled && r[i] != null) {
                                if (r[i] instanceof BigDecimal) {
                                    label = Double.toString(((Number) r[i++])
                                            .doubleValue());
                                } else {
                                    label = r[i++].toString();
                                }
                            }
                            LabelNode node = new LabelNode(label,
                                    g.getCentroid());

                            if (req.useGoodness) {
                                node.setGoodness(((Number) r[i++]).intValue());
                            }
                            newLabels.add(node);
                        }
                    }
                    long t2 = System.currentTimeMillis();
                    System.out.println("Creating labels took: " + (t2 - t1)
                            + "ms");

                    VA_Advanced distanceCalc = new VA_Advanced();
                    distanceCalc.setVaWeighting(0.0f);
                    Coordinate[] coords = new Coordinate[newLabels.size()];
                    Integer[] goodness = new Integer[newLabels.size()];
                    Double[] dst = new Double[newLabels.size()];
                    for (int j = 0; j < newLabels.size(); j++) {
                        coords[j] = newLabels.get(j).getLocation().asLatLon();
                        goodness[j] = newLabels.get(j).getGoodness();
                        dst[j] = 0d;
                    }
                    Double[] distances;

                    if (req.useGoodness) {
                        distances = distanceCalc.getVaAdvanced(coords,
                                goodness, dst);
                    } else {
                        distances = distanceCalc.getVaSimple(coords, dst);
                    }

                    for (int j = 0; j < newLabels.size(); j++) {
                        newLabels.get(j).setDistance(distances[j]);
                    }
                    long t3 = System.currentTimeMillis();
                    System.out
                            .println("Computing progressive disclosure took: "
                                    + (t3 - t1) + "ms");
                    System.out.println("Total map retrieval took: " + (t3 - t0)
                            + "ms");

                    result.labels = newLabels;
                    result.failed = false;

                } catch (Throwable e) {
                    result.cause = e;
                } finally {
                    if (result != null) {
                        if (resultQueue.size() == QUEUE_LIMIT) {
                            resultQueue.poll();
                        }
                        resultQueue.add(result);
                        req.rsc.issueRefresh();
                    }
                }

                req = requestQueue.poll();
            }

            return Status.OK_STATUS;
        }

        /*
         * (non-Javadoc)
         * 
         * @see org.eclipse.core.runtime.jobs.Job#canceling()
         */
        @Override
        protected void canceling() {
            super.canceling();

            this.canceled = true;
        }
    }

    private static final int PIXEL_SIZE_HINT = 45;

    private List<LabelNode> labels;

    private String lastLabelField;

    private MapQueryJob queryJob;

    public DbPointMapResource(DbPointMapResourceData data,
            LoadProperties loadProperties) {
        super(data, loadProperties);
        queryJob = new MapQueryJob();
    }

    private String buildQuery(IGraphicsTarget target, PixelExtent extent)
            throws VizException {

        Envelope env = null;
        try {
            Envelope e = descriptor.pixelToWorld(extent, descriptor.getCRS());
            ReferencedEnvelope ref = new ReferencedEnvelope(e,
                    descriptor.getCRS());
            env = ref.transform(MapUtil.LATLON_PROJECTION, true);
        } catch (Exception e) {
            throw new VizException("Error transforming extent", e);
        }
        // System.out.println(env);

        String geometryField = resourceData.getGeomField();

        // create the geospatial constraint from the envelope
        String geoConstraint = String.format(
                "%s && ST_SetSrid('BOX3D(%f %f, %f %f)'::box3d,4326)",
                geometryField, env.getMinX(), env.getMinY(), env.getMaxX(),
                env.getMaxY());

        // get the geometry field
        StringBuilder query = new StringBuilder("SELECT AsBinary(");
        query.append(geometryField);
        query.append(")");

        // add the label field
        String labelField = getCapability(LabelableCapability.class)
                .getLabelField();
        if (labelField != null) {
            query.append(", ");
            query.append(labelField);
        }

        // add the goodness field
        if (resourceData.getGoodnessField() != null) {
            query.append(", ");
            query.append(resourceData.getGoodnessField());
        }

        // add any additional columns
        if (resourceData.getColumns() != null) {
            for (ColumnDefinition column : resourceData.getColumns()) {
                query.append(", ");
                query.append(column);
            }
        }

        // add the geometry table
        query.append(" FROM ");
        query.append(resourceData.getTable());

        // add the geo constraint
        query.append(" WHERE ");
        query.append(geoConstraint);

        // add any addtional constraints
        if (resourceData.getConstraints() != null) {
            for (String constraint : resourceData.getConstraints()) {
                query.append(" AND ");
                query.append(constraint);
            }
        }

        query.append(';');

        return query.toString();
    }

    @Override
    protected void paintInternal(IGraphicsTarget aTarget,
            PaintProperties paintProps) throws VizException {
        PixelExtent screenExtent = (PixelExtent) paintProps.getView()
                .getExtent();

        int displayWidth = (int) (descriptor.getMapWidth() * paintProps
                .getZoomLevel());
        double kmPerPixel = (displayWidth / paintProps.getCanvasBounds().width) / 1000.0;

        double magnification = getCapability(MagnificationCapability.class)
                .getMagnification();
        double density = getCapability(DensityCapability.class).getDensity();

        double displayHintSize = PIXEL_SIZE_HINT * magnification;
        double threshold = (displayHintSize * kmPerPixel) / density;

        String labelField = getCapability(LabelableCapability.class)
                .getLabelField();
        boolean isLabeled = labelField != null;
        if ((isLabeled && !labelField.equals(lastLabelField))
                || lastExtent == null
                || !lastExtent.getEnvelope().contains(
                        clipToProjExtent(screenExtent).getEnvelope())) {
            if (!paintProps.isZooming()) {
                PixelExtent expandedExtent = getExpandedExtent(screenExtent);
                String query = buildQuery(aTarget, expandedExtent);
                queryJob.request(aTarget, this, query, isLabeled,
                        resourceData.getGoodnessField() != null);
                lastExtent = expandedExtent;
                lastLabelField = labelField;
            }
        }

        MapQueryJob.Result result = queryJob.getLatestResult();
        if (result != null) {
            if (result.failed) {
                lastExtent = null; // force to re-query when re-enabled
                throw new VizException("Error processing map query request",
                        result.cause);
            }
            labels = result.labels;
        }

        if (labels == null) {
            issueRefresh();
        } else {
            if (font == null) {
                font = aTarget.initializeFont(aTarget.getDefaultFont()
                        .getFontName(), (float) (10 * magnification), null);
                font.setSmoothing(false);
            }

            Rectangle2D charSize = aTarget.getStringBounds(font, "N");
            double charWidth = charSize.getWidth();
            double charHeight = charSize.getHeight();

            double screenToWorldRatio = paintProps.getCanvasBounds().width
                    / paintProps.getView().getExtent().getWidth();

            HorizontalAlignment horizAlign = HorizontalAlignment.LEFT;
            double offsetX = charWidth / 2.0 / screenToWorldRatio;
            double offsetY = charHeight / screenToWorldRatio;

            PointStyle pointStyle = getCapability(PointCapability.class)
                    .getPointStyle();
            if (pointStyle.equals(PointStyle.NONE)) {
                horizAlign = HorizontalAlignment.CENTER;
                offsetX = 0;
                offsetY = 0;
            }
            offsetX += getCapability(LabelableCapability.class).getxOffset()
                    / screenToWorldRatio;
            offsetY -= getCapability(LabelableCapability.class).getyOffset()
                    / screenToWorldRatio;
            List<double[]> points = new ArrayList<double[]>();
            List<DrawableString> strings = new ArrayList<DrawableString>();
            RGB color = getCapability(ColorableCapability.class).getColor();
            for (LabelNode node : labels) {
                try {
                    if (node.getDistance() > threshold) {
                        Coordinate c = node.screenLoc;
                        if (c != null && screenExtent.contains(c.x, c.y)) {
                            points.add(new double[] { c.x, c.y, 0.0 });
                            if (isLabeled && magnification != 0) {
                                DrawableString str = new DrawableString(
                                        node.label, color);
                                str.setCoordinates(c.x + offsetX, c.y + offsetY);
                                str.textStyle = TextStyle.WORD_WRAP;
                                str.horizontalAlignment = horizAlign;
                                str.verticallAlignment = VerticalAlignment.MIDDLE;
                                str.font = font;
                                strings.add(str);
                            }
                        }
                    }
                } catch (Exception e) {
                    throw new VizException("Error transforming", e);
                }
            }

            aTarget.drawPoints(points, color, pointStyle, 1.0f);
            aTarget.drawStrings(strings);
        }
    }
}
