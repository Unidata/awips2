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
import java.util.Arrays;
import java.util.List;
import java.util.concurrent.ArrayBlockingQueue;

import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.core.runtime.jobs.Job;
import org.eclipse.swt.graphics.RGB;
import org.geotools.geometry.jts.ReferencedEnvelope;

import com.raytheon.uf.common.dataquery.db.QueryResult;
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

            String labelField;

            String goodnessField;

            Envelope env;

            public Request(DbPointMapResource rsc, String labelField,
                    String goodnessField, Envelope env) {
                super();
                this.rsc = rsc;
                this.labelField = labelField;
                this.goodnessField = goodnessField;
                this.env = env;
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
                Envelope envelope, String labelField, String goodnessField) {
            if (requestQueue.size() == QUEUE_LIMIT) {
                requestQueue.poll();
            }
            requestQueue.add(new Request(rsc, labelField, goodnessField,
                    envelope));

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
                    long t0 = System.currentTimeMillis();
                    List<String> columns = new ArrayList<String>();
                    if (req.labelField != null) {
                        columns.add(req.labelField);
                    }
                    if (req.goodnessField != null
                            && req.goodnessField != req.labelField) {
                        columns.add(req.goodnessField);
                    }
                    if (resourceData.getColumns() != null) {
                        for (ColumnDefinition column : resourceData
                                .getColumns()) {
                            if (columns.contains(column.getName())) {
                                columns.remove(column.getName());
                            }
                            columns.add(column.toString());
                        }
                    }
                    columns.add("AsBinary(" + resourceData.getGeomField()
                            + ") as " + resourceData.getGeomField());

                    List<String> constraints = null;
                    if (resourceData.getConstraints() != null) {
                        constraints = Arrays.asList(resourceData
                                .getConstraints());
                    }

                    QueryResult results = DbMapQueryFactory.getMapQuery(
                            resourceData.getTable(),
                            resourceData.getGeomField()).queryWithinEnvelope(
                            req.env, columns, constraints);

                    long t1 = System.currentTimeMillis();
                    System.out.println("Maps DB query took: " + (t1 - t0)
                            + "ms");

                    List<LabelNode> newLabels = new ArrayList<LabelNode>();

                    WKBReader wkbReader = new WKBReader();
                    for (int c = 0; c < results.getResultCount(); c++) {
                        if (canceled) {
                            canceled = false;
                            result = null;
                            // System.out.println("MapQueryJob Canceled.");
                            return Status.CANCEL_STATUS;
                        }
                        Geometry g = null;
                        Object geomObj = results.getRowColumnValue(c,
                                resourceData.getGeomField());
                        if (geomObj instanceof byte[]) {
                            byte[] wkb = (byte[]) geomObj;
                            g = wkbReader.read(wkb);
                        } else {
                            statusHandler.handle(Priority.ERROR,
                                    "Expected byte[] received "
                                            + geomObj.getClass().getName()
                                            + ": " + geomObj.toString()
                                            + "\n  query=\"" + req.env + "\"");
                        }

                        if (g != null) {
                            String label = "";
                            if (req.labelField != null
                                    && results.getRowColumnValue(c,
                                            req.labelField) != null) {
                                Object r = results.getRowColumnValue(c,
                                        req.labelField);
                                if (r instanceof BigDecimal) {
                                    label = Double.toString(((Number) r)
                                            .doubleValue());
                                } else {
                                    label = r.toString();
                                }
                            }
                            LabelNode node = new LabelNode(label,
                                    g.getCentroid());

                            if (req.goodnessField != null) {
                                node.setGoodness(((Number) results
                                        .getRowColumnValue(c, req.goodnessField))
                                        .intValue());
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

                    if (req.goodnessField != null) {
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

    private void requestData(IGraphicsTarget target, PixelExtent extent)
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

        // add the label field
        String labelField = getCapability(LabelableCapability.class)
                .getLabelField();

        queryJob.request(target, this, env, labelField,
                resourceData.getGoodnessField());
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
                requestData(aTarget, expandedExtent);
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
                font.setScaleFont(false);
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
