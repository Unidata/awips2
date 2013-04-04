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
package com.raytheon.viz.gfe.ui.zoneselector;

import java.text.DecimalFormat;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Comparator;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.concurrent.ArrayBlockingQueue;

import org.apache.commons.lang.StringUtils;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.core.runtime.jobs.Job;
import org.eclipse.swt.graphics.RGB;
import org.eclipse.swt.graphics.Rectangle;
import org.geotools.geometry.jts.ReferencedEnvelope;

import com.raytheon.uf.common.dataplugin.gfe.db.objects.GridLocation;
import com.raytheon.uf.common.dataquery.db.QueryResult;
import com.raytheon.uf.common.geospatial.MapUtil;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.viz.core.DrawableString;
import com.raytheon.uf.viz.core.IExtent;
import com.raytheon.uf.viz.core.IGraphicsTarget;
import com.raytheon.uf.viz.core.IGraphicsTarget.HorizontalAlignment;
import com.raytheon.uf.viz.core.IGraphicsTarget.TextStyle;
import com.raytheon.uf.viz.core.IGraphicsTarget.VerticalAlignment;
import com.raytheon.uf.viz.core.PixelExtent;
import com.raytheon.uf.viz.core.RGBColors;
import com.raytheon.uf.viz.core.catalog.DirectDbQuery;
import com.raytheon.uf.viz.core.catalog.DirectDbQuery.QueryLanguage;
import com.raytheon.uf.viz.core.drawables.IShadedShape;
import com.raytheon.uf.viz.core.drawables.IWireframeShape;
import com.raytheon.uf.viz.core.drawables.PaintProperties;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.map.IMapDescriptor;
import com.raytheon.uf.viz.core.maps.rsc.AbstractDbMapResourceData.ColumnDefinition;
import com.raytheon.uf.viz.core.maps.rsc.DbMapResource;
import com.raytheon.uf.viz.core.maps.rsc.DbMapResourceData;
import com.raytheon.uf.viz.core.rsc.LoadProperties;
import com.raytheon.uf.viz.core.rsc.capabilities.LabelableCapability;
import com.raytheon.uf.viz.core.rsc.capabilities.OutlineCapability;
import com.raytheon.uf.viz.core.rsc.capabilities.ShadeableCapability;
import com.raytheon.viz.core.rsc.jts.JTSCompiler;
import com.raytheon.viz.core.rsc.jts.JTSCompiler.PointStyle;
import com.raytheon.viz.gfe.Activator;
import com.raytheon.viz.gfe.rsc.GFEFonts;
import com.vividsolutions.jts.geom.Coordinate;
import com.vividsolutions.jts.geom.Envelope;
import com.vividsolutions.jts.geom.Geometry;
import com.vividsolutions.jts.geom.GeometryFactory;
import com.vividsolutions.jts.geom.Point;
import com.vividsolutions.jts.geom.Polygon;
import com.vividsolutions.jts.geom.prep.PreparedGeometry;
import com.vividsolutions.jts.geom.prep.PreparedGeometryFactory;
import com.vividsolutions.jts.io.WKBReader;

/**
 * Zone Selector Resource
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Aug 11, 2011            randerso     Initial creation
 * 
 * </pre>
 * 
 * @author randerso
 * @version 1.0
 */

public class ZoneSelectorResource extends DbMapResource {
    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(ZoneSelectorResource.class);

    private static final RGB NO_ZONE_COLOR;
    static {
        String s = Activator.getDefault().getPreferenceStore()
                .getString("ZoneCombiner_noZoneColor");
        if (s.isEmpty()) {
            s = "black";
        }
        NO_ZONE_COLOR = RGBColors.getRGBColor(s);
    }

    private static GeometryFactory gf = new GeometryFactory();

    private static PreparedGeometryFactory pgf = new PreparedGeometryFactory();

    public String myWfo;

    class MapQueryJob extends Job {

        private static final int QUEUE_LIMIT = 1;

        class Request {
            IGraphicsTarget target;

            IMapDescriptor descriptor;

            ZoneSelectorResource rsc;

            String query;

            Request(IGraphicsTarget target, IMapDescriptor descriptor,
                    ZoneSelectorResource rsc, String query) {
                this.target = target;
                this.descriptor = descriptor;
                this.rsc = rsc;
                this.query = query;
            }
        }

        public class Result {
            public IWireframeShape outlineShape;

            public IWireframeShape wfoShape;

            public List<LabelNode> labels;

            public IShadedShape[] shapeList;

            public boolean failed;

            public Throwable cause;

            public String query;

            private Result(String query) {
                this.query = query;
                failed = true;
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

        public void request(IGraphicsTarget target, IMapDescriptor descriptor,
                ZoneSelectorResource rsc, String query) {
            if (requestQueue.size() == QUEUE_LIMIT) {
                requestQueue.poll();
            }
            requestQueue.add(new Request(target, descriptor, rsc, query));

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
                Result result = new Result(req.query);
                try {

                    // System.out.println(req.query);
                    // long t0 = System.currentTimeMillis();
                    QueryResult mappedResult = DirectDbQuery
                            .executeMappedQuery(req.query, "maps",
                                    QueryLanguage.SQL);

                    // long t1 = System.currentTimeMillis();
                    // System.out.println("Maps DB query took: " + (t1 - t0)
                    // + "ms");

                    List<LabelNode> newLabels = new ArrayList<LabelNode>();

                    Map<String, Geometry> resultingGeoms = new HashMap<String, Geometry>(
                            mappedResult.getResultCount());
                    List<Geometry> wfoGeoms = new ArrayList<Geometry>();

                    int numPoints = 0;
                    int wfoPoints = 0;
                    WKBReader wkbReader = new WKBReader();
                    for (int i = 0; i < mappedResult.getResultCount(); i++) {
                        if (canceled) {
                            canceled = false;
                            result = null;
                            // System.out.println("MapQueryJob Canceled.");
                            return Status.CANCEL_STATUS;
                        }
                        Geometry g = null;
                        Object obj = mappedResult.getRowColumnValue(i, 0);
                        if (obj instanceof byte[]) {
                            byte[] wkb = (byte[]) obj;
                            g = wkbReader.read(wkb);
                        } else {
                            statusHandler.handle(Priority.ERROR,
                                    "Expected byte[] received "
                                            + obj.getClass().getName() + ": "
                                            + obj.toString() + "\n  query=\""
                                            + req.query + "\"");
                        }

                        if (g != null) {
                            String zoneName = (String) mappedResult
                                    .getRowColumnValue(i, "editarea");
                            if (zoneName == null) {
                                continue;
                                // TODO: what do we do with this?
                                // zoneName = "";
                            }
                            String wfo = (String) mappedResult
                                    .getRowColumnValue(i, "wfo");

                            Geometry existingGeom = resultingGeoms
                                    .get(zoneName);
                            if (existingGeom != null) {
                                // continue;
                                numPoints -= existingGeom.getNumPoints();
                                g = mergeGeometry(g, existingGeom);
                            }
                            numPoints += g.getNumPoints();
                            resultingGeoms.put(zoneName, g);

                            if (myWfo != null && myWfo.equals(wfo)) {
                                if (existingGeom != null) {
                                    wfoPoints -= existingGeom.getNumPoints();
                                }
                                wfoPoints += g.getNumPoints();
                                wfoGeoms.add((Geometry) g.clone());
                            }

                            ZoneInfo info = req.rsc.getZoneInfo(zoneName);
                            info.setGeometry(g);
                            g.setUserData(zoneName);

                            int numGeometries = g.getNumGeometries();
                            List<Geometry> gList = new ArrayList<Geometry>(
                                    numGeometries);
                            for (int polyNum = 0; polyNum < numGeometries; polyNum++) {
                                Geometry poly = g.getGeometryN(polyNum);
                                gList.add(poly);
                            }
                            // Sort polygons in g so biggest comes first.
                            Collections.sort(gList, new Comparator<Geometry>() {
                                @Override
                                public int compare(Geometry g1, Geometry g2) {
                                    return (int) Math.signum(g2.getEnvelope()
                                            .getArea()
                                            - g1.getEnvelope().getArea());
                                }
                            });

                            for (Geometry poly : gList) {
                                Point point = poly.getInteriorPoint();
                                if (point.getCoordinate() != null) {
                                    LabelNode node = new LabelNode(zoneName,
                                            point, req.target);
                                    newLabels.add(node);
                                }
                            }
                        }
                    }

                    IWireframeShape newOutlineShape = req.target
                            .createWireframeShape(false, req.descriptor, 0.0f);
                    newOutlineShape.allocate(numPoints);

                    JTSCompiler outlineCompiler = new JTSCompiler(null,
                            newOutlineShape, req.descriptor, PointStyle.CROSS);

                    int i = 0;
                    result.shapeList = new IShadedShape[resultingGeoms.size()];
                    for (Geometry g : resultingGeoms.values()) {
                        String zoneName = (String) g.getUserData();
                        ZoneInfo info = req.rsc.getZoneInfo(zoneName);

                        try {
                            outlineCompiler.handle((Geometry) g.clone());
                        } catch (VizException e) {
                            statusHandler.handle(Priority.PROBLEM,
                                    "Error reprojecting map outline", e);
                        }

                        IShadedShape newShadedShape = computeShape(req.target,
                                req.descriptor, g, info.getColor());
                        info.setShapeIndex(i);
                        result.shapeList[i++] = newShadedShape;
                    }

                    newOutlineShape.compile();

                    result.outlineShape = newOutlineShape;
                    result.labels = newLabels;

                    if (wfoGeoms.size() > 0) {
                        IWireframeShape newWfoShape = req.target
                                .createWireframeShape(false, req.descriptor,
                                        0.0f);
                        newWfoShape.allocate(wfoPoints);

                        JTSCompiler wfoCompiler = new JTSCompiler(null,
                                newWfoShape, req.descriptor);

                        for (Geometry g : wfoGeoms) {
                            try {
                                wfoCompiler.handle(g);
                            } catch (VizException e) {
                                statusHandler.handle(Priority.PROBLEM,
                                        "Error reprojecting map outline", e);
                            }
                        }
                        newWfoShape.compile();
                        result.wfoShape = newWfoShape;
                    }

                    result.failed = false;

                    // long t2 = System.currentTimeMillis();
                    // System.out.println("Wireframe construction took: "
                    // + (t2 - t1) + "ms");
                    // System.out.println("Total map retrieval took: " + (t2 -
                    // t0)
                    // + "ms");
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

        /**
         * @param g1
         * @param g2
         * @return
         */
        private Geometry mergeGeometry(Geometry g1, Geometry g2) {
            int numPolys = g1.getNumGeometries() + g2.getNumGeometries();

            Polygon[] polys = new Polygon[numPolys];
            int i = 0;
            for (int n = 0; n < g1.getNumGeometries(); n++) {
                polys[i++] = (Polygon) g1.getGeometryN(n);
            }
            for (int n = 0; n < g2.getNumGeometries(); n++) {
                polys[i++] = (Polygon) g2.getGeometryN(n);
            }

            return gf.createMultiPolygon(polys);
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

    private static class ZoneInfo {

        private String groupLabel;

        private RGB color;

        private PreparedGeometry prepGeom;

        private int shapeIndex;

        public ZoneInfo(RGB color) {
            this.color = color;
            this.groupLabel = "";
            this.shapeIndex = -1;
        }

        /**
         * @return the groupLabel
         */
        public String getGroupLabel() {
            return groupLabel;
        }

        /**
         * @param groupLabel
         *            the groupLabel to set
         */
        public void setGroupLabel(String groupLabel) {
            this.groupLabel = groupLabel;
        }

        /**
         * @return the color
         */
        public RGB getColor() {
            return color;
        }

        /**
         * @param color
         *            the color to set
         */
        public void setColor(RGB color) {
            this.color = color;
        }

        /**
         * @return the geometry
         */
        public Geometry getGeometry() {
            Geometry g = null;
            if (prepGeom != null) {
                g = prepGeom.getGeometry();
            }
            return g;
        }

        /**
         * @param geometry
         *            the geometry to set
         */
        public void setGeometry(Geometry geometry) {
            this.prepGeom = pgf.create(geometry);
        }

        /**
         * @return the shapeIndex
         */
        public int getShapeIndex() {
            return shapeIndex;
        }

        /**
         * @param shapeIndex
         *            the shapeIndex to set
         */
        public void setShapeIndex(int shapeIndex) {
            this.shapeIndex = shapeIndex;
        }

        public boolean contains(Point p) {
            if (prepGeom != null) {
                return this.prepGeom.contains(p);
            }
            return false;
        }
    }

    private class LabelTuple {
        public double x;

        public double y;

        public String group;

        public String zone;

        public LabelTuple(double x, double y, String group, String zone) {
            this.x = x;
            this.y = y;
            this.group = group;
            this.zone = zone;
        }
    }

    private MapQueryJob queryJob;

    private Map<String, ZoneInfo> zoneData;

    private RGB defaultFillColor;

    private RGB outlineColor;

    private RGB wfoOutlineColor;

    private IWireframeShape wfoShape;

    private IShadedShape shapeList[];

    private GeometryFactory geomFactory;

    private IGraphicsTarget target;

    private boolean labelZones;

    private boolean labelZoneGroups;

    private Envelope boundingEnvelope;

    private GridLocation gloc;

    /**
     * @param data
     * @param loadProperties
     */
    public ZoneSelectorResource(DbMapResourceData data,
            LoadProperties loadProperties, GridLocation gloc) {
        super(data, loadProperties);
        this.zoneData = new HashMap<String, ZoneInfo>();
        this.geomFactory = new GeometryFactory();
        this.queryJob = new MapQueryJob();
        this.defaultFillColor = NO_ZONE_COLOR;
        this.outlineColor = RGBColors.getRGBColor("white");
        this.wfoOutlineColor = RGBColors.getRGBColor("yellow");
        this.gloc = gloc;
    }

    private ZoneInfo getZoneInfo(String zoneName) {
        ZoneInfo zoneInfo = this.zoneData.get(zoneName);
        if (zoneInfo == null) {
            zoneInfo = new ZoneInfo(this.defaultFillColor);
            this.zoneData.put(zoneName, zoneInfo);
        }
        return zoneInfo;
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.uf.viz.core.maps.rsc.DbMapResource#disposeInternal()
     */
    @Override
    protected void disposeInternal() {
        super.disposeInternal();

        if (this.wfoShape != null) {
            this.wfoShape.dispose();
        }

        if (this.shapeList != null) {
            for (IShadedShape shape : this.shapeList) {
                shape.dispose();
            }
        }
    }

    @Override
    protected void initInternal(IGraphicsTarget target) throws VizException {
        super.initInternal(target);
        getCapabilities().removeCapability(ShadeableCapability.class);
        getCapabilities().removeCapability(LabelableCapability.class);
    }

    /**
     * @param zoneName
     * @param groupLabel
     */
    public void setZoneGroupLabel(String zoneName, String groupLabel) {
        ZoneInfo info = zoneData.get(zoneName);
        if (info != null) {
            info.setGroupLabel(groupLabel);
        }
    }

    /**
     * @param zoneName
     * @param color
     */
    public void setZone(String zoneName, RGB color) {
        ZoneInfo info = zoneData.get(zoneName);
        if (info != null) {
            info.setColor(color);

            int index = info.getShapeIndex();
            if (this.target != null && index >= 0 && index < shapeList.length) {
                shapeList[index].dispose();
                shapeList[index] = computeShape(this.target, this.descriptor,
                        info.getGeometry(), color);
            }
            issueRefresh();
        }
    }

    /**
     * @param myWfo
     *            the myWfo to set
     */
    public void setMyWfo(String myWfo) {
        this.myWfo = myWfo;
    }

    /**
     * @param defaultFillColor
     *            the defaultFillColor to set
     */
    public void setDefaultFillColor(RGB defaultFillColor) {
        this.defaultFillColor = defaultFillColor;
    }

    /**
     * @param outlineColor
     *            the outlineColor to set
     */
    public void setOutlineColor(RGB outlineColor) {
        this.outlineColor = outlineColor;
    }

    /**
     * @param wfoOutlineColor
     *            the wfoOutlineColor to set
     */
    public void setWfoOutlineColor(RGB wfoOutlineColor) {
        this.wfoOutlineColor = wfoOutlineColor;
    }

    @Override
    protected void paintInternal(IGraphicsTarget aTarget,
            PaintProperties paintProps) throws VizException {
        this.target = aTarget;

        PixelExtent screenExtent = (PixelExtent) paintProps.getView()
                .getExtent();

        // compute an estimate of degrees per pixel
        double yc = screenExtent.getCenter()[1];
        double x1 = screenExtent.getMinX();
        double x2 = screenExtent.getMaxX();
        double[] c1 = descriptor.pixelToWorld(new double[] { x1, yc });
        double[] c2 = descriptor.pixelToWorld(new double[] { x2, yc });
        Rectangle canvasBounds = paintProps.getCanvasBounds();
        int screenWidth = canvasBounds.width;
        double dppX = Math.abs(c2[0] - c1[0]) / screenWidth;
        // System.out.println("c1:" + Arrays.toString(c1) + "  c2:"
        // + Arrays.toString(c2) + "  dpp:" + dppX);
        double simpLev = getSimpLev(dppX);

        if (simpLev < lastSimpLev
                || lastExtent == null
                || !lastExtent.getEnvelope().contains(
                        clipToProjExtent(screenExtent).getEnvelope())) {
            if (!paintProps.isZooming()) {
                PixelExtent clippedExtent = clipToProjExtent(screenExtent);
                String query = buildQuery(clippedExtent, simpLev);
                queryJob.request(aTarget, descriptor, this, query);
                lastExtent = clippedExtent;
                lastSimpLev = simpLev;
            }
        }

        MapQueryJob.Result result = queryJob.getLatestResult();
        if (result != null) {
            if (result.failed) {
                lastExtent = null; // force to re-query when re-enabled
                throw new VizException("Error processing map query request: "
                        + result.query, result.cause);
            }
            if (outlineShape != null) {
                outlineShape.dispose();
            }

            if (wfoShape != null) {
                wfoShape.dispose();
            }

            if (shapeList != null) {
                for (IShadedShape shape : shapeList) {
                    shape.dispose();
                }
            }
            outlineShape = result.outlineShape;
            wfoShape = result.wfoShape;
            labels = result.labels;
            shapeList = result.shapeList;
        }

        // draw the shapes
        if (shapeList != null /* && shadedShape.isDrawable() */) {
            aTarget.drawShadedShapes(paintProps.getAlpha(), 1.0f, shapeList);
        }

        if (outlineShape != null && outlineShape.isDrawable()
                && getCapability(OutlineCapability.class).isOutlineOn()) {
            aTarget.drawWireframeShape(outlineShape, this.outlineColor,
                    getCapability(OutlineCapability.class).getOutlineWidth(),
                    getCapability(OutlineCapability.class).getLineStyle());
        } else if (outlineShape == null
                && getCapability(OutlineCapability.class).isOutlineOn()) {
            issueRefresh();
        }

        if (wfoShape != null && wfoShape.isDrawable()
                && getCapability(OutlineCapability.class).isOutlineOn()) {
            aTarget.drawWireframeShape(wfoShape, this.wfoOutlineColor,
                    getCapability(OutlineCapability.class).getOutlineWidth(),
                    getCapability(OutlineCapability.class).getLineStyle());
        } else if (wfoShape == null
                && getCapability(OutlineCapability.class).isOutlineOn()) {
            issueRefresh();
        }

        if (labels != null && (this.labelZones || this.labelZoneGroups)) {
            if (font == null) {
                font = GFEFonts.getFont(aTarget, 2);
            }
            double screenToWorldRatio = paintProps.getView().getExtent()
                    .getWidth()
                    / paintProps.getCanvasBounds().width;

            IExtent extent = paintProps.getView().getExtent();
            List<DrawableString> strings = new ArrayList<DrawableString>(
                    labels.size());
            List<LabelTuple> alreadyDrawn = new ArrayList<ZoneSelectorResource.LabelTuple>(
                    labels.size());
            for (LabelNode node : labels) {
                if (extent.contains(node.getLocation())) {

                    String zone = node.getLabel();
                    String group = getZoneInfo(node.getLabel()).getGroupLabel();
                    double x = node.getLocation()[0];
                    double y = node.getLocation()[1];
                    double minDistance = 9999;
                    for (LabelTuple tuple : alreadyDrawn) {
                        if (!tuple.zone.equals(zone)
                                || !tuple.group.equals(group)) {
                            continue;
                        }
                        double distance = Math.abs(tuple.x - x)
                                + Math.abs(tuple.y - y);
                        minDistance = Math.min(distance, minDistance);
                    }
                    if (minDistance > 100 * screenToWorldRatio) {
                        String[] text = new String[] { "", "" };
                        if (this.labelZones) {
                            text[0] = zone;
                        }
                        if (this.labelZoneGroups) {
                            text[1] = group;
                        }
                        DrawableString ds = new DrawableString(text,
                                RGBColors.getRGBColor("white"));
                        ds.setCoordinates(node.getLocation()[0],
                                node.getLocation()[1]);
                        ds.font = font;
                        ds.horizontalAlignment = HorizontalAlignment.CENTER;
                        ds.verticallAlignment = VerticalAlignment.MIDDLE;
                        ds.textStyle = TextStyle.DROP_SHADOW;
                        ds.shadowColor = RGBColors.getRGBColor("black");
                        strings.add(ds);

                        alreadyDrawn.add(new LabelTuple(x, y, group, zone));
                    }
                }
            }

            aTarget.drawStrings(strings);
        }
    }

    protected String buildQuery(PixelExtent extent, double simpLev)
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

        DecimalFormat df = new DecimalFormat("0.######");
        String suffix = "_"
                + StringUtils.replaceChars(df.format(simpLev), '.', '_');

        String geometryField = resourceData.getGeomField() + suffix;

        // get the geometry field
        StringBuilder query = new StringBuilder("SELECT AsBinary(");
        query.append(geometryField);
        query.append(") as ");
        query.append(geometryField);

        // add any additional columns
        List<String> additionalColumns = new ArrayList<String>();
        if (resourceData.getColumns() != null) {
            for (ColumnDefinition column : resourceData.getColumns()) {
                query.append(", ");
                query.append(column);

                additionalColumns.add(column.getName());
            }
        }

        // add the geometry table
        query.append(" FROM ");
        query.append(resourceData.getTable());

        // add the geospatial constraint
        query.append(" WHERE ");
        query.append(getGeospatialConstraint(geometryField, env));

        // add any additional constraints
        if (resourceData.getConstraints() != null) {
            for (String constraint : resourceData.getConstraints()) {
                query.append(" AND ");
                query.append(constraint);
            }
        }

        query.append(';');

        return query.toString();
    }

    /**
     * returns the zones containing the selected coordinate
     * 
     * @param c
     *            the coordinate
     * @return the zones
     */
    public List<String> getSelectedZones(Coordinate c) {
        List<String> zones = new ArrayList<String>();

        Point p = this.geomFactory.createPoint(c);
        for (Entry<String, ZoneInfo> entry : this.zoneData.entrySet()) {
            if (entry.getValue().contains(p)) {
                zones.add(entry.getKey());
            }
        }
        return zones;
    }

    private IShadedShape computeShape(IGraphicsTarget target,
            IMapDescriptor descriptor, Geometry g, RGB color) {
        IShadedShape newShadedShape = target.createShadedShape(false,
                descriptor, true);
        JTSCompiler shapeCompiler = new JTSCompiler(newShadedShape, null,
                descriptor, PointStyle.CROSS);
        try {
            shapeCompiler.handle((Geometry) g.clone(), color);
        } catch (VizException e) {
            statusHandler.handle(Priority.PROBLEM,
                    "Error computing shaded shape", e);
        }
        newShadedShape.compile();

        return newShadedShape;
    }

    /**
     * @param labelZones
     */
    public void setLabelZones(boolean labelZones) {
        this.labelZones = labelZones;
        issueRefresh();
    }

    /**
     * @param labelZoneGroups
     */
    public void setLabelZoneGroups(boolean labelZoneGroups) {
        this.labelZoneGroups = labelZoneGroups;
        issueRefresh();
    }

    /**
     * 
     */
    public List<String> getZoneNames() {
        if (zoneData.isEmpty()) {
            try {
                StringBuilder query = new StringBuilder("SELECT ");

                // add any additional columns
                int count = 0;
                if (resourceData.getColumns() != null) {
                    for (ColumnDefinition column : resourceData.getColumns()) {
                        if (count > 0) {
                            query.append(", ");
                        }
                        query.append(column);
                        count++;
                    }
                }
                // add the geometry table
                query.append(" FROM ");
                query.append(resourceData.getTable());

                // add the geospatial constraint
                query.append(" WHERE ");
                query.append(getGeospatialConstraint(
                        resourceData.getGeomField(), null));

                // add any additional constraints
                if (resourceData.getConstraints() != null) {
                    for (String constraint : resourceData.getConstraints()) {
                        query.append(" AND ");
                        query.append(constraint);
                    }
                }

                query.append(';');

                QueryResult mappedResult = DirectDbQuery.executeMappedQuery(
                        query.toString(), "maps", QueryLanguage.SQL);

                if (mappedResult.getColumnNames().containsKey("editarea")) {
                    for (int i = 0; i < mappedResult.getResultCount(); i++) {
                        String zoneName = (String) mappedResult
                                .getRowColumnValue(i, "editarea");
                        getZoneInfo(zoneName);
                    }
                }
            } catch (VizException e) {
                statusHandler.handle(Priority.PROBLEM,
                        "Error retrieving zone names", e);
            } catch (Exception e) {
                statusHandler.handle(Priority.PROBLEM,
                        "Error computing boudning envelope", e);
            }
        }
        List<String> zoneNames = new ArrayList<String>(zoneData.keySet());
        return zoneNames;
    }

    protected String getGeospatialConstraint(String geometryField, Envelope env) {
        StringBuilder constraint = new StringBuilder();

        Geometry g1 = MapUtil.getBoundingGeometry(gloc);
        if (env != null) {
            g1 = g1.intersection(MapUtil.createGeometry(env));
        }

        constraint.append("ST_Intersects(");
        constraint.append(geometryField);
        constraint.append(", ST_SetSrid('");
        constraint.append(g1.toString());
        constraint.append("',4326))");

        return constraint.toString();
    }

    public Envelope getBoundingEnvelope() {
        if (this.boundingEnvelope == null) {
            try {
                this.boundingEnvelope = new Envelope();
                StringBuilder query = new StringBuilder("SELECT ");

                query.append("asBinary(ST_extent(");
                query.append(resourceData.getGeomField());
                query.append(")) as extent");

                // add the geometry table
                query.append(" FROM ");
                query.append(resourceData.getTable());

                // add the geospatial constraint
                query.append(" WHERE ");
                query.append(getGeospatialConstraint(
                        resourceData.getGeomField(), null));

                // add any additional constraints
                if (resourceData.getConstraints() != null) {
                    for (String constraint : resourceData.getConstraints()) {
                        query.append(" AND ");
                        query.append(constraint);
                    }
                }

                query.append(';');

                QueryResult mappedResult = DirectDbQuery.executeMappedQuery(
                        query.toString(), "maps", QueryLanguage.SQL);

                WKBReader wkbReader = new WKBReader();
                byte[] b = (byte[]) mappedResult.getRowColumnValue(0, "extent");
                if (b != null) {
                    Geometry g = wkbReader.read(b);
                    this.boundingEnvelope.expandToInclude(g
                            .getEnvelopeInternal());
                }

            } catch (VizException e) {
                statusHandler.handle(Priority.PROBLEM,
                        "Error retrieving bounding envelope", e);
            } catch (Exception e) {
                statusHandler.handle(Priority.PROBLEM,
                        "Error computing bounding envelope", e);
            }
        }
        return this.boundingEnvelope;
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.uf.viz.core.maps.rsc.DbMapResource#getLevels()
     */
    @Override
    protected double[] getLevels() {
        double[] d = super.getLevels();
        // d = new double[] { d[d.length - 1] };
        return d;
    }
}
