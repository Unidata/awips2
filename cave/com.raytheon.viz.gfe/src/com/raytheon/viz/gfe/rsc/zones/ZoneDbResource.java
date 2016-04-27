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
/**
 * 
 */
package com.raytheon.viz.gfe.rsc.zones;

import java.text.DecimalFormat;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.Collections;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Random;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import org.apache.commons.lang.StringUtils;
import org.eclipse.core.runtime.jobs.IJobChangeEvent;
import org.eclipse.core.runtime.jobs.JobChangeAdapter;
import org.eclipse.swt.graphics.RGB;
import org.geotools.coverage.grid.GeneralGridGeometry;
import org.geotools.geometry.jts.ReferencedEnvelope;
import org.opengis.referencing.FactoryException;
import org.opengis.referencing.crs.CoordinateReferenceSystem;
import org.opengis.referencing.operation.TransformException;

import com.raytheon.uf.common.dataplugin.gfe.grid.Grid2DBit;
import com.raytheon.uf.common.dataplugin.gfe.reference.ReferenceData;
import com.raytheon.uf.common.dataplugin.gfe.reference.ReferenceID;
import com.raytheon.uf.common.geospatial.MapUtil;
import com.raytheon.uf.common.geospatial.ReferencedCoordinate;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.viz.core.DrawableString;
import com.raytheon.uf.viz.core.IGraphicsTarget;
import com.raytheon.uf.viz.core.IGraphicsTarget.HorizontalAlignment;
import com.raytheon.uf.viz.core.IGraphicsTarget.LineStyle;
import com.raytheon.uf.viz.core.IGraphicsTarget.TextStyle;
import com.raytheon.uf.viz.core.IGraphicsTarget.VerticalAlignment;
import com.raytheon.uf.viz.core.PixelExtent;
import com.raytheon.uf.viz.core.catalog.DirectDbQuery;
import com.raytheon.uf.viz.core.catalog.DirectDbQuery.QueryLanguage;
import com.raytheon.uf.viz.core.drawables.IFont;
import com.raytheon.uf.viz.core.drawables.IShadedShape;
import com.raytheon.uf.viz.core.drawables.IWireframeShape;
import com.raytheon.uf.viz.core.drawables.PaintProperties;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.map.MapDescriptor;
import com.raytheon.uf.viz.core.rsc.AbstractVizResource;
import com.raytheon.uf.viz.core.rsc.LoadProperties;
import com.raytheon.uf.viz.core.rsc.capabilities.OutlineCapability;
import com.raytheon.viz.gfe.GFEPreference;
import com.raytheon.viz.gfe.core.DataManager;
import com.raytheon.viz.gfe.rsc.GFEFonts;
import com.raytheon.viz.gfe.rsc.ZoneDbResourceData;
import com.raytheon.viz.gfe.rsc.ZoneGroupableCapability;
import com.vividsolutions.jts.geom.Coordinate;
import com.vividsolutions.jts.geom.Envelope;
import com.vividsolutions.jts.geom.Geometry;
import com.vividsolutions.jts.geom.GeometryFactory;
import com.vividsolutions.jts.geom.Point;
import com.vividsolutions.jts.io.ParseException;
import com.vividsolutions.jts.io.WKBReader;

/**
 * This class use to obtain Zone's database resource. It associates mapping with
 * zone's name (ID) geomentry and maintains groupings of zones.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 *                         wldougher     Initial creation
 * Jul 11, 2011 9928       rferrel       moveGroup now takes list of groups.
 * Jun 24, 2013 2134       randerso      Fixed NullPointerException in fitToCWA.
 * Aug 14, 2014 3523       mapeters      Updated deprecated {@link DrawableString#textStyle} 
 *                                       assignments.
 * 
 * </pre>
 * 
 * @author wldougher
 * @version 1.0
 */
public class ZoneDbResource extends
        AbstractVizResource<ZoneDbResourceData, MapDescriptor> {
    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(ZoneDbResource.class);

    /** The name of the DB from which maps should be read */
    private static final String MAP_DB = "maps";

    private static final String MAP_SCHEMA = "mapdata";

    public static final String PUBLIC_TABLE = "zone";

    public static final String COUNTY_TABLE = "county";

    public static final String FIREWX_TABLE = "firewxzones";

    public static final String MARINE_TABLE = "marinezones";

    public static final String OFFSHORE_TABLE = "offshore";

    public static final String HIGHSEA_TABLE = "highsea";

    public static final String[] PUBLIC_FIELDS = { "'" + PUBLIC_TABLE + "'",
            "state || 'Z' || zone AS zonelabel", "CWA" };

    public static final String[] FIPS_FIELDS = { "'" + COUNTY_TABLE + "'",
            "state || 'C' || substr(fips,3) AS zonelabel", "CWA" };

    public static final String[] FIREWX_FIELDS = { "'" + FIREWX_TABLE + "'",
            "state || 'Z' || zone AS zonelabel", "CWA" };

    public static final String[] MARINE_FIELDS = { "'" + MARINE_TABLE + "'",
            "id as zonelabel", "wfo" };

    public static final String[] OFFSHORE_FIELDS = {
            "'" + OFFSHORE_TABLE + "'", "id as zonelabel", "wfo" };

    public static final String[] HIGHSEA_FIELDS = { "'" + HIGHSEA_TABLE + "'",
            "'HISEA'||gid as zonelabel", "wfo" };

    private static final double EXPANSION_FACTOR = 0.5;

    public static final int SELECTION_GROUP = 0;

    /** Fraction used by fitToCWA() to calculate margin width */
    private static final double BUFFER_MULT = 0.05;

    public static final int NO_GROUP = -1;

    private static final double MIN_LABEL_DISTANCE = 100;

    /**
     * FORMATTER_LAUNCHER, type. displays multiple zones, and multiple colors.
     * GHG_MONITOR, displays "clickable zones" as one color, (related to
     * hazards) MAKE_HAZARD type. displays 1 zone, all as same color.
     */
    public enum DisplayType {
        FORMATTER_LAUNCHER, GHG_MONITOR, MAKE_HAZARD, SINGLE_ZONE_ALLOWABLE
    }

    private DisplayType displayType;

    // Available simplification levels by table name
    protected Map<String, List<Double>> availSimpLevMap;

    // Previous simplification level by table name
    protected Map<String, Double> simpLevMap;

    // The site ID from the data manager
    protected String siteID;

    // The cwa ID set by callers
    protected String cwaID;

    // Whether this resource needs to be repainted.
    protected boolean dirty;

    // Like dirty, but stronger: database read needed during paint.
    protected boolean filthy;

    // The background area
    protected IShadedShape background;

    // The shapes for the colored groups
    protected ArrayList<IShadedShape> groupShapes;

    // The zone borders for all visible zones
    protected IWireframeShape outline;

    // The zone borders for zones in this site's CWA
    protected IWireframeShape cwaOutline;

    // A list of the current zone groups
    protected List<List<String>> groups;

    // A list of the zone groups on the previous call to paint()
    protected List<List<String>> oldGroups;

    // The current graphics target (only valid inside paint())
    protected IGraphicsTarget target;

    // The current paint properties (only valid inside paint())
    protected PaintProperties paintProps;

    // A reader to convert byte arrays to Geometries
    protected WKBReader wkbReader;

    // Instance of the comparator for sorting by table name
    protected TabNameComp tabNameComp;

    // Instance of the comparator for sorting by zone name
    protected ZoneNameComp zoneNameComp;

    // Records in the current query, sorted by table name
    protected DbData[] dbData;

    // Records in the current query, sorted by zone
    protected DbData[] zoneData;

    // tables visible when queryJob was last scheduled
    protected List<String> oldQTables;

    // tables visible when shapeBuilderJob was last scheduled
    protected List<String> oldSTables;

    // Map of colors to use
    protected Map<Object, RGB> colorMap;

    // For generating random colors
    protected Random rand;

    // The job that queries the database
    protected QueryJob queryJob;

    protected ShapeBuilderJob shapeBuilderJob;

    // The pixel extent of the previous call to paint()
    protected PixelExtent oldScreenExtent;

    // The simplification levels of the previous call to paint()
    protected List<String> oldSimpLevels;

    protected boolean runShapeJob;

    protected Envelope boundingEnvelope;

    protected HashMap<Geometry, String> zoneMap;

    protected int dbRqNum;

    protected int shpRqNum;

    // Map from zone IDs to geometry centroids
    protected HashMap<String, List<ReferencedCoordinate>> labelMap;

    protected String geometryType;

    protected List<IZoneChangeListener> zoneChangeListeners;

    private boolean labelZones;

    private Double scaleX;

    private Double scaleY;

    private IFont labelFont;

    /**
     * Constructor.
     * 
     * @param resourceData
     *            The ZoneDbResourceData that influence's this resource's
     *            appearance.
     * @param loadProperties
     *            Data-loading options used by superclass ctor.
     */
    public ZoneDbResource(ZoneDbResourceData resourceData,
            LoadProperties loadProperties) {
        super(resourceData, loadProperties);
        rand = new Random(System.currentTimeMillis());
        availSimpLevMap = new HashMap<String, List<Double>>();
        simpLevMap = new HashMap<String, Double>();
        queryJob = new QueryJob("Querying Database...");
        groups = new ArrayList<List<String>>(SELECTION_GROUP + 1);
        groups.add(new ArrayList<String>());
        groupShapes = new ArrayList<IShadedShape>();
        colorMap = new HashMap<Object, RGB>();
        Map<Object, RGB> rdColorMap = resourceData.getColorMap();
        if (rdColorMap != null) {
            colorMap.putAll(rdColorMap);
        }
        wkbReader = new WKBReader();
        tabNameComp = new TabNameComp();
        zoneNameComp = new ZoneNameComp();
        runShapeJob = true;
        displayType = DisplayType.FORMATTER_LAUNCHER;
        getCapability(OutlineCapability.class).setOutlineOn(true);
        getCapability(OutlineCapability.class).setLineStyle(LineStyle.SOLID);
        dbRqNum = 0;
        shpRqNum = 0;
        zoneChangeListeners = new ArrayList<IZoneChangeListener>(1);
    }

    /**
     * @see com.raytheon.uf.viz.core.rsc.AbstractVizResource#disposeInternal()
     */
    @Override
    protected void disposeInternal() {
        if (background != null) {
            background.dispose();
            background = null;
        }

        for (IShadedShape groupShape : groupShapes) {
            if (groupShape != null) {
                groupShape.dispose();
            }
        }
        groupShapes.clear();

        if (outline != null) {
            outline.dispose();
            outline = null;
        }

        if (cwaOutline != null) {
            cwaOutline.dispose();
            cwaOutline = null;
        }

        // dispose of labels and group labels?

        if (labelFont != null) {
            labelFont.dispose();
            labelFont = null;
        }
    }

    /**
     * @see com.raytheon.uf.viz.core.rsc.AbstractVizResource#initInternal(com.raytheon.uf.viz.core.IGraphicsTarget)
     */
    @Override
    protected void initInternal(IGraphicsTarget target) throws VizException {
        siteID = DataManager.getCurrentInstance().getSiteID();
        cwaID = siteID;
        // find the available simplification levels
        for (String table : resourceData.getAllTables()) {
            List<Double> levels = querySimpLevels(table);
            availSimpLevMap.put(table, levels);
        }
        shapeBuilderJob = new ShapeBuilderJob(cwaID, descriptor);
    }

    /**
     * The paint routine.
     * <p>
     * Painting is a three-stage process: the database is queried to obtain a
     * cache of geometries for the zones, the geometries are compiled into
     * wireframes and shaded shapes, and the wireframes and shaded shapes are
     * painted to the screen (by paintShapes()). The first two stages can take
     * significant amounts of time, so they are done in separate threads to keep
     * the GUI thread responsive.
     * </p>
     * <p>
     * Not all stages are necessary for each repaint. If the map is only being
     * repainted because it was hidden by another window, a call to
     * paintShapes() is all that needs to be done. If zones have changed color
     * due to zone selection or colormap changes, or if labels have been turned
     * on or off, some shapes may need to be compiled but the database does not
     * need to be queried.
     * </p>
     * <p>
     * It is important to avoid excess database reads. The maps database
     * typically resides on the EDEX server and is shared by all users. In the
     * past, database reads were generated with every mouse move. This bogged
     * down the server and the network with database requests, causing delays
     * not only for users of this class, but also for other users in unrelated
     * applications.
     * </p>
     * <p>
     * Database reads should ONLY be needed when:
     * <ul>
     * <li>Initializing the resource.</li>
     * <li>Changing tables (PUBLIC to FIREWX, for example).</li>
     * <li>Displaying a different region.</li>
     * <li>Changing zoom simplification levels (the geometries are read from a
     * different column).</li>
     * <li>The maps database has been updated.</li>
     * </ul>
     * Database reads are NOT needed when:
     * <ul>
     * <li>Selecting/deselecting zones.</li>
     * <li>Showing/hiding zone names or group numbers.</li>
     * <li>Changing zone colors.</li>
     * </ul>
     * </p>
     * 
     * @see com.raytheon.uf.viz.core.drawables.IRenderable#paintInternal(com.raytheon.uf.viz.core.IGraphicsTarget,
     *      com.raytheon.uf.viz.core.drawables.PaintProperties)
     */
    @Override
    protected void paintInternal(IGraphicsTarget target,
            PaintProperties paintProps) throws VizException {
        PixelExtent screenExtent = (PixelExtent) paintProps.getView()
                .getExtent();
        // compute an estimate of degrees per pixel
        double yc = screenExtent.getCenter()[1];
        double x1 = screenExtent.getMinX();
        double x2 = screenExtent.getMaxX();
        double[] c1 = descriptor.pixelToWorld(new double[] { x1, yc });
        double[] c2 = descriptor.pixelToWorld(new double[] { x2, yc });
        int screenHeight = paintProps.getCanvasBounds().height;
        int screenWidth = paintProps.getCanvasBounds().width;

        if (screenWidth == 0) {
            return;
        }
        double dppX = Math.abs(c2[0] - c1[0]) / screenWidth;

        this.target = target;
        this.paintProps = paintProps;
        this.scaleX = Double.valueOf(screenWidth / screenExtent.getWidth());
        this.scaleY = Double.valueOf(screenHeight / screenExtent.getHeight());

        Envelope screenEnv = screenExtent.getEnvelope();

        // See if we've zoomed in or out enough to need to read again.
        List<String> allTables = resourceData.getAllTables();
        List<String> simpLevels = new ArrayList<String>(allTables.size());
        for (String table : allTables) {
            simpLevels.add(this.simpLvlSuffix(table, dppX));
        }

        if (paintProps.isZooming()) {
            target.setNeedsRefresh(true);
        } else {
            if (oldScreenExtent == null || oldSimpLevels == null || isFilthy()
                    || !oldScreenExtent.getEnvelope().covers(screenEnv)
                    || !oldSimpLevels.equals(simpLevels)
                    || !resourceData.getTables().equals(oldQTables)) {
                String dbReadReason = "";
                if (oldScreenExtent == null) {
                    dbReadReason = "oldScreenExtent == null";
                } else if (oldSimpLevels == null) {
                    dbReadReason = "oldSimpLevels == null";
                } else if (isFilthy()) {
                    dbReadReason = "isFilthy()";
                } else if (!oldScreenExtent.getEnvelope().covers(screenEnv)) {
                    dbReadReason = "!oldScreenExtent.getEnvelope().covers(screenEnv)";
                } else if (!oldSimpLevels.equals(simpLevels)) {
                    dbReadReason = "!oldSimpLevels.equals(simpLevels)";
                } else {
                    List<?> rdgt = resourceData.getTables();
                    dbReadReason = "!resourceData.getTables().equals(oldQTables))"
                            + "\nresourceData.getTables().size():"
                            + ((rdgt == null) ? null : rdgt.size())
                            + "\noldQTables.size():"
                            + ((oldQTables == null) ? null : oldQTables.size());
                }
                PixelExtent expandedExtent = getExpandedExtent(screenExtent);
                oldScreenExtent = expandedExtent;
                oldSimpLevels = simpLevels;
                List<String> rdTables = resourceData.getTables();
                if (rdTables == null) {
                    oldQTables = null;
                } else {
                    oldQTables = new ArrayList<String>(rdTables);
                }
                String query = buildQuery(expandedExtent, dppX);
                QueryRequest qreq = new QueryRequest();
                qreq.query = query;
                qreq.target = target;
                queryJob.requestQueue.clear();
                queryJob.requestQueue.offer(qreq);
                queryJob.schedule();
                dbRqNum = (dbRqNum + 1) % 10000;
                // log(Priority.VERBOSE, "Database query " + dbRqNum
                // + " scheduled." + "Reason: " + dbReadReason);
                setFilthy(false);
            }

            DbData[] rspA = queryJob.responseQueue.poll();
            if (rspA != null) {
                // log(Priority.VERBOSE, "Database response received.");
                dbData = rspA;
                Arrays.sort(dbData, tabNameComp);
                zoneData = new DbData[rspA.length];
                System.arraycopy(rspA, 0, zoneData, 0, rspA.length);
                Arrays.sort(zoneData, zoneNameComp);
                createZoneMap();
                createLabelMap();
            }
            buildShapes(rspA);
            setDirty(false);
        }

        paintShapes();
        this.target = null;
        this.paintProps = null;
        this.scaleX = null;
        this.scaleY = null;
    }

    /**
     * Get the state of the filthy flag.
     * 
     * @return the filthy state.
     */
    public boolean isFilthy() {
        return filthy;
    }

    /**
     * Set the filthy flag to <code>filthy</code>.
     * <p>
     * Filthy is a stronger version of dirty, indicating that new geometries
     * need to be loaded before repainting. The flag is automatically cleared
     * when paintInternal() kicks off a new query job.
     * 
     * @param filthy
     *            the value to set
     */
    public void setFilthy(boolean filthy) {
        this.filthy = filthy;
    }

    /**
     * Send a status message at the given priority, usually Priority.VERBOSE.
     * 
     * @param priority
     *            The priority of the message to log
     * @param message
     */
    protected void log(Priority priority, String message) {
        statusHandler.handle(priority, "ZoneDbResource: " + message);
    }

    /**
     * Create the map from zone IDs to centroid coordinates
     */
    protected void createLabelMap() {
        labelMap = new HashMap<String, List<ReferencedCoordinate>>();
        int numGeometries;
        Geometry geom;
        Geometry poly;
        List<ReferencedCoordinate> rcList;
        ReferencedCoordinate rc;
        for (DbData data : dbData) {
            geom = null;
            try {
                geom = wkbReader.read(data.wkb);
            } catch (ParseException e) {
                statusHandler.handle(Priority.PROBLEM,
                        "Error parsing geometry for zone " + data.zone, e);
            }

            if (geom != null) {
                rcList = labelMap.get(data.zone);
                if (rcList == null) {
                    rcList = new ArrayList<ReferencedCoordinate>(2);
                    labelMap.put(data.zone, rcList);
                }
                numGeometries = geom.getNumGeometries();
                for (int geomNum = 0; geomNum < numGeometries; geomNum++) {
                    poly = geom.getGeometryN(geomNum);
                    rc = new ReferencedCoordinate(poly.getCentroid()
                            .getCoordinate());
                    rcList.add(rc);
                }
            }
        }
    }

    /**
     * Create the map from geometries to zone IDs
     */
    protected void createZoneMap() {
        zoneMap = new HashMap<Geometry, String>();
        for (DbData item : dbData) {
            try {
                Geometry geom = wkbReader.read(item.wkb);
                zoneMap.put(geom, item.zone);
            } catch (ParseException e) {
                statusHandler.handle(Priority.PROBLEM,
                        "Error parsing geometry for zone " + item.zone, e);
            }
        }
    }

    /**
     * Build the query string to get information from the maps database.
     * 
     * @param extent
     *            The PixelExtent over which the query applies
     * @param dppX
     *            Degrees (longitude) per pixel in the X direction
     * @return An SQL query string
     * @throws VizException
     */
    protected String buildQuery(PixelExtent extent, double dppX)
            throws VizException {
        // enter("buildQuery{PE}");

        Envelope env = null;
        try {
            CoordinateReferenceSystem crs = descriptor.getCRS();
            Envelope evl = descriptor.pixelToWorld(extent, crs);
            ReferencedEnvelope ref = new ReferencedEnvelope(evl, crs);
            env = ref.transform(MapUtil.LATLON_PROJECTION, true);
        } catch (Exception e) {
            throw new VizException("Error transforming extent", e);
        }

        // leave("buildQuery{PE}");
        return buildQuery(env, dppX);
    }

    /**
     * @param env
     *            The Envelope over which the query applies
     * @param dppX
     *            Degrees (longitude) per pixel in the X direction
     * @return An SQL query string
     */
    protected String buildQuery(Envelope env, double dppX) {
        // enter("buildQuery");
        StringBuilder sb = new StringBuilder();
        String union = "";
        List<String> allTables = null;
        allTables = new ArrayList<String>(resourceData.getAllTables());
        for (String table : allTables) {
            String suffix = simpLvlSuffix(table, dppX);
            String geometryField = "main.the_geom" + suffix;
            String geoConstraint = String.format(
                    "%s && ST_SetSrid('BOX3D(%f %f, %f %f)'::box3d,4326)",
                    geometryField, env.getMinX(), env.getMinY(), env.getMaxX(),
                    env.getMaxY());

            sb.append(union);
            sb.append("SELECT");
            sb.append(" AsBinary(");
            sb.append(geometryField);
            sb.append(")");
            String[] queryColumns = resourceData.getQueryColumns(table);
            for (String queryCol : queryColumns) {
                sb.append(", ");
                sb.append(queryCol);
            }
            sb.append(" FROM ");
            sb.append(MAP_SCHEMA).append(".").append(table);
            sb.append(" AS main");
            sb.append(" WHERE ");
            sb.append(geoConstraint);
            String[] constraints = resourceData.getConstraints(table);
            if (constraints != null) {
                for (String constraint : constraints) {
                    sb.append(" AND ");
                    sb.append(constraint);
                }
            }

            union = " UNION ";
        }
        sb.append(";");
        // leave("buildQuery");
        return sb.toString();
    }

    /**
     * @return
     */
    protected String buildEAQuery() {
        StringBuilder sb = new StringBuilder();
        String union = "";
        List<String> allTables = null;
        allTables = new ArrayList<String>(resourceData.getAllTables());
        for (String table : allTables) {
            sb.append(union);
            sb.append("SELECT ");
            String[] queryColumns = resourceData.getQueryColumns(table);
            for (int i = 0; i < queryColumns.length; i++) {
                sb.append(queryColumns[i]);
                if (i < queryColumns.length - 1) {
                    sb.append(", ");
                }
            }
            sb.append(" FROM ");
            sb.append(MAP_SCHEMA).append(".").append(table);
            sb.append(" AS main");
            sb.append(" WHERE ");
            String[] constraints = resourceData.getConstraints(table);
            if (constraints != null) {
                for (int i = 0; i < constraints.length; i++) {
                    sb.append(constraints[i]);
                    if (i < constraints.length - 1) {
                        sb.append(" AND ");
                    }
                }
            }

            union = " UNION ";
        }
        sb.append(";");
        return sb.toString();
    }

    /**
     * Figure out which shapes need to be rebuilt and destroy any shapes that
     * are out of date, setting any pointers to them to null.
     */
    protected ShapeBuilderRequest dropObsoleteShapes(boolean newDbData) {
        // enter("dropObsoleteShapes");
        boolean killBackground = false;
        boolean killOutline = false;
        boolean killShapes = false;

        ShapeBuilderRequest request = new ShapeBuilderRequest();

        request.target = target;
        request.dbData = dbData;
        request.zoneData = zoneData;
        // Copy groups into the request
        request.groups = new ArrayList<List<String>>(groups.size());
        for (List<String> group : groups) {
            request.groups.add(new ArrayList<String>(group));
        }
        request.tables = new ArrayList<String>(resourceData.getTables());
        request.background = background;
        request.outline = outline;
        request.cwaOutline = cwaOutline;
        if (groupShapes != null) {
            request.groupShapes = new ArrayList<IShadedShape>(groupShapes);
        }

        if (colorMap == null) {
            colorMap = new HashMap<Object, RGB>();
        }

        Map<Object, RGB> rdColorMap = resourceData.getColorMap();
        if (rdColorMap == null) {
            request.colorMap = new HashMap<Object, RGB>();
        } else {
            request.colorMap = new HashMap<Object, RGB>(rdColorMap);
        }

        request.shaded = true;

        OutlineCapability outCap = getCapability(OutlineCapability.class);
        if ((outCap != null) && outCap.isOutlineOn()) {
            request.outlined = true;
        }

        // if the database response has changed OR a redraw is being forced
        // the background, outlines, and shapes all need rebuilt.
        if (newDbData) {
            killBackground = true;
            killOutline = true;
            killShapes = true;
            runShapeJob = true;
        }

        // if the visible table has changed
        List<String> chosenTables = resourceData.getTables();
        if ((chosenTables == null) || !chosenTables.equals(oldSTables)) {
            killBackground = true;
            killOutline = true;
            if (chosenTables != null) {
                oldSTables = new ArrayList<String>(chosenTables);
            }
            runShapeJob = true;
        }

        // if the color map has changed
        if (!colorMap.equals(request.colorMap)) {
            colorMap = request.colorMap;
            killShapes = true;
            runShapeJob = true;
        }

        if (killBackground) {
            request.background = null;
        }
        if (killOutline) {
            request.outline = null;
            request.cwaOutline = null;
        }

        if (killShapes) {
            // Get rid of all the shapes
            if (request.groupShapes != null) {
                for (int idx = 0; idx < request.groupShapes.size(); idx++) {
                    request.groupShapes.set(idx, null);
                }
            }
            request.reason = newDbData ? "new database response"
                    : "new color map";
        } else {
            // if the membership of any group has changed
            int groupNum;
            boolean kill;
            StringBuilder reasonReason = new StringBuilder();
            for (groupNum = 0; groupNum < request.groups.size(); groupNum++) {
                kill = false;
                if (oldGroups == null) {
                    // Need to build shape for the new group
                    reasonReason.append(" oldGroups is null");
                    kill = true;
                } else if (groupNum >= oldGroups.size()) {
                    // Need to build shape for the new group
                    reasonReason.append(" gn>").append(oldGroups.size());
                    kill = true;
                } else if (!(request.groups.get(groupNum).equals(oldGroups
                        .get(groupNum)))) {
                    // Need to build for changed zone membership
                    reasonReason.append(String.format(" g[%d]!=old %s!=%s",
                            groupNum, request.groups.get(groupNum).toString(),
                            oldGroups.get(groupNum).toString()));
                    kill = true;
                }
                if (kill) {
                    if (groupNum < groupShapes.size()) {
                        request.groupShapes.set(groupNum, null);
                    } else {
                        request.groupShapes.add(null);
                    }
                    request.reason = "Groups changed" + reasonReason;
                    runShapeJob = true;
                }
            }
            for (; groupNum < request.groupShapes.size(); groupNum++) {
                if (request.groupShapes.get(groupNum) != null) {
                    // Rebuild to remove shapes for groups that are gone
                    request.groupShapes.set(groupNum, null);
                    if (request.reason == null) {
                        request.reason = "Extra group shapes";
                    }
                    runShapeJob = true;
                }
            }
        }
        // leave("dropObsoleteShapes");
        return request;
    }

    /**
     * Build the shapes that will be drawn to the screen.
     */
    protected void buildShapes(Object rspA) {
        // enter("buildShapes");
        if (dbData != null) {

            ShapeBuilderRequest request = dropObsoleteShapes(rspA != null);

            if (runShapeJob) {

                oldGroups = new ArrayList<List<String>>(groups.size());
                for (List<String> group : groups) {
                    oldGroups.add(new ArrayList<String>(group));
                }
                groups.clear();
                for (List<String> group : request.groups) {
                    groups.add(new ArrayList<String>(group));
                }
                shpRqNum = (shpRqNum + 1) % 10000;
                request.id = shpRqNum;
                while (!shapeBuilderJob.requestQueue.offer(request)) {
                    shapeBuilderJob.requestQueue.poll();
                }
                shapeBuilderJob.schedule();
                runShapeJob = false;
                // log(Priority.VERBOSE, "ShapeBuilderJob " + shpRqNum
                // + " scheduled.");
            }

            // Get the shapes from shapeBuilderJob (request may have been sent
            // in an earlier paint() call).
            ShapeBuilderResponse rsp = shapeBuilderJob.responseQueue.poll();
            if (rsp != null) {

                // log(Priority.VERBOSE, "ShapeBuilderJob response " + rsp.id
                // + " received.");
                if (rsp.background != null) {
                    if (background != null) {
                        background.reset();
                    }
                    background = rsp.background;
                    // log(Priority.VERBOSE, "background replaced.");
                }
                if (rsp.outline != null) {
                    if (outline != null) {
                        outline.reset();
                    }
                    outline = rsp.outline;
                    // log(Priority.VERBOSE, "outline replaced.");
                }
                if (rsp.cwaOutline != null) {
                    if (cwaOutline != null) {
                        cwaOutline.reset();
                    }
                    cwaOutline = rsp.cwaOutline;
                    // log(Priority.VERBOSE, "cwaOutline replaced.");
                }
                if (rsp.groupShapes != null) {
                    if (groupShapes != null) {
                        int dispCount = 0;
                        int rspSize = (rsp.groupShapes == null) ? 0
                                : rsp.groupShapes.size();
                        // Dispose of any group shapes that have been
                        // replaced
                        for (int groupNum = 0; groupNum < groupShapes.size(); groupNum++) {
                            IShadedShape shape = groupShapes.get(groupNum);
                            if (shape != null) {
                                if ((groupNum >= rspSize)
                                        || (shape != rsp.groupShapes
                                                .get(groupNum))) {
                                    shape.dispose();
                                    dispCount++;
                                }
                            }
                        }
                        if (dispCount > 0) {
                            // log(Priority.VERBOSE, "" + dispCount
                            // + " group shapes disposed.");
                        }
                    }
                    groupShapes = rsp.groupShapes;
                }
                colorMap.putAll(rsp.colorMap);
                Map<Object, RGB> rdColorMap = resourceData.getColorMap();
                if (colorMap.size() > rdColorMap.size()) {
                    for (Object obj : colorMap.keySet()) {
                        if (!rdColorMap.containsKey(obj)) {
                            rdColorMap.put(obj, colorMap.get(obj));
                        }
                    }
                }
            }
        }
        // leave("buildShapes");
    }

    /**
     * Paint the shapes created by buildShapes(). This method should only be
     * called from within paint().
     * 
     * @param target
     *            The graphics target on which to paint the shapes
     * @param paintProps
     *            the paint properties supplied to paint()
     * @throws VizException
     *             if thrown by target.drawXXX() methods
     */
    protected void paintShapes() throws VizException {
        float alpha = paintProps.getAlpha();

        // Draw the colored areas
        // draw the background
        if ((background != null) && background.isDrawable()) {
            target.drawShadedShape(background, alpha);
        }

        // Draw the zone outlines on top of the shaded shapes
        OutlineCapability outCap = getCapability(OutlineCapability.class);
        if ((outCap != null) && outCap.isOutlineOn()) {
            if ((outline != null) && outline.isDrawable()
                    && (cwaOutline != null) && cwaOutline.isDrawable()) {
                // draw zones in group color
                // (in single selection mode, selection is first group)
                if (groupShapes != null) {
                    for (IShadedShape groupShape : groupShapes) {
                        if ((groupShape != null) && groupShape.isDrawable()) {
                            target.drawShadedShape(groupShape, alpha);
                        }
                    }
                }
            }

            int outlineWidth = outCap.getOutlineWidth();
            LineStyle lineStyle = outCap.getLineStyle();

            // Draw the zone outlines
            if ((outline != null) && outline.isDrawable()) {
                target.drawWireframeShape(outline,
                        resourceData.getOutlineColor(), outlineWidth, lineStyle);
            }

            // Draw the highlighted zone outlines on top of the zone borders
            // (for zones that are in this site's CWA)
            if ((cwaOutline != null) && cwaOutline.isDrawable()) {
                target.drawWireframeShape(cwaOutline,
                        resourceData.getCwaColor(), outlineWidth, lineStyle);
            }
        }

        // Draw the labels on top of everything else
        if (labelMap != null) {
            paintLabels();
        }
    }

    /**
     * Return the available simplification levels for the specified table.
     * 
     * @return the levels
     */
    protected List<Double> querySimpLevels(String table) {
        List<Double> levels = null;
        try {
            StringBuilder query = new StringBuilder(
                    "SELECT f_geometry_column FROM public.geometry_columns WHERE f_table_schema='");
            query.append(MAP_SCHEMA);
            query.append("' AND f_table_name='");
            query.append(table);
            query.append("' AND f_geometry_column LIKE 'the_geom_%' order by f_geometry_column asc;");
            List<Object[]> results = DirectDbQuery.executeQuery(
                    query.toString(), MAP_DB, QueryLanguage.SQL);

            levels = new ArrayList<Double>(results.size());
            for (Object[] obj : results) {
                String s = ((String) obj[0]).substring(9).replace('_', '.');
                levels.add(Double.parseDouble(s));
            }
        } catch (VizException e) {
            statusHandler.handle(Priority.PROBLEM,
                    "Error querying available levels", e);
        }

        return levels;
    }

    /**
     * Get the color that has been chosen for the designated object. If no color
     * has been set, return a random color.
     * 
     * @param key
     *            The object to look up
     * @return The color in which the object should be drawn.
     */
    protected RGB getColor(Object key) {
        if (colorMap == null) {
            colorMap = new HashMap<Object, RGB>();
        }
        RGB color = colorMap.get(key);
        if (color == null) {
            color = new RGB(rand.nextInt(206) + 50, rand.nextInt(206) + 50,
                    rand.nextInt(206) + 50);
            colorMap.put(key, color);
        }

        return color;
    }

    /**
     * @return the dirty flag
     */
    public boolean isDirty() {
        return dirty;
    }

    /**
     * @param dirty
     *            the dirty flag value to set
     */
    public void setDirty(boolean dirty) {
        this.dirty = dirty;
    }

    /**
     * Expand the screen extent by EXPANSION_FACTOR.
     * 
     * @param screenExtent
     *            The extent of the screen in pixel coordinates.
     * 
     * @return the expanded pixel extent
     */
    protected PixelExtent getExpandedExtent(PixelExtent screenExtent) {
        PixelExtent expandedExtent = screenExtent.clone();
        expandedExtent.getEnvelope().expandBy(
                expandedExtent.getWidth() * EXPANSION_FACTOR,
                expandedExtent.getHeight() * EXPANSION_FACTOR);
        return expandedExtent;
    }

    /**
     * Get the suffix for the_geom in queries against table based on the degrees
     * per pixel estimate.
     * 
     * @param table
     *            The name of the table in the query
     * @param dpp
     *            The degrees per pixel estimate
     * @return The simplification level suffix.
     */
    protected String simpLvlSuffix(String table, double dpp) {
        String suffix = "";
        double simpLev = 0.0;
        List<Double> levels = availSimpLevMap.get(table);
        if (levels != null) {
            for (double level : levels) {
                if (dpp < level) {
                    break;
                }
                simpLev = level;
            }
        }
        if (simpLev > 0.0) {
            DecimalFormat df = new DecimalFormat("0.######");
            suffix = "_"
                    + StringUtils.replaceChars(df.format(simpLev), '.', '_');
        }
        return suffix;
    }

    /**
     * Returns a copy of the zoneGroupings list.
     * <p>
     * The original code in ZoneFileResource was much more complicated, but it
     * seemed anxious to make certain that callers received a copy of a list of
     * strings, not the original list. The calls to Collections.unmodifiableList
     * generate warnings, so they are suppressed. It would be used on a per-line
     * basis, but the compiler gets confused.
     * 
     * @return the list of zone groupings. Each zone grouping is a List of
     *         Strings, the zone IDs of the zones in the group.
     */
    public List<List<String>> getZoneGroupings() {

        List<List<String>> rtnVal = new ArrayList<List<String>>(groups.size());
        for (List<String> zoneGrouping : groups) {
            rtnVal.add(Collections.unmodifiableList(zoneGrouping));
        }

        rtnVal = Collections.unmodifiableList(rtnVal);

        return rtnVal;
    }

    public void setLabelZones(boolean labelZones) {
        this.labelZones = labelZones;
        refreshSelections();
    }

    public void setLabelGroups(boolean labelGroups) {
        getCapability(ZoneGroupableCapability.class).setZoneGrouping(
                labelGroups);
        refreshSelections();
    }

    /**
     * Add missing zones into one or more groups.
     * 
     * @param includeAllZones
     *            - True add zone labels
     * @param addToOneGroup
     *            - True add missing zones to first group otherwise put each
     *            missing zone its own group
     * @return msg - Message indicating what zones were added
     */
    public String setIncludeAllZones(boolean includeAllZones,
            boolean addToOneGroup) {
        StringBuilder sb = new StringBuilder();
        if (includeAllZones && (zoneData != null)) {
            List<String> missing = new ArrayList<String>();
            for (DbData zoneRec : zoneData) {
                boolean found = false;
                for (List<String> group : groups) {
                    if (group.contains(zoneRec.zone)) {
                        found = true;
                        break;
                    }
                }
                if (!found && !missing.contains(zoneRec.zone)) {
                    missing.add(zoneRec.zone);
                    sb.append(zoneRec.zone).append(",");
                }
            }

            List<String> group = null;
            if (addToOneGroup) {
                group = groups.get(0);
                for (String zone : missing) {
                    group.add(zone);
                }
            } else {
                // When only an empty group remove so zone numbers start at one.
                if (groups.size() == 1 && groups.get(0).size() == 0) {
                    groups.clear();
                }

                for (String zone : missing) {
                    group = new ArrayList<String>();
                    group.add(zone);
                    groups.add(group);
                }
            }

            if (sb.length() > 0) {
                signalZonesChanged();
                sb.insert(0, "Missing zones have been added [");
                sb.replace(sb.length() - 1, sb.length(), "]");
            }
        }
        return sb.toString();
    }

    /**
     * Remove all zones from any group.
     */
    public void clearAllZones() {
        groups.clear();
        groups.add(new ArrayList<String>());
        signalZonesChanged();
    }

    public Envelope getBoundingEnvelope() {
        return this.boundingEnvelope;
    }

    public void setBoundingEnvelope(Envelope boundingEnvelope) {
        this.boundingEnvelope = boundingEnvelope;
        setFilthy(true);
    }

    public void setMakeHazardType() {
        displayType = DisplayType.MAKE_HAZARD;
    }

    public void setGHGType() {
        displayType = DisplayType.GHG_MONITOR;
    }

    public void setSingleAllowableType() {
        displayType = DisplayType.SINGLE_ZONE_ALLOWABLE;
    }

    /**
     * @return the colorMap
     */
    public Map<Object, RGB> getColorMap() {
        return resourceData.getColorMap();
    }

    /**
     * @param colorMap
     *            the colorMap to set
     */
    public void setColorMap(Map<Object, RGB> colorMap) {
        resourceData.setColorMap(colorMap);
    }

    /**
     * Set the site ID of the CWA site.
     * 
     * @param cwaIdentifier
     *            The site ID of the CWA site.
     */
    public void setCwaId(String cwaIdentifier) {
        cwaID = cwaIdentifier;
        if (shapeBuilderJob != null) {
            shapeBuilderJob.setCwaID(cwaID);
        }
        setFilthy(true);
    }

    /**
     * Re-calculate the bounding box to center the view around the CWA, with a
     * "reasonable" margin. The CWA column and CWA ID must be set before this
     * method is called, so that the database can be queried for the CWA
     * geometry. Although it's somewhat redundant, setBoundingEnvelope() should
     * be called before this method, since this method will not set a bounding
     * geometry if there are database or geometry-parsing errors.
     */
    public void fitToCWA() {

        if ((cwaID == null) || "".equals(cwaID)) {
            throw new IllegalStateException("The CWA ID has not been set.");
        }

        // build a query
        StringBuilder query = new StringBuilder();
        query.append("SELECT");
        query.append(" AsBinary(ST_Envelope(ST_Collect(ST_Envelope(main.the_geom))))");
        query.append(" FROM");
        query.append(" ").append(MAP_SCHEMA).append(".").append(PUBLIC_TABLE)
                .append(" as main");
        query.append(" WHERE");
        query.append(" main.cwa LIKE '").append(cwaID).append("%'");
        query.append(" UNION (");
        query.append("SELECT");
        query.append(" AsBinary(ST_Envelope(ST_Collect(ST_Envelope(main.the_geom))))");
        query.append(" FROM");
        query.append(" ").append(MAP_SCHEMA).append(".").append(MARINE_TABLE)
                .append(" as main");
        query.append(" WHERE");
        query.append(" main.wfo LIKE '").append(cwaID).append("%'");
        query.append(")");
        query.append(" UNION (");
        query.append("SELECT");
        query.append(" AsBinary(ST_Envelope(ST_Collect(ST_Envelope(main.the_geom))))");
        query.append(" FROM");
        query.append(" ").append(MAP_SCHEMA).append(".").append(OFFSHORE_TABLE)
                .append(" as main");
        query.append(" WHERE");
        query.append(" main.wfo LIKE '").append(cwaID).append("%'");
        query.append(")");
        query.append(";");

        Exception failure = null;
        Geometry cwaGeometry = null;
        try {
            // Get the geometries in the CWA
            List<Object[]> result = DirectDbQuery.executeQuery(
                    query.toString(), MAP_DB, QueryLanguage.SQL);
            if ((result != null) && (result.size() > 0)) {
                WKBReader wkbReader = new WKBReader();
                Geometry geometry = null;
                // Combine the geometries in the CWA into one geometry
                for (Object[] row : result) {
                    byte[] wkb = (byte[]) row[0];
                    // areas w/o marine zones return a row containing null
                    if (wkb != null) {
                        geometry = wkbReader.read(wkb);
                        if (cwaGeometry == null) {
                            cwaGeometry = geometry;
                        } else {
                            cwaGeometry = cwaGeometry.union(geometry);
                        }
                    }
                }
            }
        } catch (VizException e) {
            failure = e;
        } catch (ParseException e) {
            failure = e;
        }

        if (failure == null) {
            if (cwaGeometry == null) {
                failure = new IllegalArgumentException(
                        "The CWA query returned no records.");
            } else if (cwaGeometry.isEmpty()) {
                failure = new IllegalArgumentException(
                        "The CWA geometry is empty.");
            }
        }

        if (failure == null) {
            // calculate a bounding geometry from cwaGeometry
            Geometry cwaBBox = cwaGeometry.getEnvelope();
            // double cwaDist = cwaBBox.getArea();
            // cwaDist = Math.sqrt(cwaDist);
            // cwaDist *= BUFFER_MULT;
            // Geometry shrinkWrapGeometry = cwaBBox.buffer(cwaDist);
            // setBoundingEnvelope(shrinkWrapGeometry.getEnvelopeInternal());
            setBoundingEnvelope(cwaBBox.getEnvelopeInternal());
        } else {
            statusHandler
                    .handle(Priority.PROBLEM,
                            "Error fitting view to CWA - default bounding geometry will be used",
                            failure);
        }
    }

    /**
     * Return the geometry to which the clicked point belongs.
     * 
     * @param aLatLon
     *            Coordinates of a point in Latitude/Longitude space
     * @return the Geometry (within this resource) in which aLatLon lies, or
     *         null if the point lies outside all this resource's geometries.
     */
    public Geometry clickOnExistingGeometry(Coordinate aLatLon) {
        Point p = new GeometryFactory().createPoint(aLatLon);
        for (Geometry g : zoneMap.keySet()) {
            if (g.contains(p)) {
                return g;
            }
        }
        return null;
    }

    /**
     * @param zones
     * @param groupNum
     */
    public void addZone(List<Geometry> zones, int groupNum) {
        updateZones(zones, groupNum);
    }

    /**
     * Add the specified geometries to the group indicated by zoneIndex
     * 
     * @param geoms
     *            The geometries to add to the group
     * @param zoneIndex
     *            The group number to which the geometries should be added.
     */
    public void updateZones(List<Geometry> geoms, int zoneIndex) {
        if (zoneIndex < NO_GROUP) {
            throw new IllegalArgumentException("Illegal zone index "
                    + zoneIndex);
        }
        for (Geometry geometry : geoms) {
            // get the zone id for the geometry
            String zoneId = zoneMap.get(geometry);
            // remove that zone ID from all groups
            updateZone(zoneId, zoneIndex);
        }
    }

    /**
     * Adds a single zone ID to the indicated group.
     * 
     * @param zoneId
     *            The zone ID string, i.e., 'NEZ007'.
     * @param zoneIndex
     *            The group number to assign the group to.
     */
    public void updateZone(String zoneId, int zoneIndex) {

        if (zoneId != null) {
            for (List<String> zoneGroup : groups) {
                zoneGroup.remove(zoneId);
            }
            if (zoneIndex >= 0) {
                while (zoneIndex > groups.size() - 1) {
                    groups.add(new ArrayList<String>());
                }
                Map<Object, RGB> rscCmap = resourceData.getColorMap();
                if (rscCmap != null) {
                    Integer ziInteger = Integer.valueOf(zoneIndex);
                    RGB idxColor = rscCmap.get(ziInteger);
                    if (idxColor == null) {
                        idxColor = new RGB(rand.nextInt(206) + 50,
                                rand.nextInt(206) + 50, rand.nextInt(206) + 50);
                        rscCmap.put(ziInteger, idxColor);
                    }
                }
                // add zoneId to zoneIndex'th group
                List<String> zoneGroup = groups.get(zoneIndex);
                // sort zones within group so trivial changes don't cause
                // shaded shape generation
                String[] zoneGroupA = zoneGroup.toArray(new String[0]);
                int idx = Arrays.binarySearch(zoneGroupA, zoneId);
                if (idx < 0) {
                    idx = Math.abs(idx) - 1;
                }
                zoneGroup.add(idx, zoneId);
            }
        }
        signalZonesChanged();
    }

    /**
     * Get the group number for geom. If geom is not in a group, return -1.
     * 
     * @param geom
     *            The geometry whose group # is to be determined.
     * @return The group number for the geometry, or -1 if the geometry is not
     *         in any group.
     */
    public int getZoneIndex(Geometry geom) {
        String zoneId = zoneMap.get(geom);
        return getZoneIndex(zoneId);
    }

    /**
     * Get the zone ID from the geometry.
     * 
     * @param geom
     *            - Geometry for the zone
     * @return zoneId - when geom is associated with a zone otherwise null
     */
    public String getZoneId(Geometry geom) {
        return zoneMap.get(geom);
    }

    /**
     * Get the group number for the zone with the given ID. If zoneId is not in
     * a group, return -1.
     * 
     * @param zoneId
     *            the The zone ID whose group # is to be determined.
     * @return The group number for the zone ID, or -1 if the zone ID is not in
     *         any group.
     */
    public int getZoneIndex(String zoneId) {
        for (int i = 0; i < groups.size(); i++) {
            List<String> group = groups.get(i);
            if ((group != null) && group.contains(zoneId)) {
                return i;
            }
        }
        return NO_GROUP;
    }

    /**
     * 
     */
    public void refreshSelections() {
        setDirty(true);
    }

    public void compactList() {
        // remove empty groups
        int i = 0;
        while (i < groups.size()) {
            if (groups.get(i).size() > 0) {
                i++;
            } else {
                groups.remove(i);
            }
        }
    }

    /**
     * Move or clear the list of groups based on the desired zone operation.
     * 
     * @param selectedGroups
     *            - List of groups to move/clear
     * @param operation
     *            The zone operation to perform
     */
    // public void moveGroup(final List<List<String>> selectedGroups,
    // final ZoneOperation operation) {
    //
    // // Nothing to do return without marking dirty.
    // if (selectedGroups.size() == 0) {
    // return;
    // }
    //
    // // clean up the groups then get a sorted list of the indices.
    // cleanup(groups);
    // int selIdx = 0;
    // int[] selectedIndices = new int[selectedGroups.size()];
    // for (List<String> group : selectedGroups) {
    // selectedIndices[selIdx++] = groups.indexOf(group);
    // }
    // Arrays.sort(selectedIndices);
    //
    // int idx = -1;
    // List<String> temp;
    // switch (operation) {
    // case CLEAR:
    // for (idx = 0; idx < selectedIndices.length; ++idx) {
    // groups.get(selectedIndices[idx]).clear();
    // }
    // break;
    // case MOVE_DOWN:
    // selIdx = selectedIndices.length - 1;
    // if (selectedIndices[selIdx] < groups.size() - 1) {
    // for (selIdx = selectedIndices.length - 1; selIdx >= 0; --selIdx) {
    // idx = selectedIndices[selIdx];
    // temp = groups.get(idx + 1);
    // groups.set(idx + 1, groups.get(idx));
    // groups.set(idx, temp);
    // }
    // }
    // break;
    // case MOVE_UP:
    // selIdx = 0;
    // if (selectedIndices[selIdx] > 0) {
    // for (selIdx = 0; selIdx < selectedIndices.length; ++selIdx) {
    // idx = selectedIndices[selIdx];
    // temp = groups.get(idx - 1);
    // groups.set(idx - 1, groups.get(idx));
    // groups.set(idx, temp);
    // }
    // }
    // break;
    // case MOVE_TO_BOTTOM:
    // selIdx = selectedIndices.length - 1;
    // if (selectedIndices[selIdx] < groups.size() - 1) {
    // int delta = groups.size() - selectedIndices[selIdx] - 1;
    // for (selIdx = selectedIndices.length - 1; selIdx >= 0; --selIdx) {
    // idx = selectedIndices[selIdx];
    // temp = groups.remove(idx);
    // groups.add(idx + delta, temp);
    // }
    // }
    // break;
    // case MOVE_TO_TOP:
    // selIdx = 0;
    // if (selectedIndices[selIdx] > 0) {
    // int delta = selectedIndices[selIdx];
    // for (selIdx = 0; selIdx < selectedIndices.length; ++selIdx) {
    // idx = selectedIndices[selIdx];
    // temp = groups.remove(idx);
    // groups.add(idx - delta, temp);
    // }
    // // cleanup(groups);
    // }
    // break;
    // default:
    // statusHandler.handle(Priority.PROBLEM, "Unknown move code "
    // + operation);
    // }
    //
    // setDirty(true);
    // }

    /**
     * Remove nulls and empty lists from list.
     * 
     * @param list
     *            a list of lists, possibly containing nulls
     */
    private void cleanup(List<List<String>> list) {
        Iterator<List<String>> it = list.iterator();
        while (it.hasNext()) {
            List<? extends Object> litem = it.next();
            if ((litem == null) || litem.isEmpty()) {
                it.remove();
            }
        }
    }

    /**
     * Get the geometry type string from the database metadata.
     * 
     * @return the geometry type string
     */
    protected String getGeometryType() {
        if (geometryType == null) {
            try {
                String table = resourceData.getMainTable();
                StringBuilder query = new StringBuilder(
                        "SELECT type FROM public.geometry_columns WHERE f_table_schema='");
                query.append(MAP_SCHEMA);
                query.append("' AND f_table_name='");
                query.append(table);
                query.append("' LIMIT 1;");
                List<Object[]> results = DirectDbQuery.executeQuery(
                        query.toString(), MAP_DB, QueryLanguage.SQL);

                geometryType = (String) results.get(0)[0];
            } catch (Throwable e) {
                statusHandler.handle(Priority.PROBLEM,
                        "Error querying geometry type", e);
            }
        }

        return geometryType;
    }

    /**
     * @return true if the zones in the resource are described by lines.
     */
    protected boolean isLineal() {
        return getGeometryType().endsWith("LINESTRING");
    }

    /**
     * @return true if the zones in the resource are described by polygons.
     */
    protected boolean isPolygonal() {
        return getGeometryType().endsWith("POLYGON");
    }

    /**
     * @return true if the zones in the resource are described by points.
     */
    protected boolean isPuntal() {
        return getGeometryType().endsWith("POINT");
    }

    public void setZoneIdsToShow(String[] zoneIdsToShow) {
        groups.get(SELECTION_GROUP).clear();
        List<String> selectedZones = expandZones(zoneIdsToShow);
        for (String zoneId : selectedZones) {
            updateZone(zoneId, SELECTION_GROUP);
        }
        refreshSelections();
    }

    /**
     * The GeoID of a warning sometimes contains entries of the form NEZ001>003.
     * This is a shorthand for NEZ001, NEZ002, NEZ003. This method takes the
     * input array of strings and creates a List of strings consisting of the
     * expanded zone entries.
     * 
     * @param zoneIdsToShow
     *            A list of zone IDs, possibly containing consecutive-zone
     *            shorthand.
     * @return The input list, with zone shorthand expanded.
     */
    private List<String> expandZones(String[] zoneIdsToShow) {
        List<String> selectedZones = null;
        if (zoneIdsToShow == null) {
            selectedZones = new ArrayList<String>(0);
        } else {
            // A pattern for multi-zone regions.
            Pattern consecZonePat = Pattern
                    .compile("\\s*(\\w{2})(\\w+)(\\d{3})>(\\d{3})\\s*");

            // The number of selected zones will be at least the same length
            // as zoneIdsToShow
            selectedZones = new ArrayList<String>(zoneIdsToShow.length);

            for (String zid : zoneIdsToShow) {

                Matcher csecMatch = consecZonePat.matcher(zid);
                if (csecMatch.matches()) {
                    // Extract the pieces of the zones from the match
                    String st = csecMatch.group(1);
                    String ztype = csecMatch.group(2);
                    String startDigits = csecMatch.group(3);
                    String endDigits = csecMatch.group(4);

                    // Convert the start and end strings to integers
                    int startNum = Integer.valueOf(startDigits);
                    int endNum = Integer.valueOf(endDigits);

                    // Loop from start to end inclusive, generating expanded
                    // zone IDs
                    String expanded = null;
                    for (int zoneNum = startNum; zoneNum <= endNum; zoneNum++) {
                        expanded = String
                                .format("%s%s%03d", st, ztype, zoneNum);
                        selectedZones.add(expanded);
                    }

                } else {
                    selectedZones.add(zid);
                }

            }
        }

        return selectedZones;
    }

    /**
     * Find the list of zones (in allTables, not just the visible ones) that
     * contain the specified coordinate.
     * 
     * @param aLatLon
     *            a Coordinate
     * @return An array of zone ID strings
     */
    public String[] getOverlappingZones(Coordinate aLatLon) {
        Geometry coordGeom = new GeometryFactory().createPoint(aLatLon);
        ArrayList<String> overlappingZones = new ArrayList<String>();
        for (Geometry geom : zoneMap.keySet()) {
            if (geom.contains(coordGeom)) {
                overlappingZones.add(zoneMap.get(geom));
            }
        }
        return overlappingZones.toArray(new String[0]);
    }

    /**
     * Get the zone IDs of all zones that geom overlaps.
     * <p>
     * This method was written to help initialize the map in MakeHazardDialog
     * from a temporary hazard grid. When it runs, paint() may not have been
     * called and/or the database query in paint() may not have completed. We
     * also can't use paint()'s method of setting a "try again" flag and letting
     * the system re-invoke the method. Furthermore, paint() might actually be
     * running, so it could steal our database response if we used the same
     * query job.
     * <p>
     * The net result of the conditions listed above is that this method has to
     * perform its own database query with a local QueryJob and wait for it to
     * complete. A listener that notifies the main thread when the query job
     * completes is attached to the query job, the query job is scheduled, and
     * the main thread waits to be notified. The UI is frozen while we wait, so
     * a timed wait is used. This may cause a null to be returned erroneously if
     * the database connection is slow.
     * <p>
     * The listener is never removed from the job because both are local and go
     * out of scope when the method ends.
     * <p>
     * This method is also used in MakeHazard to get the zones overlapped by the
     * edit area when the user doesn't select any zones in the map.
     * 
     * @param geom
     *            The geometry that defines the area.
     * @param areaThreshold
     *            A fraction (0.0<=threshold<=1.0) that describes how much of
     *            the zone's area must be overlapped by the geometry for its ID
     *            to be in the returned list.
     * @return an array of zone IDs
     */
    public String[] getOverlappingZones(Geometry geom, double areaThreshold) {
        String[] result = null;

        Envelope env = getBoundingEnvelope();
        QueryRequest queryRequest = new QueryRequest();
        queryRequest.query = buildQuery(env, 0.0);
        queryRequest.target = null;
        QueryJob lqj = new QueryJob("Get overlapping zones");
        lqj.requestQueue.clear();
        lqj.requestQueue.add(queryRequest);
        lqj.addJobChangeListener(new JobChangeAdapter() {
            @Override
            public void done(IJobChangeEvent event) {
                synchronized (ZoneDbResource.this) {
                    ZoneDbResource.this.notify();
                }
            }
        });
        synchronized (this) {
            // calling schedule inside synch block ensures wait() sees notify()
            lqj.schedule();
            try {
                wait(25L * 1000L);
            } catch (InterruptedException e) {
                statusHandler
                        .handle(Priority.VERBOSE,
                                "GetOverlappingZones(Geometry): Interrupted during wait().");
            }
        }
        DbData[] rslt = lqj.responseQueue.poll();
        if (rslt == null) {
            // error or slow connection; not sure which
            statusHandler
                    .handle(Priority.VERBOSE,
                            "GetOverlappingZones(Geometry): No database response was received.");
        } else {
            List<String> zoneIDs = new ArrayList<String>(rslt.length);
            Geometry db_geom;
            for (DbData data : rslt) {
                db_geom = null;
                try {
                    db_geom = wkbReader.read(data.wkb);
                    Geometry intersectGeom = db_geom.intersection(geom);
                    if (intersectGeom.getArea() >= db_geom.getArea()
                            * areaThreshold) {
                        zoneIDs.add(data.zone);
                    }
                } catch (ParseException e) {
                    statusHandler.handle(Priority.PROBLEM,
                            "Error parsing database geometry.", e);
                }
            }
            result = zoneIDs.toArray(new String[zoneIDs.size()]);
        }

        return result;
    }

    public String[] getAreaList(Grid2DBit mask, DataManager dm,
            double areaThreshold) {
        String[] result = null;

        QueryRequest queryRequest = new QueryRequest();
        queryRequest.query = buildEAQuery();
        queryRequest.target = null;
        MakeHazardQueryJob lqj = new MakeHazardQueryJob("Get overlapping zones");
        lqj.requestQueue.clear();
        lqj.requestQueue.add(queryRequest);
        lqj.addJobChangeListener(new JobChangeAdapter() {
            @Override
            public void done(IJobChangeEvent event) {
                synchronized (ZoneDbResource.this) {
                    ZoneDbResource.this.notify();
                }
            }
        });
        synchronized (this) {
            // calling schedule inside synch block ensures wait() sees notify()
            lqj.schedule();
            try {
                wait(25L * 1000L);
            } catch (InterruptedException e) {
                statusHandler
                        .handle(Priority.VERBOSE,
                                "GetOverlappingZones(Geometry): Interrupted during wait().");
            }
        }
        MakeHazardDbData[] rslt = lqj.responseQueue.poll();
        if (rslt == null) {
            // error or slow connection; not sure which
            statusHandler
                    .handle(Priority.VERBOSE,
                            "GetOverlappingZones(Geometry): No database response was received.");
        } else {
            List<String> zoneIDs = new ArrayList<String>(rslt.length);
            List<ReferenceID> refIds = new ArrayList<ReferenceID>(rslt.length);
            for (MakeHazardDbData data : rslt) {
                refIds.add(new ReferenceID(data.zone));
            }

            List<ReferenceData> refDataList = dm.getRefManager()
                    .getReferenceData(refIds);
            for (ReferenceData refData : refDataList) {
                Grid2DBit editAreaMask = refData.getGrid();
                Grid2DBit intersect = editAreaMask.and(mask);
                float ratio = (float) intersect.numberOfBitsSet()
                        / (float) editAreaMask.numberOfBitsSet();
                if (ratio >= areaThreshold) {
                    zoneIDs.add(refData.getId().getName());
                }
            }

            result = zoneIDs.toArray(new String[zoneIDs.size()]);
        }

        return result;
    }

    /**
     * Return the group # of the first group that contains no zone IDs, or 1 if
     * no groups have zone IDs or the display type has been set to MAKE_HAZARD
     * or SINGLE_ZONE_ALLOWABLE.
     * 
     * @return A group number
     */
    public int getNextIndex() {

        int index = 0;
        if (!groups.isEmpty()
                && (displayType != DisplayType.SINGLE_ZONE_ALLOWABLE)
                && (displayType != DisplayType.MAKE_HAZARD)) {
            for (index = 0; index < groups.size(); index++) {
                List<String> group = groups.get(index);
                if (group.isEmpty()) {
                    break;
                }
            }
        }
        return index;
    }

    /**
     * Move a list of zone IDs to the group with the designated index.
     * 
     * @param zoneIds
     *            A list of zone IDs to assign to the group.
     * @param index
     *            A group number.
     */
    public void setGroup(List<String> zoneIds, int index) {
        if (index < NO_GROUP) {
            throw new IllegalArgumentException("Illegal zone index " + index);
        }
        for (String zoneId : zoneIds) {
            updateZone(zoneId, index);
        }
    }

    /**
     * Add the listener to the listeners that are called when zones change.
     * 
     * @param listener
     */
    public void addZoneChangeListener(IZoneChangeListener listener) {
        if (!zoneChangeListeners.contains(listener)) {
            zoneChangeListeners.add(listener);
        }
    }

    /**
     * Remove the specified listener from the list of zone change listeners, if
     * it is in the list.
     * 
     * @param listener
     */
    public void removeZoneChangeListener(IZoneChangeListener listener) {
        zoneChangeListeners.remove(listener);
    }

    /**
     * Remove all the zone change listeners.
     */
    public void removeAllZoneChangeListeners() {
        zoneChangeListeners.clear();
    }

    /**
     * Notify listeners that zones have changed.
     */
    public void signalZonesChanged() {
        for (IZoneChangeListener listener : zoneChangeListeners) {
            listener.zoneChanged();
        }
    }

    // public List<String> getEditAreaZones(Float areaThreshold) {
    // IReferenceSetManager refMgr = DataManager.getCurrentInstance()
    // .getRefManager();
    // ReferenceData refData = refMgr.getActiveRefSet();
    // Geometry geom = refData.getPolygons(CoordinateType.GRID);
    // String[] zones = getOverlappingZones(geom, areaThreshold);
    // return null;
    // }

    /**
     * Paint the zone and/or group labels.
     * 
     * @throws VizException
     *             if thrown by paintLabelSet()
     */
    protected void paintLabels() throws VizException {
        // Draw the labels on top of everything else
        if (labelFont == null) {
            int fontNum = 2;
            if (GFEPreference.contains("ZoneMapLabel_font")) {
                fontNum = GFEPreference.getIntPreference("ZoneMapLabel_font");
            }

            labelFont = GFEFonts.getFont(target, fontNum);
        }

        if (labelZones) {
            paintZoneLabels();
        }

        if (getCapability(ZoneGroupableCapability.class).isZoneGrouping()) {
            paintGroupLabels();
        }
    }

    /**
     * Paint the labels for zones.
     * 
     * @throws VizException
     *             if thrown by paintLabelSet()
     */
    protected void paintZoneLabels() throws VizException {
        RGB color = resourceData.getLabelColor();
        GeneralGridGeometry mapGeometry = descriptor.getGridGeometry();
        List<ReferencedCoordinate> refCoordList;
        HorizontalAlignment horiz = HorizontalAlignment.CENTER;
        VerticalAlignment vert = VerticalAlignment.BOTTOM;

        Collection<String> tables = resourceData.getTables();

        if (isPuntal()) {
            horiz = HorizontalAlignment.LEFT;
            vert = VerticalAlignment.TOP;
        }

        // draw zone labels
        List<LabelTuple> alreadyDrawn = new ArrayList<LabelTuple>(
                labelMap.size());
        for (DbData data : dbData) {
            if (tables.contains(data.tableName)) {
                refCoordList = labelMap.get(data.zone);
                if (refCoordList != null) {
                    paintLabelSet(" " + data.zone, labelFont, null, color,
                            horiz, vert, refCoordList, mapGeometry, ""
                                    + data.zone, alreadyDrawn);
                }
            }
        }
    }

    /**
     * Paint the labels for groups.
     * 
     * @throws VizException
     *             if thrown by paintLabelSet()
     */
    protected void paintGroupLabels() throws VizException {
        RGB color = resourceData.getLabelColor();
        GeneralGridGeometry mapGeometry = descriptor.getGridGeometry();
        List<ReferencedCoordinate> refCoordList;
        HorizontalAlignment horiz = HorizontalAlignment.CENTER;
        VerticalAlignment vert = VerticalAlignment.TOP;

        if (isPuntal()) {
            horiz = HorizontalAlignment.LEFT;
            vert = VerticalAlignment.TOP;
        }

        // draw group labels
        List<LabelTuple> alreadyDrawn = new ArrayList<LabelTuple>(
                labelMap.size());
        for (Collection<String> group : groups) {
            int groupNum = groups.indexOf(group);
            if (group != null) {
                for (String zone : group) {
                    refCoordList = labelMap.get(zone);
                    if (refCoordList != null) {
                        paintLabelSet(" " + (groupNum + 1), labelFont,
                                IGraphicsTarget.TextStyle.DROP_SHADOW, color,
                                horiz, vert, refCoordList, mapGeometry, ""
                                        + zone, alreadyDrawn);
                    }
                }
            }
        }
    }

    /**
     * Paint text, with the specified appearance options, at the locations given
     * by refCoordList. The referenced coordinates are converted to screen
     * coordinates with mapGeometry. Labels are filtered according to a distance
     * function; any label whose coordinates are too close to a label that has
     * already been drawn is skipped. As each label is painted, its location is
     * added to the list of labels that have already been drawn.
     * <p>
     * The distance function (abs(xdiff)+abs(ydiff)) is the same one used in
     * AWIPS I.
     * <p>
     * The "too close" value is given by MIN_LABEL_DISTANCE.
     * 
     * @param text
     *            The text to draw
     * @param font
     *            The font in which to draw the text
     * @param style
     *            The style in which to draw the text
     * @param color
     *            The color in which to draw the text
     * @param horiz
     *            The horizontal alignment of the text
     * @param vert
     *            The vertical alignment of the text
     * @param refCoordList
     *            The list of referenced coordinates at which to draw the text
     * @param mapGeometry
     *            The geometry used to convert referenced coordinates to pixel
     *            coordinates
     * @param alreadyDrawn
     *            The pixel coordinates of labels that have already been drawn.
     * @throws VizException
     *             if thrown by target.drawString
     */
    protected void paintLabelSet(String text, IFont font,
            IGraphicsTarget.TextStyle style, RGB color,
            HorizontalAlignment horiz, VerticalAlignment vert,
            List<ReferencedCoordinate> refCoordList,
            GeneralGridGeometry mapGeometry, String zone,
            List<LabelTuple> alreadyDrawn) throws VizException {

        Coordinate coord;
        DrawableString ds = new DrawableString("", color);
        ds.font = font;
        ds.addTextStyle(TextStyle.DROP_SHADOW);
        ds.horizontalAlignment = horiz;
        ds.verticalAlignment = vert;
        refcLoop: for (ReferencedCoordinate refCoord : refCoordList) {
            try {
                coord = refCoord.asPixel(mapGeometry);
                for (LabelTuple old : alreadyDrawn) {
                    if (old.zone.equals(zone) && old.text.equals(text)) {
                        double distance = scaleX * Math.abs(old.x - coord.x)
                                + scaleY * Math.abs(old.y - coord.y);
                        if (distance < MIN_LABEL_DISTANCE) {
                            continue refcLoop;
                        }
                    }
                }
                ds.setText(text, color);
                ds.setCoordinates(coord.x, coord.y);
                target.drawStrings(ds);
                alreadyDrawn.add(new LabelTuple(coord.x, coord.y, text, zone));
            } catch (TransformException e) {
                statusHandler.handle(Priority.PROBLEM,
                        "Error transforming geometry for " + text, e);
            } catch (FactoryException e) {
                statusHandler.handle(Priority.PROBLEM, "FactoryException for "
                        + text, e);
            }
        }
    }

    /**
     * A class for keeping track of which labels have been drawn.
     * 
     * <pre>
     * 
     * SOFTWARE HISTORY
     * 
     * Date         Ticket#    Engineer    Description
     * ------------ ---------- ----------- --------------------------
     * Oct 13, 2010            wldougher     Initial creation
     * 
     * </pre>
     * 
     * @author wldougher
     * @version 1.0
     */
    private class LabelTuple {
        public double x;

        public double y;

        public String text;

        public String zone;

        public LabelTuple(double x, double y, String text, String zone) {
            this.x = x;
            this.y = y;
            this.text = text;
            this.zone = zone;
        }
    }

    // private LinkedList<String> methodNames = new LinkedList<String>();

    // private LinkedList<Long> methodDates = new LinkedList<Long>();

    // private void enter(String methodName) {
    // methodNames.push(methodName);
    // methodDates.push(new Long(System.currentTimeMillis()));
    // log(Priority.VERBOSE, methodName + "() entered.");
    // }

    // private void leave(String methodName) {
    // long now = System.currentTimeMillis();
    // int idx = methodNames.indexOf(methodName);
    // if (idx < 0) {
    // log(Priority.VERBOSE,
    // "leave() called without matching enter() for " + methodName
    // + "().");
    // return;
    // }
    // String name = null;
    // Long stime = null;
    // StringBuilder sb = new StringBuilder();
    // long dur = 0;
    // do {
    // name = methodNames.pop();
    // stime = methodDates.pop();
    // dur = now - stime.longValue();
    // sb.setLength(0);
    // sb.append(name).append("() completed. duration=").append(dur)
    // .append(" ms");
    // if (!name.equals(methodName)) {
    // sb.append(" (unmatched)");
    // }
    // log(Priority.VERBOSE, sb.toString());
    // } while (!name.equals(methodName));
    // }
}
