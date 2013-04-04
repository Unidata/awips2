package gov.noaa.nws.ncep.viz.overlays.resources;


import gov.noaa.nws.ncep.viz.common.dbQuery.NcDirectDbQuery;
import gov.noaa.nws.ncep.viz.resources.INatlCntrsResource;

import java.text.DecimalFormat;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Random;
import java.util.concurrent.ArrayBlockingQueue;

import org.apache.commons.lang.StringUtils;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.core.runtime.jobs.Job;
import org.eclipse.swt.graphics.RGB;
import org.geotools.geometry.jts.ReferencedEnvelope;
import org.opengis.referencing.crs.CoordinateReferenceSystem;

import com.raytheon.uf.common.geospatial.MapUtil;
import com.raytheon.uf.common.geospatial.ReferencedCoordinate;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.viz.core.IGraphicsTarget;
import com.raytheon.uf.viz.core.IGraphicsTarget.HorizontalAlignment;
import com.raytheon.uf.viz.core.IGraphicsTarget.VerticalAlignment;
import com.raytheon.uf.viz.core.PixelExtent;
import com.raytheon.uf.viz.core.catalog.DirectDbQuery.QueryLanguage;
import com.raytheon.uf.viz.core.drawables.IFont;
import com.raytheon.uf.viz.core.drawables.IShadedShape;
import com.raytheon.uf.viz.core.drawables.IWireframeShape;
import com.raytheon.uf.viz.core.drawables.PaintProperties;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.map.IMapDescriptor;
import com.raytheon.uf.viz.core.map.MapDescriptor;
import com.raytheon.uf.viz.core.rsc.AbstractVizResource;
import com.raytheon.uf.viz.core.rsc.LoadProperties;
import com.raytheon.uf.viz.core.rsc.capabilities.ColorableCapability;
import com.raytheon.viz.core.rsc.jts.JTSCompiler;
import com.raytheon.viz.core.rsc.jts.JTSCompiler.PointStyle;
import com.vividsolutions.jts.geom.Coordinate;
import com.vividsolutions.jts.geom.Envelope;
import com.vividsolutions.jts.geom.Geometry;
import com.vividsolutions.jts.geom.Point;
import com.vividsolutions.jts.io.WKBReader;

/**
 * TODO Add Description
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * July 20, 2009           mgao        Initial creation
 * July 30, 2009           ghull       implement INatlCntrsResource
 * Nov 18,  2009           ghull       Incorporate to11d6 changes 
 * Oct 19, 2012  898       sgurung     Fix for fuzzy fonts
 * 
 * </pre>
 * 
 * This class is copied over from com.raytheon.viz.core.rsc.DbMapResource
 * 
 * @author mgao
 * @version 1.0
 */
public class DbOverlayResource extends
    AbstractVizResource<DbOverlayResourceData, MapDescriptor> implements INatlCntrsResource {
    private static final transient IUFStatusHandler statusHandler = UFStatus.getHandler(DbOverlayResource.class);

	private static class LabelNode {
		private final String label;

        private final ReferencedCoordinate location;

        LabelNode(String label, Point c) {
            this.label = label;
            this.location = new ReferencedCoordinate(c.getCoordinate());
        }

        /**
         * @return the label
         */
        public String getLabel() {
            return label;
        }

        /**
         * @return the location
         */
        public ReferencedCoordinate getLocation() {
            return location;
        }
    }

    private static class MapQueryJob extends Job {

        private static final int QUEUE_LIMIT = 1;
        private String dbName=null;//uma
        
        private static class Request {
            static Random rand = new Random(System.currentTimeMillis());

            IGraphicsTarget target;

            IMapDescriptor descriptor;

            boolean labeled;

            boolean shaded;

            String query;

            Map<Object, RGB> colorMap;

            Request(IGraphicsTarget target, IMapDescriptor descriptor,
                    String query, boolean labeled, boolean shaded,
                    Map<Object, RGB> colorMap) {
                this.target = target;
                this.descriptor = descriptor;
                this.query = query;
                this.labeled = labeled;
                this.shaded = shaded;
                this.colorMap = colorMap;
            }

            RGB getColor(Object key) {
                if (colorMap == null) {
                    colorMap = new HashMap<Object, RGB>();
                }
                RGB color = colorMap.get(key);
                if (color == null) {
               		color = new RGB(rand.nextInt(206) + 50,
                            rand.nextInt(206) + 50, rand.nextInt(206) + 50);
                    colorMap.put(key, color);
                }

                return color;
            }
        }

        public static class Result {
            public IWireframeShape outlineShape;

            public List<LabelNode> labels;

            public IShadedShape shadedShape;

            public Map<Object, RGB> colorMap;

            private Result(IWireframeShape outlineShape,
                    List<LabelNode> labels, IShadedShape shadedShape,
                    Map<Object, RGB> colorMap) {
                this.outlineShape = outlineShape;
                this.labels = labels;
                this.shadedShape = shadedShape;
                this.colorMap = colorMap;
            }
        }

        private ArrayBlockingQueue<Request> requestQueue = new ArrayBlockingQueue<Request>(
                QUEUE_LIMIT);

        private ArrayBlockingQueue<Result> resultQueue = new ArrayBlockingQueue<Result>(
                QUEUE_LIMIT);

        private boolean canceled;

        public MapQueryJob( String dbname ) {//uma
            super("Retrieving map...");
            dbName = dbname;//uma
        }

        public void request(IGraphicsTarget target, IMapDescriptor descriptor,
                String query, boolean labeled, boolean shaded,
                Map<Object, RGB> colorMap) {
            if (requestQueue.size() == QUEUE_LIMIT) {
                requestQueue.poll();
            }
            requestQueue.add(new Request(target, descriptor, query, labeled,
                    shaded, colorMap));

            this.cancel();
            this.schedule();
        }

        public Result getLatestResult() {
            return resultQueue.poll();
        }

        public int numRequests() {
            return requestQueue.size();
        }

        public int numResults() {
            return resultQueue.size();
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
                try {
                    // System.out.println(req.query);
                    // long t0 = System.currentTimeMillis();
                 /*   List<Object[]> results = NcDirectDbQuery.executeQuery(
                            req.query, "maps", QueryLanguage.SQL);*/ //uma
                	 List<Object[]> results = NcDirectDbQuery.executeQuery(
                             req.query, dbName, QueryLanguage.SQL);
                	 
                    // long t1 = System.currentTimeMillis();
                    // System.out.println("Maps DB query took: " + (t1 - t0)
                    // + "ms");

                    IWireframeShape newOutlineShape = req.target
                            .createWireframeShape(false, req.descriptor, 0.0f);

                    List<LabelNode> newLabels = new ArrayList<LabelNode>();

                    IShadedShape newShadedShape = null;
                    if (req.shaded) {
                        newShadedShape = req.target.createShadedShape(false,
                                req.descriptor, true);
                    }

                    JTSCompiler jtsCompiler = new JTSCompiler(newShadedShape,
                            newOutlineShape, req.descriptor, PointStyle.CROSS);

                    WKBReader wkbReader = new WKBReader();
                    for (Object[] result : results) {
                        if (canceled) {
                            canceled = false;
                            // System.out.println("MapQueryJob Canceled.");
                            return Status.CANCEL_STATUS;
                        }
                        int i = 0;
                        byte[] wkb = (byte[]) result[i++];
                        Geometry g = wkbReader.read(wkb);
                        // System.out.println(name + ": " + g.toText());

                        if (req.labeled && result[i] != null && g != null) {
                            LabelNode node = new LabelNode(result[i++]
                                    .toString(), g.getCentroid());

                            newLabels.add(node);
                        }

                        if (!(g instanceof Point)) {
                            RGB color = null;
                            if (req.shaded && result[i] != null) {
                                Object shadedField = result[i++];
                                color = req.getColor(shadedField);
                            }
                            try {
                                jtsCompiler.handle(g, color);
                            } catch (VizException e) {
//                                UFStatus.handle(Priority.PROBLEM, Activator.PLUGIN_ID,
//                                        StatusConstants.CATEGORY_WORKSTATION,
//                                        null, "Error reprojecting map outline", e);
                            	System.out.println("Error reprojecting map outline:"+e.getMessage());
                            }
                        }
                    }

                    newOutlineShape.compile();

                    if (req.shaded) {
                        newShadedShape.compile();
                    }

                    if (resultQueue.size() == QUEUE_LIMIT) {
                        resultQueue.poll();
                    }
                    resultQueue.add(new Result(newOutlineShape, newLabels,
                            newShadedShape, req.colorMap));
                    req.target.setNeedsRefresh(true);

                    // long t2 = System.currentTimeMillis();
                    // System.out.println("Wireframe construction took: "
                    // + (t2 - t1) + "ms");
                    // System.out.println("Total map retrieval took: " + (t2 -
                    // t0)
                    // + "ms");
                } catch (Exception e) {
                    statusHandler.handle(Priority.PROBLEM,
                            "Error processing map query rquest", e);
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

    private static final double EXPANSION_FACTOR = 0.25;

    private IWireframeShape outlineShape;

    private List<LabelNode> labels;

    private IShadedShape shadedShape;

    private Map<Object, RGB> colorMap;

    private double[] levels;

    private PixelExtent lastExtent;

    private double lastSimpLev;

    private boolean lastLabeled;

    private boolean lastShaded;

    private MapQueryJob queryJob;

    private String geometryType;
    
    private DbOverlayResourceData ncRscData;

    public DbOverlayResource(DbOverlayResourceData data, LoadProperties loadProperties) {
        super(data, loadProperties);
        ncRscData = this.resourceData;
        queryJob = new MapQueryJob(resourceData.getDbName());//uma
    }

    @Override
    public void disposeInternal() {
        if (outlineShape != null) {
            outlineShape.dispose();
        }

        if (shadedShape != null) {
            shadedShape.dispose();
        }
        lastExtent = null;
    }

    @Override
    public void initInternal(IGraphicsTarget target) throws VizException {
    }

    private String buildQuery(IGraphicsTarget target, PixelExtent extent,
            double simpLev) throws VizException {

        Envelope env = null;
        try {
            Envelope e = descriptor.pixelToWorld(extent, descriptor.getCRS());
            ReferencedEnvelope ref = new ReferencedEnvelope(e, descriptor
                    .getCRS());

            if ( descriptor.getCRS().getName().toString().startsWith("MCIDAS") ) {
                env = new Envelope(-180., 180., -90., 90.);
            }
            else {
            	env = ref.transform(MapUtil.LATLON_PROJECTION, true);
            }
        } catch (Exception e) {
            throw new VizException("Error transforming extent", e);
        }

        DecimalFormat df = new DecimalFormat("0.######");
        String suffix = "_"
                + StringUtils.replaceChars(df.format(simpLev), '.', '_');

        String geometryField = resourceData.getGeomField() + suffix;

        if( env.getMinX() == Double.NaN || 
            env.getMaxX() == Double.NaN ||
            env.getMinY() == Double.NaN ||
            env.getMaxY() == Double.NaN ) {
            System.out.println("Extents is not valid for DB Overlay query");
        }
        // create the geospatial constraint from the envelope
        String geoConstraint = String.format(
                "%s && ST_SetSrid('BOX3D(%f %f, %f %f)'::box3d,4326)",
                geometryField, env.getMinX(), env.getMinY(), env.getMaxX(), env
                        .getMaxY());

        // get the geometry field
        StringBuilder query = new StringBuilder("SELECT AsBinary(");
        query.append(geometryField);
        query.append(")");

        // add the label field
        if (resourceData.getLabelField() != null) {
            query.append(", ");
            query.append(resourceData.getLabelField());
        }

        // add the shading field
        if (resourceData.getShadingField() != null) {
            query.append(", ");
            query.append(resourceData.getShadingField());
        }

        // add any additional columns
        if (resourceData.getColumns() != null) {
            for (String column : resourceData.getColumns()) {
                query.append(", ");
                query.append(column);
            }
        }

        // add the geometry table
        query.append(" FROM ");
        query.append(resourceData.getTables()[0]);

        // add any additional tables
        if (resourceData.getTables().length > 1) {
            for (int i = 1; i < resourceData.getTables().length; i++) {
                query.append(", ");
                query.append(resourceData.getTables()[i]);
            }
        }

        // add the geo constraint
        query.append(" WHERE ");
        query.append(geoConstraint);

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
     * @param screenExtent
     * @return
     */
    protected PixelExtent getExpandedExtent(PixelExtent screenExtent) {
        PixelExtent expandedExtent = screenExtent.clone();
        expandedExtent.getEnvelope().expandBy(
                expandedExtent.getWidth() * EXPANSION_FACTOR,
                expandedExtent.getHeight() * EXPANSION_FACTOR);
        return expandedExtent;
    }

    /**
     * @param dpp
     * @return
     */
    protected double getSimpLev(double dpp) {
        double simpLev = 0;
        for (double level : getLevels()) {
            if (dpp < level) {
                break;
            }
            simpLev = level;
        }
        // System.out.println("dpp: " + dpp + ", simpLev: " + simpLev);
        return simpLev;
    }

    @Override
    public void paintInternal(IGraphicsTarget aTarget, PaintProperties paintProps)
            throws VizException {
        PixelExtent screenExtent = (PixelExtent) paintProps.getView()
                .getExtent();

        // compute an estimate of degrees per pixel
        double yc = screenExtent.getCenter()[1];
        double x1 = screenExtent.getMinX();
        double x2 = screenExtent.getMaxX();
        double[] c1 = descriptor.pixelToWorld(new double[] { x1, yc });
        double[] c2 = descriptor.pixelToWorld(new double[] { x2, yc });
        double simpLev;
        
        if ( (c1 != null) && (c2 != null) ) {
        	int screenWidth = paintProps.getCanvasBounds().width;
        	double dppX = Math.abs(c2[0] - c1[0]) / screenWidth;

        	simpLev = getSimpLev(dppX);
        }
        else {
        	simpLev = lastSimpLev;
        }

        boolean isLabeled = resourceData.getLabelField() != null
        	&& resourceData.isLabeled(); 
        boolean isShaded = isPolygonal()
        	&& resourceData.isShaded(); 

        if (simpLev < lastSimpLev
                || (isLabeled && !lastLabeled)
                || (isShaded && !lastShaded)
                || lastExtent == null
                || !lastExtent.getEnvelope().contains(
                        screenExtent.getEnvelope())) {
        	
            if (!paintProps.isZooming()) {
                PixelExtent expandedExtent = getExpandedExtent(screenExtent);
                String query = buildQuery(aTarget, expandedExtent, simpLev);
                
                queryJob.request(aTarget, descriptor, query, isLabeled,
                        isShaded, colorMap);
                lastExtent = expandedExtent;
                lastSimpLev = simpLev;
                lastLabeled = isLabeled;
                lastShaded = isShaded;
            }
        }

        MapQueryJob.Result result = queryJob.getLatestResult();
        if (result != null) {
            if (outlineShape != null) {
                outlineShape.dispose();
            }

            if (shadedShape != null) {
                shadedShape.dispose();
            }
            outlineShape = result.outlineShape;
            labels = result.labels;
            shadedShape = result.shadedShape;
            colorMap = result.colorMap;
        }

        float alpha = paintProps.getAlpha();

        if (shadedShape != null && shadedShape.isDrawable() && isShaded) {
            aTarget.drawShadedShape(shadedShape, alpha);
        }

        if (outlineShape != null && outlineShape.isDrawable()
                && ncRscData.isOutlineOn() ) {
            aTarget.drawWireframeShape(outlineShape, 
            		ncRscData.getColor(),  
                    ncRscData.getLineWidth(),
                    ncRscData.getLineStyle() );

        } else if (outlineShape == null
        		&& ncRscData.isOutlineOn() ) {
            aTarget.setNeedsRefresh(true);
        }

        if (labels != null && isLabeled) {
            IFont font = aTarget.initializeFont(aTarget.getDefaultFont()
                    .getFontName(), 10, null);
			font.setSmoothing(false);
			font.setScaleFont(false);

            for (LabelNode node : labels) {
                try {
                    Coordinate c = node.location.asPixel(descriptor
                            .getGridGeometry());

                    aTarget.drawString(font, node.label, c.x, c.y, 0.0,
                            IGraphicsTarget.TextStyle.NORMAL, getCapability(
                                    ColorableCapability.class).getColor(),
                            HorizontalAlignment.CENTER,
                            VerticalAlignment.MIDDLE, null);
                } catch (Exception e) {
                    throw new VizException("Unable to transform", e);
                }

            }
            font.dispose();
        }
    }

    @Override
    public void project(CoordinateReferenceSystem mapData) throws VizException {

    	if (this.outlineShape != null) {
            outlineShape.dispose();
            this.outlineShape = null;
        }

        if (this.shadedShape != null) {
            shadedShape.dispose();
            this.shadedShape = null;
        }

        lastExtent = null;
    }

    /**
     * @return the levels
     */
    protected double[] getLevels() {
        if (levels == null) {
            try {
                int p = resourceData.getTables()[0].indexOf('.');
                String schema = resourceData.getTables()[0].substring(0, p);
                String table = resourceData.getTables()[0].substring(p + 1);
                StringBuilder query = new StringBuilder(
                        "SELECT f_geometry_column FROM public.geometry_columns WHERE f_table_schema='");
                query.append(schema);
                query.append("' AND f_table_name='");
                query.append(table);
                query.append("' AND f_geometry_column LIKE '");
                query.append(resourceData.getGeomField());
                query.append("_%';");
                /*List<Object[]> results = NcDirectDbQuery.executeQuery(query
                        .toString(), "maps", QueryLanguage.SQL);/*/                    //uma
                List<Object[]> results = NcDirectDbQuery.executeQuery(query
                        .toString(),resourceData.getDbName(), QueryLanguage.SQL);

                levels = new double[results.size()];
                int i = 0;
                for (Object[] obj : results) {
                    String s = ((String) obj[0]).substring(
                            resourceData.getGeomField().length() + 1).replace(
                            '_', '.');
                    levels[i++] = Double.parseDouble(s);
                }
                Arrays.sort(levels);
            } catch (VizException e) {
                statusHandler.handle(Priority.PROBLEM,
                        "Error querying available levels", e);
            }

        }
        return levels;
    }

    protected String getGeometryType() {
        if (geometryType == null) {
            try {
                int p = resourceData.getTables()[0].indexOf('.');
                String schema = resourceData.getTables()[0].substring(0, p);
                String table = resourceData.getTables()[0].substring(p + 1);
                StringBuilder query = new StringBuilder(
                        "SELECT type FROM geometry_columns WHERE f_table_schema='");
                query.append(schema);
                query.append("' AND f_table_name='");
                query.append(table);
                query.append("' LIMIT 1;");
               /* List<Object[]> results = NcDirectDbQuery.executeQuery(query
                        .toString(), "maps", QueryLanguage.SQL);*/    //uma
                List<Object[]> results = NcDirectDbQuery.executeQuery(query
                        .toString(), resourceData.getDbName(), QueryLanguage.SQL);
                
                
                geometryType = (String) results.get(0)[0];
            } catch (Throwable e) {
                statusHandler.handle(Priority.PROBLEM,
                        "Error querying geometry type", e);
            }
        }

        return geometryType;
    }

    protected boolean isLineal() {
        return getGeometryType().endsWith("LINESTRING");
    }

    protected boolean isPolygonal() {
        return getGeometryType().endsWith("POLYGON");
    }

    /*
     * (non-Javadoc)
     * 
     * @see java.lang.Object#toString()
     */
    @Override
    public String toString() {
        return this.resourceData.toString();
    }

	@Override
	public void resourceAttrsModified() {
		// Nothing to do. The modified color, linewidth.... will be picked up on the next paint
	}
}
