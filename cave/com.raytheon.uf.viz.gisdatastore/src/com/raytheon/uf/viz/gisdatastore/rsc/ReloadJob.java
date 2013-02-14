package com.raytheon.uf.viz.gisdatastore.rsc;

import java.awt.geom.Rectangle2D;
import java.math.BigDecimal;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Comparator;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Random;
import java.util.concurrent.ArrayBlockingQueue;

import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.core.runtime.jobs.Job;
import org.eclipse.swt.graphics.RGB;
import org.geotools.data.DefaultQuery;
import org.geotools.data.FeatureSource;
import org.geotools.factory.CommonFactoryFinder;
import org.geotools.factory.GeoTools;
import org.geotools.feature.FeatureCollection;
import org.opengis.feature.simple.SimpleFeature;
import org.opengis.feature.simple.SimpleFeatureType;
import org.opengis.filter.Filter;
import org.opengis.filter.FilterFactory2;
import org.opengis.referencing.crs.CoordinateReferenceSystem;

import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.viz.core.DrawableString;
import com.raytheon.uf.viz.core.IGraphicsTarget;
import com.raytheon.uf.viz.core.drawables.IShadedShape;
import com.raytheon.uf.viz.core.drawables.IWireframeShape;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.rsc.capabilities.ColorableCapability;
import com.raytheon.uf.viz.gisdatastore.Activator;
import com.raytheon.uf.viz.gisdatastore.rsc.DataStoreResource.DisplayAttributes;
import com.raytheon.uf.viz.gisdatastore.rsc.DataStoreResource.LabelNode;
import com.raytheon.viz.core.rsc.jts.JTSCompiler;
import com.raytheon.viz.core.rsc.jts.JTSCompiler.PointStyle;
import com.vividsolutions.jts.geom.Geometry;
import com.vividsolutions.jts.geom.GeometryCollection;
import com.vividsolutions.jts.geom.Point;

class ReloadJob extends Job {

    private static final int QUEUE_LIMIT = 1;

    private static Random rand = new Random(System.currentTimeMillis());

    private static int requestCounter = 0;

    public class Request {
        int number;

        IGraphicsTarget target;

        DataStoreResource rsc;

        String geomField;

        String labelField;

        String shadingField;

        Geometry boundingGeom;

        Map<Object, RGB> colorMap;

        boolean isProduct;

        boolean highlightsOnly;

        Request(IGraphicsTarget target, DataStoreResource rsc,
                Geometry boundingGeom, String geomField, String labelField,
                String shadingField, Map<Object, RGB> colorMap,
                boolean isProduct, boolean highlightsOnly) {
            this.number = requestCounter++;
            this.target = target;
            this.rsc = rsc;
            this.boundingGeom = boundingGeom;
            this.geomField = geomField;
            this.labelField = labelField;
            this.shadingField = shadingField;
            this.colorMap = colorMap;
            this.isProduct = isProduct;
            this.highlightsOnly = highlightsOnly;
        }

        RGB getColor(Object key) {
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
    }

    public class Result {
        public IWireframeShape outlineShape;

        public List<LabelNode> labels;

        public IShadedShape shadedShape;

        public Map<Object, RGB> colorMap;

        public boolean failed;

        public Throwable cause;

        public IWireframeShape highlightShape;

        public boolean highlightsOnly;

        private Result() {
            failed = true;
        }

        public void dispose() {
            if (outlineShape != null) {
                outlineShape.dispose();
                outlineShape = null;
            }

            if (shadedShape != null) {
                shadedShape.dispose();
                shadedShape = null;
            }

            if (highlightShape != null) {
                highlightShape.dispose();
                highlightShape = null;
            }
        }
    }

    private Request pendingRequest = null;

    private ArrayBlockingQueue<Result> resultQueue = new ArrayBlockingQueue<Result>(
            QUEUE_LIMIT);

    public ReloadJob() {
        super("Loading ...");
    }

    public void request(IGraphicsTarget target, DataStoreResource rsc,
            Geometry boundingGeom, String geomField, String labelField,
            String shadingField, Map<Object, RGB> colorMap, boolean isProduct,
            boolean highlightsOnly) {

        synchronized (this) {
            pendingRequest = new Request(target, rsc, boundingGeom, geomField,
                    labelField, shadingField, colorMap, isProduct,
                    highlightsOnly);
        }

        this.schedule();
    }

    public Result getLatestResult() {
        return resultQueue.poll();
    }

    /*
     * (non-Javadoc)
     * 
     * @see org.eclipse.core.runtime.jobs.Job#run(org.eclipse.core.runtime.
     * IProgressMonitor)
     */
    @Override
    protected IStatus run(IProgressMonitor monitor) {
        Request req = null;
        synchronized (this) {
            req = pendingRequest;
            pendingRequest = null;
        }
        while (req != null) {
            System.out.println("Processing request: " + req.number);

            Result result = new Result();
            FeatureCollection<SimpleFeatureType, SimpleFeature> featureCollection = null;
            Iterator<SimpleFeature> featureIterator = null;
            try {
                if (pendingRequest != null) {
                    System.out.println("Canceling request: " + req.number);
                    result.dispose();
                    result = null;
                    return Status.CANCEL_STATUS;
                }

                List<String> fields = new ArrayList<String>();
                fields.add(req.geomField);
                if (req.labelField != null && !fields.contains(req.labelField)) {
                    fields.add(req.labelField);
                }
                if (req.shadingField != null
                        && !fields.contains(req.shadingField)) {
                    fields.add(req.shadingField);
                }

                IWireframeShape newOutlineShape = req.target
                        .createWireframeShape(false, req.rsc.getDescriptor());

                IWireframeShape newHighlightShape = req.target
                        .createWireframeShape(false, req.rsc.getDescriptor());

                List<LabelNode> newLabels = new ArrayList<LabelNode>();

                IShadedShape newShadedShape = null;
                if (req.isProduct || req.shadingField != null) {
                    newShadedShape = req.target.createShadedShape(false,
                            req.rsc.getDescriptor().getGridGeometry(), true);
                }

                // TODO: pass the crs on to JTSCompiler to indicate
                // projection of shapefile data.
                SimpleFeatureType schema = req.rsc.getSchema();
                CoordinateReferenceSystem crs = schema.getGeometryDescriptor()
                        .getCoordinateReferenceSystem();

                JTSCompiler jtsCompiler = new JTSCompiler(newShadedShape,
                        newOutlineShape, req.rsc.getDescriptor(),
                        PointStyle.CROSS);

                JTSCompiler highlightCompiler = new JTSCompiler(null,
                        newHighlightShape, req.rsc.getDescriptor(),
                        PointStyle.CROSS);

                String shapeField = schema.getGeometryDescriptor()
                        .getLocalName();

                DefaultQuery query = new DefaultQuery();

                String typeName = req.rsc.getTypeName();
                query.setTypeName(typeName);
                query.setPropertyNames(fields);

                if (req.boundingGeom != null) {
                    FilterFactory2 ff = CommonFactoryFinder
                            .getFilterFactory2(GeoTools.getDefaultHints());

                    List<Geometry> geomList = new ArrayList<Geometry>();
                    flattenGeometry(req.boundingGeom, geomList);

                    List<Filter> filterList = new ArrayList<Filter>(
                            geomList.size());
                    for (Geometry g : geomList) {
                        Filter filter = ff.intersects(ff.property(shapeField),
                                ff.literal(g));
                        filterList.add(filter);
                    }
                    query.setFilter(ff.or(filterList));
                }

                FeatureSource<SimpleFeatureType, SimpleFeature> featureSource = req.rsc
                        .getDataStore().getFeatureSource(typeName);

                featureCollection = featureSource.getFeatures(query);
                featureIterator = featureCollection.iterator();

                // TODO: do we need to implement the GeometryCache/gidMap
                // stuff like in DbMapResource?

                List<Geometry> resultingGeoms = new ArrayList<Geometry>();
                List<Geometry> highlightGeoms = new ArrayList<Geometry>();
                int numPoints = 0;
                while (featureIterator.hasNext()) {
                    if (pendingRequest != null) {
                        System.out.println("Canceling request: " + req.number);
                        result.dispose();
                        result = null;
                        return Status.CANCEL_STATUS;
                    }

                    SimpleFeature f = featureIterator.next();
                    String id = f.getID();
                    DisplayAttributes da = req.rsc.getDisplayAttributes(id);
                    if (!da.isVisible()) {
                        continue;
                    }

                    Geometry g = (Geometry) f.getAttribute(req.geomField);
                    if (da.isHighlighted()) {
                        highlightGeoms.add((Geometry) g.clone());
                    }

                    if (req.highlightsOnly) {
                        continue;
                    }

                    Object labelAttr = null;
                    Object shadingAttr = null;
                    for (String name : fields) {
                        if (name.equals(req.labelField)) {
                            labelAttr = f.getAttribute(name);
                        }
                        if (name.equals(req.shadingField)) {
                            shadingAttr = f.getAttribute(name);
                        }
                    }

                    if (labelAttr != null && g != null) {
                        String label;
                        if (labelAttr instanceof BigDecimal) {
                            label = Double.toString(((Number) labelAttr)
                                    .doubleValue());
                        } else {
                            label = labelAttr.toString();
                        }
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
                                return (int) Math
                                        .signum(g2.getEnvelope().getArea()
                                                - g1.getEnvelope().getArea());
                            }
                        });

                        for (Geometry poly : gList) {
                            Point point = poly.getInteriorPoint();
                            if (point.getCoordinate() != null) {
                                double[] location = req.rsc
                                        .getDescriptor()
                                        .worldToPixel(
                                                new double[] {
                                                        point.getCoordinate().x,
                                                        point.getCoordinate().y });

                                DrawableString ds = new DrawableString(label,
                                        null);
                                ds.font = req.rsc.font;
                                Rectangle2D rect = req.target
                                        .getStringsBounds(ds);

                                LabelNode node = new LabelNode(label, location,
                                        rect);
                                newLabels.add(node);
                            }
                        }
                    }

                    if (g != null) {
                        numPoints += g.getNumPoints();
                        resultingGeoms.add(g);
                        if (req.shadingField != null) {
                            g.setUserData(shadingAttr);
                        }
                    }
                }

                newOutlineShape.allocate(numPoints);
                RGB outlineColor = req.rsc.getCapability(
                        ColorableCapability.class).getColor();
                for (Geometry g : resultingGeoms) {
                    if (pendingRequest != null) {
                        System.out.println("Canceling request: " + req.number);
                        result.dispose();
                        result = null;
                        return Status.CANCEL_STATUS;
                    }

                    RGB color = null;
                    Object shadedField = g.getUserData();
                    if (shadedField != null) {
                        color = req.getColor(shadedField);
                    } else {
                        color = outlineColor;
                    }

                    try {
                        jtsCompiler.handle(g, color, true);
                    } catch (VizException e) {
                        Activator.statusHandler.handle(Priority.PROBLEM,
                                "Error reprojecting map outline", e);
                    }
                }

                newOutlineShape.compile();

                if (req.isProduct || req.shadingField != null) {
                    newShadedShape.compile();
                }

                for (Geometry g : highlightGeoms) {
                    if (pendingRequest != null) {
                        System.out.println("Canceling request: " + req.number);
                        result.dispose();
                        result = null;
                        return Status.CANCEL_STATUS;
                    }

                    try {
                        highlightCompiler.handle(g, true);
                    } catch (VizException e) {
                        Activator.statusHandler.handle(Priority.PROBLEM,
                                "Error reprojecting map outline", e);
                    }
                }

                // uncomment to see boungingGeom as highlight for debug purposes
                // highlightCompiler.handle(req.boundingGeom, true);

                newHighlightShape.compile();

                result.outlineShape = newOutlineShape;
                result.labels = newLabels;
                result.shadedShape = newShadedShape;
                result.colorMap = req.colorMap;
                result.highlightShape = newHighlightShape;
                result.highlightsOnly = req.highlightsOnly;
                result.failed = false;
            } catch (Throwable e) {
                result.cause = e;
            } finally {
                if (featureIterator != null) {
                    featureCollection.close(featureIterator);
                }
                if (result != null) {
                    System.out.println("Completed request: " + req.number);
                    if (resultQueue.size() == QUEUE_LIMIT) {
                        resultQueue.poll();
                    }
                    resultQueue.add(result);
                    req.rsc.issueRefresh();
                }
            }

            synchronized (this) {
                req = pendingRequest;
                pendingRequest = null;
            }
        }

        return Status.OK_STATUS;
    }

    /**
     * Recursively traverse a Geometry and expand all geometry collections into
     * a list of geometries
     * 
     * @param geometry
     *            the geometry to flatten
     * @param geomList
     *            the list of geometries
     */
    private void flattenGeometry(Geometry geometry, List<Geometry> geomList) {
        if (geometry instanceof GeometryCollection) {
            GeometryCollection collection = (GeometryCollection) geometry;
            for (int i = 0; i < collection.getNumGeometries(); i++) {
                flattenGeometry(collection.getGeometryN(i), geomList);
            }
        } else {
            geomList.add(geometry);
        }
    }
}