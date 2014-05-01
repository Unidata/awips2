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

package com.raytheon.viz.shapefile.rsc;

import java.io.File;
import java.io.IOException;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Random;
import java.util.Set;

import org.apache.commons.lang.StringUtils;
import org.eclipse.swt.graphics.RGB;
import org.geotools.coverage.grid.GeneralGridGeometry;
import org.geotools.data.DefaultQuery;
import org.geotools.data.shapefile.ShapefileDataStore;
import org.geotools.data.shapefile.indexed.IndexType;
import org.geotools.data.shapefile.indexed.IndexedShapefileDataStore;
import org.geotools.factory.CommonFactoryFinder;
import org.geotools.factory.GeoTools;
import org.geotools.feature.FeatureIterator;
import org.geotools.geometry.jts.ReferencedEnvelope;
import org.opengis.feature.IllegalAttributeException;
import org.opengis.feature.simple.SimpleFeature;
import org.opengis.filter.Filter;
import org.opengis.filter.FilterFactory2;
import org.opengis.filter.expression.Expression;
import org.opengis.referencing.crs.CoordinateReferenceSystem;

import com.raytheon.uf.common.geospatial.MapUtil;
import com.raytheon.uf.common.geospatial.ReferencedCoordinate;
import com.raytheon.uf.viz.core.IGraphicsTarget;
import com.raytheon.uf.viz.core.IGraphicsTarget.HorizontalAlignment;
import com.raytheon.uf.viz.core.PixelExtent;
import com.raytheon.uf.viz.core.datastructure.WireframeCache;
import com.raytheon.uf.viz.core.drawables.IFont;
import com.raytheon.uf.viz.core.drawables.IShadedShape;
import com.raytheon.uf.viz.core.drawables.IWireframeShape;
import com.raytheon.uf.viz.core.drawables.PaintProperties;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.map.MapDescriptor;
import com.raytheon.uf.viz.core.maps.rsc.AbstractMapResource;
import com.raytheon.uf.viz.core.rsc.LoadProperties;
import com.raytheon.uf.viz.core.rsc.capabilities.ColorableCapability;
import com.raytheon.uf.viz.core.rsc.capabilities.LabelableCapability;
import com.raytheon.uf.viz.core.rsc.capabilities.OutlineCapability;
import com.raytheon.uf.viz.core.rsc.capabilities.ShadeableCapability;
import com.raytheon.viz.core.rsc.jts.JTSCompiler;
import com.vividsolutions.jts.geom.Coordinate;
import com.vividsolutions.jts.geom.Envelope;
import com.vividsolutions.jts.geom.Geometry;
import com.vividsolutions.jts.geom.GeometryFactory;
import com.vividsolutions.jts.geom.MultiPoint;
import com.vividsolutions.jts.geom.Point;
import com.vividsolutions.jts.geom.Polygonal;
import com.vividsolutions.jts.index.strtree.STRtree;

/**
 * Shapefile resource
 * 
 * Provides shapefile rendering support
 * 
 * <pre>
 * 
 *      SOFTWARE HISTORY
 *     
 *      Date            Ticket#     Engineer    Description
 *      ------------	----------	-----------	--------------------------
 *      7/1/06                      chammack    Initial Creation.
 *      02/11/09                    njensen     Refactored to new rsc architecture
 * 
 * </pre>
 * 
 * @author chammack
 * 
 * 
 */
public class ShapefileResource extends
        AbstractMapResource<ShapefileResourceData, MapDescriptor> {

    protected ShapefileResource(ShapefileResourceData resourceData,
            LoadProperties props) {
        super(resourceData, props);
    }

    protected static final double INQUIRY_TOLERANCE = 0.001;

    protected static final double POINT_INQUIRY_TOLERANCE = 0.01;

    protected String shapeField = null;

    protected IndexedShapefileDataStore dataStore;

    protected GeneralGridGeometry initializedGridGeometry;

    protected IWireframeShape outlineShape;

    protected IShadedShape shadedShape;

    protected Map<Object, RGB> colorMap;

    protected boolean isShadedCalculated;

    protected String colorMapAttribute;

    protected float simplificationLevel;

    protected String[] theShapefileAttributes;

    protected boolean isRegularPolygon;

    protected boolean isSpatiallySplit;

    protected STRtree rTree;

    protected IGraphicsTarget lastTarget;

    protected IFont font;

    private static final int MAX_DISPLAYED_LABELS = 150;

    protected class LabelNode {
        public String label;

        public Coordinate c;
    }

    /** *********************** Move colormaps to better location **** */
    static final int[] rgbRed = { 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
            0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 7, 23, 39, 55, 71, 87, 103, 119, 135,
            151, 167, 183, 199, 215, 231, 247, 255, 255, 255, 255, 255, 255,
            255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 246, 228, 211,
            193, 175, 158, 140 };

    static final int[] rgbGreen = { 0, 0, 0, 0, 0, 0, 0, 0, 0, 11, 27, 43, 59,
            75, 91, 107, 123, 139, 155, 171, 187, 203, 219, 235, 251, 255, 255,
            255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255,
            255, 247, 231, 215, 199, 183, 167, 151, 135, 119, 103, 87, 71, 55,
            39, 23, 7, 0, 0, 0, 0, 0, 0, 0 };

    static final int[] rgbBlue = { 0, 143, 159, 175, 191, 207, 223, 239, 255,
            255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255,
            255, 255, 255, 247, 231, 215, 199, 183, 167, 151, 135, 119, 103,
            87, 71, 55, 39, 23, 7, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
            0, 0, 0, 0, 0, 0, 0, 0 };

    /*
     * (non-Javadoc)
     * 
     * @seecom.raytheon.viz.core.rsc.IVizResource#init(com.raytheon.viz.core.
     * IGraphicsTarget)
     */
    @Override
    protected void initInternal(IGraphicsTarget target) throws VizException {
        super.initInternal(target);
        getCapability(LabelableCapability.class).setAvailableLabelFields(
                resourceData.getLabelFields());

        loadSDS();

        if (colorMapAttribute != null) {
            getCapability(ShadeableCapability.class).setShadingField(
                    colorMapAttribute);
        } else {
            colorMapAttribute = getCapability(ShadeableCapability.class)
                    .getShadingField();
        }
        if (colorMapAttribute != null) {
            getCapability(ShadeableCapability.class).setAvailableShadingFields(
                    new String[] { colorMapAttribute });
        } else {
            getCapability(ShadeableCapability.class).setAvailableShadingFields(
                    new String[] {});
        }
        if (this.colorMapAttribute != null && this.colorMap == null) {
            generateUniqueColorMap(colorMapAttribute);
        }

        IWireframeShape cachedShape = WireframeCache.getInstance()
                .checkWireframe(resourceData.getFilename(),
                        descriptor.getGridGeometry());
        if (cachedShape != null) {
            this.outlineShape = cachedShape;
        } else {
            long t0 = System.currentTimeMillis();
            if (simplificationLevel > 0.0f) {

                outlineShape = target.createWireframeShape(false, descriptor,
                        0.0f, isSpatiallySplit, new PixelExtent(descriptor
                                .getGridGeometry().getGridRange().getLow(0),
                                descriptor.getGridGeometry().getGridRange()
                                        .getHigh(0), descriptor
                                        .getGridGeometry().getGridRange()
                                        .getLow(1), descriptor
                                        .getGridGeometry().getGridRange()
                                        .getHigh(1)));
            } else {
                outlineShape = target.createWireframeShape(false, descriptor);
            }

            shadedShape = target.createShadedShape(false, descriptor,
                    !isRegularPolygon);

            loadShp();

            WireframeCache.getInstance().registerWireframe(
                    resourceData.getFilename(), descriptor.getGridGeometry(),
                    this.outlineShape);
            System.out.println("Shapefile load took: "
                    + (System.currentTimeMillis() - t0) + "ms");
        }

        this.initializedGridGeometry = this.descriptor.getGridGeometry();
        this.lastTarget = target;
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.viz.core.rsc.IVizResource#isApplicable(com.raytheon.viz.
     * core.PixelExtent)
     */
    public boolean isApplicable(PixelExtent extent) {
        return true;
    }

    /*
     * (non-Javadoc)
     * 
     * @seecom.raytheon.viz.core.rsc.IVizResource#paint(com.raytheon.viz.core.
     * IGraphicsTarget, com.raytheon.viz.core.PixelExtent, double, float)
     */
    @Override
    @SuppressWarnings("unchecked")
    protected void paintInternal(IGraphicsTarget aTarget,
            PaintProperties paintProps) throws VizException {
        float alpha = paintProps.getAlpha();

        if (isShadedCalculated
                && getCapability(ShadeableCapability.class).getShadingField() != null
                && shadedShape != null && shadedShape.isDrawable()) {
            float opacity = getCapability(ShadeableCapability.class)
                    .getOpacity();
            aTarget.drawShadedShape(shadedShape, alpha * opacity);
        }

        if (getCapability(OutlineCapability.class).isOutlineOn()
                && outlineShape != null && outlineShape.isDrawable()) {
            aTarget.drawWireframeShape(outlineShape,
                    getCapability(ColorableCapability.class).getColor(),
                    getCapability(OutlineCapability.class).getOutlineWidth(),
                    getCapability(OutlineCapability.class).getLineStyle(),
                    alpha);
        }

        if (getCapability(LabelableCapability.class).getLabelField() != null
                && this.rTree != null && this.rTree.size() > 0) {

            Envelope env = this.descriptor.pixelToWorld(paintProps.getView()
                    .getExtent());

            List<LabelNode> results = this.rTree.query(env);

            if (results.size() < MAX_DISPLAYED_LABELS) {
                if (font == null) {
                    font = aTarget.initializeFont(aTarget.getDefaultFont()
                            .getFontName(), 10, null);
                }

                double screenToWorldRatio = paintProps.getCanvasBounds().width
                        / paintProps.getView().getExtent().getWidth();

                for (LabelNode n : results) {
                    double[] pixel = descriptor.worldToPixel(new double[] {
                            n.c.x, n.c.y });
                    if (pixel == null) {
                        continue;
                    }

                    double offsetX = getCapability(LabelableCapability.class)
                            .getxOffset() / screenToWorldRatio;
                    double offsetY = getCapability(LabelableCapability.class)
                            .getyOffset() / screenToWorldRatio;

                    aTarget.drawString(
                            font,
                            n.label,
                            pixel[0] + offsetX,
                            pixel[1] + offsetY,
                            0.0,
                            IGraphicsTarget.TextStyle.NORMAL,
                            getCapability(ColorableCapability.class).getColor(),
                            HorizontalAlignment.CENTER, null);

                }
                font.dispose();
            }
        }
    }

    protected void loadSDS() throws VizException {
        try {
            File file = resourceData.getShapeFile();

            // Use GRX -- RTree index
            // takes longer to build initially, but faster queries than QuadTree

            dataStore = new IndexedShapefileDataStore(file.toURI().toURL(),
                    null, false, true, IndexType.QIX);

            theShapefileAttributes = ShapefileUtil.getAttributes(dataStore,
                    true);

        } catch (Exception e) {
            throw new VizException("Error loading shapefile: ", e);
        }

    }

    protected void loadShp() throws VizException {

        // use a temporary tree until it's built
        STRtree rTree = new STRtree();

        FeatureIterator<SimpleFeature> featureIterator = null;
        try {

            // TODO: pass the crs on to JTSCompiler to indicate projection of
            // shapefile data.
            CoordinateReferenceSystem crs = dataStore.getFeatureSource()
                    .getSchema().getGeometryDescriptor()
                    .getCoordinateReferenceSystem();

            shapeField = dataStore.getFeatureSource().getSchema()
                    .getGeometryDescriptor().getLocalName();

            String[] types = dataStore.getTypeNames();

            DefaultQuery query = new DefaultQuery();

            query.setTypeName(types[0]);

            if (isShadedCalculated) {
                Set<String> valSet = new HashSet<String>();
                valSet.add(resourceData.getLabelFields()[0]);
                valSet.add(colorMapAttribute);
                valSet.add(shapeField);
                query.setPropertyNames(valSet.toArray(new String[valSet.size()]));
            } else {
                query.setPropertyNames(new String[] {
                        resourceData.getLabelFields()[0], shapeField });
            }

            featureIterator = dataStore.getFeatureSource().getFeatures(query)
                    .features();

            JTSCompiler jtsCompiler = new JTSCompiler(shadedShape,
                    outlineShape, descriptor);

            StringBuffer sb = new StringBuffer();

            while (featureIterator.hasNext()) {
                SimpleFeature f = featureIterator.next();
                Geometry o = (Geometry) f.getDefaultGeometry();
                sb.setLength(0);

                Point center = o.getCentroid();
                if (resourceData.getLabelFields() != null) {

                    sb.append(f.getAttribute(0));

                }

                LabelNode node = new LabelNode();
                node.c = center.getCoordinate();
                node.label = sb.toString();

                rTree.insert(new Envelope(center.getCoordinate()), node);

                RGB color = null;

                if (isShadedCalculated) {
                    Object attrib = f.getAttribute(0);
                    color = colorMap.get(attrib);
                }
                try {
                    jtsCompiler.handle(o, color);
                } catch (Exception e) {
                    // TODO Auto-generated catch block
                    e.printStackTrace();
                }
            }

            rTree.build();

            // assign into the rTree attribute for use by the paint method
            // don't do this until after finished inserting or the next insert
            // after a paint call will fail
            this.rTree = rTree;

            outlineShape.compile();

            if (isShadedCalculated) {
                shadedShape.compile();
            }

        } catch (Exception e) {
            throw new VizException("Error reading shapefile "
                    + resourceData.getFilename().toString() + "", e);
        } finally {
            if (featureIterator != null) {
                featureIterator.close();
            }

        }

    }

    @Override
    public Map<String, Object> interrogate(ReferencedCoordinate coord)
            throws VizException {
        Map<String, Object> retVal = new HashMap<String, Object>();
        FeatureIterator<SimpleFeature> featureIterator = null;

        try {
            Coordinate latLon = coord.asLatLon();
            String[] types = dataStore.getTypeNames();

            DefaultQuery query = new DefaultQuery();
            query.setTypeName(types[0]);
            query.setPropertyNames(theShapefileAttributes);

            FilterFactory2 ff = CommonFactoryFinder.getFilterFactory2(GeoTools
                    .getDefaultHints());
            ReferencedEnvelope bbox = new ReferencedEnvelope(latLon.x
                    - INQUIRY_TOLERANCE, latLon.x + INQUIRY_TOLERANCE, latLon.y
                    - INQUIRY_TOLERANCE, latLon.y + INQUIRY_TOLERANCE,
                    MapUtil.LATLON_PROJECTION);
            Filter bboxFilter = ff.bbox(ff.property(shapeField), bbox);

            query.setFilter(bboxFilter);
            featureIterator = dataStore.getFeatureSource().getFeatures(query)
                    .features();

            GeometryFactory gf = new GeometryFactory();
            boolean done = false;
            String geomName = dataStore.getSchema().getGeometryDescriptor()
                    .getLocalName();
            Class<?> geomType = dataStore.getSchema().getGeometryDescriptor()
                    .getType().getBinding();
            boolean pointType = false;
            if (geomType == Point.class || geomType == MultiPoint.class) {
                pointType = true;
            }

            while (featureIterator.hasNext() && !done) {
                SimpleFeature f = featureIterator.next();

                if (pointType == false
                        && !((Geometry) f.getDefaultGeometry()).intersects(gf
                                .toGeometry(bbox))) {
                    continue;
                }

                for (int k = 0; k < theShapefileAttributes.length; k++) {

                    if (!theShapefileAttributes[k].equals(geomName)) {
                        retVal.put(theShapefileAttributes[k], f.getAttribute(k)
                                .toString());
                    }

                }
                done = true;
            }
        } catch (Exception e) {
            throw new VizException("Error interrogating resource: ", e);
        } finally {
            if (featureIterator != null) {
                featureIterator.close();
            }
        }

        return retVal;
    }

    @Override
    @SuppressWarnings("unchecked")
    public String inspect(ReferencedCoordinate coord) throws VizException {
        FeatureIterator<SimpleFeature> featureIterator = null;

        StringBuffer sb = new StringBuffer();
        try {
            Coordinate latLon = coord.asLatLon();
            String[] types = dataStore.getTypeNames();

            DefaultQuery query = new DefaultQuery();
            query.setTypeName(types[0]);

            String[] fields = new String[resourceData.getLabelFields().length + 1];
            System.arraycopy(resourceData.getLabelFields(), 0, fields, 0,
                    resourceData.getLabelFields().length);
            fields[resourceData.getLabelFields().length] = shapeField;

            query.setPropertyNames(fields);
            FilterFactory2 ff = CommonFactoryFinder.getFilterFactory2(GeoTools
                    .getDefaultHints());
            Class geomType = dataStore.getSchema().getGeometryDescriptor()
                    .getType().getBinding();
            boolean polygonal = Polygonal.class.isAssignableFrom(geomType);

            GeometryFactory gf = new GeometryFactory();
            Point llPoint = gf.createPoint(latLon);
            Filter filter;
            if (polygonal) {
                filter = ff.contains(ff.property(shapeField),
                        ff.literal(llPoint));
            } else {
                // TODO: make the box sized in screen pixels not degrees
                ReferencedEnvelope bbox = new ReferencedEnvelope(latLon.x
                        - POINT_INQUIRY_TOLERANCE, latLon.x
                        + POINT_INQUIRY_TOLERANCE, latLon.y
                        - POINT_INQUIRY_TOLERANCE, latLon.y
                        + POINT_INQUIRY_TOLERANCE, MapUtil.LATLON_PROJECTION);
                Geometry envelopeGeometry = gf.toGeometry(bbox);
                filter = ff.intersects(ff.literal(envelopeGeometry),
                        ff.property(shapeField));
                // filter = ff.dwithin(ff.property(shapeField), ff
                // .literal(llPoint), 1.0, "km");
            }

            query.setFilter(filter);
            featureIterator = dataStore.getFeatureSource().getFeatures(query)
                    .features();

            boolean done = false;
            while (featureIterator.hasNext() && !done) {
                SimpleFeature f = featureIterator.next();

                for (int k = 0; k < resourceData.getLabelFields().length; k++) {

                    if (k != 0) {
                        sb.append(" ");
                    }
                    String s = f.getAttribute(k).toString();
                    sb.append(StringUtils.isBlank(s) ? "unknown" : s);

                }
                done = true;
            }

        } catch (Throwable e) {
            throw new VizException("Error inspecting resource: ", e);
        } finally {
            if (featureIterator != null) {
                featureIterator.close();
            }
        }
        return sb.toString();
    }

    protected void useColorMap(int number, String attribute) {

        colorMap = new HashMap<Object, RGB>();
        switch (number) {
        case -22:
            colorMap.put(-3, new RGB(0, 0, 0));
            colorMap.put(-2, new RGB(50, 79, 79));
            colorMap.put(-1, new RGB(0, 236, 236));
            colorMap.put(0, new RGB(1, 160, 246));
            colorMap.put(1, new RGB(0, 0, 246));
            colorMap.put(2, new RGB(0, 255, 0));
            colorMap.put(3, new RGB(0, 200, 0));
            colorMap.put(4, new RGB(0, 144, 0));
            colorMap.put(5, new RGB(255, 255, 0));
            colorMap.put(6, new RGB(231, 192, 0));
            colorMap.put(7, new RGB(255, 144, 0));
            colorMap.put(8, new RGB(255, 0, 0));
            colorMap.put(9, new RGB(214, 0, 0));
            colorMap.put(10, new RGB(192, 0, 0));
            colorMap.put(11, new RGB(255, 0, 255));
            colorMap.put(12, new RGB(153, 85, 201));
            colorMap.put(13, new RGB(235, 235, 235));
            break;
        }
        isShadedCalculated = true;
        getCapability(ShadeableCapability.class).setShadingField(attribute);
        colorMapAttribute = attribute;
    }

    protected void generateUniqueColorMap(String attribute) throws VizException {

        FeatureIterator<SimpleFeature> featureIterator = null;
        try {
            String[] types = dataStore.getTypeNames();
            DefaultQuery dq = new DefaultQuery();
            dq.setTypeName(types[0]);
            dq.setPropertyNames(new String[] { attribute });
            featureIterator = dataStore.getFeatureSource().getFeatures(dq)
                    .features();

            Random rand = new Random(System.currentTimeMillis());

            colorMap = new HashMap<Object, RGB>();

            while (featureIterator.hasNext()) {
                SimpleFeature f = featureIterator.next();
                Object attrib = f.getAttribute(0);
                RGB c = colorMap.get(attrib);
                if (c == null) {
                    c = new RGB(rand.nextInt(255), rand.nextInt(255),
                            rand.nextInt(255));
                    colorMap.put(attrib, c);
                }
            }
            isShadedCalculated = true;
            getCapability(ShadeableCapability.class).setShadingField(attribute);
            colorMapAttribute = attribute;
        } catch (Exception e) {
            throw new VizException("Error generating colormap: ", e);
        } finally {
            if (featureIterator != null) {
                featureIterator.close();
            }
        }

    }

    public static HashMap<Object, RGB> generateRandomAttributeMap(
            String attribute, File theShape) throws VizException {
        FeatureIterator<SimpleFeature> featureIterator = null;
        try {

            ShapefileDataStore theSDS = new ShapefileDataStore(theShape.toURI()
                    .toURL(), null, true);

            String[] types = theSDS.getTypeNames();
            DefaultQuery dq = new DefaultQuery();
            dq.setTypeName(types[0]);
            dq.setPropertyNames(new String[] { attribute });
            featureIterator = theSDS.getFeatureSource().getFeatures(dq)
                    .features();

            Random rand = new Random(System.currentTimeMillis());

            HashMap<Object, RGB> theColorMap = new HashMap<Object, RGB>();

            while (featureIterator.hasNext()) {
                SimpleFeature f = featureIterator.next();
                Object attrib = f.getAttribute(0);
                RGB c = theColorMap.get(attrib);
                if (c == null) {
                    c = new RGB(rand.nextInt(255), rand.nextInt(255),
                            rand.nextInt(255));
                    theColorMap.put(attrib, c);
                }
            }

            return theColorMap;
        } catch (Exception e) {
            throw new VizException("Error generating colormap: ", e);
        } finally {
            if (featureIterator != null) {
                featureIterator.close();
            }
        }
    }

    public static HashMap<Object, RGB> generateRGBColorMap(String attribute,
            File theShape) throws VizException {
        FeatureIterator<SimpleFeature> featureIterator = null;
        try {

            ShapefileDataStore theSDS = new ShapefileDataStore(theShape.toURI()
                    .toURL(), null, true);

            String[] types = theSDS.getTypeNames();
            DefaultQuery dq = new DefaultQuery();
            dq.setTypeName(types[0]);
            dq.setPropertyNames(new String[] { attribute });
            featureIterator = theSDS.getFeatureSource().getFeatures(dq)
                    .features();

            HashMap<Object, RGB> theColorMap = new HashMap<Object, RGB>();

            // Calc max/min
            float max = Float.MIN_VALUE;
            float min = Float.MAX_VALUE;

            while (featureIterator.hasNext()) {
                SimpleFeature f = featureIterator.next();
                float attrib;
                Object o = f.getAttribute(0);
                if (o instanceof String) {
                    attrib = Float.parseFloat((String) o);
                } else {
                    attrib = ((Number) o).floatValue();
                }
                if (max < attrib) {
                    max = attrib;
                }

                if (min > attrib) {
                    min = attrib;
                }
            }

            float diff = max - min;

            // Second pass, assign colors
            featureIterator = theSDS.getFeatureSource().getFeatures(dq)
                    .features();
            while (featureIterator.hasNext()) {
                SimpleFeature f = featureIterator.next();
                Object o = f.getAttribute(0);
                float attrib;
                if (o instanceof String) {
                    attrib = Float.parseFloat((String) o);
                } else {
                    attrib = ((Number) o).floatValue();
                }

                RGB c = theColorMap.get(o);
                if (c == null) {
                    int idx = (int) ((attrib - min) / diff * 63);
                    c = new RGB(rgbRed[idx], rgbGreen[idx], rgbBlue[idx]);
                    theColorMap.put(o, c);
                }

            }

            return theColorMap;
        } catch (IOException e) {
            throw new VizException("Error generating colormap: ", e);
        } catch (IllegalAttributeException e) {
            throw new VizException("Error generating colormap: ", e);
        } finally {
            if (featureIterator != null) {
                featureIterator.close();
            }
        }
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.viz.core.rsc.IVizResource#dispose()
     */
    @Override
    protected void disposeInternal() {

        WireframeCache.getInstance().unregisterWireframe(
                resourceData.getFilename(), descriptor.getGridGeometry());

        if (shadedShape != null) {
            shadedShape.dispose();
        }
        shadedShape = null;

        if (colorMap != null) {
            colorMap.clear();
        }

        if (font != null) {
            font.dispose();
        }

        lastTarget = null;

    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.viz.core.rsc.IVizResource#getCoordinateReferenceSystem()
     */
    public CoordinateReferenceSystem getCoordinateReferenceSystem() {
        return null;
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.viz.core.rsc.capabilities.IAnalyzableResource#intersection
     * (com.vividsolutions.jts.geom.Geometry)
     */
    public Geometry intersection(Geometry geom) throws VizException {
        FeatureIterator<SimpleFeature> featureIterator = null;
        List<Geometry> geomList = new ArrayList<Geometry>();

        try {

            String[] types = dataStore.getTypeNames();

            DefaultQuery query = new DefaultQuery();
            query.setTypeName(types[0]);

            String[] fields = new String[resourceData.getLabelFields().length + 1];
            for (int i = 0; i < resourceData.getLabelFields().length; i++) {
                fields[i] = resourceData.getLabelFields()[i];
            }
            fields[resourceData.getLabelFields().length] = shapeField;

            query.setPropertyNames(fields);
            FilterFactory2 ff = CommonFactoryFinder.getFilterFactory2(GeoTools
                    .getDefaultHints());
            ReferencedEnvelope bbox = new ReferencedEnvelope(
                    geom.getEnvelopeInternal(), MapUtil.LATLON_PROJECTION);
            Expression geometry = ff.property(shapeField);

            Filter bboxFilter = ff.bbox(geometry, bbox);

            query.setFilter(bboxFilter);
            featureIterator = dataStore.getFeatureSource().getFeatures(query)
                    .features();

            GeometryFactory gf = new GeometryFactory();

            while (featureIterator.hasNext()) {
                SimpleFeature f = featureIterator.next();

                Geometry g = (Geometry) f.getDefaultGeometry();
                if (!g.intersects(gf.toGeometry(bbox))) {
                    continue;
                }

                Geometry intersection = g.intersection(geom);
                geomList.add(intersection);
            }

            if (geomList.size() == 0) {
                return null;
            }
            if (geomList.size() == 1) {
                return geomList.get(0);
            }

            Geometry[] geomArray = geomList.toArray(new Geometry[geomList
                    .size()]);
            return gf.createGeometryCollection(geomArray);

        } catch (Exception e) {
            throw new VizException("Error inspecting resource: ", e);
        } finally {
            if (featureIterator != null) {
                featureIterator.close();
            }
        }

    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.viz.core.rsc.capabilities.IAnalyzableResource#union(com.
     * vividsolutions.jts.geom.Geometry)
     */
    public Geometry union(Geometry geom) {
        // TODO Auto-generated method stub
        return null;
    }

    /**
     * @return the isRegularPolygon
     */
    public boolean isRegularPolygon() {
        return isRegularPolygon;
    }

    /**
     * @return the colorMap
     */
    public Map<Object, RGB> getColorMap() {
        return colorMap;
    }

    /**
     * @param colorMap
     *            the colorMap to set
     */
    public void setColorMap(Map<Object, RGB> colorMap) {
        this.colorMap = colorMap;
        this.isShadedCalculated = true;
    }

    /**
     * @param colorMap
     *            the colorMap to set
     */
    public void setColorMapId(int colormapId) {
        useColorMap(colormapId, colorMapAttribute);
    }

    /**
     * @return the colorMapAttribute
     */
    public String getColorMapAttribute() {
        return colorMapAttribute;
    }

    /**
     * @param colorMapAttribute
     *            the colorMapAttribute to set
     */
    public void setColorMapAttribute(String colorMapAttribute)
            throws VizException {
        this.colorMapAttribute = colorMapAttribute;
    }

    /**
     * @return the isSpatiallySplit
     */
    public boolean isSpatiallySplit() {
        return isSpatiallySplit;
    }

    /**
     * @param isSpatiallySplit
     *            the isSpatiallySplit to set
     */
    public void setSpatiallySplit(boolean isSpatiallySplit) {
        this.isSpatiallySplit = isSpatiallySplit;
    }

    /**
     * @return the simplificationLevel
     */
    public float getSimplificationLevel() {
        return simplificationLevel;
    }

    /**
     * @param simplificationLevel
     *            the simplificationLevel to set
     */
    public void setSimplificationLevel(float simplificationLevel) {
        this.simplificationLevel = simplificationLevel;
    }

    /**
     * @param isRegularPolygon
     *            the isRegularPolygon to set
     */
    public void setRegularPolygon(boolean isRegularPolygon) {
        this.isRegularPolygon = isRegularPolygon;
    }

    @Override
    public void project(CoordinateReferenceSystem mapData) throws VizException {

        if (this.outlineShape != null) {
            WireframeCache.getInstance().unregisterWireframe(
                    resourceData.getFilename(), initializedGridGeometry);

        }
        if (this.shadedShape != null) {
            this.shadedShape.reset();
            this.shadedShape = null;
        }

        this.initInternal(this.lastTarget);
    }

    public IndexedShapefileDataStore getShapefileObject() {
        return dataStore;
    }
}
