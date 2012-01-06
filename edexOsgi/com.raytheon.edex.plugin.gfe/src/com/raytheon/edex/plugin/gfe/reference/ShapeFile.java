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
package com.raytheon.edex.plugin.gfe.reference;

import java.io.File;
import java.io.IOException;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.NoSuchElementException;

import org.geotools.data.DefaultQuery;
import org.geotools.data.DefaultTransaction;
import org.geotools.data.FeatureSource;
import org.geotools.data.shapefile.indexed.IndexType;
import org.geotools.data.shapefile.indexed.IndexedShapefileDataStore;
import org.geotools.factory.CommonFactoryFinder;
import org.geotools.factory.GeoTools;
import org.geotools.feature.FeatureCollection;
import org.geotools.feature.FeatureIterator;
import org.opengis.feature.IllegalAttributeException;
import org.opengis.feature.simple.SimpleFeature;
import org.opengis.feature.simple.SimpleFeatureType;
import org.opengis.feature.type.AttributeDescriptor;
import org.opengis.filter.Filter;
import org.opengis.filter.FilterFactory2;

import com.vividsolutions.jts.geom.Geometry;
import com.vividsolutions.jts.geom.LineString;
import com.vividsolutions.jts.geom.MultiLineString;
import com.vividsolutions.jts.geom.MultiPoint;
import com.vividsolutions.jts.geom.MultiPolygon;
import com.vividsolutions.jts.geom.Point;
import com.vividsolutions.jts.geom.Polygon;

/**
 * Class to allow reading (and optionally filtering) of an ESRI shape file
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date			Ticket#		Engineer	Description
 * ------------	----------	-----------	--------------------------
 * Apr 11, 2008		#1075	randerso	Initial creation
 * 
 * </pre>
 * 
 * @author randerso
 * @version 1.0
 */

public class ShapeFile {
    // move to MapDef
    public interface IMapBackgroundFilter {
        public boolean filter(Map<String, String> atts);
    }

    // move to EditAreaDef
    public interface IEditAreaNamer {
        public String getEditAreaName(Map<String, String> atts);
    }

    private IMapBackgroundFilter filter; // move to MapDef

    private String displayName; // move to MapDef

    private Object editAreaName; // move to EditAreaDef

    private String groupName; // move to EditAreaDef

    public static enum ShapeType {
        NONE, POINT, POLYLINE, POLYGON
    };

    private File location;

    private Filter attributeFilter;

    private IndexedShapefileDataStore dataStore;

    private String[] shapefileAttributes;

    private FeatureCollection<SimpleFeatureType, SimpleFeature> featureCollection;

    private FeatureIterator<SimpleFeature> featureIterator;

    private DefaultTransaction trx;

    private String shapeField;

    private ShapeType type;

    private Geometry boundingGeom;

    private DefaultQuery query;

    /**
     * Create a shape file
     * 
     * @param location
     *            the pathname of a file or directory containing the shape file.
     *            If a directory is supplied, it is assumed it contains a single
     *            shape file and the first *.shp file located will be used.
     */
    public ShapeFile(String path) {
        this(new File(path));
    }

    /**
     * Create a shape file
     * 
     * @param location
     *            the file or directory containing the shape file. If a
     *            directory is supplied, it is assumed it contains a single
     *            shape file and the first *.shp file located will be used.
     */
    public ShapeFile(File location) {
        this.location = location;
    }

    /**
     * @throws IOException
     * 
     */
    public void open() throws IOException {
        File file = getFile();

        dataStore = new IndexedShapefileDataStore(file.toURI().toURL(), null,
                true, true, IndexType.QIX);

        shapeField = dataStore.getSchema().getGeometryDescriptor()
                .getLocalName();
        featureCollection = null;
        featureIterator = null;
        String[] types = dataStore.getTypeNames();

        query = new DefaultQuery();
        query.setTypeName(types[0]);

        FilterFactory2 ff = CommonFactoryFinder.getFilterFactory2(GeoTools
                .getDefaultHints());

        Filter filter = null;
        if (boundingGeom != null) {
            filter = ff.intersects(ff.property(shapeField), ff
                    .literal(boundingGeom));
        }

        if (attributeFilter != null) {
            if (filter == null) {
                filter = attributeFilter;
            } else {
                filter = ff.and(attributeFilter, filter);
            }
        }

        if (filter != null) {
            query.setFilter(filter);
        }

        FeatureSource<SimpleFeatureType, SimpleFeature> featureSource = dataStore
                .getFeatureSource();
        featureCollection = featureSource.getFeatures(query);
        featureIterator = featureCollection.features();
    }

    /**
     * Returns the path of the .shp file
     * 
     * @return
     */
    public File getFile() {
        File file = this.location;
        if (file.exists() && file.isDirectory()) {
            for (File path : file.listFiles()) {
                if (path.getName().endsWith(".shp")) {
                    return path;
                }
            }
        }
        return file;
    }

    /**
     * @throws IOException
     * 
     */
    public void close() throws IOException {
        if (trx != null) {
            trx.close();
            trx = null;
        }

        if (featureIterator != null) {
            featureIterator.close();
            featureIterator = null;
            featureCollection = null;
        }

        if (dataStore != null) {
            dataStore.dispose();
            dataStore = null;
        }
    }

    public boolean hasNext() throws IOException {
        if (featureIterator == null) {
            throw new IOException("ShapeFile is not open");
        }
        return featureIterator.hasNext();
    }

    public SimpleFeature next() throws NoSuchElementException, IOException,
            IllegalAttributeException {
        if (featureIterator == null) {
            throw new IOException("ShapeFile is not open");
        }
        return featureIterator.next();
    }

    public synchronized ShapeType getShapeType() throws IOException {
        if (this.type == null) {
            boolean closeIt = false;
            if (dataStore == null) {
                open();
                closeIt = true;
            }

            Class<?> geometryType = dataStore.getSchema()
                    .getGeometryDescriptor().getType().getBinding();

            if (geometryType == Point.class || geometryType == MultiPoint.class) {
                this.type = ShapeType.POINT;
            } else if (geometryType == LineString.class
                    || geometryType == MultiLineString.class) {
                this.type = ShapeType.POLYLINE;
            } else if (geometryType == Polygon.class
                    || geometryType == MultiPolygon.class) {
                this.type = ShapeType.POLYGON;
            } else {
                this.type = ShapeType.NONE;
            }

            if (closeIt) {
                close();
            }
        }
        return this.type;
    }

    public synchronized String[] getAttributeNames() throws IOException {
        if (shapefileAttributes == null) {
            List<AttributeDescriptor> at = dataStore.getSchema()
                    .getAttributeDescriptors();
            if (at == null || at.size() == 0) {
                return null;
            }

            shapefileAttributes = new String[at.size() - 1];

            int j = 0;

            for (int i = 0; i < at.size(); i++) {
                if (!at.get(i).getLocalName().equals(shapeField)) {
                    shapefileAttributes[j] = at.get(i).getLocalName();
                    j++;
                }
            }
        }
        return shapefileAttributes;
    }

    /**
     * @param i
     * @return
     * @throws IOException
     */
    public Map<String, String> getAttributes(SimpleFeature f)
            throws IOException {
        Map<String, String> retVal = new HashMap<String, String>();
        for (String at : getAttributeNames()) {
            retVal.put(at, f.getAttribute(at).toString());
        }

        return retVal;
    }

    /*
     * (non-Javadoc)
     * 
     * @see java.lang.Object#toString()
     */
    @Override
    public String toString() {
        return this.getFile().getName();
    }

    /**
     * @return the filter
     */
    public IMapBackgroundFilter getMapBackgroundFilter() {
        return filter;
    }

    /**
     * @param filter
     *            the filter to set
     */
    public void setMapBackgroundFilter(IMapBackgroundFilter filter) {
        this.filter = filter;
    }

    public void setBoundingGeometry(Geometry geom) {
        this.boundingGeom = geom;
    }

    /**
     * @return the attributeFilter
     */
    public Filter getAttributeFilter() {
        return attributeFilter;
    }

    /**
     * @param attributeFilter
     *            the attributeFilter to set
     */
    public void setAttributeFilter(Filter attributeFilter) {
        this.attributeFilter = attributeFilter;
    }

    /**
     * @return
     * @throws IOException
     */
    public int getFeatureCount() throws IOException {
        if (this.featureCollection == null) {
            throw new IOException(
                    "Shapefile must be open when calling getFeatureCount");
        }
        return this.featureCollection.size();
    }

    public String getDisplayName() {
        return displayName;
    }

    public void setDisplayName(String name) {
        this.displayName = name;
    }

    public String getGroupName() {
        return groupName;
    }

    public void setGroupName(String groupName) {
        this.groupName = groupName;
    }

    public Object getEditAreaName() {
        return editAreaName;
    }

    public void setEditAreaName(Object editAreaName) {
        this.editAreaName = editAreaName;
    }

    public static void main(String[] args) {
        ShapeFile shapeFile = new ShapeFile(new File(
                "/home/randerso/shapefiles/States"));

        try {
            shapeFile.open();
            System.out.println(shapeFile.getShapeType());

            while (shapeFile.hasNext()) {
                SimpleFeature feature = shapeFile.next();
                for (Object attribute : feature.getAttributes()) {
                    if (!(attribute instanceof Geometry)) {
                        System.out.println(attribute);
                    }
                }
            }

        } catch (IOException e) {
            // TODO Auto-generated catch block
            e.printStackTrace();
        } finally {
            try {
                if (shapeFile != null) {
                    shapeFile.close();
                }
            } catch (IOException e) {
                // TODO Auto-generated catch block
                e.printStackTrace();
            }
        }
    }
}
