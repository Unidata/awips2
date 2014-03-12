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

import java.io.IOException;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Date;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.NoSuchElementException;
import java.util.Properties;

import org.geotools.data.DataStore;
import org.geotools.data.Query;
import org.geotools.data.postgis.PostgisNGDataStoreFactory;
import org.geotools.data.simple.SimpleFeatureCollection;
import org.geotools.data.simple.SimpleFeatureIterator;
import org.geotools.data.simple.SimpleFeatureSource;
import org.geotools.factory.CommonFactoryFinder;
import org.geotools.factory.GeoTools;
import org.hibernate.engine.SessionFactoryImplementor;
import org.opengis.feature.IllegalAttributeException;
import org.opengis.feature.simple.SimpleFeature;
import org.opengis.feature.simple.SimpleFeatureType;
import org.opengis.feature.type.AttributeDescriptor;
import org.opengis.filter.Filter;
import org.opengis.filter.FilterFactory2;
import org.opengis.geometry.BoundingBox;

import com.raytheon.edex.plugin.gfe.exception.MissingLocalMapsException;
import com.raytheon.uf.common.dataquery.db.QueryResult;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.edex.core.EDEXUtil;
import com.raytheon.uf.edex.database.tasks.SqlQueryTask;
import com.vividsolutions.jts.geom.Geometry;
import com.vividsolutions.jts.geom.LineString;
import com.vividsolutions.jts.geom.MultiLineString;
import com.vividsolutions.jts.geom.MultiPoint;
import com.vividsolutions.jts.geom.MultiPolygon;
import com.vividsolutions.jts.geom.Point;
import com.vividsolutions.jts.geom.Polygon;

/**
 * Class to allow reading (and optionally filtering) of an PostGIS table
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date			Ticket#		Engineer	Description
 * ------------	----------	-----------	--------------------------
 * Sep 18, 2012		 #1091  randerso	Initial creation
 * Mar 28, 2013      #1837  dgilling    Change error handling in 
 *                                      getLastUpdated().
 * Mar 11, 2014      #2718 randerso     Changes for GeoTools 10.5
 * 
 * </pre>
 * 
 * @author randerso
 * @version 1.0
 */

public class DbShapeSource {
    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(DbShapeSource.class);

    public static enum ShapeType {
        NONE, POINT, POLYLINE, POLYGON
    }

    private static final String DB_NAME = "maps";

    private static final String SCHEMA_NAME = "mapdata";

    private static DataStore _dataStore;

    private static int refCount = 0;

    private boolean filtered = false;

    private String displayName;

    private boolean hasEditAreaName = false;

    private String groupName;

    private String instanceName;

    private String tableName;

    private List<String> attributeNames;

    private SimpleFeatureCollection featureCollection;

    private SimpleFeatureIterator featureIterator;

    private String shapeField;

    private ShapeType type;

    private Geometry boundingGeom;

    private BoundingBox boundingBox;

    private Query query;

    private SimpleFeatureType schema;

    /**
     * Create a DbShapeSource
     * 
     * @param tableName
     *            the name of the database table.
     * @throws IOException
     */
    public DbShapeSource(String tableName) throws IOException {
        this.tableName = tableName.toLowerCase();
        refCount++;
    }

    @Override
    protected void finalize() throws Throwable {
        refCount--;
        if (refCount == 0) {
            if (_dataStore != null) {
                _dataStore.dispose();
                _dataStore = null;
            }
        }
        super.finalize();
    }

    private synchronized DataStore getDataStore() throws IOException {
        if (_dataStore == null) {
            SessionFactoryImplementor sessionFactory = (SessionFactoryImplementor) EDEXUtil
                    .getESBComponent("mapsSessionFactory");
            Properties props = sessionFactory.getProperties();
            String host = props.getProperty("db.addr");
            String port = props.getProperty("db.port");
            String user = props.getProperty("connection.username");
            String passwd = props.getProperty("connection.password");
            PostgisNGDataStoreFactory factory = new PostgisNGDataStoreFactory();
            Map<String, Object> params = new HashMap<String, Object>();
            params.put("host", host);
            params.put("port", port);
            params.put("database", DB_NAME);
            params.put("schema", SCHEMA_NAME);
            params.put("user", user);
            params.put("passwd", passwd);
            params.put("dbtype", "postgis");
            params.put("wkb enabled", true);

            _dataStore = factory.createDataStore(params);
        }
        return _dataStore;
    }

    /**
     * @throws IOException
     * @throws MissingLocalMapsException
     * 
     */
    public void open() throws IOException, MissingLocalMapsException {
        DataStore dataStore = getDataStore();
        try {
            schema = dataStore.getSchema(this.tableName);
        } catch (IOException e) {
            throw new MissingLocalMapsException(e);
        }

        shapeField = schema.getGeometryDescriptor().getLocalName();
        featureCollection = null;
        featureIterator = null;

        query = new Query();
        query.setTypeName(this.tableName);
        List<String> propNames = new ArrayList<String>(getAttributeNames());
        propNames.add(shapeField);
        query.setPropertyNames(propNames);

        FilterFactory2 ff = CommonFactoryFinder.getFilterFactory2(GeoTools
                .getDefaultHints());

        Filter filter = null;
        if (boundingGeom != null) {
            filter = ff.intersects(ff.property(shapeField),
                    ff.literal(boundingGeom));
        }

        if (boundingBox != null) {
            filter = ff.bbox(ff.property(shapeField), boundingBox);
        }

        if (filter != null) {
            query.setFilter(filter);
        }

        SimpleFeatureSource featureSource = dataStore
                .getFeatureSource(this.tableName);
        featureCollection = featureSource.getFeatures(query);
        featureIterator = featureCollection.features();
    }

    /**
     * Returns the database table name
     * 
     * @return
     */
    public String getTableName() {
        return this.tableName;
    }

    /**
     * @throws IOException
     * 
     */
    public void close() throws IOException {
        if (featureIterator != null) {
            featureIterator.close();
            featureIterator = null;
            featureCollection = null;
        }
    }

    public boolean hasNext() throws IOException {
        if (featureIterator == null) {
            throw new IOException("DataStore is not open");
        }
        return featureIterator.hasNext();
    }

    public SimpleFeature next() throws NoSuchElementException, IOException,
            IllegalAttributeException {
        if (featureIterator == null) {
            throw new IOException("DataStore is not open");
        }
        return featureIterator.next();
    }

    public synchronized ShapeType getShapeType() throws IOException,
            MissingLocalMapsException {
        if (this.type == null) {
            boolean closeIt = false;
            if (schema == null) {
                open();
                closeIt = true;
            }

            Class<?> geometryType = schema.getGeometryDescriptor().getType()
                    .getBinding();

            if ((geometryType == Point.class)
                    || (geometryType == MultiPoint.class)) {
                this.type = ShapeType.POINT;
            } else if ((geometryType == LineString.class)
                    || (geometryType == MultiLineString.class)) {
                this.type = ShapeType.POLYLINE;
            } else if ((geometryType == Polygon.class)
                    || (geometryType == MultiPolygon.class)) {
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

    public synchronized List<String> getAttributeNames() throws IOException {
        if (attributeNames == null) {
            List<AttributeDescriptor> attrDesc = schema
                    .getAttributeDescriptors();
            if ((attrDesc == null) || (attrDesc.size() == 0)) {
                return null;
            }

            attributeNames = new ArrayList<String>(attrDesc.size());
            for (AttributeDescriptor at : attrDesc) {
                Class<?> atType = at.getType().getBinding();
                if (!Geometry.class.isAssignableFrom(atType)) {
                    attributeNames.add(at.getLocalName());
                }
            }
        }
        return attributeNames;
    }

    /**
     * @param i
     * @return
     * @throws IOException
     */
    public Map<String, Object> getAttributes(SimpleFeature f)
            throws IOException {
        Map<String, Object> retVal = new HashMap<String, Object>();
        for (String at : getAttributeNames()) {
            Object attr = f.getAttribute(at);
            if (attr != null) {
                retVal.put(at, attr);
            }
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
        return this.getTableName();
    }

    /**
     * @return the filter
     */
    public boolean isFiltered() {
        return this.filtered;
    }

    /**
     * @param filter
     *            the filter to set
     */
    public void setFiltered(boolean filtered) {
        this.filtered = filtered;
    }

    public void setBoundingGeometry(Geometry geom) {
        this.boundingGeom = geom;
    }

    public void setBoundingBox(BoundingBox bbox) {
        this.boundingBox = bbox;
    }

    /**
     * @return
     * @throws IOException
     */
    public int getFeatureCount() throws IOException {
        if (this.featureCollection == null) {
            throw new IOException(
                    "DataStore must be open when calling getFeatureCount");
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

    public boolean hasEditAreaName() {
        return this.hasEditAreaName;
    }

    public void setHasEditAreaName(boolean hasEditAreaName) {
        this.hasEditAreaName = hasEditAreaName;
    }

    public String getInstanceName() {
        return instanceName;
    }

    public void setInstanceName(String instanceName) {
        this.instanceName = instanceName;
    }

    public static void main(String[] args) {
        long t0 = System.currentTimeMillis();
        DbShapeSource dbShape = null;

        try {
            dbShape = new DbShapeSource("States");
            dbShape.open();
            System.out.println("Open Took " + (System.currentTimeMillis() - t0)
                    + " ms");

            System.out.println(dbShape.getShapeType());

            while (dbShape.hasNext()) {
                SimpleFeature feature = dbShape.next();
                Map<String, Object> attributes = dbShape.getAttributes(feature);
                List<String> keys = new ArrayList<String>(attributes.keySet());
                Collections.sort(keys);
                for (String key : keys) {
                    Object attribute = attributes.get(key);
                    System.out.println(key + ": " + attribute);
                }
                System.out.println();
            }

        } catch (IOException e) {
            statusHandler.error(
                    "IOException reading " + dbShape.getTableName(), e);
        } catch (MissingLocalMapsException e) {
            statusHandler.error("Could not locate " + dbShape.getTableName()
                    + " in the maps database.", e);
        } finally {
            try {
                if (dbShape != null) {
                    dbShape.close();
                }
            } catch (IOException e) {
                statusHandler.error(
                        "IOException closing " + dbShape.getTableName(), e);
            }
        }
        System.out.println("Took " + (System.currentTimeMillis() - t0) + " ms");
    }

    public Date getLastUpdated() throws MissingLocalMapsException {
        String sqlQuery = "SELECT import_time FROM " + SCHEMA_NAME
                + ".map_version WHERE table_name = '" + this.tableName + "';";
        try {
            SqlQueryTask task = new SqlQueryTask(sqlQuery, DB_NAME);
            QueryResult result = task.execute();
            return (Date) result.getRowColumnValue(0, 0);
        } catch (Exception e) {
            // statusHandler.handle(Priority.PROBLEM, e.getLocalizedMessage(),
            // e);
            throw new MissingLocalMapsException(e);
        }
    }
}
