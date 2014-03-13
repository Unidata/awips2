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

package com.raytheon.edex.uengine.tasks.output;

import java.io.File;
import java.util.List;
import java.util.Map;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.geotools.data.FeatureWriter;
import org.geotools.data.Transaction;
import org.geotools.data.shapefile.ShapefileDataStore;
import org.geotools.data.store.ContentFeatureSource;
import org.geotools.feature.simple.SimpleFeatureTypeBuilder;
import org.opengis.feature.simple.SimpleFeature;
import org.opengis.feature.simple.SimpleFeatureType;

import com.raytheon.edex.uengine.tasks.ScriptTask;
import com.raytheon.uf.edex.core.props.PropertiesFactory;
import com.vividsolutions.jts.geom.GeometryCollection;
import com.vividsolutions.jts.geom.Point;

/**
 * Derived from original uEngine task Shapefile.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Apr 11, 2007            njensen     Initial Creation
 * Mar 11, 2014  #2718     randerso    Changes for GeoTools 10.5
 * </PRE>
 * 
 */
public class Shapefile extends ScriptTask {
    Log logger = LogFactory.getLog(getClass());

    public Shapefile(GeometryCollection aGeometryCollection,
            Map<String, List<?>> anAttributeMap, String anIcaoFilePrefix) {
        collection = aGeometryCollection;
        shapefileAttributes = anAttributeMap;
        icaoFilePrefix = anIcaoFilePrefix;
    }

    private GeometryCollection collection;

    private Map<String, List<?>> shapefileAttributes;

    private String icaoFilePrefix;

    private String filePrefix = "";

    private String destDir = null;

    private boolean ignoreDefaultDataDir;

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.edex.uengine.js.tasks.ScriptTask#execute()
     */
    @Override
    public Object execute() {
        String uEngineDir = PropertiesFactory.getInstance().getEnvProperties()
                .getEnvValue("UENGINEOUTDIR");
        String shapefileDir = null;
        String defaultDataDir = PropertiesFactory.getInstance()
                .getEnvProperties().getEnvValue("DEFAULTDATADIR");

        // Set up what our data schema will look like
        SimpleFeatureTypeBuilder builder = new SimpleFeatureTypeBuilder();
        builder.setName("Shape");
        builder.add("the_geom", Point.class);

        // get a unique name
        String fileName = java.util.UUID.randomUUID().toString();

        // File theShape = new File(uEngineDir + "/" + filePrefix + fileName +
        // ".shp");
        // GeometryFactory geomFactory = new GeometryFactory();

        File theShape;

        // if a destdir is specified, check if the defaultdata dir should be
        // used
        // if no destdir is specified, write to the uEngineDir
        if (destDir != null) {
            if (!ignoreDefaultDataDir) {
                shapefileDir = defaultDataDir + destDir;
            } else {
                shapefileDir = destDir;
            }
        } else {
            shapefileDir = uEngineDir;
        }

        if (icaoFilePrefix != null) {
            theShape = new File(shapefileDir + "/" + icaoFilePrefix + ".shp");
        } else {
            theShape = new File(shapefileDir + "/" + filePrefix + fileName
                    + ".shp");
        }

        // iterate through each String/List pair in the set
        // create an attribute for each key
        for (Map.Entry<String, List<?>> entry : shapefileAttributes.entrySet()) {
            // get the class of the first element of a list
            builder.add(entry.getKey(), entry.getValue().get(0).getClass());
        }

        try {

            // get the canonical path
            theShape = theShape.getCanonicalFile();

            // get the attribute types as an array and create the feature type
            SimpleFeatureType featureType = builder.buildFeatureType();

            // Load up the datastore

            ShapefileDataStore shpDS = new ShapefileDataStore(theShape.toURI()
                    .toURL());
            shpDS.setMemoryMapped(true);

            // Tell the datastore to create the dbf schema if it does not
            // exist
            shpDS.createSchema(featureType);

            // Get the transaction object
            ContentFeatureSource fs = shpDS.getFeatureSource();
            Transaction trx = fs.getTransaction();

            // Open a feature writer with the transaction
            FeatureWriter<SimpleFeatureType, SimpleFeature> writer = shpDS
                    .getFeatureWriter(trx);

            for (int counter = 0; counter < collection.getNumGeometries(); counter++) {

                // Load the feature to write, and set all of the properties by
                // name
                SimpleFeature feature = writer.next();

                // iterate through all
                for (Map.Entry<String, List<?>> entry : shapefileAttributes
                        .entrySet()) {
                    feature.setAttribute(entry.getKey(),
                            entry.getValue().get(counter));
                }

                feature.setDefaultGeometry(collection.getGeometryN(counter));

                writer.write();
            }
            writer.close();

        } catch (Exception e) {
            logger.error("Error creating shapefile", e);
        }

        return theShape.toURI();
    }

    public GeometryCollection getCollection() {
        return collection;
    }

    public void setCollection(GeometryCollection aCollection) {
        collection = aCollection;
    }

    public String getDestDir() {
        return destDir;
    }

    public void setDestDir(String aDestDir) {
        destDir = aDestDir;
    }

    public String getFilePrefix() {
        return filePrefix;
    }

    public void setFilePrefix(String aFilePrefix) {
        filePrefix = aFilePrefix;
    }

    public String getIcaoFilePrefix() {
        return icaoFilePrefix;
    }

    public void setIcaoFilePrefix(String aIcaoFilePrefix) {
        icaoFilePrefix = aIcaoFilePrefix;
    }

    public boolean isIgnoreDefaultDataDir() {
        return ignoreDefaultDataDir;
    }

    public void setIgnoreDefaultDataDir(boolean aIgnoreDefaultDataDir) {
        ignoreDefaultDataDir = aIgnoreDefaultDataDir;
    }

    public Map<?, ?> getShapefileAttributes() {
        return shapefileAttributes;
    }

    public void setShapefileAttributes(Map<String, List<?>> aShapefileAttributes) {
        shapefileAttributes = aShapefileAttributes;
    }

}
