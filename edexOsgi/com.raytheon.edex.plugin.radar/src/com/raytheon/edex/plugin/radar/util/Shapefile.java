package com.raytheon.edex.plugin.radar.util;

import java.io.File;
import java.io.IOException;
import java.io.Serializable;
import java.net.MalformedURLException;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.Hashtable;
import java.util.List;
import java.util.Map;
import java.util.Set;

import org.geotools.data.DataStore;
import org.geotools.data.DataStoreFactorySpi;
import org.geotools.data.DataStoreFinder;
import org.geotools.data.DefaultTransaction;
import org.geotools.data.FeatureStore;
import org.geotools.data.Transaction;
import org.geotools.data.shapefile.ShapefileDataStore;
import org.geotools.data.shapefile.ShapefileDataStoreFactory;
import org.geotools.data.simple.SimpleFeatureCollection;
import org.geotools.data.simple.SimpleFeatureSource;
import org.geotools.feature.DefaultFeatureCollection;
import org.geotools.feature.FeatureIterator;
import org.geotools.feature.simple.SimpleFeatureBuilder;
import org.geotools.feature.simple.SimpleFeatureTypeBuilder;
import org.opengis.feature.simple.SimpleFeature;
import org.opengis.feature.simple.SimpleFeatureType;

import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.vividsolutions.jts.geom.Geometry;

/**
 * Represents a shape file. Basically a convenience function to simplify the
 * tasks of reading/writing shapefiles (using GeoTools). Most of the shapefile
 * reading/writing/editing code has been copied or adapted from the GeoTools
 * tutorials which come with the GeoTools software.
 * <p>
 * Typical usage (reads a shapefile, get an attribute, adds a new attribute
 * column, then writes it):<br>
 * <code>
 * Shapefile s = new Shapefile("ObjectIDs", "myShapefile.shp");<br>
 * s.readShapefile();<br>
 * int anAttributeValue = s.getAttribute(featureID, "anAttribute", Integer.class);<br>
 * s.addAttribute("newAttribute", Double.class, valuesTable)
 * s.writeShapefile("newShapefile");
 * </code>
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#     Engineer    Description
 * ------------ ----------  ----------- --------------------------
 * 10Oct2011    10520       JWork       Initial check-in.
 * 11Mar2014    #2718       randerso    Changes for GeoTools 10.5
 * </pre>
 * 
 */

public class Shapefile {
    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(Shapefile.class);

    /**
     * The geometries of the objects in this Shapefile, stored along with object
     * ids.
     */
    private Map<String, Geometry> theGeometriesMap;

    /**
     * The features associated with each object in this Shapefile, stored along
     * with object ids.
     */
    private Map<String, SimpleFeature> theFeaturesMap;

    /** The actual file which this shapefile has been created from */
    private File theFile;

    /**
     * The FeatureSource for this shapefile, required as a template for writing
     * shapefiles
     */
    private SimpleFeatureSource theFeatureSource;

    /** Optional column used to store object ids (by default will use FID). */
    private String theIDColumn = null;

    /**
     * Create a new Shapefile object.
     * 
     * @param anIDColumn
     *            The ID of the Feature Object to get the attribute value(s)
     *            from.
     * @param aFile
     */
    public Shapefile(String anIDColumn, File aFile) {
        init(aFile);
        this.theIDColumn = anIDColumn;
    }

    /**
     * Create a new Shapefile object without a ID Column.
     * 
     * @param aFile
     */
    public Shapefile(File aFile) {
        init(aFile);
    }

    /**
     * Get the value of an attribute.
     * 
     * @param featureID
     *            The ID of the object to get the attribute value from.
     * @param attributeName
     *            The name of the attribute.
     * @param anAttributeClass
     *            The class of the obect to be returned.
     * @return The attribute as an Object or
     */
    @SuppressWarnings("unchecked")
    public <T> T getAttribute(String featureID, String attributeName,
            Class<T> anAttributeClass) {
        if (this.checkFeatures()) {
            // Check if there are some features in this Shapefile
            SimpleFeature feature = this.theFeaturesMap.get(featureID);
            if (feature == null) {
                if (statusHandler.isPriorityEnabled(Priority.PROBLEM)) {
                    statusHandler.handle(Priority.PROBLEM,
                            "Shapefile.getAttribute() error: no feature with id: "
                                    + featureID + " in this Shapefile");
                }
                return null;
            }
            Object attribute = feature.getAttribute(attributeName);
            T attributeCast = null; // The attribute cast to it's correct class.
            if (anAttributeClass.isAssignableFrom(anAttributeClass)) {
                attributeCast = (T) attribute;
            } else {
                if (statusHandler.isPriorityEnabled(Priority.PROBLEM)) {
                    statusHandler.handle(Priority.PROBLEM, "The attribute "
                            + attributeName + " cannot be assigned to a "
                            + anAttributeClass.getName() + ", it is a: "
                            + attribute.getClass().getName());
                }
            }
            return attributeCast;
        } else { // No features in this Shapefile
            return null;
        }
    }

    /**
     * Add a new attribute to this Shapefile.
     * 
     * @param anAttributeName
     *            The name of the attribute to add.
     * @param aValuesClass
     *            The class of the values to be added.
     * @param attributeValues
     *            A map of the IDs of each feature and the associated value of
     *            the new attribute
     * @return True if the operation was successful, false otherwise.
     */
    public boolean addAttribute(String anAttributeName, Class<?> aValuesClass,
            Map<String, Object> attributeValues) {
        if (!this.checkFeatures()) {
            return false;
        }
        // Check the input values are ok
        List<String> badIDs = new ArrayList<String>();
        for (String i : attributeValues.keySet()) {
            if (!this.theFeaturesMap.containsKey(i)) {
                badIDs.add(i);
            }
        }
        if (badIDs.size() > 0) {
            if (statusHandler.isPriorityEnabled(Priority.PROBLEM)) {
                statusHandler.handle(
                        Priority.PROBLEM,
                        "Shapefile.addAttribute() error: could not find features associated "
                                + "with the following IDs: "
                                + badIDs.toString());
            }
            return false;
        } // if badIDs
        /*
         * Method works be re-building all features with the new attributed
         * added
         */
        // Create a feature type builder to create new features with the added
        // attribute
        SimpleFeatureTypeBuilder featureTypeBuilder = new SimpleFeatureTypeBuilder();
        // Use first feature to initialise the feature builder
        featureTypeBuilder.init(theFeaturesMap.values().iterator().next()
                .getFeatureType());
        // Add the new attribute
        featureTypeBuilder.add(anAttributeName, aValuesClass);
        // Create a feature builder to create the new features
        SimpleFeatureBuilder featureBuilder = new SimpleFeatureBuilder(
                featureTypeBuilder.buildFeatureType());
        // Iterate over all existing features, creating new ones with the added
        // attribute
        Map<String, SimpleFeature> newFeatures = new Hashtable<String, SimpleFeature>();
        for (String id : this.theFeaturesMap.keySet()) {
            SimpleFeature newFeature = featureBuilder.buildFeature(String
                    .valueOf(id));
            SimpleFeature existingFeature = this.theFeaturesMap.get(id);
            // Add all existing attributes to the new feature
            for (int i = 0; i < existingFeature.getAttributeCount(); i++) {
                newFeature.setAttribute(i, existingFeature.getAttribute(i));
            }
            // Add the new attribute to the new feature
            newFeature.setAttribute(anAttributeName, attributeValues.get(id));
            // Replace the existing feature with the new one
            newFeatures.put(id, newFeature);
        }
        // Finally replace all old features with the old ones
        for (String id : this.theFeaturesMap.keySet()) {
            this.theFeaturesMap.put(id, newFeatures.get(id));
        }
        return true;
    }

    /**
     * Read in all objects stored in the shapefile, adding them to this
     * Shapefile object.
     * 
     * @return true if everything was read in successfully, false otherwise.
     */
    public boolean readShapefile() {

        this.clear(); // Clear all objects from this Shapefile

        // Connection to the shapefile
        Map<String, Serializable> connectParameters = new HashMap<String, Serializable>();

        try {
            connectParameters.put("url", this.theFile.toURI().toURL());
            // connectParameters.put("create spatial index", true);
            DataStore dataStore = DataStoreFinder
                    .getDataStore(connectParameters);

            // we are now connected
            String[] typeNames = dataStore.getTypeNames();
            String typeName = typeNames[0];

            this.theFeatureSource = dataStore.getFeatureSource(typeName);
            SimpleFeatureCollection collection = theFeatureSource.getFeatures();
            FeatureIterator<SimpleFeature> iterator = collection.features();

            try {
                while (iterator.hasNext()) {
                    SimpleFeature feature = iterator.next();
                    // Set the feature's ID
                    String id = null;
                    try {
                        if (this.theIDColumn != null) {
                            id = (String) feature
                                    .getAttribute(this.theIDColumn);
                        } else {
                            id = feature.getID();
                        }
                    } catch (ClassCastException e) {
                        if (statusHandler.isPriorityEnabled(Priority.PROBLEM)) {
                            statusHandler
                                    .handle(Priority.PROBLEM,
                                            "Shapfile.readShapefile() error: cannot read integer ids from the "
                                                    + "column: "
                                                    + this.theIDColumn
                                                    + ". Check this column stores unique integer IDs.");
                        }
                        return false;
                    } catch (NullPointerException e) {
                        if (statusHandler.isPriorityEnabled(Priority.PROBLEM)) {
                            statusHandler
                                    .handle(Priority.PROBLEM,
                                            "Shapfile.readObjects() error: cannot read integer ids from the"
                                                    + "column: "
                                                    + this.theIDColumn
                                                    + ". Check this column stores unique integer IDs.");
                        }
                        return false;
                    } catch (NumberFormatException e) {
                        if (statusHandler.isPriorityEnabled(Priority.PROBLEM)) {
                            statusHandler.handle(Priority.PROBLEM,
                                    "Shapfile.readObjects() error: cannot cast this feature's "
                                            + "ID to an integer. (?)");
                        }
                        return false;
                    }

                    if (theFeaturesMap.containsKey(id)) {
                        if (statusHandler.isPriorityEnabled(Priority.PROBLEM)) {
                            statusHandler.handle(Priority.PROBLEM,
                                    "Shapefile.readObjects() error: this feature's ID ("
                                            + id + ")is not unique ");
                        }
                        return false;
                    }
                    theFeaturesMap.put(id, feature);
                    Geometry geometry = (Geometry) feature.getDefaultGeometry();
                    theGeometriesMap.put(id, geometry);
                } // while iterator.hasNext()
            } finally {
                if (iterator != null) {
                    iterator.close();
                }
            }
        } catch (MalformedURLException e) {
            if (statusHandler.isPriorityEnabled(Priority.PROBLEM)) {
                statusHandler.handle(Priority.PROBLEM,
                        "Malformed URL Exception", e);
            }
            return false;
        } catch (IOException e) {
            if (statusHandler.isPriorityEnabled(Priority.PROBLEM)) {
                statusHandler.handle(Priority.PROBLEM, "IO Exception", e);
            }
            return false;
        }
        return true;
    } // readShapefile

    /**
     * Write out all the features stored in this Shapefile to a shapefile.
     * 
     * @param anOutputFileName
     *            The name of the shapefile to write to.
     * @return True if the operation was a success, false otherwise.
     */
    public boolean writeShapefile(String anOutputFileName) {
        if (!this.checkFeatures()) {
            return false;
        }
        // Check the output file can be written to
        File outFile = new File(anOutputFileName);
        // Create a feature collection to write the features out to
        DefaultFeatureCollection outFeatures = new DefaultFeatureCollection();
        for (SimpleFeature simpleFeature : this.theFeaturesMap.values()) {
            outFeatures.add(simpleFeature);
        }
        try {
            // Don't really get the rest, copied from the GeoTools tutorial.
            DataStoreFactorySpi factory = new ShapefileDataStoreFactory();
            Map<String, Serializable> create = new HashMap<String, Serializable>();
            create.put("url", outFile.toURI().toURL());
            create.put("create spatial index", Boolean.TRUE);
            ShapefileDataStore newDataStore = (ShapefileDataStore) factory
                    .createNewDataStore(create);
            newDataStore.createSchema(outFeatures.getSchema());
            Transaction transaction = new DefaultTransaction("create");
            String typeName = newDataStore.getTypeNames()[0];
            FeatureStore<SimpleFeatureType, SimpleFeature> featureStore;
            featureStore = (FeatureStore<SimpleFeatureType, SimpleFeature>) newDataStore
                    .getFeatureSource(typeName);
            featureStore.setTransaction(transaction);
            try {
                featureStore.addFeatures(outFeatures);
                transaction.commit();
            } catch (Exception problem) {
                if (statusHandler.isPriorityEnabled(Priority.PROBLEM)) {
                    statusHandler
                            .handle(Priority.PROBLEM,
                                    "Shapefile.writeShapefile() caught a problem trying to write: ",
                                    problem);
                }
                problem.printStackTrace();
                transaction.rollback();
                return false;
            } finally {
                transaction.close();
            }

        } catch (IOException e) {
            if (statusHandler.isPriorityEnabled(Priority.PROBLEM)) {
                statusHandler
                        .handle(Priority.PROBLEM,
                                "Shapefile.writeShapefile() caught an IOException trying to write shapefile.",
                                e);
            }
            return false;
        }
        return true;
    }

    /**
     * Resets this <code>Shapefile</code> by removing all existing features and
     * re-reading the original shapefile.
     */
    public void reset() {
        this.clear();
        this.readShapefile();
    }

    /**
     * Return the ID's of all the features in this shapfile. This can be used to
     * iterate over all <code>SimpleFeature</code>s or all <code>Geometry</code>
     * s.
     * 
     * @return The ID of every feature currently in this <code>Shapefile</code>.
     */
    public Set<String> getFeatureIDs() {
        return this.theFeaturesMap.keySet();
    }

    /**
     * Get the feature with the associated ID.
     * 
     * @param id
     * @return The Feature or null if no feature is found.
     */
    public SimpleFeature getFeature(int id) {
        if (!this.theFeaturesMap.containsKey(id)) {
            if (statusHandler.isPriorityEnabled(Priority.PROBLEM)) {
                statusHandler.handle(Priority.PROBLEM,
                        "Shapefile.getFeature() error, no feature found with ID: "
                                + id);
            }
        }
        return this.theFeaturesMap.get(id);
    }

    /**
     * Get the geometry of the object with the associated ID.
     * 
     * @param id
     * @return The Geometry or null if no feature is found.
     */
    public Geometry getGeometry(int id) {
        if (!this.theGeometriesMap.containsKey(id)) {
            if (statusHandler.isPriorityEnabled(Priority.PROBLEM)) {
                statusHandler.handle(Priority.PROBLEM,
                        "Shapefile.getFeature() error, no feature found with ID: "
                                + id);
            }
        }
        return this.theGeometriesMap.get(id);
    }

    /**
     * Get the feature with the associated ID.
     * 
     * @param id
     * @return The Feature or null if no feature is found.
     */
    public SimpleFeature getFeature(String id) {
        if (!this.theFeaturesMap.containsKey(id)) {
            if (statusHandler.isPriorityEnabled(Priority.PROBLEM)) {
                statusHandler.handle(Priority.PROBLEM,
                        "Shapefile.getFeature() error, no feature found with ID: "
                                + id);
            }
        }
        return this.theFeaturesMap.get(id);
    }

    /**
     * Make sure there are some features in this Shapefile, return false if not.
     */
    private boolean checkFeatures() {
        if ((this.theFeaturesMap == null) || (this.theFeaturesMap.size() == 0)) {
            return false;
        } else {
            return true;
        }
    }

    /**
     * Clears all objects from this Shapefile, useful for when the shape file is
     * going to be re-read.
     */
    private void clear() {
        this.theGeometriesMap.clear();
        this.theFeaturesMap.clear();
    }

    /**
     * Initializes the class attributes
     */
    private void init(File aFile) {
        this.theGeometriesMap = new Hashtable<String, Geometry>();
        this.theFeaturesMap = new Hashtable<String, SimpleFeature>();
        this.theFile = aFile;

    }
}
