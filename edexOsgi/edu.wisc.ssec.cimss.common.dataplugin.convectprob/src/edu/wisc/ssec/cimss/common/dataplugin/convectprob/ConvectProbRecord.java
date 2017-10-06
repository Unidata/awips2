package edu.wisc.ssec.cimss.common.dataplugin.convectprob;

import javax.persistence.Entity;
import javax.persistence.SequenceGenerator;
import javax.persistence.Table;
import javax.persistence.Transient;
import javax.persistence.UniqueConstraint;

import org.hibernate.annotations.Index;

import com.raytheon.uf.common.dataplugin.PluginDataObject;
import com.raytheon.uf.common.dataplugin.persist.IPersistable;
import com.raytheon.uf.common.dataplugin.persist.PersistablePluginDataObject;
import com.raytheon.uf.common.datastorage.IDataStore;
import com.raytheon.uf.common.datastorage.records.IDataRecord;
import com.raytheon.uf.common.datastorage.records.IntegerDataRecord;
import com.raytheon.uf.common.datastorage.records.StringDataRecord;
import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.vividsolutions.jts.geom.Geometry;
import com.vividsolutions.jts.io.WKTReader;

import edu.wisc.ssec.cimss.common.dataplugin.convectprob.impl.ShapeObject;

/**
 * NOAA/CIMSS Prob Severe Model Data Record Definition
 *
 * Data record that stores attributes of NOAA/CIMSS Prob Severe Model shapes
 *
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#     Engineer    Description
 * ------------ ----------  ----------- --------------------------
 * Mar 27, 2014 DCS 15298   lcronce     Initial Creation.
 *
 * </pre
 *
 * @author Lee Cronce
 * @version 1.0
 *
 */
@Entity
@SequenceGenerator(initialValue = 1, name = PluginDataObject.ID_GEN, sequenceName = "convectprobseq")
@Table(name = ConvectProbRecord.PLUGIN_NAME, uniqueConstraints = { @UniqueConstraint(columnNames = { "refTime" }) })
/*
 * Both refTime and forecastTime are included in the refTimeIndex since
 * forecastTime is unlikely to be used.
 */
@org.hibernate.annotations.Table(appliesTo = ConvectProbRecord.PLUGIN_NAME, indexes = { @Index(name = "convectprob_refTimeIndex", columnNames = {
        "refTime", "forecastTime" }) })
@DynamicSerialize
public class ConvectProbRecord extends PersistablePluginDataObject implements
IPersistable {

    /** Serializable id * */
    private static final long serialVersionUID = 1L;

    public static final String PLUGIN_NAME = "convectprob";

    // Data store data item names
    @Transient
    private static final String[] DATA_NAMES = { "types", "probabilities", "mucapes", "ebshears", "meshes", "rcemisses", "rcicecfs", "polygons", "objectids" };

    private final static transient IUFStatusHandler statusHandler = UFStatus.getHandler(ConvectProbRecord.class);

    @Transient
    private String[] types = null;

    @Transient
    private int[] probabilities = null;

    @Transient
    private String[] mucapes = null;

    @Transient
    private String[] ebshears = null;

    @Transient
    private String[] meshes = null;

    @Transient
    private String[] rcemisses = null;

    @Transient
    private String[] rcicecfs = null;

    @Transient
    private String[] polygons = null;

    @Transient
    private String[] objectids = null;

    @Transient
    private Object[] dataArrays = null;

    @Transient
    private int insertIndex = 0;

    // Used to track
    @Transient
    long persistTime = 0L;

    /**
     * Required empty constructor.
     */
    public ConvectProbRecord() {
    }

    /**
     * Constructs a data record from a dataURI
     *
     * @param data URI
     */
    public ConvectProbRecord(String uri) {
        super(uri);
    }

    /**
     * Constructs a convectprob record with a
     * given amount of shapes
     *
     * @param size of arrays based on number of shapes
     */
    public ConvectProbRecord(int arraysSize) {
        types = new String[arraysSize];
        probabilities = new int[arraysSize];
        mucapes = new String[arraysSize];
        ebshears = new String[arraysSize];
        meshes = new String[arraysSize];
        rcemisses = new String[arraysSize];
        rcicecfs = new String[arraysSize];
        polygons = new String[arraysSize];
        objectids = new String[arraysSize];
        dataArrays = new Object[] { types, probabilities, mucapes, ebshears, meshes, rcemisses, rcicecfs, polygons, objectids };
        insertIndex = 0;
    }

    /**
     * Adds a convectprob shape to the record collection.
     *
     * @param shape to add
     */
    public void addShape(ShapeObject shape) {
        try {
            types[insertIndex] = shape.getType();
            probabilities[insertIndex] = shape.getProbability();
            mucapes[insertIndex] = shape.getMucape();
            ebshears[insertIndex] = shape.getEbshear();
            meshes[insertIndex] = shape.getMesh();
            rcemisses[insertIndex] = shape.getRcemiss();
            rcicecfs[insertIndex] = shape.getRcicecf();
            polygons[insertIndex] = shape.getPolygon();
            objectids[insertIndex] = shape.getObjectid();
            insertIndex++;
        } catch (Exception e) {
            statusHandler.error("insertIndex is out of bounds: " + Integer.toString(insertIndex), e);
        }
    }

    /**
     * Retrieve names of data contained in data record
     *
     * @return string array of data record field names
     */
    public String[] getDataNames() {
        return DATA_NAMES;
    }

    /**
     * Retrieve data contained within the data record
     *
     * @return array of data objects stored in record
     */
    public Object[] getDataArrays() {
        return dataArrays;
    }

    /**
     * Set data to be contained within the data record
     *
     * @param array of data objects to be set
     */
    public void setDataArrays(Object[] dataArrays) {
        this.dataArrays = dataArrays;
    }

    /**
     * Retrieves the shape types
     *
     * @return shape types
     */
    public String[] getTypes() {
        return types;
    }

    /**
     * Retrieves the shape probabilities
     *
     * @return shape probabilities of severe
     */
    public int[] getProbabilities() {
        return probabilities;
    }

    /**
     *Retrieves the MUCAPE associated with shapes
     *
     * @return shape MUCAPEs
     */
    public String[] getMucapes() {
        return mucapes;
    }

    /**
     * Retrieves the EB shear associated with shapes
     *
     * @return shape EB Shears
     */
    public String[] getEbshears() {
        return ebshears;
    }

    /**
     * Retrieves the MESH associated with the shapes
     *
     * @return shape MESHes
     */
    public String[] getMeshes() {
        return meshes;
    }

    /**
     * Retrieves the emissivities associated with the shapes
     *
     * @return shape top of troposphere emissivities
     */
    public String[] getRcemisses() {
        return rcemisses;
    }

    /**
     * Retrieves the ice cloud fractions associated with the shapes
     *
     * @return rates of change of ice cloud fraction
     */
    public String[] getRcicecfs() {
        return rcicecfs;
    }

    /**
     * Retrieves the shape polygons
     *
     * @return polygons defining the data record shapes
     */
    public String[] getPolygons() {
        return polygons;
    }

    /**
     * Retrieves the shape objectids
     *
     * @return objectids specifying the objectids of the shapes
     */
    public String[] getObjectids() {
        return objectids;
    }

    /**
     * Creating Geometry objects from String representation
     * of shape polygons
     *
     * @return Geometry objects of shape polygons
     */
    public Geometry[] getPolyGeoms() {
        Geometry[] polyGeoms = new Geometry[polygons.length];
        WKTReader reader = new WKTReader();
        int i = 0;
        for (String poly : polygons) {
            try {
                polyGeoms[i] = reader.read(poly);
                i++;
            } catch (Exception e) {
                statusHandler.error("Well Known Text reader could not read selected text: " + poly, e);
            }
        }
        return polyGeoms;
    }

    /**
     * Sets the data arrays from the store.
     *
     * @param dataStore
     */
    public void retrieveFromDataStore(IDataStore dataStore) {
        try {
            IDataRecord[] dataRec = dataStore.retrieve(getDataURI());
            dataArrays = new Object[dataRec.length];
            for (int i = 0; i < dataRec.length; i++) {
                if (dataRec[i].getName().equals("types")) {
                    types = ((StringDataRecord) dataRec[i]).getStringData();
                    dataArrays[i] = types;
                } else if (dataRec[i].getName().equals("probabilities")) {
                    probabilities = ((IntegerDataRecord) dataRec[i]).getIntData();
                    dataArrays[i] = probabilities;
                } else if (dataRec[i].getName().equals("mucapes")) {
                    mucapes = ((StringDataRecord) dataRec[i]).getStringData();
                    dataArrays[i] = mucapes;
                } else if (dataRec[i].getName().equals("ebshears")) {
                    ebshears = ((StringDataRecord) dataRec[i]).getStringData();
                    dataArrays[i] = ebshears;
                } else if (dataRec[i].getName().equals("meshes")) {
                    meshes = ((StringDataRecord) dataRec[i]).getStringData();
                    dataArrays[i] = meshes;
                } else if (dataRec[i].getName().equals("rcemisses")) {
                    rcemisses = ((StringDataRecord) dataRec[i]).getStringData();
                    dataArrays[i] = rcemisses;
                } else if (dataRec[i].getName().equals("rcicecfs")) {
                    rcicecfs = ((StringDataRecord) dataRec[i]).getStringData();
                    dataArrays[i] = rcicecfs;
                } else if (dataRec[i].getName().equals("polygons")) {
                    polygons = ((StringDataRecord) dataRec[i]).getStringData();
                    dataArrays[i] = polygons;
                } else if (dataRec[i].getName().equals("objectids")) {
                    objectids = ((StringDataRecord) dataRec[i]).getStringData();
                    dataArrays[i] = objectids;
                }
            }
            setDataArrays(dataArrays);

        } catch (Exception e) {
            statusHandler.error("Error retrieving data from the convectprob data store", e);
        }

    }

    /**
     * @see com.raytheon.uf.common.dataplugin.PluginDataObject#getPluginName()
     */
    @Override
    public String getPluginName() {
        return PLUGIN_NAME;
    }
}
