
package gov.noaa.nws.ncep.edex.plugin.mosaic.common;

import gov.noaa.nws.ncep.edex.plugin.mosaic.util.MosaicConstants;
import gov.noaa.nws.ncep.edex.plugin.mosaic.util.MosaicConstants.MapValues;
import gov.noaa.nws.ncep.edex.plugin.mosaic.util.level3.DataLevelThreshold;
import gov.noaa.nws.ncep.edex.plugin.mosaic.util.level3.SymbologyBlock;
import gov.noaa.nws.ncep.edex.plugin.mosaic.util.level3.SymbologyPacket;
import gov.noaa.nws.ncep.edex.plugin.mosaic.util.level3.SymbologyPoint;

import java.io.ByteArrayInputStream;
import java.io.FileNotFoundException;
import java.util.ArrayList;
import java.util.Calendar;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import javax.persistence.Access;
import javax.persistence.AccessType;
import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.SequenceGenerator;
import javax.persistence.Table;
import javax.persistence.Transient;
import javax.persistence.UniqueConstraint;
import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlRootElement;

import org.hibernate.annotations.Cache;
import org.hibernate.annotations.CacheConcurrencyStrategy;
import org.hibernate.annotations.Index;
import org.opengis.referencing.crs.ProjectedCRS;
import org.opengis.referencing.operation.MathTransform;

import com.raytheon.uf.common.dataplugin.IDecoderGettable;
import com.raytheon.uf.common.dataplugin.PluginDataObject;
import com.raytheon.uf.common.dataplugin.annotations.DataURI;
import com.raytheon.uf.common.dataplugin.persist.IPersistable;
import com.raytheon.uf.common.dataplugin.persist.PersistablePluginDataObject;
import com.raytheon.uf.common.datastorage.IDataStore;
import com.raytheon.uf.common.datastorage.Request;
import com.raytheon.uf.common.datastorage.StorageException;
import com.raytheon.uf.common.datastorage.records.ByteDataRecord;
import com.raytheon.uf.common.datastorage.records.ShortDataRecord;
import com.raytheon.uf.common.geospatial.ISpatialEnabled;
import com.raytheon.uf.common.geospatial.ISpatialObject;
import com.raytheon.uf.common.geospatial.MapUtil;
import com.raytheon.uf.common.serialization.DynamicSerializationManager;
import com.raytheon.uf.common.serialization.DynamicSerializationManager.SerializationType;
import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;
import com.raytheon.uf.common.serialization.annotations.DynamicSerializeElement;
import com.vividsolutions.jts.geom.Coordinate;

/**
 * Decoder implementation for mosaic plugin
 * 
 * <pre>
 * Date         Ticket#         Engineer    Description
 * ------------ ----------      ----------- --------------------------
 * 09/2009      143				L. Lin     	Initial creation
 * 11/2009      143             L. Lin      Add parameters sourceId and
 * 											trueElevationAngle in mosaic record.
 * 1/2011		143				T. Lee		Add resolution to key for AWC 1km NSSL
 * 										Extracted prod name from mosaicInfo.txt
 * 6/2012       825             G. Hull     rm prodName from URI. Use prodCode where needed.
 * 09/2012						B. Hebbard  Merge out RTS changes from OB12.9.1
 * Apr 4, 2013        1846 bkowal      Added an index on refTime and forecastTime
 * Apr 12, 2013 1857            bgonzale    Added SequenceGenerator annotation.
 * May 07, 2013 1869            bsteffen    Remove dataURI column from
 *                                          PluginDataObject.
 * 
 * </pre>
 * 
 * This code has been developed by the SIB for use in the AWIPS2 system.
 * 
 * @author L. Lin
 * @version 1.0
 */


@Entity
@SequenceGenerator(initialValue = 1, name = PluginDataObject.ID_GEN, sequenceName = "mosaicseq")
@Table(name = "mosaic", uniqueConstraints = { @UniqueConstraint(columnNames = { "dataURI" }) })
/*
 * Both refTime and forecastTime are included in the refTimeIndex since
 * forecastTime is unlikely to be used.
 */
@org.hibernate.annotations.Table(
		appliesTo = "mosaic",
		indexes = {
				@Index(name = "mosaic_refTimeIndex", columnNames = { "refTime", "forecastTime" } )
		}
)
@Cache(usage = CacheConcurrencyStrategy.TRANSACTIONAL)
@XmlRootElement
@XmlAccessorType(XmlAccessType.NONE)
@DynamicSerialize
public class MosaicRecord extends PersistablePluginDataObject implements
        IPersistable, ISpatialEnabled, IMosaicRecord {

    private static final long serialVersionUID = 1L;

    @Column
    @DataURI(position = 1)
    @DynamicSerializeElement
    @XmlElement(nillable = false)
    private Integer productCode;

    @Column
    @DataURI(position=2)
    @DynamicSerializeElement
    @XmlElement(nillable = false)
    private Integer resolution;

    @Column(length = 16)
    @DynamicSerializeElement
    @XmlElement(nillable = false)
    private String prodName;

    @Column
    @DynamicSerializeElement
    @XmlElement(nillable = false)
    private Float latitude;

    @Column
    @DynamicSerializeElement
    @XmlElement(nillable = false)
    private Float longitude;

    @Column
    @DynamicSerializeElement
    @XmlElement(nillable = false)
    private Float elevation;

    @Column
    @DynamicSerializeElement
    @XmlElement(nillable = false)
    private Float trueElevationAngle;

    // 10,000 for US
    @Column
    @DynamicSerializeElement
    @XmlElement(nillable = false)
    private Integer sourceId;

    @Column
    @DynamicSerializeElement
    @XmlElement(nillable = false)
    private Integer volumeCoveragePattern;

    @Column
    @DynamicSerializeElement
    @XmlElement(nillable = false)
    private Calendar issueTime;

    @Column
    @DynamicSerializeElement
    @XmlElement(nillable = false)
    private Calendar scanTime;

    @Column
    @DynamicSerializeElement
    @XmlElement(nillable = false)
    private Calendar generationTime;

    @Column
    @DynamicSerializeElement
    @XmlElement(nillable = false)
    private Integer numLevels;

    @Column
    @DynamicSerializeElement
    @XmlElement(nillable = false)
    private Integer nx;

    @Column
    @DynamicSerializeElement
    @XmlElement(nillable = false)
    private Integer ny;

    @Column
    @DynamicSerializeElement
    @XmlElement(nillable = false)
    private String format;

    @Column(length = 16)
    @DynamicSerializeElement
    @XmlElement(nillable = false)
    private String unit;

    @Transient
    private byte[] rawData;

    @Transient
    private byte[] headerBlock;

    @Transient
    private short[] thresholds;

    @Transient
    private short[] productDependentValues;

    @Transient
    private Object[] decodedThresholds;

    @Transient
    private SymbologyBlock symbologyBlock;

    @Transient
    private HashMap<MosaicConstants.MapValues, Map<String, Map<MosaicConstants.MapValues, String>>> productVals = new HashMap<MosaicConstants.MapValues, Map<String, Map<MosaicConstants.MapValues, String>>>();

    @Transient
    private HashMap<MosaicConstants.MapValues, Map<MosaicConstants.MapValues, String>> recordVals = new HashMap<MosaicConstants.MapValues, Map<MosaicConstants.MapValues, String>>();

    @Transient
    private Map<MosaicDataKey, MosaicDataPoint> symbologyData = new HashMap<MosaicDataKey, MosaicDataPoint>();

    public MosaicRecord() {
        super();
        this.nx = 0;
        this.ny = 0;
        rawData = null;
        thresholds = new short[16];
        decodedThresholds = new Object[16];
    }

    public MosaicRecord(int rows, int columns) {
        this();
        this.nx = rows;
        this.ny = columns;
        rawData = new byte[rows * columns];
        thresholds = new short[16];
        decodedThresholds = new Object[16];
    }

    /**
     * Constructs a mosaic record from a dataURI
     * 
     * @param uri
     *            The dataURI
     * @param tableDef
     *            The table definition associated with this class
     */
    public MosaicRecord(String uri) {
        super(uri);
    }

    /**
     * @param dataStore
     * @throws StorageException
     */
    public void retrieveFromDataStore(IDataStore dataStore)
            throws StorageException, FileNotFoundException {

        if ("Radial".equals(format) || "Raster".equals(format)) {
            // ByteDataRecord byteData = (ByteDataRecord) dataStore.retrieve(
            // getDataURI(), "Data");
            ByteDataRecord byteData = (ByteDataRecord) dataStore.retrieve(
                    getDataURI(), "Data", Request.ALL);
            setRawData(byteData.getByteData());
        }

        if ("Graphic".equals(format) || "XY".equals(format)
                || "Graph".equals(format)) {

            try {
                // ByteDataRecord byteData = (ByteDataRecord)
                // dataStore.retrieve(
                // getDataURI(), "Symbology");
                ByteDataRecord byteData = (ByteDataRecord) dataStore.retrieve(
                        getDataURI(), "Symbology", Request.ALL);
                ByteArrayInputStream bais = new ByteArrayInputStream(
                        byteData.getByteData());
                Object o = DynamicSerializationManager.getManager(
                        SerializationType.Thrift).deserialize(bais);
                setSymbologyBlock((SymbologyBlock) o);
            } catch (Throwable e) {
                setSymbologyBlock(null);
            }

            try {
                ByteDataRecord byteData = (ByteDataRecord) dataStore.retrieve(
                        getDataURI(), "ProductVals", Request.ALL);
                ByteArrayInputStream bais = new ByteArrayInputStream(
                        byteData.getByteData());
                Object o = DynamicSerializationManager.getManager(
                        SerializationType.Thrift).deserialize(bais);
                setProductVals((HashMap<MosaicConstants.MapValues, Map<String, Map<MosaicConstants.MapValues, String>>>) o);
            } catch (Throwable e) {
                setProductVals(null);
            }

        }
        try {
            ShortDataRecord shortData = (ShortDataRecord) dataStore.retrieve(
                    getDataURI(), "Thresholds", Request.ALL);
            setThresholds(shortData.getShortData());
        } catch (Exception e) {
            setThresholds(null);
        }

        try {
            ShortDataRecord depValData = (ShortDataRecord) dataStore.retrieve(
                    getDataURI(), "DependentValues", Request.ALL);
            setProductDependentValues(depValData.getShortData());
        } catch (Throwable e) {
            setProductDependentValues(null);
        }
    }

    public String getProdName() {
        return prodName;
    }

    public void setProdName(String prodName) {
        this.prodName = prodName;
    }

    /**
     * @return the productCode
     */
    public Integer getProductCode() {
        return productCode;
    }

    /**
     * @param productCode
     *            the productCode to set
     */
    public void setProductCode(Integer productCode) {
        this.productCode = productCode;
    }

    public Integer getNx() {
        return nx;
    }

    public void setNx(Integer nx) {
        this.nx = nx;
    }

    public Integer getNy() {
        return ny;
    }

    public void setNy(Integer ny) {
        this.ny = ny;
    }

    public Integer getResolution() {
        return resolution;
    }

    public void setResolution(Integer resolution) {
        this.resolution = resolution;
    }

    public Integer getSourceId() {
        return sourceId;
    }

    public void setSourceId(Integer sourceId) {
        this.sourceId = sourceId;
    }

    public Float getLatitude() {
        return latitude;
    }

    public void setLatitude(Float latitude) {
        this.latitude = latitude;
    }

    public Float getLongitude() {
        return longitude;
    }

    public void setLongitude(Float longitude) {
        this.longitude = longitude;
    }

    public Float getTrueElevationAngle() {
        return trueElevationAngle;
    }

    public void setTrueElevationAngle(Float trueElevationAngle) {
        this.trueElevationAngle = trueElevationAngle;
    }

    public byte[] getHeaderBlock() {
        return headerBlock;
    }

    public void setHeaderBlock(byte[] headerBlock) {
        this.headerBlock = headerBlock;
    }

    public byte[] getRawData() {
        return rawData;
    }

    public void setRawData(byte[] mosaicData) {
        this.rawData = mosaicData;
    }

    public int getOpMode() {
        return 1;
    }

    public void setRawDataValue(int radial, int bin, byte value) {
        if ((radial < nx) && (bin < ny)) {
            rawData[radial * ny + bin] = value;
        }
    }

    public byte getRawDataValue(int radial, int bin) {
        return rawData[radial * ny + bin];
    }

    public short getThreshold(int i) {
        return thresholds[i];
    }

    public Object getDecodedThreshold(int i) {
        return decodedThresholds[i];
    }

    public void setThreshold(int i, short threshold) {
        this.thresholds[i] = threshold;
        this.decodedThresholds[i] = new DataLevelThreshold(threshold).decode();
    }

    public short[] getThresholds() {
        return thresholds;
    }

    public Object[] getDecodedThresholds() {
        return decodedThresholds;
    }

    public void setThresholds(short[] thresholds) {
        this.thresholds = thresholds;
        this.decodedThresholds = null;
        if (thresholds != null) {
            this.decodedThresholds = new Object[thresholds.length];
            int i = 0;
            DataLevelThreshold dl = new DataLevelThreshold();
            for (short threshold : thresholds) {
                dl.set(threshold);
                this.decodedThresholds[i++] = dl.decode();
            }
        }
    }

    public Integer getNumLevels() {
        return numLevels;
    }

    public void setNumLevels(Integer numLevels) {
        this.numLevels = numLevels;
    }

    public Calendar getIssueTime() {
        return issueTime;
    }

    public void setIssueTime(Calendar issueTime) {
        this.issueTime = issueTime;
    }

    public Calendar getScanTime() {
        return scanTime;
    }

    public void setScanTime(Calendar scanTime) {
        this.scanTime = scanTime;
    }

    public Calendar getGenerationTime() {
        return generationTime;
    }

    public void setGenerationTime(Calendar generationTime) {
        this.generationTime = generationTime;
    }

    public String getFormat() {
        return format;
    }

    public void setFormat(String format) {
        this.format = format;
    }

    public Integer getVolumeCoveragePattern() {
        return volumeCoveragePattern;
    }

    public void setVolumeCoveragePattern(Integer volumeCoveragePattern) {
        this.volumeCoveragePattern = volumeCoveragePattern;
    }

    /**
     * @return the unit
     */
    public String getUnit() {
        return unit;
    }

    /**
     * @param unit
     *            the unit to set
     */
    public void setUnit(String unit) {
        this.unit = unit;
    }

    public Float getElevation() {
        return elevation;
    }

    public void setElevation(Float elevation) {
        this.elevation = elevation;
    }

    /**
     * Get the IDecoderGettable reference for this record.
     * 
     * @return The IDecoderGettable reference for this record. Null for this
     *         class.
     */
    @Override
    public IDecoderGettable getDecoderGettable() {
        return null;
    }

    /**
     * @return the symbologyBlock
     */
    public SymbologyBlock getSymbologyBlock() {
        return symbologyBlock;
    }

    /**
     * @param symbologyBlock
     *            the symbologyBlock to set
     */
    public void setSymbologyBlock(SymbologyBlock symbologyBlock) {
        this.symbologyBlock = symbologyBlock;
    }

    public short getProductDependentValue(int value) {
        return productDependentValues[value];
    }

    /**
     * @return the productDependentValues
     */
    public short[] getProductDependentValues() {
        return productDependentValues;
    }

    /**
     * @param productDependentValues
     *            the productDependentValues to set
     */
    public void setProductDependentValues(short[] productDependentValues) {
        this.productDependentValues = productDependentValues;
    }

    public ProjectedCRS getCRS() {
        return MapUtil.constructStereographic(MapUtil.AWIPS_EARTH_RADIUS,
                MapUtil.AWIPS_EARTH_RADIUS, this.latitude, this.longitude);
    }

    /**
     * @return the symbologyData
     */
    public Map<MosaicDataKey, MosaicDataPoint> getSymbologyData() {
        return symbologyData;
    }

    /**
     * @param symbologyData
     *            the symbologyData to set
     */
    public void setSymbologyData(
            Map<MosaicDataKey, MosaicDataPoint> symbologyData) {
        this.symbologyData = symbologyData;
    }

    public Integer getDataValue(Coordinate latLon) {
        double[] input = { latLon.x, latLon.y };
        double[] output = new double[2];
        try {
            MathTransform mt = MapUtil.getTransformFromLatLon(getCRS());

            mt.transform(input, 0, output, 0, 1);
        } catch (Exception e) {
            return null;
        }

        int dataValue = 0;

        // **********************************
        // Raster case
        // **********************************
        if ("Raster".equals(this.getFormat())) {

            int col = (int) (output[0] / this.getResolution() + (this.getNy() / 2));

            int row = (int) (output[1] / this.getResolution() + (this.getNy() / 2));

            row = this.getNy() - row - 1;

            if ((row >= 0) && (row < this.getNy()) && (col >= 0)
                    && (col < this.getNy())) {
                dataValue = this.getRawDataValue(row, col);
            }
        }

        return dataValue;
    }

    /**
     * @param productVals
     *            the productVals to set
     */
    public void setProductVals(
            HashMap<MosaicConstants.MapValues, Map<String, Map<MosaicConstants.MapValues, String>>> map) {
        this.productVals = map;
    }

    public void printSymbologyData() {
        System.out
                .println("******************************************************************************");
        System.out.println("*** START Symbology Data Contents");
        System.out
                .println("******************************************************************************");

        for (MosaicDataKey key : symbologyData.keySet()) {
            System.out.println("Key: " + key);
            MosaicDataPoint data = symbologyData.get(key);
            HashMap<Integer, ArrayList<SymbologyPoint>> displayPointData = data
                    .getDisplayPointData();
            for (Integer pktType : displayPointData.keySet()) {
                System.out.println("\tPt Type: " + pktType);
                for (SymbologyPoint currPt : displayPointData.get(pktType)) {
                    System.out.println("\t\t" + currPt);
                }
            }

            HashMap<Integer, ArrayList<SymbologyPacket>> displayPacketData = data
                    .getDisplayPacketData();
            for (Integer pktType : displayPacketData.keySet()) {
                System.out.println("\tPkt Type: " + pktType);
                for (SymbologyPacket currPkt : displayPacketData.get(pktType)) {
                    System.out.println("\t\t" + currPkt);
                }
            }
        }

        System.out
                .println("******************************************************************************");
        System.out.println("*** END Symbology Data Contents");
        System.out
                .println("******************************************************************************");
    }

    /*
     * Return the product values map for a type, a storm id, and the desired
     * value type
     * 
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.edex.plugin.mosaic.IMosaicRecord#getProductVals(com.raytheon
     * .edex.util.mosaic.MosaicConstants.MapValues)
     */
    @Override
    public String getProductVals(MosaicConstants.MapValues type, String id,
            MapValues valueName) {
        return productVals.get(type).get(id).get(valueName);
    }

    /**
     * Return the product values map for the product type and for a certain
     * storm id
     * 
     * @param type
     * @param id
     * @return
     */
    public Map<MapValues, String> getProductValsPerStorm(
            MosaicConstants.MapValues type, String id) {
        return productVals.get(type).get(id);
    }

    /**
     * Return the product values map for a certain product type
     * 
     * @param type
     * @return
     */
    public Map<String, Map<MapValues, String>> getProductValsPerType(
            MosaicConstants.MapValues type) {
        return productVals.get(type);
    }

    /**
     * Return the entire product values map
     * 
     * @return
     */
    public Map<MosaicConstants.MapValues, Map<String, Map<MapValues, String>>> getProductValsMap() {
        return productVals;
    }

    /**
     * @return the recordVals
     */
    public String getProductValsPerRecord(MosaicConstants.MapValues type,
            MosaicConstants.MapValues val) {
        return recordVals.get(type).get(val);
    }

    /**
     * @return the recordVals
     */
    public HashMap<MosaicConstants.MapValues, Map<MosaicConstants.MapValues, String>> getRecordVals() {
        return recordVals;
    }

    /**
     * @param recordVals
     *            the recordVals to set
     */
    public void setRecordVals(
            HashMap<MosaicConstants.MapValues, Map<MosaicConstants.MapValues, String>> recordVals) {
        this.recordVals = recordVals;
    }

    /**
     * Search in the map for the storm id and send back the feature if that is
     * the case
     * 
     * @param type
     * @param id
     * @return
     */
    public List<String> getFeatures(MosaicConstants.MapValues type, String id) {
        List<String> list = new ArrayList<String>();
        for (Map.Entry<String, Map<MosaicConstants.MapValues, String>> entry : productVals
                .get(type).entrySet()) {
            if (id.trim().equals(
                    entry.getValue()
                            .get(MosaicConstants.MapValues.MESO_STORM_ID)
                            .toString().trim())) {
                list.add(entry.getKey());
            }
        }
        return list;
    }

    @Override
    public ISpatialObject getSpatialObject() {
        // TODO Auto-generated method stub
        return null;
    }

    @Override
    @Column
    @Access(AccessType.PROPERTY)
    public String getDataURI() {
        return super.getDataURI();
    }
}
