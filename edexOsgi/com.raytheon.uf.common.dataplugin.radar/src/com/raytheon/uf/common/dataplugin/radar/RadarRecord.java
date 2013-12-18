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

package com.raytheon.uf.common.dataplugin.radar;

import java.awt.geom.Point2D;
import java.text.ParsePosition;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Date;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.Vector;

import javax.measure.unit.Unit;
import javax.measure.unit.UnitFormat;
import javax.persistence.Access;
import javax.persistence.AccessType;
import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.ManyToOne;
import javax.persistence.PrimaryKeyJoinColumn;
import javax.persistence.SequenceGenerator;
import javax.persistence.Table;
import javax.persistence.Transient;
import javax.persistence.UniqueConstraint;
import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlRootElement;

import org.geotools.referencing.GeodeticCalculator;
import org.hibernate.annotations.Index;
import org.opengis.referencing.crs.ProjectedCRS;

import com.raytheon.uf.common.dataplugin.IDecoderGettable;
import com.raytheon.uf.common.dataplugin.PluginDataObject;
import com.raytheon.uf.common.dataplugin.annotations.DataURI;
import com.raytheon.uf.common.dataplugin.persist.IHDFFilePathProvider;
import com.raytheon.uf.common.dataplugin.persist.PersistablePluginDataObject;
import com.raytheon.uf.common.dataplugin.radar.RadarDataPoint.RadarProductType;
import com.raytheon.uf.common.dataplugin.radar.level3.AlertAdaptationParameters;
import com.raytheon.uf.common.dataplugin.radar.level3.AlertMessage;
import com.raytheon.uf.common.dataplugin.radar.level3.CellTrendDataPacket;
import com.raytheon.uf.common.dataplugin.radar.level3.CellTrendVolumeScanPacket;
import com.raytheon.uf.common.dataplugin.radar.level3.DMDPacket;
import com.raytheon.uf.common.dataplugin.radar.level3.DMDPacket.DMDAttributeIDs;
import com.raytheon.uf.common.dataplugin.radar.level3.DataLevelThreshold;
import com.raytheon.uf.common.dataplugin.radar.level3.GFMPacket;
import com.raytheon.uf.common.dataplugin.radar.level3.GFMPacket.GFMAttributeIDs;
import com.raytheon.uf.common.dataplugin.radar.level3.GSMBlock.GSMMessage;
import com.raytheon.uf.common.dataplugin.radar.level3.GenericDataPacket;
import com.raytheon.uf.common.dataplugin.radar.level3.GraphicBlock;
import com.raytheon.uf.common.dataplugin.radar.level3.HdaHailPacket;
import com.raytheon.uf.common.dataplugin.radar.level3.HdaHailPacket.HdaHailPoint;
import com.raytheon.uf.common.dataplugin.radar.level3.Layer;
import com.raytheon.uf.common.dataplugin.radar.level3.LinkedContourVectorPacket;
import com.raytheon.uf.common.dataplugin.radar.level3.LinkedVectorPacket;
import com.raytheon.uf.common.dataplugin.radar.level3.MesocyclonePacket;
import com.raytheon.uf.common.dataplugin.radar.level3.MesocyclonePacket.MesocyclonePoint;
import com.raytheon.uf.common.dataplugin.radar.level3.SCITDataPacket;
import com.raytheon.uf.common.dataplugin.radar.level3.SCITDataPacket.SCITDataCell;
import com.raytheon.uf.common.dataplugin.radar.level3.SpecialGraphicSymbolPacket;
import com.raytheon.uf.common.dataplugin.radar.level3.SpecialGraphicSymbolPacket.SpecialGraphicPoint;
import com.raytheon.uf.common.dataplugin.radar.level3.StormIDPacket;
import com.raytheon.uf.common.dataplugin.radar.level3.StormIDPacket.StormIDPoint;
import com.raytheon.uf.common.dataplugin.radar.level3.SymbologyBlock;
import com.raytheon.uf.common.dataplugin.radar.level3.SymbologyPacket;
import com.raytheon.uf.common.dataplugin.radar.level3.SymbologyPoint;
import com.raytheon.uf.common.dataplugin.radar.level3.TVSPacket;
import com.raytheon.uf.common.dataplugin.radar.level3.TVSPacket.TVSPoint;
import com.raytheon.uf.common.dataplugin.radar.level3.TabularBlock;
import com.raytheon.uf.common.dataplugin.radar.level3.TextSymbolPacket;
import com.raytheon.uf.common.dataplugin.radar.level3.UnlinkedContourVectorPacket;
import com.raytheon.uf.common.dataplugin.radar.level3.UnlinkedVectorPacket;
import com.raytheon.uf.common.dataplugin.radar.level3.WindBarbPacket;
import com.raytheon.uf.common.dataplugin.radar.level3.WindBarbPacket.WindBarbPoint;
import com.raytheon.uf.common.dataplugin.radar.level3.generic.AreaComponent;
import com.raytheon.uf.common.dataplugin.radar.level3.generic.GenericDataComponent;
import com.raytheon.uf.common.dataplugin.radar.level3.generic.GenericDataParameter;
import com.raytheon.uf.common.dataplugin.radar.units.DigitalVilUnit;
import com.raytheon.uf.common.dataplugin.radar.util.RadarConstants;
import com.raytheon.uf.common.dataplugin.radar.util.RadarConstants.MapValues;
import com.raytheon.uf.common.geospatial.CRSCache;
import com.raytheon.uf.common.geospatial.ISpatialEnabled;
import com.raytheon.uf.common.geospatial.MapUtil;
import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;
import com.raytheon.uf.common.serialization.annotations.DynamicSerializeElement;
import com.raytheon.uf.common.time.DataTime;
import com.raytheon.uf.common.units.PiecewisePixel;
import com.vividsolutions.jts.geom.Coordinate;

/**
 * Record implementation for radar plugin
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#     Engineer    Description
 * ------------ ----------  ----------- --------------------------
 * Feb 14, 2007 139         Phillippe   Initial creation
 * Sep 14, 2007 379         jkorman     Added populateDataStore() and
 *                                      getPersistenceTime() from new
 *                                      IPersistable
 * Oct 09, 2007 465         randerso    Updated to better represent level 3 data
 * Nov 29, 2007 472         jkorman     Added IDecoderGettable interface.
 * Mar 04, 2013 DCS51       zwang       Handle MIGFA product
 * Mar 18, 2013 1804        bsteffen    Remove AlphanumericValues from radar
 *                                      HDF5.
 * Apr 04, 2013 1846        bkowal      Added an index on refTime and
 *                                      forecastTime
 * Apr 08, 2013 1293        bkowal      Removed references to hdffileid.
 * Apr 12, 2013 1857        bgonzale    Added SequenceGenerator annotation.
 * May 07, 2013 1869        bsteffen    Remove dataURI column from
 *                                      PluginDataObject.
 * Aug 30, 2013 2298        rjpeter     Make getPluginName abstract
 * Dec 18, 2013 16002       kshrestha   Added logic to match all dBZ values in the DHR with AWIPS1
 * 
 * </pre>
 * 
 * @author bphillip
 * @version 1
 */
@Entity
@SequenceGenerator(initialValue = 1, name = PluginDataObject.ID_GEN, sequenceName = "radarseq")
@Table(name = "radar", uniqueConstraints = { @UniqueConstraint(columnNames = { "dataURI" }) })
/*
 * Both refTime and forecastTime are included in the refTimeIndex since
 * forecastTime is unlikely to be used.
 */
@org.hibernate.annotations.Table(appliesTo = "radar", indexes = { @Index(name = "radar_refTimeIndex", columnNames = {
        "refTime", "forecastTime" }) })
@XmlRootElement
@XmlAccessorType(XmlAccessType.NONE)
@DynamicSerialize
public class RadarRecord extends PersistablePluginDataObject implements
        ISpatialEnabled, IRadarRecord {

    private static final long serialVersionUID = 1L;

    @Column
    @DataURI(position = 2)
    @DynamicSerializeElement
    @XmlElement(nillable = false)
    private Integer productCode;

    @Column
    @DynamicSerializeElement
    @XmlElement(nillable = false)
    private Integer gateResolution;

    @Column(length = 7)
    @DataURI(position = 1)
    @DynamicSerializeElement
    @XmlElement(nillable = false)
    private String icao;

    @Column
    @DataURI(position = 3)
    @DynamicSerializeElement
    @XmlElement(nillable = false)
    private Double layer = 0.0;

    @Column
    @DataURI(position = 4)
    @DynamicSerializeElement
    @XmlElement(nillable = false)
    private Double primaryElevationAngle;

    @Column
    @DynamicSerializeElement
    @XmlElement(nillable = false)
    private Float trueElevationAngle;

    @Column
    @DynamicSerializeElement
    @XmlElement(nillable = false)
    private Date volScanTime;

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
    private Integer elevationNumber;

    @Column
    @DynamicSerializeElement
    @XmlElement(nillable = false)
    private Integer volumeCoveragePattern;

    @Column
    @DynamicSerializeElement
    @XmlElement(nillable = false)
    private Integer numLevels;

    @Column
    @DynamicSerializeElement
    @XmlElement(nillable = false)
    private Integer numRadials;

    @Column
    @DynamicSerializeElement
    @XmlElement(nillable = false)
    private Integer numBins;

    @Column(length = 7)
    @DynamicSerializeElement
    @XmlElement(nillable = false)
    private String mnemonic;

    @Column
    @DynamicSerializeElement
    @XmlElement(nillable = false)
    private String format;

    @Column
    @DynamicSerializeElement
    @XmlElement(nillable = false)
    private String displayModes;

    @Column
    @DynamicSerializeElement
    @XmlElement(nillable = false)
    private Integer operationalMode;

    @Column(length = 16)
    @DynamicSerializeElement
    @XmlElement(nillable = false)
    private String unit;

    @ManyToOne
    @PrimaryKeyJoinColumn
    @XmlElement
    @DynamicSerializeElement
    private RadarStation location;

    @Column
    @DynamicSerializeElement
    @XmlElement(nillable = false)
    private Integer istart;

    @Column
    @DynamicSerializeElement
    @XmlElement(nillable = false)
    private Integer jstart;

    @Column
    @DynamicSerializeElement
    @XmlElement(nillable = false)
    private Integer xscale;

    @Column
    @DynamicSerializeElement
    @XmlElement(nillable = false)
    private Integer yscale;

    @Column
    @DynamicSerializeElement
    @XmlElement(nillable = false)
    private Boolean lastElevationAngle = false;

    @Transient
    protected RadarStoredData storedData;

    @Transient
    protected Object[] decodedThresholds;

    @Transient
    public double srmSpeed = 0;

    @Transient
    public double srmDirection = 0;

    @Transient
    public Date srmMovement = null;

    @Transient
    public String srmSourceName = null;

    @Transient
    public byte[] srmData = null;

    @Transient
    protected transient ProjectedCRS crs;

    @Transient
    protected boolean addSpatial = true;

    public static enum OperationalModes {
        MAINTENANCE(0), CLEAN_AIR(1), PRECIP(2);

        private int mode;

        OperationalModes(int mode) {
            this.mode = mode;
        }

        public int getValue() {
            return mode;
        }

        public static OperationalModes valueOf(int code) {
            OperationalModes rval = null;

            for (OperationalModes currMode : OperationalModes.values()) {
                if (currMode.getValue() == code) {
                    rval = currMode;
                    break;
                }
            }

            return rval;
        }
    }

    public RadarRecord() {
        super();
        this.numRadials = 0;
        this.numBins = 0;
        this.storedData = new RadarStoredData();
        this.storedData.setRawData(null);
        this.storedData.setAngleData(null);
        this.storedData.setThresholds(new short[16]);
        decodedThresholds = new Object[16];
    }

    public RadarRecord(int numRadials, int numBins) {
        this();
        this.numRadials = numRadials;
        this.numBins = numBins;
        this.storedData = new RadarStoredData();
        this.storedData.setRawData(new byte[numRadials * numBins]);
        this.storedData.setAngleData(new float[numRadials]);
        this.storedData.setThresholds(new short[16]);
        decodedThresholds = new Object[16];
    }

    /**
     * Constructs a radar record from a dataURI
     * 
     * @param uri
     *            The dataURI
     * @param tableDef
     *            The table definition associated with this class
     */
    public RadarRecord(String uri) {
        super(uri);
    }

    public RadarRecord(RadarRecord that) {
        this.id = that.id;
        this.dataURI = that.dataURI;
        this.dataTime = that.dataTime;
        this.insertTime = that.insertTime;
        this.messageData = that.messageData;
        this.productCode = that.productCode;
        this.gateResolution = that.gateResolution;
        this.icao = that.icao;
        this.layer = that.layer;
        this.primaryElevationAngle = that.primaryElevationAngle;
        this.trueElevationAngle = that.trueElevationAngle;
        this.volScanTime = that.volScanTime;
        this.latitude = that.latitude;
        this.longitude = that.longitude;
        this.elevation = that.elevation;
        this.elevationNumber = that.elevationNumber;
        this.volumeCoveragePattern = that.volumeCoveragePattern;
        this.numLevels = that.numLevels;
        this.numRadials = that.numRadials;
        this.numBins = that.numBins;
        this.mnemonic = that.mnemonic;
        this.format = that.format;
        this.displayModes = that.displayModes;
        this.operationalMode = that.operationalMode;
        this.unit = that.unit;
        this.location = that.location;
        this.istart = that.istart;
        this.jstart = that.jstart;
        this.xscale = that.xscale;
        this.yscale = that.yscale;
        this.lastElevationAngle = that.lastElevationAngle;
        this.storedData = that.storedData;
        this.decodedThresholds = that.decodedThresholds;
        this.srmSpeed = that.srmSpeed;
        this.srmDirection = that.srmDirection;
        this.srmMovement = that.srmMovement;
        this.srmSourceName = that.srmSourceName;
        this.srmData = that.srmData;
        this.crs = that.crs;
        this.addSpatial = that.addSpatial;
    }

    @Override
    public RadarStation getSpatialObject() {
        return location;
    }

    /**
     * @return the icao
     */
    public String getIcao() {
        return icao;
    }

    /**
     * @param icao
     *            the icao to set
     */
    public void setIcao(String icao) {
        this.icao = icao;
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

    /**
     * @return the elevationNumber
     */
    public Integer getElevationNumber() {
        return elevationNumber;
    }

    /**
     * @param elevationNumber
     *            the elevationNumber to set
     */
    public void setElevationNumber(Integer elevationNumber) {
        this.elevationNumber = elevationNumber;
    }

    /**
     * @return the elevation angle
     */
    public Float getTrueElevationAngle() {
        return trueElevationAngle;
    }

    /**
     * @param elevation
     *            The elevation to set
     */
    public void setTrueElevationAngle(Float elevation) {
        this.trueElevationAngle = elevation;
    }

    public Integer getNumRadials() {
        return numRadials;
    }

    public void setNumRadials(Integer numRadials) {
        this.numRadials = numRadials;
    }

    public Integer getNumBins() {
        return numBins;
    }

    public void setNumBins(Integer numBins) {
        this.numBins = numBins;
    }

    public Integer getGateResolution() {
        return gateResolution;
    }

    public void setGateResolution(Integer gateResolution) {
        this.gateResolution = gateResolution;
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

    public Integer getIstart() {
        return istart;
    }

    public void setIstart(Integer istart) {
        this.istart = istart;
    }

    public Integer getJstart() {
        return jstart;
    }

    public void setJstart(Integer jstart) {
        this.jstart = jstart;
    }

    public Integer getXscale() {
        return xscale;
    }

    public void setXscale(Integer xscale) {
        this.xscale = xscale;
    }

    public Integer getYscale() {
        return yscale;
    }

    public void setYscale(Integer yscale) {
        this.yscale = yscale;
    }

    public boolean isLastElevationAngle() {
        if (lastElevationAngle == null) {
            return false;
        } else {
            return lastElevationAngle;
        }
    }

    public void setLastElevationAngle(boolean lastElevationAngle) {
        this.lastElevationAngle = lastElevationAngle;
    }

    public byte[] getRawData() {
        return getStoredData().getRawData();
    }

    public void setRawData(byte[] radarData) {
        getStoredData().setRawData(radarData);
    }

    /**
     * @return the rawShortData
     */
    public short[] getRawShortData() {
        return getStoredData().getRawShortData();
    }

    /**
     * @param rawShortData
     *            the rawShortData to set
     */
    public void setRawShortData(short[] rawShortData) {
        getStoredData().setRawShortData(rawShortData);
    }

    /**
     * @return the operationalMode
     */
    public Integer getOperationalMode() {
        return operationalMode;
    }

    /**
     * @param operationalMode
     *            the operationalMode to set
     */
    public void setOperationalMode(Integer operationalMode) {
        this.operationalMode = operationalMode;
    }

    public void setRawDataValue(int radial, int bin, byte value) {
        byte[] rawData = getRawData();
        if ((rawData != null) && (numRadials != null) && (numBins != null)) {
            if ((radial < numRadials) && (bin < numBins)) {
                rawData[(radial * numBins) + bin] = value;
            }
        }
    }

    public byte getRawDataValue(int radial, int bin) {
        byte[] rawData = getRawData();
        if ((rawData != null) && (numRadials != null) && (numBins != null)) {
            if ((radial < numRadials) && (bin < numBins)) {
                return rawData[(radial * numBins) + bin];
            }
        }
        return 0;
    }

    // If this is short data return that, if this byte data return the unsigned
    // byte value
    public short getRawShortDataValue(int radial, int bin) {
        if ((radial < numRadials) && (bin < numBins)) {
            short[] rawShortData = getRawShortData();
            byte[] rawData = getRawData();
            if (rawShortData != null) {
                return rawShortData[(radial * numBins) + bin];
            } else if (rawData != null) {
                return (short) (rawData[(radial * numBins) + bin] & 0xFF);
            }
        }
        return 0;
    }

    // Return the unsigned value of the short or byte as an integer
    public int getRawIntDataValue(int radial, int bin) {
        if ((radial < numRadials) && (bin < numBins)) {
            short[] rawShortData = getRawShortData();
            byte[] rawData = getRawData();
            if (rawShortData != null) {
                return rawShortData[(radial * numBins) + bin] & 0xFFFF;
            } else if (rawData != null) {
                return rawData[(radial * numBins) + bin] & 0xFF;
            }
        }
        return 0;
    }

    @XmlElement
    public float[] getAngleData() {
        return getStoredData().getAngleData();
    }

    public void setAngleData(float[] angleData) {
        getStoredData().setAngleData(angleData);
    }

    public void setRadialAzimuth(int radial, float azimuth) {
        if (radial < numRadials) {
            getAngleData()[radial] = azimuth;
        }
    }

    public float getRadialAzimuth(int radial) {
        return getAngleData()[radial];
    }

    public short getThreshold(int i) {
        return getThresholds()[i];
    }

    public Object getDecodedThreshold(int i) {
        return getDecodedThresholds()[i];
    }

    public void setThreshold(int i, short threshold) {
        this.getThresholds()[i] = threshold;
        this.decodedThresholds[i] = new DataLevelThreshold(threshold).decode();
    }

    public short[] getThresholds() {
        return getStoredData().getThresholds();
    }

    public Object[] getDecodedThresholds() {
        return decodedThresholds;
    }

    public void setThresholds(short[] thresholds) {
        getStoredData().setThresholds(thresholds);
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

    public String getMnemonic() {
        return mnemonic;
    }

    public void setMnemonic(String mnemonic) {
        this.mnemonic = mnemonic;
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
     * Gets the parameter unit as a javax.measure.Unit<?> object. If the
     * parameter unit string cannot be successfully converted to a
     * javax.measure.Unit<?> object, Unit.ONE is returned
     * 
     * @return The parameter unit as a javax.measure.Unit<?> object
     */
    public Unit<?> getUnitObject() {
        Unit<?> retVal = Unit.ONE;

        if (unit != null) {
            try {
                retVal = UnitFormat.getUCUMInstance().parseProductUnit(unit,
                        new ParsePosition(0));
            } catch (Exception e) {
                // Unable to parse
                retVal = Unit.ONE;
            }
        }

        return retVal;
    }

    /**
     * Get the actual unit of the raw bytes with any scale/offset or other
     * information from the thresholds.
     * 
     * @return a unit object that describes the actual byte values.
     */
    public Unit<?> getDataUnit() {
        Unit<?> rval = null;
        int numLevels = getNumLevels();
        Object[] thresholds = getDecodedThresholds();
        if (numLevels <= 16) {
            ArrayList<Integer> pixel = new ArrayList<Integer>();
            ArrayList<Float> real = new ArrayList<Float>();
            if ("V".equals(getDisplayModes())) {
                // V does this weird thing at zero, they have a data value of -1
                // at index 7 which just symbolizes that the data goes from -10
                // - 0, which seems pointless, and A1 also throws it out
                int p = 1;
                for (int i = 0; i < numLevels; i++) {
                    if (i == 7) {
                        continue;
                    }
                    if (thresholds[i] instanceof Float) {
                        pixel.add(p);
                        real.add((Float) thresholds[i]);
                    }
                    p++;
                }
            } else {
                for (int i = 0; i < numLevels; i++) {
                    if (thresholds[i] instanceof Float) {
                        if (real.contains(thresholds[i])) {
                            // Try to determine if we can treat one of these
                            // different
                            Float fVal = (Float) thresholds[i];
                            Integer prevI = pixel.get(real
                                    .indexOf(thresholds[i]));
                            DataLevelThreshold prevThresh = new DataLevelThreshold(
                                    getThreshold(prevI));
                            DataLevelThreshold currThresh = new DataLevelThreshold(
                                    getThreshold(i));
                            if (prevThresh.isLessThan()
                                    || prevThresh.isGtrThan()) {
                                if (prevThresh.isLessThan()) {
                                    getDecodedThresholds()[prevI] = "<"
                                            + fVal.intValue();
                                } else {
                                    getDecodedThresholds()[prevI] = ">"
                                            + fVal.intValue();
                                }
                                real.remove(fVal);
                                real.add(fVal);
                                pixel.remove(prevI);
                                pixel.add(i);
                                continue;
                            } else if (currThresh.isLessThan()) {
                                getDecodedThresholds()[i] = "<"
                                        + fVal.intValue();
                                continue;
                            } else if (currThresh.isGtrThan()) {
                                getDecodedThresholds()[i] = ">"
                                        + fVal.intValue();
                                continue;
                            }
                        }
                        pixel.add(i);
                        real.add((Float) thresholds[i]);

                    }
                }
            }

            if (pixel.size() == 0) {
                return getUnitObject();
            }

            double[] pix = new double[pixel.size()];
            int i = 0;
            for (Integer p : pixel) {
                pix[i++] = p;
            }

            double[] std = new double[real.size()];

            boolean allZeroes = true;

            i = 0;
            for (Float r : real) {
                allZeroes = allZeroes && (r == 0);
                std[i++] = r;
            }
            if (allZeroes) {
                // allZeroes is not a valid unit and is basically disgarded,
                // this check is done for CFC
                rval = getUnitObject();
            } else {
                rval = new PiecewisePixel(getUnitObject(), pix, std);
            }
        } else if (getProductCode() == 134) {
            // Digital Vil is all messy in the spec.
            rval = new DigitalVilUnit(getThresholds());
        } else if (getThreshold(5) == 0) {
            // The offset and scale are set as ints
            double offset = getThreshold(0);
            double scale = getThreshold(1);
            int nLevels = getThreshold(2);
            // I believe all products have at least one flag value and DHR is
            // reporting 256 levels
            if (nLevels > 255) {
                nLevels = 255;
            }
            double[] pix = { 256 - nLevels, 255 };
            if (getProductCode() == 155) {
                pix = new double[] { 129, 149 };
            } else if(getProductCode() == 32) {
                pix = new double[]{ 2, 256 };
            }

            double[] data = { offset, offset + ((nLevels - 1) * scale) };
            rval = new PiecewisePixel(getUnitObject(), pix, data);

        } else {
            // The offset and scale are set as floats
            double scale = Float.intBitsToFloat((getThreshold(0) << 16)
                    + getThreshold(1));
            if (scale == 0.0) {
                // 0.0 is sometimes used by HC and leads to a massively invalid
                // data range.
                scale = 1.0;
            }
            double offset = Float.intBitsToFloat((getThreshold(2) << 16)
                    + getThreshold(3));
            int nLevels = getThreshold(5);
            if (nLevels < 0) {
                // Only for DPR, the 65536 can't be encoded in a short
                nLevels = getNumLevels();
            }
            int nFlags = getThreshold(6);

            double[] pix = { nFlags, nLevels };
            double[] data = { (nFlags - offset) / scale,
                    (nLevels - offset) / scale };

            rval = new PiecewisePixel(getUnitObject(), pix, data);

        }
        return rval;
    }

    /**
     * @param unit
     *            the unit to set
     */
    public void setUnit(String unit) {
        this.unit = unit;
    }

    public Double getPrimaryElevationAngle() {
        return primaryElevationAngle;
    }

    public void setPrimaryElevationAngle(Double primaryElevationAngle) {
        this.primaryElevationAngle = primaryElevationAngle;
    }

    /**
     * @return the layer
     */
    public Double getLayer() {
        return layer;
    }

    /**
     * @param layer
     *            the layer to set
     */
    public void setLayer(Double layer) {
        this.layer = layer;
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

    public RadarStation getLocation() {
        return location;
    }

    public void setLocation(RadarStation location) {
        this.location = location;
    }

    /**
     * @return the graphicBlock
     */
    public GraphicBlock getGraphicBlock() {
        return getStoredData().getGraphicBlock();
    }

    /**
     * @param graphicBlock
     *            the graphicBlock to set
     */
    public void setGraphicBlock(GraphicBlock graphicBlock) {
        getStoredData().setGraphicBlock(graphicBlock);
    }

    /**
     * @return the tabularBlock
     */
    public TabularBlock getTabularBlock() {
        return getStoredData().getTabularBlock();
    }

    /**
     * @param tabularBlock
     *            the tabularBlock to set
     */
    public void setTabularBlock(TabularBlock tabularBlock) {
        getStoredData().setTabularBlock(tabularBlock);
    }

    /**
     * @return the symbologyBlock
     */
    public SymbologyBlock getSymbologyBlock() {
        return getStoredData().getSymbologyBlock();
    }

    /**
     * @param symbologyBlock
     *            the symbologyBlock to set
     */
    public void setSymbologyBlock(SymbologyBlock symbologyBlock) {
        getStoredData().setSymbologyBlock(symbologyBlock);
    }

    /**
     * @param gsmBlock
     *            the gsmBlock to set
     */
    public void setGsmMessage(GSMMessage gsmMessage) {
        getStoredData().setGsmMessage(gsmMessage);
    }

    public void setAlertMessage(AlertMessage alertMessage) {
        getStoredData().setAlertMessage(alertMessage);
    }

    public AlertMessage getAlertMessage() {
        return getStoredData().getAlertMessage();
    }

    /**
     * @param stormIDs
     *            the stormIDs to set
     */
    public void setStormIDs(Map<String, RadarDataKey> stormIDs) {
        getStoredData().setStormIDs(stormIDs);
    }

    /**
     * @return the gsmBlock
     */
    public GSMMessage getGsmMessage() {
        return getStoredData().getGsmMessage();
    }

    public short getProductDependentValue(int value) {
        return getProductDependentValues()[value];
    }

    /**
     * @return the productDependentValues
     */
    public short[] getProductDependentValues() {
        return getStoredData().getProductDependentValues();
    }

    /**
     * @param productDependentValues
     *            the productDependentValues to set
     */
    public void setProductDependentValues(short[] productDependentValues) {
        getStoredData().setProductDependentValues(productDependentValues);
    }

    public ProjectedCRS getCRS() {
        if (crs == null) {
            crs = CRSCache.getInstance().constructStereographic(
                    MapUtil.AWIPS_EARTH_RADIUS, MapUtil.AWIPS_EARTH_RADIUS,
                    this.latitude, this.longitude);
        }

        return crs;

    }

    public void setCRS(ProjectedCRS crs) {
        this.crs = crs;
    }

    /**
     * @return the symbologyData
     */
    public Map<RadarDataKey, RadarDataPoint> getSymbologyData() {
        return getStoredData().getSymbologyData();
    }

    /**
     * @param symbologyData
     *            the symbologyData to set
     */
    public void setSymbologyData(Map<RadarDataKey, RadarDataPoint> symbologyData) {
        getStoredData().setSymbologyData(symbologyData);
    }

    /**
     * Gets the lat/lon of the packets based on the i/j values in the packets
     * and the lat/lon of the radar.
     * 
     * @param storm
     * @return
     */
    public Coordinate convertStormLatLon(double i, double j) {
        Coordinate currLoc = new Coordinate();

        GeodeticCalculator gc = new GeodeticCalculator();
        gc.setStartingGeographicPoint(this.longitude, this.latitude);

        // angle formed by i/j
        double az = Math.atan2(i, j);
        az = Math.toDegrees(az);
        if (az > 180) {
            az -= 360;
        }

        // distance
        double mag = Math.sqrt((i * i) + (j * j));
        // 1 i or j is 1/4 km, so convert to meters
        mag *= 250;

        gc.setDirection(az, mag);

        Point2D dstPoint = gc.getDestinationGeographicPoint();

        currLoc.x = dstPoint.getX();
        currLoc.y = dstPoint.getY();

        return currLoc;
    }

    @Override
    public int getPacketVals(String stormID, String valueName) {
        // return symbologyData.get(stormID).get(valueName);
        return 0;
    }

    /**
     * @param productVals
     *            the productVals to set
     */
    public void setProductVals(
            HashMap<RadarConstants.MapValues, Map<String, Map<RadarConstants.MapValues, String>>> map) {
        getStoredData().setProductVals(map);
    }

    /**
     * @return the aapMessage
     */
    public AlertAdaptationParameters getAapMessage() {
        return getStoredData().getAapMessage();
    }

    /**
     * @param aapMessage
     *            the aapMessage to set
     */
    public void setAapMessage(AlertAdaptationParameters aapMessage) {
        getStoredData().setAapMessage(aapMessage);
    }

    public Map<String, RadarDataKey> getStormIDs() {
        return getStoredData().getStormIDs();
    }

    @Override
    public List<String> getStormIDList() {
        List<String> rval = new ArrayList<String>();
        Map<String, RadarDataKey> stormIDs = getStormIDs();
        if (stormIDs != null) {
            // get the storm ids from the Storm ID lookup map for symbology data
            rval.addAll(stormIDs.keySet());
        }

        return rval;
    }

    private <T> void addPacketData(double i, double j, int type, T currData,
            boolean needToConvert) {
        addPacketData(i, j, "", type, RadarProductType.GENERAL, currData,
                needToConvert);
    }

    private <T> void addPacketData(double i, double j, int type,
            RadarProductType productType, T currData, boolean needToConvert) {
        addPacketData(i, j, "", type, productType, currData, needToConvert);
    }

    private <T> void addPacketData(double i, double j, String stormID,
            int type, RadarProductType productType, T currData,
            boolean needToConvert) {

        // Convert x/y to lon/lat
        if (needToConvert) {
            Coordinate coor;

            // for MIGFA, i/j unit is 1km, for other radar products, unit is
            // 1/4km
            if (type == 140) {
                coor = convertStormLatLon(i * 4.0, j * 4.0);
            } else {
                coor = convertStormLatLon(i, j);
            }

            i = coor.x;
            j = coor.y;
        }

        // create the key
        RadarDataKey dispPoint = new RadarDataKey();
        dispPoint.setLon(i);
        dispPoint.setLat(j);

        // Get the Radar Data container
        RadarDataPoint dataContainer;
        Map<RadarDataKey, RadarDataPoint> symbologyData = getSymbologyData();
        if (symbologyData.containsKey(dispPoint)) {
            dataContainer = symbologyData.get(dispPoint);
        } else {
            dataContainer = new RadarDataPoint();
            symbologyData.put(dispPoint, dataContainer);
        }

        // Flag data points that will only have specific data inside so they can
        // be rendered slicker
        dataContainer.setDataType(productType);

        // Add Packet/Point to data container
        if (currData instanceof SymbologyPacket) {
            // Add current symbology packet to the container
            dataContainer.addDisplayData(type, (SymbologyPacket) currData);
        } else if (currData instanceof SymbologyPoint) {
            // Add current symbology point to the container
            dataContainer.addDisplayData(type, (SymbologyPoint) currData);
        } else {
            dataContainer.addDisplayData(type, (GenericDataComponent) currData);
        }
    }

    /**
     * Used to group correlated packets (i.e. a text packet with a storm id will
     * be grouped with the packet containing the storm's data) and allows for
     * easy data access
     * 
     * @param sb
     *            The symbology block to process
     */
    public void correlateSymbologyPackets() {
        // get the packets from each layer
        double i = 0;
        double j = 0;
        int type;

        SymbologyPacket currPacket;
        SymbologyPacket[] layerPackets;
        boolean convertLatLon = true;

        // Go through each layer that contains multiple packets
        for (Layer currLayer : this.getSymbologyBlock().getLayers()) {
            layerPackets = currLayer.getPackets();

            int count = 0;
            // Go through each packet in the specified layer
            PACKET: for (int k = 0; k < layerPackets.length; k++) {
                currPacket = layerPackets[k];
                i = 0;
                j = 0;

                type = this.productCode;

                // Get i/j for the data map key
                if (currPacket instanceof TextSymbolPacket) {
                    TextSymbolPacket packet = (TextSymbolPacket) currPacket;

                    i = packet.getI();
                    j = packet.getJ();

                    if (packet.getTheText().contains("\"")) {
                        // && (k + 3) < layerPackets.length) {

                        // Marks the beginning of the SCIT data packets in the
                        // layer
                        //
                        // Should be in this order:
                        //
                        // TextSymbolPacket - Text: "" "
                        // StormIDPacket - i/j matches TextSymbolPacket
                        // SCITDataPacket - contains past (!)
                        // SCITDataPacket - contains forecast (#)

                        // if (layerPackets[k + 1] instanceof StormIDPacket
                        // && layerPackets[k + 2] instanceof SCITDataPacket
                        // && layerPackets[k + 3] instanceof SCITDataPacket) {

                        addPacketData(i, j, type, RadarProductType.STI, packet,
                                convertLatLon);

                        if (((k + 1) < layerPackets.length)
                                && (layerPackets[k + 1] instanceof StormIDPacket)) {
                            StormIDPacket stormPkt = (StormIDPacket) layerPackets[k + 1];
                            addPacketData(i, j, type, RadarProductType.STI,
                                    stormPkt, convertLatLon);
                            count++;
                        }

                        if (((k + 2) < layerPackets.length)
                                && (layerPackets[k + 2] instanceof SCITDataPacket)) {
                            SCITDataPacket scitPktPast = (SCITDataPacket) layerPackets[k + 2];
                            addPacketData(i, j, type, RadarProductType.STI,
                                    scitPktPast, convertLatLon);
                            count++;

                            if (((k + 3) < layerPackets.length)
                                    && (layerPackets[k + 3] instanceof SCITDataPacket)) {
                                SCITDataPacket scitPktFst = (SCITDataPacket) layerPackets[k + 3];
                                addPacketData(i, j, type, RadarProductType.STI,
                                        scitPktFst, convertLatLon);
                                count++;
                            }
                        } else {
                            count++;
                        }
                        k = count;
                        continue PACKET;
                    } else {
                        // type = RadarProductType.TEXT;
                    }
                } else if (currPacket instanceof StormIDPacket) {
                    // type = RadarProductType.STORM_ID;
                    StormIDPacket packet = (StormIDPacket) currPacket;
                    for (StormIDPoint currPoint : packet.getPoints()) {
                        i = currPoint.getI();
                        j = currPoint.getJ();

                        addPacketData(i, j, type, currPoint, convertLatLon);
                    }
                    continue PACKET;
                } else if (currPacket instanceof SpecialGraphicSymbolPacket) {
                    SpecialGraphicSymbolPacket packet = (SpecialGraphicSymbolPacket) currPacket;

                    Vector<Integer> mesoFeatureTypes = new Vector<Integer>();
                    Collections.addAll(mesoFeatureTypes, new Integer[] { 1, 2,
                            3, 4, 9, 10, 11 });

                    for (SpecialGraphicPoint currPoint : packet.getPoints()) {
                        i = currPoint.getI();
                        j = currPoint.getJ();

                        // 1 2 3 4 9 10 11 ==> Meso
                        if (mesoFeatureTypes.contains(currPoint
                                .getPointFeatureType())) {
                            // type = RadarProductType.MESO;
                        } else {
                            // 5, 6, 7, 8 ==> (E)TVS
                            // type = RadarProductType.TVS;
                        }

                        addPacketData(i, j, type, currPoint, convertLatLon);
                    }
                    continue PACKET;
                } else if (currPacket instanceof SCITDataPacket) {
                    SCITDataPacket packet = (SCITDataPacket) currPacket;
                    // Key to the current position
                    // Indicated by the text packet with " as the value
                    for (SCITDataCell currCell : packet.getPoints()) {
                        if (currCell.getText().contains("\"")) {
                            i = currCell.getI();
                            j = currCell.getJ();
                        }
                    }
                    // type = RadarProductType.SCIT;
                } else if (currPacket instanceof HdaHailPacket) {
                    HdaHailPacket packet = (HdaHailPacket) currPacket;
                    // type = RadarProductType.HAIL;
                    for (HdaHailPoint currPoint : packet.getPoints()) {
                        i = currPoint.getI();
                        j = currPoint.getJ();

                        addPacketData(i, j, type, currPoint, convertLatLon);
                    }
                    continue PACKET;
                } else if (currPacket instanceof WindBarbPacket) {
                    WindBarbPacket packet = (WindBarbPacket) currPacket;

                    // type = RadarProductType.WIND_BARB;
                    for (WindBarbPoint currPoint : packet.getPoints()) {
                        i = currPoint.getI();
                        j = currPoint.getJ();
                        addPacketData(i, j, type, currPoint, convertLatLon);
                    }
                    continue PACKET;
                } else if ((currPacket instanceof UnlinkedVectorPacket)
                        || (currPacket instanceof UnlinkedContourVectorPacket)) {
                    // Nothing to do,
                    // Doesn't pertain to a specific location (?), so just store
                    // with index 0,0
                } else if ((currPacket instanceof LinkedVectorPacket)
                        || (currPacket instanceof LinkedContourVectorPacket)) {
                    // Nothing to do,
                    // Doesn't pertain to a specific location (?), so just store
                    // with index 0,0

                } else if (currPacket instanceof TVSPacket) {
                    TVSPacket packet = (TVSPacket) currPacket;

                    // type = RadarProductType.TVS;
                    for (TVSPoint currPoint : packet.getPoints()) {
                        i = currPoint.getI();
                        j = currPoint.getJ();
                        addPacketData(i, j, type, currPoint, convertLatLon);
                    }
                    if (((k + 1) < layerPackets.length)
                            && (layerPackets[k + 1] instanceof StormIDPacket)) {
                        StormIDPacket stormPkt = (StormIDPacket) layerPackets[++k];
                        addPacketData(i, j, type, RadarProductType.TVS,
                                stormPkt, convertLatLon);
                    }
                    continue PACKET;
                } else if (currPacket instanceof MesocyclonePacket) {
                    // Should cover meso and 3d correlated shear
                    MesocyclonePacket packet = (MesocyclonePacket) currPacket;

                    // type = RadarProductType.MESO;
                    for (MesocyclonePoint currPoint : packet.getPoints()) {
                        i = currPoint.getI();
                        j = currPoint.getJ();
                        addPacketData(i, j, type, currPoint, convertLatLon);
                    }
                    continue PACKET;
                } else if (currPacket instanceof CellTrendDataPacket) {
                    CellTrendDataPacket packet = (CellTrendDataPacket) currPacket;
                    i = packet.getI();
                    j = packet.getJ();
                } else if (currPacket instanceof CellTrendVolumeScanPacket) {
                    // This packet doesn't contain lat/lon, just scan times
                } else if (currPacket instanceof DMDPacket) {
                    // Need to get each component/feature out and located the
                    // thing
                    DMDPacket packet = (DMDPacket) currPacket;

                    // The lat/lon is actual lat/lon
                    convertLatLon = false;

                    Map<MapValues, String> map = new HashMap<MapValues, String>();
                    for (GenericDataParameter param : packet.getParams()
                            .values()) {
                        DMDAttributeIDs id = null;
                        id = DMDAttributeIDs.getAttribute(param.getId());
                        if (id != null) {
                            for (MapValues vals : MapValues.values()) {
                                if (id.getName().equals(vals.getName())) {
                                    if (vals.getName().equals(
                                            DMDAttributeIDs.LAST_ELEV_FLAG
                                                    .getName())) {
                                        if ("1".equals(param.getValue())) {
                                            lastElevationAngle = true;
                                        } else {
                                            lastElevationAngle = false;
                                        }
                                    }
                                    map.put(vals, param.getValue());
                                }
                            }
                        }
                    }
                    getMapRecordVals().put(MapValues.DMD_TYPE, map);
                    AreaComponent currFeature;
                    for (GenericDataComponent currComponent : packet
                            .getFeatures().values()) {
                        currFeature = (AreaComponent) currComponent;
                        i = currFeature.getPoints().get(0).getCoordinate2();
                        j = currFeature.getPoints().get(0).getCoordinate1();

                        addPacketData(i, j, type, RadarProductType.GENERIC,
                                currFeature, convertLatLon);
                    }
                    continue PACKET;
                } else if (currPacket instanceof GFMPacket) {
                    // Need to get each component/feature out and located the
                    // thing
                    GFMPacket packet = (GFMPacket) currPacket;

                    // need to convert x/y to lon/lat
                    convertLatLon = true;

                    Map<MapValues, String> map = new HashMap<MapValues, String>();

                    for (GenericDataParameter param : packet.getParams()
                            .values()) {

                        GFMAttributeIDs id = null;
                        id = GFMAttributeIDs.getAttribute(param.getId());

                        if (id != null) {
                            for (MapValues vals : MapValues.values()) {
                                if (id.getName().equals(vals.getName())) {
                                    map.put(vals, param.getValue());
                                }
                            }
                        }
                    }
                    getMapRecordVals().put(MapValues.GFM_TYPE, map);

                    AreaComponent currFeature;
                    for (GenericDataComponent currComponent : packet
                            .getFeatures().values()) {
                        currFeature = (AreaComponent) currComponent;
                        // first point of GFM
                        i = currFeature.getPoints().get(0).getCoordinate1();
                        j = currFeature.getPoints().get(0).getCoordinate2();

                        addPacketData(i, j, type, RadarProductType.GENERIC,
                                currFeature, convertLatLon);
                    }
                    continue PACKET;
                } else if (currPacket instanceof GenericDataPacket) {
                    // Generic Packet will contain most of the data for the
                    // product, so, in general, nothing probably needs to be
                    // done here
                } else {
                    System.out.println("Need to handle: "
                            + currPacket.getClass());
                }

                addPacketData(i, j, type, currPacket, convertLatLon);
            }
        }

        populateStormIDs();
    }

    private void populateStormIDs() {
        Map<RadarDataKey, RadarDataPoint> symbologyData = getSymbologyData();
        Map<String, RadarDataKey> stormIDs = getStormIDs();
        if (symbologyData != null) {
            for (RadarDataKey currKey : symbologyData.keySet()) {
                RadarDataPoint currPoint = symbologyData.get(currKey);
                if (!"".equals(currPoint.getStormID())) {
                    stormIDs.put(currPoint.getStormID(), currKey);
                }
            }
        }
    }

    public void printSymbologyData() {
        System.out
                .println("******************************************************************************");
        System.out.println("*** START Symbology Data Contents");
        System.out
                .println("******************************************************************************");
        Map<RadarDataKey, RadarDataPoint> symbologyData = getSymbologyData();
        for (RadarDataKey key : symbologyData.keySet()) {
            System.out.println("Key: " + key);
            RadarDataPoint data = symbologyData.get(key);
            HashMap<Integer, HashMap<Integer, SymbologyPoint>> displayPointData = data
                    .getDisplayPointData();
            for (Integer pktType : displayPointData.keySet()) {
                System.out.println("\tPt Type: " + pktType);
                for (SymbologyPoint currPt : displayPointData.get(pktType)
                        .values()) {
                    System.out.println("\t\t" + currPt);
                }
            }

            HashMap<Integer, HashMap<Integer, SymbologyPacket>> displayPacketData = data
                    .getDisplayPacketData();
            for (Integer pktType : displayPacketData.keySet()) {
                System.out.println("\tPkt Type: " + pktType);
                for (SymbologyPacket currPkt : displayPacketData.get(pktType)
                        .values()) {
                    System.out.println("\t\t" + currPkt);
                }
            }

            HashMap<Integer, HashMap<Integer, GenericDataComponent>> displayGenPacketData = data
                    .getDisplayGenericPointData();
            for (Integer pktType : displayGenPacketData.keySet()) {
                System.out.println("\tGeneric Pkt Type: " + pktType);
                for (GenericDataComponent currPkt : displayGenPacketData.get(
                        pktType).values()) {
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
     * com.raytheon.uf.common.dataplugin.radar.IRadarRecord#getProductVals(com
     * .raytheon .edex.util.radar.RadarConstants.MapValues)
     */
    @Override
    public String getProductVals(RadarConstants.MapValues type, String id,
            MapValues valueName) {
        return getMapProductVals().get(type).get(id).get(valueName);
    }

    /**
     * Return the entire product values map
     * 
     * @return
     */
    public Map<RadarConstants.MapValues, Map<String, Map<MapValues, String>>> getMapProductVals() {
        return getStoredData().getProductVals();
    }

    /**
     * @return the recordVals
     */
    public String getRecordVals(RadarConstants.MapValues type,
            RadarConstants.MapValues val) {
        return getMapRecordVals().get(type).get(val);
    }

    /**
     * @return the entire record values map
     */
    public Map<RadarConstants.MapValues, Map<RadarConstants.MapValues, String>> getMapRecordVals() {
        return getStoredData().getMapRecordVals();
    }

    /**
     * @param recordVals
     *            the recordVals to set
     */
    public void setMapRecordVals(
            Map<RadarConstants.MapValues, Map<RadarConstants.MapValues, String>> recordVals) {
        getStoredData().setMapRecordVals(recordVals);
    }

    /**
     * Get a list of all the ids, whether they be storm ids (STI) or circ ids
     * (MESO)
     * 
     * @param type
     * @return
     */
    public List<String> getIds(RadarConstants.MapValues type) {
        List<String> list = new ArrayList<String>();
        try {
            Set<String> set = getMapProductVals().get(type).keySet();
            list.addAll(set);
        } catch (Exception e) {
            return null;
        }
        return list;
    }

    /**
     * Tests if the current record is in expandedMode. From the OB9 code, it is
     * assumed to be in expandedMode if the displayMode contains "V" and "m" and
     * the first threshold has a value of 10.
     * 
     * @return
     */
    public boolean isExpandedMode() {
        boolean rval = false;
        Object threshold = this.decodedThresholds[1];

        if (threshold instanceof Float) {
            if ((Float.compare(10f, (Float) threshold) == 0)
                    && this.displayModes.contains("V")
                    && this.displayModes.contains("m")) {
                rval = true;
            }
        }
        return rval;
    }

    public void setDisplayModes(String displayModes) {
        this.displayModes = displayModes;
    }

    /**
     * @return the displayModes
     */
    public String getDisplayModes() {
        return this.displayModes;
    }

    @Override
    public DataTime getDataTime() {
        DataTime time = super.getDataTime();
        if ((time != null) && addSpatial) {
            time.setLevelValue(primaryElevationAngle);
        } else if (!addSpatial) {
            time.setLevelValue(null);
        }
        return time;
    }

    public void setAddSpatial(boolean addSpatial) {
        this.addSpatial = addSpatial;
    }

    /**
     * Refers actually to the reftime as the volume scan time needs to go in the
     * datauri
     */
    public Date getVolScanTime() {
        return volScanTime;
    }

    public void setVolScanTime(Date volScanTime) {
        this.volScanTime = volScanTime;
    }

    @Override
    public IHDFFilePathProvider getHDFPathProvider() {
        return RadarPathProvider.getInstance();
    }

    public RadarStoredData getStoredData() {
        if (storedData == null) {
            // We use it internally alow and having this function never return
            // null saves lots of null checking
            storedData = new RadarStoredData();
        }
        return storedData;
    }

    public void setStoredData(RadarStoredData storedData) {
        this.storedData = storedData;
        this.setThresholds(storedData.getThresholds());
    }

    @Override
    @Column
    @Access(AccessType.PROPERTY)
    public String getDataURI() {
        return super.getDataURI();
    }

    @Override
    public String getPluginName() {
        return "radar";
    }
}