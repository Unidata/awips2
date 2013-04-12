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
package com.raytheon.uf.common.dataplugin.preciprate;

import java.io.ByteArrayInputStream;
import java.io.FileNotFoundException;
import java.util.Map;

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

import org.geotools.coverage.grid.GeneralGridEnvelope;
import org.geotools.coverage.grid.GridGeometry2D;
import org.geotools.geometry.GeneralEnvelope;
import org.hibernate.annotations.Index;
import org.opengis.referencing.crs.ProjectedCRS;

import com.raytheon.uf.common.dataplugin.IDecoderGettable;
import com.raytheon.uf.common.dataplugin.PluginDataObject;
import com.raytheon.uf.common.dataplugin.annotations.DataURI;
import com.raytheon.uf.common.dataplugin.persist.PersistablePluginDataObject;
import com.raytheon.uf.common.dataplugin.radar.RadarStation;
import com.raytheon.uf.common.dataplugin.radar.util.RadarConstants.DHRValues;
import com.raytheon.uf.common.datastorage.IDataStore;
import com.raytheon.uf.common.datastorage.StorageException;
import com.raytheon.uf.common.datastorage.records.ByteDataRecord;
import com.raytheon.uf.common.datastorage.records.FloatDataRecord;
import com.raytheon.uf.common.datastorage.records.IDataRecord;
import com.raytheon.uf.common.geospatial.MapUtil;
import com.raytheon.uf.common.monitor.processing.IMonitorProcessing;
import com.raytheon.uf.common.serialization.DynamicSerializationManager;
import com.raytheon.uf.common.serialization.DynamicSerializationManager.SerializationType;
import com.raytheon.uf.common.serialization.SerializationException;
import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;
import com.raytheon.uf.common.serialization.annotations.DynamicSerializeElement;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;

/**
 * Record implementation for Precipitation Rate plugin
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#     Engineer    Description
 * ------------ ----------  ----------- --------------------------
 * 01/25/10      3796       D. Hladky   Initial release
 * Apr 4, 2013        1846 bkowal      Added an index on refTime and forecastTime
 * 04/08/13      1293       bkowal      Removed references to hdffileid.
 * Apr 12, 2013  1857       bgonzale    Added SequenceGenerator annotation.
 * 
 * </pre>
 * 
 * @author dhladky
 * @version 1
 */
@Entity
@SequenceGenerator(initialValue = 1, name = PluginDataObject.ID_GEN, sequenceName = "preciprateseq")
@Table(name = "preciprate", uniqueConstraints = { @UniqueConstraint(columnNames = { "dataURI" }) })
/*
 * Both refTime and forecastTime are included in the refTimeIndex since
 * forecastTime is unlikely to be used.
 */
@org.hibernate.annotations.Table(
		appliesTo = "preciprate",
		indexes = {
				@Index(name = "preciprate_refTimeIndex", columnNames = { "refTime", "forecastTime" } )
		}
)
@XmlRootElement
@XmlAccessorType(XmlAccessType.NONE)
@DynamicSerialize
public class PrecipRateRecord extends PersistablePluginDataObject
        implements IMonitorProcessing {
    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(PrecipRateRecord.class);

    private static final long serialVersionUID = 76774564365671L;

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
    private Integer volumeCoveragePattern;

    @ManyToOne
    @PrimaryKeyJoinColumn
    @XmlElement
    @DynamicSerializeElement
    private RadarStation location;

    @Column
    @DynamicSerializeElement
    @XmlElement(nillable = false)
    private Double coefficent = 0.0;

    @Column
    @DynamicSerializeElement
    @XmlElement(nillable = false)
    private Double acoefficent = 0.0;

    @Column
    @DynamicSerializeElement
    @XmlElement(nillable = false)
    private Double hailcap = 0.0;

    @Column
    @DynamicSerializeElement
    @XmlElement(nillable = false)
    private Double bias = 0.0;

    @Transient
    private Map<DHRValues, Double> dhrMap;

    @Transient
    protected byte[] rawData;

    @Transient
    @DynamicSerializeElement
    @XmlElement
    protected float[] angleData;

    @Transient
    protected transient ProjectedCRS crs;

    /**
     * Default Constructor
     */
    public PrecipRateRecord() {
    }

    /**
     * Get NX
     * 
     * @return
     */
    @Override
    public Integer getNx() {
        return getNumBins();
    }

    /**
     * get the NY
     * 
     * @return
     */
    @Override
    public Integer getNy() {
        return getNumRadials();
    }

    /**
     * Get DX in meters
     * 
     * @return
     */
    public Integer getDx() {
        return getGateResolution();
    }

    /**
     * get the DY, in meters
     * 
     * @return
     */
    public Integer getDy() {
        return getGateResolution();
    }

    /**
     * power value of the ZR equation
     * 
     * @return
     */
    public Double getCoefficent() {
        return coefficent;
    }

    public void setCoefficent(Double coefficent) {
        this.coefficent = coefficent;
    }

    /**
     * multiplier factor of the ZR equation
     * 
     * @return
     */
    public Double getAcoefficent() {
        return acoefficent;
    }

    public void setAcoefficent(Double acoefficent) {
        this.acoefficent = acoefficent;
    }

    /**
     * max hail size in MM
     * 
     * @return
     */
    public Double getHailcap() {
        return hailcap;
    }

    public void setHailcap(Double hailcap) {
        this.hailcap = hailcap;
    }

    /**
     * bias value of the ZR equation
     * 
     * @return
     */
    public Double getBias() {
        return bias;
    }

    public void setBias(Double bias) {
        this.bias = bias;
    }

    /**
     * Construct a 2D GridGeometry to use for display
     * 
     * @return
     */
    @Override
    public GridGeometry2D getGridGeometry() {
        ProjectedCRS crs = getCrs();

        GridGeometry2D gridGeometry2D = null;

        GeneralEnvelope generalEnvelope = new GeneralEnvelope(2);
        generalEnvelope.setCoordinateReferenceSystem(crs);

        double maxExtent = (this.getDx() * (this.getNx() / 2));

        generalEnvelope.setRange(0, -maxExtent, maxExtent);
        generalEnvelope.setRange(1, -maxExtent, maxExtent);

        gridGeometry2D = new GridGeometry2D(new GeneralGridEnvelope(new int[] {
                0, 0 }, new int[] { this.getNx(), this.getNy() }, false),
                generalEnvelope);

        return gridGeometry2D;
    }

    /**
     * Sets the data array from the store.
     * 
     * @param dataStore
     */
    @Override
    @SuppressWarnings("unchecked")
    public void retrieveFromDataStore(IDataStore dataStore) {
        IDataRecord[] dataRec = null;
        try {
            dataRec = dataStore.retrieve(getDataURI());
        } catch (FileNotFoundException e1) {
            statusHandler
                    .handle(Priority.PROBLEM, e1.getLocalizedMessage(), e1);
        } catch (StorageException e1) {
            e1.printStackTrace();
            statusHandler
                    .handle(Priority.PROBLEM, e1.getLocalizedMessage(), e1);

        }
        if (dataRec != null) {
            for (int i = 0; i < dataRec.length; i++) {
                if (dataRec[i].getName().equals("Data")) {
                    setRawData(((ByteDataRecord) dataRec[i]).getByteData());
                } else if (dataRec[i].getName().equals("Angles")) {
                    setAngleData(((FloatDataRecord) dataRec[i]).getFloatData());
                } else if (dataRec[i].getName().equals("DHRMap")) {
                    try {
                        ByteDataRecord byteData = (ByteDataRecord) dataRec[i];
                        ByteArrayInputStream bais = new ByteArrayInputStream(
                                byteData.getByteData());
                        Object o = DynamicSerializationManager.getManager(
                                SerializationType.Thrift).deserialize(bais);
                        setDhrMap((Map<DHRValues, Double>) o);
                    } catch (SerializationException e) {
                        e.printStackTrace();
                        setDhrMap(null);
                    }
                }
            }
        }
    }

    /**
     * Used for debugging.
     */
    @Override
    public String toString() {
        StringBuffer sb = new StringBuffer();
        sb.append("Icao: " + getIcao() + "\n");
        sb.append("Field: " + getParameterName() + "\n");
        sb.append("Unit: " + getParameterUnit() + "\n");
        sb.append("dataTime: "
                + getDataTime().getValidTime().getTime().toString() + "\n");
        sb.append("Nx: " + getNx() + "\n");
        sb.append("Ny: " + getNy() + "\n");
        sb.append("Dx: " + getDx() + "\n");
        sb.append("Dy: " + getDy() + "\n");
        sb.append("lat: " + getLatitude() + "\n");
        sb.append("lon: " + getLongitude() + "\n");
        sb.append("CRS: " + getCrs().toWKT() + "\n");
        sb.append("WFO: " + getLocation().getWfoId() + "\n");

        return sb.toString();
    }

    /**
     * Gets the actual string text for the name
     * 
     * @return
     */
    public String getParameterName() {
        return "preciprate";
    }

    /**
     * Gets the actual string text for the unit
     * 
     * @return
     */
    public String getParameterUnit() {
        return "in/hr";
    }

    public Map<DHRValues, Double> getDhrMap() {
        return dhrMap;
    }

    public void setDhrMap(Map<DHRValues, Double> dhrMap) {
        this.dhrMap = dhrMap;
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.common.monitor.processing.IMonitorProcessing#getDataArray
     * ()
     */
    @Override
    public float[] getDataArray() {
        // TODO Auto-generated method stub
        return null;
    }

    /**
     * @return the gateResolution
     */
    public Integer getGateResolution() {
        return gateResolution;
    }

    /**
     * @param gateResolution
     *            the gateResolution to set
     */
    public void setGateResolution(Integer gateResolution) {
        this.gateResolution = gateResolution;
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
     * @return the latitude
     */
    public Float getLatitude() {
        return latitude;
    }

    /**
     * @param latitude
     *            the latitude to set
     */
    public void setLatitude(Float latitude) {
        this.latitude = latitude;
    }

    /**
     * @return the longitude
     */
    public Float getLongitude() {
        return longitude;
    }

    /**
     * @param longitude
     *            the longitude to set
     */
    public void setLongitude(Float longitude) {
        this.longitude = longitude;
    }

    /**
     * @return the numRadials
     */
    public Integer getNumRadials() {
        return numRadials;
    }

    /**
     * @param numRadials
     *            the numRadials to set
     */
    public void setNumRadials(Integer numRadials) {
        this.numRadials = numRadials;
    }

    /**
     * @return the numBins
     */
    public Integer getNumBins() {
        return numBins;
    }

    /**
     * @param numBins
     *            the numBins to set
     */
    public void setNumBins(Integer numBins) {
        this.numBins = numBins;
    }

    /**
     * @return the mnemonic
     */
    public String getMnemonic() {
        return mnemonic;
    }

    /**
     * @param mnemonic
     *            the mnemonic to set
     */
    public void setMnemonic(String mnemonic) {
        this.mnemonic = mnemonic;
    }

    /**
     * @return the volumeCoveragePattern
     */
    public Integer getVolumeCoveragePattern() {
        return volumeCoveragePattern;
    }

    /**
     * @param volumeCoveragePattern
     *            the volumeCoveragePattern to set
     */
    public void setVolumeCoveragePattern(Integer volumeCoveragePattern) {
        this.volumeCoveragePattern = volumeCoveragePattern;
    }

    /**
     * @return the location
     */
    public RadarStation getLocation() {
        return location;
    }

    /**
     * @param location
     *            the location to set
     */
    public void setLocation(RadarStation location) {
        this.location = location;
    }

    /**
     * @return the rawData
     */
    public byte[] getRawData() {
        return rawData;
    }

    /**
     * @param rawData
     *            the rawData to set
     */
    public void setRawData(byte[] rawData) {
        this.rawData = rawData;
    }

    /**
     * @return the angleData
     */
    public float[] getAngleData() {
        return angleData;
    }

    /**
     * @param angleData
     *            the angleData to set
     */
    public void setAngleData(float[] angleData) {
        this.angleData = angleData;
    }

    /**
     * @return the crs
     */
    public ProjectedCRS getCrs() {
        return MapUtil.constructStereographic(MapUtil.AWIPS_EARTH_RADIUS,
                MapUtil.AWIPS_EARTH_RADIUS, getLatitude(), getLongitude());
    }

    /**
     * @param crs
     *            the crs to set
     */
    public void setCrs(ProjectedCRS crs) {
        this.crs = crs;
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.common.dataplugin.PluginDataObject#getDecoderGettable()
     */
    @Override
    public IDecoderGettable getDecoderGettable() {
        return null;
    }

}
