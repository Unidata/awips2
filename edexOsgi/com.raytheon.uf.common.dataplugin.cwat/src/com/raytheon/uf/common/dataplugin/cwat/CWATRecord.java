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
package com.raytheon.uf.common.dataplugin.cwat;

import java.io.ByteArrayInputStream;
import java.util.HashMap;

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

import org.geotools.coverage.grid.GeneralGridEnvelope;
import org.geotools.coverage.grid.GridGeometry2D;
import org.geotools.geometry.GeneralEnvelope;
import org.hibernate.annotations.Index;
import org.opengis.referencing.crs.ProjectedCRS;

import com.raytheon.uf.common.dataplugin.IDecoderGettable;
import com.raytheon.uf.common.dataplugin.PluginDataObject;
import com.raytheon.uf.common.dataplugin.annotations.DataURI;
import com.raytheon.uf.common.dataplugin.persist.IPersistable;
import com.raytheon.uf.common.dataplugin.persist.PersistablePluginDataObject;
import com.raytheon.uf.common.dataplugin.radar.RadarStation;
import com.raytheon.uf.common.datastorage.IDataStore;
import com.raytheon.uf.common.datastorage.Request;
import com.raytheon.uf.common.datastorage.records.ByteDataRecord;
import com.raytheon.uf.common.datastorage.records.IDataRecord;
import com.raytheon.uf.common.datastorage.records.ShortDataRecord;
import com.raytheon.uf.common.geospatial.ISpatialEnabled;
import com.raytheon.uf.common.geospatial.ISpatialObject;
import com.raytheon.uf.common.geospatial.MapUtil;
import com.raytheon.uf.common.monitor.scan.ThreatLocation;
import com.raytheon.uf.common.monitor.scan.ThreatReport;
import com.raytheon.uf.common.serialization.DynamicSerializationManager;
import com.raytheon.uf.common.serialization.DynamicSerializationManager.SerializationType;
import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;
import com.raytheon.uf.common.serialization.annotations.DynamicSerializeElement;

/**
 * Record implementation for CWAT plugin
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#     Engineer    Description
 * ------------ ----------  ----------- --------------------------
 * 06/03/09      2037       D. Hladky   Initial release
 * Apr 4, 2013        1846 bkowal      Added an index on refTime and forecastTime
 * 04/08/13      1293       bkowal      Removed references to hdffileid.
 * Apr 12, 2013  1857       bgonzale    Added SequenceGenerator annotation.
 * May 07, 2013 1869        bsteffen    Remove dataURI column from
 *                                      PluginDataObject.
 * 
 * </pre>
 * 
 * @author dhladky
 * @version 1
 */
@Entity
@SequenceGenerator(initialValue = 1, name = PluginDataObject.ID_GEN, sequenceName = "cwatseq")
@Table(name = "cwat", uniqueConstraints = { @UniqueConstraint(columnNames = { "dataURI" }) })
/*
 * Both refTime and forecastTime are included in the refTimeIndex since
 * forecastTime is unlikely to be used.
 */
@org.hibernate.annotations.Table(
		appliesTo = "cwat",
		indexes = {
				@Index(name = "cwat_refTimeIndex", columnNames = { "refTime", "forecastTime" } )
		}
)
@XmlRootElement
@XmlAccessorType(XmlAccessType.NONE)
@DynamicSerialize
public class CWATRecord extends PersistablePluginDataObject
        implements IPersistable, ISpatialEnabled {

    private static final long serialVersionUID = 76774564365671L;

    public static String THREATS = "Storm Threat";

    @Column(length = 7)
    @DataURI(position = 1)
    @DynamicSerializeElement
    @XmlElement(nillable = false)
    private String icao;

    @Column(length = 30)
    @DataURI(position = 2)
    @DynamicSerializeElement
    @XmlElement(nillable = false)
    private String fieldName;

    @ManyToOne
    @PrimaryKeyJoinColumn
    @XmlElement
    @DynamicSerializeElement
    private RadarStation spatialInfo;

    @Column
    @DynamicSerializeElement
    @XmlElement(nillable = false)
    public Integer nx = 0;

    @Column
    @DynamicSerializeElement
    @XmlElement(nillable = false)
    public Integer ny = 0;

    @Column
    @DynamicSerializeElement
    @XmlElement(nillable = false)
    public Integer dx = 0;

    @Column
    @DynamicSerializeElement
    @XmlElement(nillable = false)
    public Integer dy = 0;

    @Column
    @DynamicSerializeElement
    @XmlElement(nillable = false)
    public Integer maxScti = 0;

    @Transient
    private short[] data_array = null;

    @Transient
    private HashMap<ThreatLocation, ThreatReport> threats = new HashMap<ThreatLocation, ThreatReport>();

    /**
     * Default Constructor
     */
    public CWATRecord() {
    }

    /**
     * Constructs a record from a dataURI
     * 
     * @param uri
     *            The dataURI
     * @param tableDef
     *            The table definition associated with this class
     */
    public CWATRecord(String uri) {
        super(uri);
    }

    /**
     * 
     * Enumeration of the fieldNames for cwat
     * 
     * <pre>
     * 
     * SOFTWARE HISTORY
     * 
     * Date         Ticket#    Engineer    Description
     * ------------ ---------- ----------- --------------------------
     * Mar 20, 2009            dhladky     Initial creation
     * 
     * </pre>
     * 
     * @author dhladky
     * @version 1.0
     */
    public enum DATA_TYPE {

        CWAT("CWA Threat Index");

        private final String fieldName;

        private DATA_TYPE(String name) {
            fieldName = name;
        }

        public String getFieldName() {
            return fieldName;
        }
    };

    /**
     * 
     * Enumeration of the fieldUNits for cwat
     * 
     * <pre>
     * 
     * SOFTWARE HISTORY
     * 
     * Date         Ticket#    Engineer    Description
     * ------------ ---------- ----------- --------------------------
     * Mar 20, 2009            dhladky     Initial creation
     * 
     * </pre>
     * 
     * @author dhladky
     * @version 1.0
     */
    public enum UNIT_TYPE {

        CWAT("%");

        private final String fieldUnit;

        private UNIT_TYPE(String unit) {
            fieldUnit = unit;
        }

        public String getFieldUnit() {
            return fieldUnit;
        }
    };

    /**
     * Set the icao
     * 
     * @param icao
     */
    public void setIcao(String icao) {
        this.icao = icao;
    }

    /**
     * Gets the icao
     * 
     * @return
     */
    public String getIcao() {
        return icao;
    }

    /**
     * Set the fieldName
     * 
     * @param fieldName
     */
    public void setFieldName(String fieldName) {
        this.fieldName = fieldName;
    }

    /**
     * Gets the fieldName
     * 
     * @return
     */
    public String getFieldName() {
        return fieldName;
    }

    /**
     * set NX
     * 
     * @param nx
     */
    public void setNx(Integer nx) {
        this.nx = nx;
    }

    /**
     * Get NX
     * 
     * @return
     */
    public Integer getNx() {
        return nx;
    }

    /**
     * Set the NY
     * 
     * @param ny
     */
    public void setNy(Integer ny) {
        this.ny = ny;
    }

    /**
     * get the NY
     * 
     * @return
     */
    public Integer getNy() {
        return ny;
    }

    /**
     * set DX in meters
     * 
     * @param dx
     */
    public void setDx(Integer dx) {
        this.dx = dx;
    }

    /**
     * Get DX in meters
     * 
     * @return
     */
    public Integer getDx() {
        return dx;
    }

    /**
     * Set the DY, in meters
     * 
     * @param dy
     */
    public void setDy(Integer dy) {
        this.dy = dy;
    }

    /**
     * get the DY, in meters
     * 
     * @return
     */
    public Integer getDy() {
        return dy;
    }

    /**
     * The max value for the SCTI
     * 
     * @param maxSCTI
     */
    public void setMaxScti(Integer maxScti) {
        this.maxScti = maxScti;
    }

    /**
     * Gets the maximum SCTI value
     * 
     * @return
     */
    public Integer getMaxScti() {
        return maxScti;
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

    public RadarStation getSpatialInfo() {
        return spatialInfo;
    }

    /**
     * Set that spatial info object.
     * 
     * @param spatialInfo
     */
    public void setSpatialInfo(RadarStation spatialInfo) {
        this.spatialInfo = spatialInfo;
    }

    /**
     * Set the data array
     * 
     * @param data_array
     */
    public void setDataArray(short[] data_array) {
        this.data_array = data_array;
    }

    /**
     * Get the data array
     * 
     * @param data_array
     */
    public short[] getDataArray() {
        return data_array;
    }

    /**
     * Gets the projected CRS
     * 
     * @return
     */
    public ProjectedCRS getCRS() {
        return MapUtil.constructStereographic(MapUtil.AWIPS_EARTH_RADIUS,
                MapUtil.AWIPS_EARTH_RADIUS, getSpatialInfo().getLat(),
                getSpatialInfo().getLon());
    }

    /**
     * Construct a 2D GridGeometry to use for display
     * 
     * @return
     */
    public GridGeometry2D getGridGeometry() {
        ProjectedCRS crs = this.getCRS();

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
    public void retrieveFromDataStore(IDataStore dataStore) {

        try {
            IDataRecord[] dataRec = dataStore.retrieve(getDataURI());
            for (int i = 0; i < dataRec.length; i++) {
                if (dataRec[i] instanceof ShortDataRecord) {
                    setDataArray(((ShortDataRecord) dataRec[i]).getShortData());
                }
            }
            retrieveMapFromDataStore(dataStore);

        } catch (Exception se) {
            se.printStackTrace();
        }
    }

    /**
     * Gets the Hash out of the
     * 
     * @param dataStore
     */

    @SuppressWarnings("unchecked")
    public void retrieveMapFromDataStore(IDataStore dataStore) {
        try {
            if (getThreats().size() < 1) {
                ByteDataRecord byteData = (ByteDataRecord) dataStore.retrieve(
                        getDataURI(), THREATS, Request.ALL);
                ByteArrayInputStream bais = new ByteArrayInputStream(
                        byteData.getByteData());
                Object o = DynamicSerializationManager.getManager(
                        SerializationType.Thrift).deserialize(bais);
                setThreats((HashMap<ThreatLocation, ThreatReport>) o);
            }
        } catch (Throwable e) {
            e.printStackTrace();
        }
    }

    /**
     * Used for debugging.
     */
    @Override
    public String toString() {
        StringBuffer sb = new StringBuffer();
        sb.append("\n dataURI: " + getDataURI() + "\n");
        sb.append("data_array: " + getDataArray().length + "\n");
        sb.append("Threats map: " + getThreats().size() + "\n");
        sb.append("Icao: " + getIcao() + "\n");
        sb.append("Field: " + getParameterName() + "\n");
        sb.append("Unit: " + getParameterUnit() + "\n");
        sb.append("dataTime: "
                + getDataTime().getValidTime().getTime().toString() + "\n");
        sb.append("PersistanceTime: " + getPersistenceTime().toString() + "\n");
        sb.append("Nx: " + getNx() + "\n");
        sb.append("Ny: " + getNy() + "\n");
        sb.append("Dx: " + getDx() + "\n");
        sb.append("Dy: " + getDy() + "\n");
        sb.append("MaxSCTI: " + getMaxScti() + "\n");
        sb.append("lat: " + getSpatialInfo().getLat() + "\n");
        sb.append("lon: " + getSpatialInfo().getLon() + "\n");
        sb.append("CRS: " + getCRS().toWKT() + "\n");
        sb.append("WFO: " + getSpatialInfo().getWfoId() + "\n");

        return sb.toString();
    }

    /**
     * Gets the actual string text for the name
     * 
     * @return
     */
    public String getParameterName() {
        String paramName = null;
        for (DATA_TYPE name : DATA_TYPE.values()) {
            if (getFieldName().equals(name.name())) {
                paramName = name.getFieldName();
            }
        }
        return paramName;
    }

    /**
     * Gets the actual string text for the unit
     * 
     * @return
     */
    public String getParameterUnit() {
        String paramUnit = null;
        for (UNIT_TYPE name : UNIT_TYPE.values()) {
            if (getFieldName().equals(name.name())) {
                paramUnit = name.getFieldUnit();
            }
        }
        return paramUnit;
    }

    @Override
    public ISpatialObject getSpatialObject() {
        return getSpatialInfo();
    }

    /**
     * Sets the HashMap<ThreatLocation, ThreatReport>containing the data
     * 
     * @param threats
     */
    public void setThreats(HashMap<ThreatLocation, ThreatReport> threats) {
        this.threats = threats;
    }

    /**
     * Gets the Hash containing the data, when you've populated it!
     * 
     * @return
     */
    public HashMap<ThreatLocation, ThreatReport> getThreats() {
        return threats;
    }

    @Override
    @Column
    @Access(AccessType.PROPERTY)
    public String getDataURI() {
        return super.getDataURI();
    }
}
