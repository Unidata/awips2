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
package com.raytheon.uf.common.dataplugin.qpf;

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
import com.raytheon.uf.common.datastorage.IDataStore;
import com.raytheon.uf.common.datastorage.records.FloatDataRecord;
import com.raytheon.uf.common.datastorage.records.IDataRecord;
import com.raytheon.uf.common.geospatial.ISpatialEnabled;
import com.raytheon.uf.common.geospatial.ISpatialObject;
import com.raytheon.uf.common.geospatial.MapUtil;
import com.raytheon.uf.common.monitor.processing.IMonitorProcessing;
import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;
import com.raytheon.uf.common.serialization.annotations.DynamicSerializeElement;

/**
 * Record implementation for QPF plugin
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#     Engineer    Description
 * ------------ ----------  ----------- --------------------------
 * 2/24/09      2027        D. Hladky   Initial release
 * 4/27/12      #562        dgilling    Rename getters/setters to 
 *                                      match Java conventions.
 * Apr 4, 2013        1846 bkowal      Added an index on refTime and forecastTime
 * 04/08/13     #1293       bkowal      Removed references to hdffileid.
 * Apr 12, 2013 1857        bgonzale    Added SequenceGenerator annotation.
 * 
 * </pre>
 * 
 * @author dhladky
 * @version 1
 */
@Entity
@SequenceGenerator(initialValue = 1, name = PluginDataObject.ID_GEN, sequenceName = "qpfseq")
@Table(name = "qpf", uniqueConstraints = { @UniqueConstraint(columnNames = { "dataURI" }) })
/*
 * Both refTime and forecastTime are included in the refTimeIndex since
 * forecastTime is unlikely to be used.
 */
@org.hibernate.annotations.Table(
		appliesTo = "qpf",
		indexes = {
				@Index(name = "qpf_refTimeIndex", columnNames = { "refTime", "forecastTime" } )
		}
)
@XmlRootElement
@XmlAccessorType(XmlAccessType.NONE)
@DynamicSerialize
public class QPFRecord extends PersistablePluginDataObject
        implements ISpatialEnabled, IMonitorProcessing {

    private static final long serialVersionUID = 767763365671L;

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
    public Float nsvelocity = 0.0f;

    @Column
    @DynamicSerializeElement
    @XmlElement(nillable = false)
    public Float ewvelocity = 0.0f;

    @Column
    @DynamicSerializeElement
    @XmlElement(nillable = false)
    public Float avgdir = 0.0f;

    @Column
    @DynamicSerializeElement
    @XmlElement(nillable = false)
    public Float avgspd = 0.0f;

    @Transient
    private float[] data_array = null;

    /**
     * Default Constructor
     */
    public QPFRecord() {
    }

    /**
     * Constructs a record from a dataURI
     * 
     * @param uri
     *            The dataURI
     * @param tableDef
     *            The table definition associated with this class
     */
    public QPFRecord(String uri) {
        super(uri);
    }

    /**
     * 
     * Enumeration of the fieldNames for QPF
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
        P10("QPF .10 1hr probability"), P25("QPF .25 1hr probability"), P50(
                "QPF .50 1hr probability"), P75("QPF .75 1hr probability"), PRECIP_AMT(
                "QPF 1hr categorical rain fcst."), AV_VIL(
                "SCAN 1hr average VIL");

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
     * Enumeration of the fieldUNits for QPF
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
        P10("%"), P25("%"), P50("%"), P75("%"), PRECIP_AMT("in"), AV_VIL("");

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
    @Override
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
    @Override
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
     * Set the East West velocity, m/s (U)
     * 
     * @param ew_velocity
     */
    public void setEwvelocity(Float ewvelocity) {
        this.ewvelocity = ewvelocity;
    }

    /**
     * get the EW velocity, in m/s (U)
     * 
     * @return
     */
    public Float getEwvelocity() {
        return ewvelocity;
    }

    /**
     * Set the North South velocity, m/s (V)
     * 
     * @param ns_velocity
     */
    public void setNsvelocity(Float nsvelocity) {
        this.nsvelocity = nsvelocity;
    }

    /**
     * get the NS velocity, in m/s (V)
     * 
     * @return
     */
    public Float getNsvelocity() {
        return nsvelocity;
    }

    /**
     * Set the avg_spd, m/s
     * 
     * @param avg_spd
     */
    public void setAvgspd(Float avgspd) {
        this.avgspd = avgspd;
    }

    /**
     * get the avg_spd, in m/s
     * 
     * @return
     */
    public Float getAvgspd() {
        return avgspd;
    }

    /**
     * Set the avg_dir, degrees
     * 
     * @param avg_dir
     */
    public void setAvgdir(Float avgdir) {
        this.avgdir = avgdir;
    }

    /**
     * get the avg_dir, in degrees
     * 
     * @return
     */
    public Float getAvgdir() {
        return avgdir;
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
    public void setDataArray(float[] data_array) {
        this.data_array = data_array;
    }

    /**
     * Get the data array
     * 
     * @param data_array
     */
    @Override
    public float[] getDataArray() {
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
    @Override
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
    @Override
    public void retrieveFromDataStore(IDataStore dataStore) {

        try {
            IDataRecord[] dataRec = dataStore.retrieve(getDataURI());
            for (int i = 0; i < dataRec.length; i++) {
                if (dataRec[i] instanceof FloatDataRecord) {
                    setDataArray(((FloatDataRecord) dataRec[i]).getFloatData());
                }
            }
        } catch (Exception se) {
            se.printStackTrace();
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

}
