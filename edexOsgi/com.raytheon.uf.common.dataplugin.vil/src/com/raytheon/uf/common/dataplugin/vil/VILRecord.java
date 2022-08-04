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
package com.raytheon.uf.common.dataplugin.vil;

import java.io.FileNotFoundException;

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

import org.geotools.coverage.grid.GeneralGridEnvelope;
import org.geotools.coverage.grid.GridGeometry2D;
import org.geotools.geometry.GeneralEnvelope;
import org.hibernate.annotations.Index;
import org.opengis.referencing.crs.ProjectedCRS;

import com.raytheon.uf.common.dataplugin.PluginDataObject;
import com.raytheon.uf.common.dataplugin.annotations.DataURI;
import com.raytheon.uf.common.dataplugin.persist.PersistablePluginDataObject;
import com.raytheon.uf.common.dataplugin.radar.RadarStation;
import com.raytheon.uf.common.datastorage.IDataStore;
import com.raytheon.uf.common.datastorage.StorageException;
import com.raytheon.uf.common.datastorage.records.FloatDataRecord;
import com.raytheon.uf.common.datastorage.records.IDataRecord;
import com.raytheon.uf.common.geospatial.ISpatialEnabled;
import com.raytheon.uf.common.geospatial.ISpatialObject;
import com.raytheon.uf.common.geospatial.MapUtil;
import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;
import com.raytheon.uf.common.serialization.annotations.DynamicSerializeElement;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;

/**
 * Record implementation for VIL plugin
 *
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date          Ticket#  Engineer   Description
 * ------------- -------- ---------- -------------------------------------------
 * Jan 14, 2009  2027     D. Hladky  Initial release
 * Apr 04, 2013  1846     bkowal     Added an index on refTime and forecastTime
 * Apr 08, 2013  1293     bkowal     Removed references to hdffileid.
 * Apr 12, 2013  1857     bgonzale   Added SequenceGenerator annotation.
 * May 07, 2013  1869     bsteffen   Remove dataURI column from
 *                                   PluginDataObject.
 * Aug 30, 2013  2298     rjpeter    Make getPluginName abstract
 * Oct 14, 2013  2361     njensen    Removed XML annotations
 * Jul 09, 2018  6706     randerso   Corrected maxExtent calculation. Code
 *                                   cleanuup
 *
 * </pre>
 *
 * @author dhladky
 */
@Entity
@SequenceGenerator(initialValue = 1, name = PluginDataObject.ID_GEN, sequenceName = "vilseq")
@Table(name = "vil", uniqueConstraints = {
        @UniqueConstraint(columnNames = { "dataURI" }) })
/*
 * Both refTime and forecastTime are included in the refTimeIndex since
 * forecastTime is unlikely to be used.
 */
@org.hibernate.annotations.Table(appliesTo = "vil", indexes = {
        @Index(name = "vil_refTimeIndex", columnNames = { "refTime",
                "forecastTime" }) })
@DynamicSerialize
public class VILRecord extends PersistablePluginDataObject
        implements ISpatialEnabled {
    private static final IUFStatusHandler statusHandler = UFStatus
            .getHandler(VILRecord.class);

    private static final long serialVersionUID = 767763365671L;

    private static final String cubed = "\u00b3";

    @Column(length = 7)
    @DataURI(position = 1)
    @DynamicSerializeElement
    private String icao;

    @Column(length = 30)
    @DataURI(position = 2)
    @DynamicSerializeElement
    private String fieldName;

    @ManyToOne
    @PrimaryKeyJoinColumn
    @DynamicSerializeElement
    private RadarStation spatialInfo;

    @Column
    @DynamicSerializeElement
    public Integer nx = 0;

    @Column
    @DynamicSerializeElement
    public Integer ny = 0;

    @Column
    @DynamicSerializeElement
    public Integer dx = 0;

    @Column
    @DynamicSerializeElement
    public Integer dy = 0;

    @Transient
    private float[] data_array = null;

    @Transient
    public GridGeometry2D gridGeometry2D = null;

    /**
     * Default Constructor
     */
    public VILRecord() {
    }

    /**
     * Constructs a record from a dataURI
     *
     * @param uri
     *            The dataURI
     * @param tableDef
     *            The table definition associated with this class
     */
    public VILRecord(String uri) {
        super(uri);
    }

    /**
     * Enumeration of the fieldNames for VIL
     */
    public enum DATA_TYPE {
        VILD("VIL Density"),
        DVILD("Digital VIL Density"),
        EDVILD("Enhanced Digital VIL Density");

        private final String fieldName;

        private DATA_TYPE(String name) {
            fieldName = name;
        }

        public String getFieldName() {
            return fieldName;
        }
    };

    /**
     * Enumeration of the fieldUnits for QPF
     */
    public enum UNIT_TYPE {
        VILD("g/m" + cubed), DVILD("g/m" + cubed), EDVILD("g/m" + cubed);

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
    public GridGeometry2D getGridGeometry() {
        if (gridGeometry2D == null) {
            ProjectedCRS crs = this.getCRS();
            GeneralEnvelope generalEnvelope = new GeneralEnvelope(2);
            generalEnvelope.setCoordinateReferenceSystem(crs);
            double maxExtent = (getDx() * (this.getNx())) / 2;
            generalEnvelope.setRange(0, -maxExtent, maxExtent);
            generalEnvelope.setRange(1, -maxExtent, maxExtent);

            gridGeometry2D = new GridGeometry2D(
                    new GeneralGridEnvelope(new int[] { 0, 0 },
                            new int[] { this.getNx(), this.getNy() }, false),
                    generalEnvelope);
        }

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
            for (IDataRecord element : dataRec) {
                if (element instanceof FloatDataRecord) {
                    setDataArray(((FloatDataRecord) element).getFloatData());
                }
            }
        } catch (StorageException | FileNotFoundException e) {
            statusHandler.error("Error retrieving VIL data", e);
        }
    }

    /**
     * Used for debugging.
     */
    @Override
    public String toString() {
        StringBuilder sb = new StringBuilder();
        sb.append("\n dataURI: " + getDataURI() + "\n");
        sb.append("data_array: " + getDataArray().length + "\n");
        sb.append("Icao: " + getIcao() + "\n");
        sb.append("Field: " + getFieldName() + "\n");
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

    @Override
    @Column
    @Access(AccessType.PROPERTY)
    public String getDataURI() {
        return super.getDataURI();
    }

    @Override
    public String getPluginName() {
        return "vil";
    }
}
