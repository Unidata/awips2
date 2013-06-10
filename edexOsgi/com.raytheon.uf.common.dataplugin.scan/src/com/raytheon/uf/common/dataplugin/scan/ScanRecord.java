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
package com.raytheon.uf.common.dataplugin.scan;

import java.io.ByteArrayInputStream;
import java.util.Date;
import java.util.Set;

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

import org.geotools.coverage.grid.GridGeometry2D;
import org.hibernate.annotations.Index;

import com.raytheon.uf.common.dataplugin.IDecoderGettable;
import com.raytheon.uf.common.dataplugin.PluginDataObject;
import com.raytheon.uf.common.dataplugin.annotations.DataURI;
import com.raytheon.uf.common.dataplugin.persist.PersistablePluginDataObject;
import com.raytheon.uf.common.dataplugin.scan.data.ModelData;
import com.raytheon.uf.common.dataplugin.scan.data.ScanTableData;
import com.raytheon.uf.common.dataplugin.scan.data.SoundingData;
import com.raytheon.uf.common.datastorage.IDataStore;
import com.raytheon.uf.common.datastorage.Request;
import com.raytheon.uf.common.datastorage.records.ByteDataRecord;
import com.raytheon.uf.common.monitor.scan.config.SCANConfigEnums.ScanTables;
import com.raytheon.uf.common.serialization.DynamicSerializationManager;
import com.raytheon.uf.common.serialization.DynamicSerializationManager.SerializationType;
import com.raytheon.uf.common.serialization.SerializationException;
import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;
import com.raytheon.uf.common.serialization.annotations.DynamicSerializeElement;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;

/**
 * Rehash of SCAN
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#     Engineer    Description
 * ------------ ----------  ----------- --------------------------
 * 03/17/10     2521     D. Hladky   Initial release
 * 02/01/13     1649      D. Hladky  better logging,
 * Feb 28, 2013 1731        bsteffen    Optimize construction of scan resource.
 * Apr 4, 2013        1846 bkowal      Added an index on refTime and forecastTime
 * Apr 8, 2013  1293        bkowal      Removed references to hdffileid.
 * Apr 12, 2013 1857        bgonzale    Added SequenceGenerator annotation.
 * May 07, 2013 1869        bsteffen    Remove dataURI column from
 *                                      PluginDataObject.
 * 
 * </pre>
 * 
 * @author dhladky
 * @version 1
 */

@Entity
@SequenceGenerator(initialValue = 1, name = PluginDataObject.ID_GEN, sequenceName = "scanseq")
@Table(name = "scan", uniqueConstraints = { @UniqueConstraint(columnNames = { "dataURI" }) })
/*
 * Both refTime and forecastTime are included in the refTimeIndex since
 * forecastTime is unlikely to be used.
 */
@org.hibernate.annotations.Table(
		appliesTo = "scan",
		indexes = {
				@Index(name = "scan_refTimeIndex", columnNames = { "refTime", "forecastTime" } )
		}
)
@XmlRootElement
@XmlAccessorType(XmlAccessType.NONE)
@DynamicSerialize
public class ScanRecord extends PersistablePluginDataObject {

    /**
     * 
     */
    private static final long serialVersionUID = 5983810116816447875L;
    
    private static final IUFStatusHandler statusHandler = UFStatus
    .getHandler(ScanRecord.class);
   
    @Column(length = 7)
    @DataURI(position = 1)
    @DynamicSerializeElement
    @XmlElement(nillable = false)
    private String icao;

    @Column(length = 7)
    @DataURI(position = 2)
    @DynamicSerializeElement
    @XmlElement(nillable = false)
    private String type;

    @Column(length = 7)
    @DataURI(position = 3)
    @DynamicSerializeElement
    @XmlElement(nillable = false)
    private double tilt;

    @Transient
    public GridGeometry2D stationGeometry = null;

    @Column
    @DynamicSerializeElement
    @XmlElement(nillable = false)
    public Date volScanTime = null;

    @Column
    @DynamicSerializeElement
    @XmlElement(nillable = false)
    private boolean lastElevationAngle;

    /** table data **/
    @Transient
    public ScanTableData<?> tableData = null;

    @Transient
    /* cell data only */
    public SoundingData sd = null;

    @Transient
    /* cell data only */
    public ModelData md = null;

    public ScanRecord() {
        super();
    }

    public ScanRecord(String uri) {
        super(uri);
    }

    @Override
    public IDecoderGettable getDecoderGettable() {
        // TODO Auto-generated method stub
        return null;
    }

    /**
     * Gets the type of Table
     * 
     * @return
     */
    public String getType() {
        return type;
    }

    /** Set the table type saved */
    public void setType(String type) {
        this.type = type;
    }

    /**
     * Gets the radar station ICAO
     * 
     * @return
     */
    public String getIcao() {
        return icao;
    }

    public void setIcao(String icao) {
        this.icao = icao;
    }

    /**
     * Sends a String from the TABLE enum for which table data to grab
     */
    public ScanTableData<?> getTableData() {
        return tableData;
    }

    public void setTableData(ScanTableData<?> table) {
        this.tableData = table;
    }

    /**
     * Get the Keys for my table
     * 
     * @param type
     * @param time
     * @return
     */
    public Set<String> getTableKeys(ScanTables type) {
        Set<String> keySet = null;
        try {
            ScanTableData<?> table = getTableData();
            keySet = table.getTableData().keySet();
        } catch (NullPointerException npe) {
            return null;
        }
        return keySet;
    }

    /**
     * Set the Sounding Data
     * 
     * @param ed
     */
    public void setSoundingData(SoundingData sd) {
        this.sd = sd;
    }

    /**
     * Get the SoundingData
     * 
     * @return
     */
    public SoundingData getSoundingData() {
        return sd;
    }

    /**
     * Set the Model Data
     * 
     * @param md
     */
    public void setModelData(ModelData md) {
        this.md = md;
    }

    /**
     * Get the ModelData
     * 
     * @return
     */
    public ModelData getModelData() {
        return md;
    }

    /**
     * Set the Model Data
     * 
     * @param md
     */
    public void setTilt(double tilt) {
        this.tilt = tilt;
    }

    /**
     * Get the ModelData
     * 
     * @return
     */
    public double getTilt() {
        return tilt;
    }

    /**
     * Gets the station Geometry for the WFO
     * 
     * @return
     */
    public GridGeometry2D getStationGeometry() {
        return stationGeometry;
    }

    public void setStationGeometry(GridGeometry2D stationGeometry) {
        this.stationGeometry = stationGeometry;
    }

    /**
     * Set the TableData from the serialized form that is stored in hdf5.
     * 
     * @param byteData
     * @throws SerializationException
     */
    public void setTableData(ByteDataRecord byteData)
            throws SerializationException {
        ByteArrayInputStream bais = new ByteArrayInputStream(
                byteData.getByteData());
        Object o = DynamicSerializationManager.getManager(
                SerializationType.Thrift).deserialize(bais);
        setTableData((ScanTableData<?>) o);
    }

    /**
     * Gets the Hash out of the datastore by HUC
     * 
     * @param dataStore
     * @param huc
     */
    public void retrieveMapFromDataStore(IDataStore dataStore) {
        try {
            ByteDataRecord byteData = (ByteDataRecord) dataStore.retrieve(
                    getDataURI(), getType(), Request.ALL);
            setTableData(byteData);
        } catch (Throwable e) {
            statusHandler.handle(Priority.ERROR, "Couldn't load Table data!" + getDataURI());
        }
    }

    /**
     * Gets the Sounding Data available in this record
     * 
     * @param dataStore
     */
    public void retrieveSoundingDataFromDataStore(IDataStore dataStore) {
        try {
            ByteDataRecord byteData = (ByteDataRecord) dataStore.retrieve(
                    getDataURI(), getType() + "/sounding", Request.ALL);
            ByteArrayInputStream bais = new ByteArrayInputStream(
                    byteData.getByteData());
            Object o = DynamicSerializationManager.getManager(
                    SerializationType.Thrift).deserialize(bais);
            setSoundingData((SoundingData) o);
        } catch (Throwable e) {
            statusHandler.handle(Priority.ERROR, "Couldn't load Sounding data!" + getDataURI());
        }
    }

    /**
     * Gets the Sounding Data available in this record
     * 
     * @param dataStore
     */
    public void retrieveModelDataFromDataStore(IDataStore dataStore) {
        try {
            ByteDataRecord byteData = (ByteDataRecord) dataStore.retrieve(
                    getDataURI(), getType() + "/model", Request.ALL);
            ByteArrayInputStream bais = new ByteArrayInputStream(
                    byteData.getByteData());
            Object o = DynamicSerializationManager.getManager(
                    SerializationType.Thrift).deserialize(bais);
            setModelData((ModelData) o);
        } catch (Throwable e) {
            statusHandler.handle(Priority.ERROR, "Couldn't load Model data!" + getDataURI());
        }
    }

    /**
     * Gets the volume scan time
     * 
     * @return
     */
    public Date getVolScanTime() {
        return volScanTime;
    }

    /**
     * set the volume scan time
     * 
     * @param volScanTime
     */
    public void setVolScanTime(Date volScanTime) {
        this.volScanTime = volScanTime;
    }

    public boolean isLastElevationAngle() {
        return lastElevationAngle;
    }

    public void setLastElevationAngle(boolean lastElevationAngle) {
        this.lastElevationAngle = lastElevationAngle;
    }

    /**
     * Used for debugging.
     */
    @Override
    public String toString() {
        StringBuffer sb = new StringBuffer();
        sb.append("\n dataURI: " + getDataURI() + "\n");
        if (tableData != null) {
            for (Object key : tableData.getTableData().keySet()) {
                sb.append(key + " : "
                        + tableData.getTableData().get(key).toString() + "\n");
            }
        }

        return sb.toString();
    }

    @Override
    @Column
    @Access(AccessType.PROPERTY)
    public String getDataURI() {
        return super.getDataURI();
    }
}