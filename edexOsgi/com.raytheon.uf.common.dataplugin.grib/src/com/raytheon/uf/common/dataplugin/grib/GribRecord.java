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

package com.raytheon.uf.common.dataplugin.grib;

import java.util.Arrays;
import java.util.Calendar;
import java.util.Date;
import java.util.TimeZone;

import javax.persistence.CascadeType;
import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.ManyToOne;
import javax.persistence.PrimaryKeyJoinColumn;
import javax.persistence.Table;
import javax.persistence.Transient;
import javax.persistence.UniqueConstraint;
import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlAttribute;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlRootElement;

import com.raytheon.uf.common.dataplugin.IDecoderGettable;
import com.raytheon.uf.common.dataplugin.IPrecomputedRange;
import com.raytheon.uf.common.dataplugin.annotations.DataURI;
import com.raytheon.uf.common.dataplugin.persist.IHDFFilePathProvider;
import com.raytheon.uf.common.dataplugin.persist.IPersistable;
import com.raytheon.uf.common.dataplugin.persist.PersistablePluginDataObject;
import com.raytheon.uf.common.geospatial.ISpatialEnabled;
import com.raytheon.uf.common.geospatial.ISpatialObject;
import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;
import com.raytheon.uf.common.serialization.annotations.DynamicSerializeElement;

/**
 * Deprecated, use grid
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#     Engineer    Description
 * ------------ ----------  ----------- --------------------------
 * 4/7/09       1994        bphillip    Initial Creation
 * 
 * </pre>
 * 
 * @author bphillip
 * @version 1
 */
@Entity
@Table(name = "grib", uniqueConstraints = { @UniqueConstraint(columnNames = { "dataURI" }) })
@XmlRootElement
@XmlAccessorType(XmlAccessType.NONE)
@DynamicSerialize
@Deprecated
public class GribRecord extends PersistablePluginDataObject implements
        IPersistable, ISpatialEnabled, IPrecomputedRange {

    private static final long serialVersionUID = 1L;

    /** GRIB master tables version number (currently 2) (See Table 1.0) */
    @Column
    @XmlAttribute
    @DynamicSerializeElement
    private int masterTableVersion;

    /**
     * Version number of GRIB local tables used to augment Master Tables (See
     * Table 1.1)
     */
    @Column
    @XmlAttribute
    @DynamicSerializeElement
    private int localTableVersion;

    /** Significance of reference time (See Table 1.2) */
    @Column
    @XmlAttribute
    @DynamicSerializeElement
    private int refTimeSignificance;

    /** Production status of processed data in the GRIB message (See Table 1.3) */
    @Column
    @XmlAttribute
    @DynamicSerializeElement
    private int productionStatus;

    /** Type of processed data in this GRIB message (See Table 1.4) */
    @Column
    @XmlAttribute
    @DynamicSerializeElement
    private int processedDataType;

    /** Denotes if local section is present */
    @Column
    @XmlAttribute
    @DynamicSerializeElement
    private boolean localSectionUsed;

    /** The values extracted from the local section */
    @Transient
    @DynamicSerializeElement
    private int[] localSection;

    /**
     * Denotes if this is a thinned grid, therefore containing a list of thinned
     * points per row
     */
    @Column
    @XmlAttribute
    @DynamicSerializeElement
    private boolean thinnedGrid;

    /**
     * The number of points in each row of a quasi-regular grid, if applicable
     */
    @Transient
    @DynamicSerializeElement
    private int[] thinnedPts;

    /**
     * Denotes if this grid is a hybrid level grid, therefore containing a list
     * of hybrid level information
     */
    @Column
    @XmlAttribute
    @DynamicSerializeElement
    private boolean hybridGrid;

    /**
     * The values of the optional coordinate list provided with hybrid level
     * parameters
     */
    @Transient
    @DynamicSerializeElement
    private float[] hybridCoordList;

    /** The model information */
    @ManyToOne(cascade = { CascadeType.REFRESH })
    @PrimaryKeyJoinColumn
    @DataURI(position = 1, embedded = true)
    @XmlElement
    @DynamicSerializeElement
    private GribModel modelInfo;

    private boolean isVector = false;

    @Column
    @XmlAttribute
    @DynamicSerializeElement
    @DataURI(position = 2)
    private int gridVersion = 0;

    @Column
    @XmlAttribute
    @DynamicSerializeElement
    private Integer resCompFlags;

    @Column
    @XmlAttribute
    @DynamicSerializeElement
    private double dataMin;

    @Column
    @XmlAttribute
    @DynamicSerializeElement
    private double dataMax;

    /**
     * Creates an empty GribRecord
     */
    public GribRecord() {

    }

    /**
     * Creates a GribRecord from the provided uri
     * 
     * @param uri
     *            The dataURI used to construct the record
     */
    public GribRecord(String uri) {
        super(uri);
    }

    /**
     * Copy constructor
     */
    public GribRecord(GribRecord recordToCopy) {
        if (recordToCopy.dataTime != null) {
            this.dataTime = recordToCopy.dataTime.clone();
        }
        this.dataURI = recordToCopy.dataURI;
        this.setHdfFileId(recordToCopy.getHdfFileId());
        this.id = recordToCopy.id;
        this.identifier = recordToCopy.identifier;
        if (recordToCopy.insertTime != null) {
            this.insertTime = (Calendar) recordToCopy.insertTime.clone();
        }
        this.messageData = recordToCopy.messageData;
        this.pluginName = recordToCopy.pluginName;
        this.gridVersion = recordToCopy.gridVersion;
        if (recordToCopy.hybridCoordList != null) {
            this.hybridCoordList = Arrays.copyOf(recordToCopy.hybridCoordList,
                    recordToCopy.hybridCoordList.length);
        }
        this.hybridGrid = recordToCopy.hybridGrid;
        this.isVector = recordToCopy.isVector;
        if (recordToCopy.localSection != null) {
            this.localSection = Arrays.copyOf(recordToCopy.localSection,
                    recordToCopy.localSection.length);
        }
        this.localSectionUsed = recordToCopy.localSectionUsed;
        this.localTableVersion = recordToCopy.localTableVersion;
        this.masterTableVersion = recordToCopy.masterTableVersion;
        this.modelInfo = new GribModel(recordToCopy.getModelInfo());
        this.processedDataType = recordToCopy.processedDataType;
        this.productionStatus = recordToCopy.productionStatus;
        this.refTimeSignificance = recordToCopy.refTimeSignificance;
        this.resCompFlags = recordToCopy.resCompFlags;
        this.thinnedGrid = recordToCopy.thinnedGrid;
        if (recordToCopy.thinnedPts != null) {
            this.thinnedPts = Arrays.copyOf(recordToCopy.thinnedPts,
                    recordToCopy.thinnedPts.length);
        }
    }

    @Override
    public IHDFFilePathProvider getHDFPathProvider() {
        return GribPathProvider.getInstance();
    }

    @Override
    public IDecoderGettable getDecoderGettable() {
        return null;
    }

    @Override
    public Date getPersistenceTime() {
        Calendar c = getInsertTime();
        if (c == null)
            return null;

        return c.getTime();
    }

    @Override
    public void setPersistenceTime(Date persistTime) {
        Calendar c = Calendar.getInstance(TimeZone.getTimeZone("GMT"));
        c.setTime(persistTime);
        setInsertTime(c);
    }

    @Override
    public ISpatialObject getSpatialObject() {
        return modelInfo.getLocation();
    }

    /**
     * Gets the model information
     * 
     * @return The model information
     */
    public GribModel getModelInfo() {
        return modelInfo;
    }

    /**
     * Sets the model information
     * 
     * @param modelInfo
     *            The model information
     */
    public void setModelInfo(GribModel modelInfo) {
        this.modelInfo = modelInfo;
    }

    /**
     * Gets the master table version
     * 
     * @return The master table version
     */
    public int getMasterTableVersion() {
        return masterTableVersion;
    }

    /**
     * Sets the master table version
     * 
     * @param masterTableVersion
     *            The master table version
     */
    public void setMasterTableVersion(int masterTableVersion) {
        this.masterTableVersion = masterTableVersion;
    }

    /**
     * Gets the local table version
     * 
     * @return The local table version
     */
    public int getLocalTableVersion() {
        return localTableVersion;
    }

    /**
     * Sets the local table version
     * 
     * @param localTableVersion
     *            The local table version
     */
    public void setLocalTableVersion(int localTableVersion) {
        this.localTableVersion = localTableVersion;
    }

    /**
     * Gets the reference time significance
     * 
     * @return The reference time significance
     */
    public int getRefTimeSignificance() {
        return refTimeSignificance;
    }

    /**
     * Sets the reference time significance
     * 
     * @param refTimeSignificance
     *            The reference time significance
     */
    public void setRefTimeSignificance(int refTimeSignificance) {
        this.refTimeSignificance = refTimeSignificance;
    }

    /**
     * Gets the production status
     * 
     * @return The production status
     */
    public int getProductionStatus() {
        return productionStatus;
    }

    /**
     * Sets the production status
     * 
     * @param productionStatus
     *            The production status
     */
    public void setProductionStatus(int productionStatus) {
        this.productionStatus = productionStatus;
    }

    /**
     * Gets ths processed data type
     * 
     * @return The processed data type
     */
    public int getProcessedDataType() {
        return processedDataType;
    }

    /**
     * Sets the processed data type
     * 
     * @param processedDataType
     *            The processed data type
     */
    public void setProcessedDataType(int processedDataType) {
        this.processedDataType = processedDataType;
    }

    /**
     * Gets if local section is present
     * 
     * @return localSectionUsed
     */
    public boolean isLocalSectionUsed() {
        return localSectionUsed;
    }

    /**
     * Sets localSectionUsed
     * 
     * @param localSectionUsed
     *            True if localsection if used, else false
     */
    public void setLocalSectionUsed(boolean localSectionUsed) {
        this.localSectionUsed = localSectionUsed;
    }

    /**
     * Gets the local section data
     * 
     * @return The local section data
     */
    public int[] getLocalSection() {
        return localSection;
    }

    /**
     * Sets the local section data
     * 
     * @param localSection
     *            The local section data
     */
    public void setLocalSection(int[] localSection) {
        this.localSection = localSection;
    }

    /**
     * Gets the Hybrid coordinate list
     * 
     * @return The hybrid coordinate list
     */
    public float[] getHybridCoordList() {
        return hybridCoordList;
    }

    /**
     * Sets the hybrid coordinate list
     * 
     * @param hybridCoordList
     *            The hybrid coordinate list
     */
    public void setHybridCoordList(float[] hybridCoordList) {
        this.hybridCoordList = hybridCoordList;
    }

    /**
     * Gets the number of points in each row for a thinned grid
     * 
     * @return The thinned row points
     */
    public int[] getThinnedPts() {
        return thinnedPts;
    }

    /**
     * Sets the number of points in each row for a thinned grid
     * 
     * @param thinnedPts
     *            The thinned row points
     */
    public void setThinnedPts(int[] thinnedPts) {
        this.thinnedPts = thinnedPts;
    }

    public boolean isThinnedGrid() {
        return thinnedGrid;
    }

    public void setThinnedGrid(boolean thinnedGrid) {
        this.thinnedGrid = thinnedGrid;
    }

    public boolean isHybridGrid() {
        return hybridGrid;
    }

    public void setHybridGrid(boolean hybridGrid) {
        this.hybridGrid = hybridGrid;
    }

    public boolean isVector() {
        return this.isVector;
    }

    public void setVector(boolean isVector) {
        this.isVector = isVector;
    }

    public int getGridVersion() {
        return gridVersion;
    }

    public void setGridVersion(int gridVersion) {
        this.gridVersion = gridVersion;
    }

    public Integer getResCompFlags() {
        return resCompFlags;
    }

    public void setResCompFlags(Integer resCompFlags) {
        this.resCompFlags = resCompFlags;
    }

    public void setId(int id) {
        this.id = id;
    }

    /**
     * @return the dataMin
     */
    @Override
    public double getDataMin() {
        return dataMin;
    }

    /**
     * @param dataMin
     *            the dataMin to set
     */
    public void setDataMin(double dataMin) {
        this.dataMin = dataMin;
    }

    /**
     * @return the dataMax
     */
    @Override
    public double getDataMax() {
        return dataMax;
    }

    /**
     * @param dataMax
     *            the dataMax to set
     */
    public void setDataMax(double dataMax) {
        this.dataMax = dataMax;
    }

    @Override
    public int hashCode() {
        final int prime = 31;
        int result = 1;
        result = prime * result + gridVersion;
        result = prime * result + Arrays.hashCode(hybridCoordList);
        result = prime * result + (hybridGrid ? 1231 : 1237);
        result = prime * result + (isVector ? 1231 : 1237);
        result = prime * result + Arrays.hashCode(localSection);
        result = prime * result + (localSectionUsed ? 1231 : 1237);
        result = prime * result + localTableVersion;
        result = prime * result + masterTableVersion;
        result = prime * result
                + ((modelInfo == null) ? 0 : modelInfo.hashCode());
        result = prime * result + processedDataType;
        result = prime * result + productionStatus;
        result = prime * result + refTimeSignificance;
        result = prime * result
                + ((resCompFlags == null) ? 0 : resCompFlags.hashCode());
        result = prime * result + (thinnedGrid ? 1231 : 1237);
        result = prime * result + Arrays.hashCode(thinnedPts);
        return result;
    }

    @Override
    public boolean equals(Object obj) {
        if (this == obj)
            return true;
        if (obj == null)
            return false;
        if (getClass() != obj.getClass())
            return false;
        GribRecord other = (GribRecord) obj;
        if (!this.dataTime.getRefTimeAsCalendar().equals(
                other.getDataTime().getRefTimeAsCalendar())) {
            // if the reftimes are not equal equality is false
            return false;
        } else if (this.dataTime.getFcstTime() != other.getDataTime()
                .getFcstTime()) {
            // if the reftimes are equal but the forecast times are not then
            // equality is false
            return false;
        }

        if (gridVersion != other.gridVersion)
            return false;
        if (!Arrays.equals(hybridCoordList, other.hybridCoordList))
            return false;
        if (hybridGrid != other.hybridGrid)
            return false;
        if (isVector != other.isVector)
            return false;
        if (!Arrays.equals(localSection, other.localSection))
            return false;
        if (localSectionUsed != other.localSectionUsed)
            return false;
        if (localTableVersion != other.localTableVersion)
            return false;
        if (masterTableVersion != other.masterTableVersion)
            return false;
        if (modelInfo == null) {
            if (other.modelInfo != null)
                return false;
        } else if (!modelInfo.equals(other.modelInfo))
            return false;
        if (processedDataType != other.processedDataType)
            return false;
        if (productionStatus != other.productionStatus)
            return false;
        if (refTimeSignificance != other.refTimeSignificance)
            return false;
        if (resCompFlags == null) {
            if (other.resCompFlags != null)
                return false;
        } else if (!resCompFlags.equals(other.resCompFlags))
            return false;
        if (thinnedGrid != other.thinnedGrid)
            return false;
        if (!Arrays.equals(thinnedPts, other.thinnedPts))
            return false;
        return true;
    }

}
