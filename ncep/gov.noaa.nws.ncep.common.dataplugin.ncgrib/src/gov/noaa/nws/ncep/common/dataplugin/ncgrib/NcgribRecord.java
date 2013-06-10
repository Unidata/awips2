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

package gov.noaa.nws.ncep.common.dataplugin.ncgrib;

import java.util.Arrays;
import java.util.Calendar;

import javax.persistence.CascadeType;
import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.FetchType;
import javax.persistence.ManyToOne;
import javax.persistence.PrimaryKeyJoinColumn;
import javax.persistence.SequenceGenerator;
import javax.persistence.Table;
import javax.persistence.Transient;
import javax.persistence.UniqueConstraint;
import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlAttribute;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlRootElement;

import org.hibernate.annotations.Fetch;
import org.hibernate.annotations.FetchMode;
import org.hibernate.annotations.Index;

import com.raytheon.uf.common.dataplugin.IDecoderGettable;
import com.raytheon.uf.common.dataplugin.PluginDataObject;
import com.raytheon.uf.common.dataplugin.annotations.DataURI;
import com.raytheon.uf.common.dataplugin.persist.IHDFFilePathProvider;
import com.raytheon.uf.common.dataplugin.persist.PersistablePluginDataObject;
import com.raytheon.uf.common.geospatial.ISpatialEnabled;
import com.raytheon.uf.common.geospatial.ISpatialObject;
import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;
import com.raytheon.uf.common.serialization.annotations.DynamicSerializeElement;

/**
 * Record class for the ncgrib plugin.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#     Engineer    Description
 * ------------ ----------  ----------- --------------------------
 * 4/7/09       1994        bphillip    Initial Creation
 * 10/13/10      276        llin        Modified for NC GRIB.
 * 03/07/12      606        ghull       Added eventName to URI for NcInventory updating.
 * Apr 4, 2013        1846 bkowal      Added an index on refTime and forecastTime
 * 04/08/13     1293        bkowal      Removed references to hdffileid.
 * Apr 12, 2013 1857        bgonzale    Added SequenceGenerator annotation.
 * 
 * </pre>
 * 
 * @author bphillip
 * @version 1
 */
@Entity
@SequenceGenerator(initialValue = 1, name = PluginDataObject.ID_GEN, sequenceName = "ncgribseq")
@Table(name = "ncgrib", uniqueConstraints = { @UniqueConstraint(columnNames = { "dataURI" }) })
/*
 * Both refTime and forecastTime are included in the refTimeIndex since
 * forecastTime is unlikely to be used.
 */
@org.hibernate.annotations.Table(
		appliesTo = "ncgrib",
		indexes = {
				@Index(name = "ncgrib_refTimeIndex", columnNames = { "refTime", "forecastTime" } )
		}
)
@XmlRootElement
@XmlAccessorType(XmlAccessType.NONE)
@DynamicSerialize
public class NcgribRecord extends PersistablePluginDataObject implements
        ISpatialEnabled {

    private static final long serialVersionUID = 1L;

    /** GRIB master tables version number (currently 2) (See Table 1.0) */
    @Column
    @XmlAttribute
    @DynamicSerializeElement
    private int masterTableVersion;

	/**
     * Version number of GRIB local tables used to augment Master Tables (See
     * Table 1.1) 0 - local tables not used, only table entries and templates
     * the current master table are valid.  (Currently 1)
     */
    @Column
    @XmlAttribute
    @DynamicSerializeElement
    private int localTableVersion;

    /** Significance of reference time (See Table 1.2)
     * 0 for analysis, 1 for forecast, 2 for verifying time,
     * and 3 for observation time
     */
    @Column
    @XmlAttribute
    @DynamicSerializeElement
    private int refTimeSignificance;

    /** Processed data type in this GRIB message (See Table 1.4)
     *  0 for analysis, 1 for forecast, 2 for both, ....
     *  or PDT in table 4.3
     *  This refers to PDT# in GEMPAK output
     */
    @Column
    @XmlAttribute
    @DynamicSerializeElement
    private int processedDataType;
    
    /** Denotes if local section is present (currently false)*/
    @Column
    @XmlAttribute
    @DynamicSerializeElement
    private boolean localSectionUsed;

    /** The values extracted from the local section (in hdf5) */
    @Transient
    @DynamicSerializeElement
    private int[] localSection;

    /**
     * Denotes if this is a thinned grid, therefore containing a list of thinned
     * points per row (currently false)
     */
    @Column
    @XmlAttribute
    @DynamicSerializeElement
    private boolean thinnedGrid;

    /**
     * The number of points in each row of a quasi-regular grid, if applicable
     * (if yes, store in hdf5)
     */
    @Transient
    @DynamicSerializeElement
    private int[] thinnedPts;

    /**
     * Denotes if this grid is a hybrid level grid, therefore containing a list
     * of hybrid level information (currently false)
     */
    @Column
    @XmlAttribute
    @DynamicSerializeElement
    private boolean hybridGrid;

    /**
     * The values of the optional coordinate list provided with hybrid level
     * parameters (if yes, store in hdf5)
     */
    @Transient
    @DynamicSerializeElement
    private float[] hybridCoordList;

    /** The model information in ncgrib_models child table*/
    @ManyToOne(cascade = { CascadeType.REFRESH }, fetch = FetchType.EAGER)
    @PrimaryKeyJoinColumn
    @Fetch(FetchMode.SELECT)
    @DataURI(position = 1, embedded = true)
    @XmlElement
    @DynamicSerializeElement
    private NcgribModel modelInfo;

    private boolean isVector = false;

    /** The short model name (i.e.NAM80) This should be interpreted from
     *  the generating process number and grid id : 96 for gfs,
     *  114 for NAEFS, 84 for meso NAM 12KM, 86 for RUC, 81 for GFS analysis,
     *  82 for analysis GDAS, etc...
     *  information form ON388 - table A
     *  Generating Process or Model from originating center 7
     *  which is NCEP
     */
    @Column
    @XmlAttribute
    @DynamicSerializeElement
    @DataURI(position = 2)
    private String modelName;

    @Column
    @XmlAttribute
    @DynamicSerializeElement
    @DataURI(position = 3)
    private int gridVersion = 0;

    /** The name of ingested file
     */
    @Column
    @XmlAttribute
    @DynamicSerializeElement
    @DataURI(position = 4)
    private String fileName;
    
    /** The name of event such as Hurricane or 
     *  Volcano 
     */
    @Column
    @XmlAttribute
    @DynamicSerializeElement
    @DataURI(position = 8)
    private String eventName;
    
    /** Type of Generating Process 
     */
    @Column
    @XmlAttribute
    @DynamicSerializeElement
    @DataURI(position = 7)
    private int processType;
    
    /** Resolution and componet flags
     * (See Table 3.3)
     */
    @Column
    @XmlAttribute
    @DynamicSerializeElement
    private Integer resCompFlags;

    /**
     * Indicate the discipline of the processed
     * data contained within a GRIB message -
     * 0 for Meteorological products in table 0.0
     * This refers to DIS# in GEMPAK output
     */
    @Column
    @XmlAttribute
    @DynamicSerializeElement
    private int discipline;

    /**
     * Parameter category by product discipline
     * 0 for temperature in table 4.1 by discipline 0
     */
    @Column
    @XmlAttribute
    @DynamicSerializeElement
    private int category;

    /**
     * Parameter number by product discipline and parameter category
     * 9 for temperature anomaly in table 4.2-0-0 for discipline 0
     * and category 0
     * This refers to ID# in GEMPAK output
     */
    @Column
    @XmlAttribute
    @DynamicSerializeElement
    private int parameterId;
   
    /** pdt - Product definition template number.
     */
    @Column
    @XmlAttribute
    @DynamicSerializeElement
    private int pdt;
    
    /**
     * Fixed surface types or vertical coordinate ID 1
     * 2 for cloud base level and 100 for isobaric surface
     * in table 4.5 or VCRDGRID1.TBL for NCEP
     * This refers to VCD# in GEMPAK output
     * The location in pds[9]
     */
    @Column
    @XmlAttribute
    @DynamicSerializeElement
    private int vcrdId1;

    /**
     * Fixed surface types or vertical coordinate ID 2
     * 2 for cloud base level and 100 for isobaric surface
     * in table 4.5 or VCRDGRID1.TBL for NCEP
     * This refers to VCD# in GEMPAK output
     * The location in pds[12]
     */
    @Column
    @XmlAttribute
    @DynamicSerializeElement
    private int vcrdId2;

    /**
     * Scaled value of first fixed surface in GRIB2- TEMPLATE 4.1
     * This refers to LEVEL1 in GEMPAK output
     */
    @Column
    @XmlAttribute
    @DynamicSerializeElement
    private int glevel1;
    
    private float decodedLevel1;

    /**
     * Scaled value of second fixed surface in GRIB2- TEMPLATE 4.1
     * This refers to LEVEL2 in GEMPAK output
     */
    @Column
    @XmlAttribute
    @DynamicSerializeElement
    private int glevel2;

    private float decodedLevel2;

    /**
     * The gempak vertical coordinate grid name
     */
    @Column
    @XmlAttribute
    @DynamicSerializeElement
    @DataURI(position = 6)
    private String vcord;
    
    /**
     * The gempak abbreviation grid name
     */
    @Column
    @XmlAttribute
    @DynamicSerializeElement
    @DataURI(position = 5)
    private String parm;
    
    /**
     * The gempak scale for decoding the grid field
     */
    @Column
    @XmlAttribute
    @DynamicSerializeElement
    private String scale;
    
    /**
     * The forecast interval
     */
    @Column
    @XmlAttribute
    @DynamicSerializeElement
    private int interval;
    
    /**
     * Creates an empty NcgribRecord
     */
    public NcgribRecord() {

    }

    /**
     * Creates a NcgribRecord from the provided uri
     * 
     * @param uri
     *            The dataURI used to construct the record
     */
    public NcgribRecord(String uri) {
        super(uri);
    }

    /**
     * Copy constructor
     */
    public NcgribRecord(NcgribRecord recordToCopy) {
        if (recordToCopy.dataTime != null) {
            this.dataTime = recordToCopy.dataTime.clone();
        }
        this.dataURI = recordToCopy.dataURI;
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
        this.modelInfo = new NcgribModel(recordToCopy.getModelInfo());
        this.processedDataType = recordToCopy.processedDataType;
        this.refTimeSignificance = recordToCopy.refTimeSignificance;
        this.resCompFlags = recordToCopy.resCompFlags;
        this.thinnedGrid = recordToCopy.thinnedGrid;
        if (recordToCopy.thinnedPts != null) {
            this.thinnedPts = Arrays.copyOf(recordToCopy.thinnedPts,
                    recordToCopy.thinnedPts.length);
        }
        this.modelName = recordToCopy.modelName;
        this.fileName = recordToCopy.fileName;
        this.eventName = recordToCopy.eventName;
        this.processType = recordToCopy.processType;
        this.discipline = recordToCopy.discipline;
        this.category = recordToCopy.category;
        this.parameterId = recordToCopy.parameterId;
        this.pdt = recordToCopy.pdt;
        this.vcrdId1 = recordToCopy.vcrdId1;
        this.vcrdId2 = recordToCopy.vcrdId2;
        this.vcord = recordToCopy.vcord;
        this.glevel1 = recordToCopy.glevel1;
        this.glevel2 = recordToCopy.glevel2;
        this.parm = recordToCopy.parm;
        this.scale = recordToCopy.scale;
    }

    @Override
    public IHDFFilePathProvider getHDFPathProvider() {
        return NcgribPathProvider.getInstance();
    }

    @Override
    public IDecoderGettable getDecoderGettable() {
        return null;
    }

    @Override
    public ISpatialObject getSpatialObject() {
        return modelInfo.getLocation();
    }

    /**
    public void setSpatialObject(NcgridCoverage location) {
        modelInfo.setLocation(location);
    }*/
    
    /**
     * Gets the model information
     * 
     * @return The model information
     */
    public NcgribModel getModelInfo() {
        return modelInfo;
    }

    /**
     * Sets the model information
     * 
     * @param modelInfo
     *            The model information
     */
    public void setModelInfo(NcgribModel modelInfo) {
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

    public String getModelName() {
		return modelName;
	}

	public void setModelName(String modelName) {
		this.modelName = modelName;
	}

	public int getDiscipline() {
		return discipline;
	}

	public void setDiscipline(int discipline) {
		this.discipline = discipline;
	}

	public int getCategory() {
		return category;
	}

	public void setCategory(int category) {
		this.category = category;
	}

	public int getParameterId() {
		return parameterId;
	}

	public void setParameterId(int parameterId) {
		this.parameterId = parameterId;
	}

	public int getPdt() {
		return pdt;
	}

	public void setPdt(int pdt) {
		this.pdt = pdt;
	}

	public String getFileName() {
		return fileName;
	}

	public void setFileName(String fileName) {
		this.fileName = fileName;
	}
	
	public String getEventName() {
		return eventName;
	}

	public void setEventName(String eventName) {
		this.eventName = eventName;
	}

	public int getProcessType() {
		return processType;
	}

	public void setProcessType(int processType) {
		this.processType = processType;
	}
	
	public int getVcrdId1() {
		return vcrdId1;
	}

	public void setVcrdId1(int vcrdId1) {
		this.vcrdId1 = vcrdId1;
	}

	public int getVcrdId2() {
		return vcrdId2;
	}

	public void setVcrdId2(int vcrdId2) {
		this.vcrdId2 = vcrdId2;
	}

	public int getGlevel1() {
		return glevel1;
	}

	public void setGlevel1(int glevel1) {
		this.glevel1 = glevel1;
	}

	public int getGlevel2() {
		return glevel2;
	}

	public void setGlevel2(int glevel2) {
		this.glevel2 = glevel2;
	}

	public String getVcord() {
		return vcord;
	}

	public void setVcord(String vcord) {
		this.vcord = vcord;
	}

	public String getParm() {
		return parm;
	}

	public void setParm(String parm) {
		this.parm = parm;
	}

	public String getScale() {
		return scale;
	}

	public void setScale(String scale) {
		this.scale = scale;
	}

	public int getInterval() {
		return interval;
	}

	public void setInterval(int interval) {
		this.interval = interval;
	}

	/**
	 * @return the decodedLevel1
	 */
	public float getDecodedLevel1() {
		return decodedLevel1;
	}

	/**
	 * @param decodedLevel1 the decodedLevel1 to set
	 */
	public void setDecodedLevel1(float decodedLevel1) {
		this.decodedLevel1 = decodedLevel1;
	}

	/**
	 * @return the decodedLevel2
	 */
	public float getDecodedLevel2() {
		return decodedLevel2;
	}

	/**
	 * @param decodedLevel2 the decodedLevel2 to set
	 */
	public void setDecodedLevel2(float decodedLevel2) {
		this.decodedLevel2 = decodedLevel2;
	}

}
