/**
 * This class performs mapping to database for McIDAS area file plug-in.
 * 
 * 
 * <pre>
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#     Engineer    Description
 * ------------ ----------  ----------- --------------------------
 * 08/2009      144         T. Lee      Created
 * 11/2009		144			T. Lee		Implemented area name and
 *										added file name
 * 12/2009		144			T. Lee		Added calType, satelliteId
 *										and imageTypeNumber
 * 05/2010		144			L. Lin		Migration to TO11DR11.
 * 09/2012					B. Hebbard  Merge out RTS changes from OB12.9.1
 * </pre>
 * 
 * @author tlee
 * @version 1
 */

package gov.noaa.nws.ncep.common.dataplugin.mcidas;

import java.util.Calendar;

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

import org.hibernate.annotations.Cache;
import org.hibernate.annotations.CacheConcurrencyStrategy;

import com.raytheon.uf.common.dataplugin.IDecoderGettable;
import com.raytheon.uf.common.dataplugin.annotations.DataURI;
import com.raytheon.uf.common.dataplugin.persist.IPersistable;
import com.raytheon.uf.common.dataplugin.persist.PersistablePluginDataObject;
import com.raytheon.uf.common.geospatial.ISpatialEnabled;
import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;
import com.raytheon.uf.common.serialization.annotations.DynamicSerializeElement;

@Entity
@Table(name = "mcidas", uniqueConstraints = { @UniqueConstraint(columnNames = { "dataURI" }) })
@Cache(usage = CacheConcurrencyStrategy.TRANSACTIONAL)
@XmlRootElement
@XmlAccessorType(XmlAccessType.NONE)
@DynamicSerialize
public class McidasRecord extends PersistablePluginDataObject implements
        IPersistable, ISpatialEnabled {

    private static final long serialVersionUID = 1L;

    /** The satellite name */
    @Column(length = 32)
    @DataURI(position = 1)
    @XmlAttribute
    @DynamicSerializeElement
    private String satelliteName;

    /** The area name */
    @Column(length = 64)
    @DataURI(position = 2)
    @XmlAttribute
    @DynamicSerializeElement
    private String areaName;

    /** The resolution */
    @Column
    @DataURI(position = 3)
    @XmlAttribute
    @DynamicSerializeElement
    private Integer resolution;

    /** The image type */
    @Column(length = 32)
    @DataURI(position = 4)
    @XmlAttribute
    @DynamicSerializeElement
    private String imageType;

    /**
     * The creation time
     */
    @Column
    @XmlAttribute
    @DynamicSerializeElement
    private Calendar creationTime;

    /**
     * The image time
     */
    @Column
    @XmlAttribute
    @DynamicSerializeElement
    private Calendar imageTime;

    /**
     * Size of logical records in bytes for product.
     */
    @Column
    @XmlAttribute
    @DynamicSerializeElement
    private Integer sizeRecords;

    /** Satellite projection */
    @Column(length = 16)
    @XmlAttribute
    @DynamicSerializeElement
    private String projection;

    /** The report type */
    @Column(length = 16)
    @XmlAttribute
    @DynamicSerializeElement
    private String reportType;

    /** The calibration type */
    @Column(length = 16)
    @XmlAttribute
    @DynamicSerializeElement
    private String calType;

    /** The satellite ID */
    @Column
    @XmlAttribute
    @DynamicSerializeElement
    private Integer satelliteId;

    /** The image type number */
    @Column
    @XmlAttribute
    @DynamicSerializeElement
    private Integer imageTypeNumber;

    /*
     * Length of prefix in bytes.
     */
    private Integer prefix;

    /*
     * Validation code. if these bytes are non-zero, they must match the first
     * four bytes of each DATA block line prefix or the line's data is ignored.
     */
    private Integer validCode;

    /*
     * File name ingested to the end point.
     */
    private String inputFileName;

    @ManyToOne
    @PrimaryKeyJoinColumn
    @XmlElement
    @DynamicSerializeElement
    private McidasMapCoverage coverage;

    /** Area file header block */
    @Transient
    private byte[] headerBlock;

    @Override
    public McidasMapCoverage getSpatialObject() {
        return coverage;
    }

    public McidasMapCoverage getCoverage() {
        return coverage;
    }

    public void setCoverage(McidasMapCoverage coverage) {
        this.coverage = coverage;
    }

    /**
     * No-arg constructor.
     */
    public McidasRecord() {
        satelliteName = null;
        imageType = null;
        resolution = null;
        projection = null;
        imageTime = null;
        areaName = null;
    }

    /**
     * Constructs a McIDAS satellite record from a dataURI
     * 
     * @param uri
     *            The dataURI
     */
    public McidasRecord(String uri) {
        super(uri);
    }

    
    /**
     * Set the time to be used for the persistence time for this object.
     * 
     * @param persistTime
     *            The persistence time to be used.
     */
    public void setPersistenceTime(Calendar persistTime) {
        setInsertTime(persistTime);
    }

    public Integer getSizeRecords() {
        return sizeRecords;
    }

    public void setSizeRecords(Integer sizeRecords) {
        this.sizeRecords = sizeRecords;
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

    public String getReportType() {
        return reportType;
    }

    public void setReportType(String reportType) {
        this.reportType = reportType;
    }

    public String getSatelliteName() {
        return satelliteName;
    }

    public Calendar getCreationTime() {
        return creationTime;
    }

    public void setCreationTime(Calendar creationTime) {
        this.creationTime = creationTime;
    }

    public Calendar getImageTime() {
        return imageTime;
    }

    public void setImageTime(Calendar imageTime) {
        this.imageTime = imageTime;
    }

    public String getProjection() {
        return projection;
    }

    public void setProjection(String projection) {
        this.projection = projection;
    }

    public void setSatelliteName(String satelliteName) {
        this.satelliteName = satelliteName;
    }

    public String getImageType() {
        return imageType;
    }

    public void setImageType(String imageType) {
        this.imageType = imageType;
    }

    public Integer getResolution() {
        return resolution;
    }

    public void setResolution(Integer resolution) {
        this.resolution = resolution;
    }

    public byte[] getHeaderBlock() {
        return headerBlock;
    }

    public void setHeaderBlock(byte[] headerBlock) {
        this.headerBlock = headerBlock;
    }

    public String getAreaName() {
        return areaName;
    }

    public void setAreaName(String areaName) {
        this.areaName = areaName;
    }

    public Integer getPrefix() {
        return prefix;
    }

    public void setPrefix(Integer prefix) {
        this.prefix = prefix;
    }

    public Integer getValidCode() {
        return validCode;
    }

    public void setValidCode(Integer validCode) {
        this.validCode = validCode;
    }

    public String getInputFileName() {
        return inputFileName;
    }

    public void setInputFileName(String inputFileName) {
        this.inputFileName = inputFileName;
    }

    public String getCalType() {
        return calType;
    }

    public void setCalType(String calType) {
        this.calType = calType;
    }

    public Integer getSatelliteId() {
        return satelliteId;
    }

    public void setSatelliteId(Integer satelliteId) {
        this.satelliteId = satelliteId;
    }

    public Integer getImageTypeNumber() {
        return imageTypeNumber;
    }

    public void setImageTypeNumber(Integer imageTypeNumber) {
        this.imageTypeNumber = imageTypeNumber;
    }
}