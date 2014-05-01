package gov.noaa.nws.ncep.common.dataplugin.ntrans;

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

import com.raytheon.uf.common.dataplugin.IDecoderGettable;
import com.raytheon.uf.common.dataplugin.PluginDataObject;
import com.raytheon.uf.common.dataplugin.annotations.DataURI;
import com.raytheon.uf.common.dataplugin.persist.IHDFFilePathProvider;
import com.raytheon.uf.common.dataplugin.persist.PersistablePluginDataObject;
import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;
import com.raytheon.uf.common.serialization.annotations.DynamicSerializeElement;

/**
 * NtransRecord
 * 
 * This java class performs the mapping to the database table for NTRANS
 * Metafiles
 * 
 * <pre>
 * SOFTWARE HISTORY
 * 
 * Date         Ticket# Author      Description
 * ------------ ------- ----------  ----------- --------------------------
 * 02/2013      B. Hebbard  Initial creation    
 * Apr 4, 2013  1846 bkowal Added an index on refTime and forecastTime  
 * Apr 12, 2013       1857 bgonzale    Added SequenceGenerator annotation.
 * May 07, 2013 bsteffen    Remove dataURI column from PluginDataObject.
 * Feb 11, 2014 2784    rferrel     Remove override of setIdentifier.
 * </pre>
 * 
 * This code has been developed by the SIB for use in the AWIPS2 system.
 */

@Entity
@SequenceGenerator(initialValue = 1, name = PluginDataObject.ID_GEN, sequenceName = "ntransseq")
@Table(name = "ntrans", uniqueConstraints = { @UniqueConstraint(columnNames = { "dataURI" }) })
/*
 * Both refTime and forecastTime are included in the refTimeIndex since
 * forecastTime is unlikely to be used.
 */
@org.hibernate.annotations.Table(appliesTo = "ntrans", indexes = { @Index(name = "ntrans_refTimeIndex", columnNames = {
        "refTime", "forecastTime" }) })
@Cache(usage = CacheConcurrencyStrategy.TRANSACTIONAL)
@XmlRootElement
@XmlAccessorType(XmlAccessType.NONE)
@DynamicSerialize
public class NtransRecord extends PersistablePluginDataObject {

    private static final long serialVersionUID = 1L;

    @Column
    @DataURI(position = 1)
    @XmlElement
    @DynamicSerializeElement
    private String modelName;

    @Column
    @DataURI(position = 2)
    @XmlElement
    @DynamicSerializeElement
    private String metafileName;

    @Column
    @DataURI(position = 3)
    @XmlElement
    @DynamicSerializeElement
    private String productName;

    @Column
    @XmlElement
    @DynamicSerializeElement
    private String validTimeString;

    @Column
    @XmlElement
    @DynamicSerializeElement
    private String reportType;

    @Transient
    private byte[] imageData;

    @Column
    @XmlElement
    @DynamicSerializeElement
    private int imageSizeX;

    @Column
    @XmlElement
    @DynamicSerializeElement
    private int imageSizeY;

    @Column
    @XmlElement
    @DynamicSerializeElement
    private int imageByteCount;

    @Column
    @XmlElement
    @DynamicSerializeElement
    private int frameNumberInFile;

    @Column
    @XmlElement
    @DynamicSerializeElement
    private int totalFramesInFile;

    /**
     * Default Constructor
     */
    public NtransRecord() {
    }

    /**
     * Constructs an NTRANS record from a dataURI
     * 
     * @param uri
     *            The dataURI
     */
    public NtransRecord(String uri) {
        super(uri);
    }

    public String getModelName() {
        return modelName;
    }

    public void setModelName(String model) {
        this.modelName = model;
    }

    public String getMetafileName() {
        return metafileName;
    }

    public void setMetafileName(String inputMetaFileName) {
        this.metafileName = inputMetaFileName;
    }

    public String getValidTimeString() {
        return validTimeString;
    }

    public void setValidTimeString(String validTimeString) {
        this.validTimeString = validTimeString;
    }

    public String getProductName() {
        return productName;
    }

    public void setProductName(String productNameString) {
        this.productName = productNameString;
    }

    public String getReportType() {
        return reportType;
    }

    public void setReportType(String reportType) {
        this.reportType = reportType;
    }

    public byte[] getImageData() {
        return imageData;
    }

    public void setImageData(byte[] imageData) {
        this.imageData = imageData;
    }

    public int getImageSizeX() {
        return imageSizeX;
    }

    public void setImageSizeX(int imageSizeX) {
        this.imageSizeX = imageSizeX;
    }

    public int getImageSizeY() {
        return imageSizeY;
    }

    public void setImageSizeY(int imageSizeY) {
        this.imageSizeY = imageSizeY;
    }

    public int getImageByteCount() {
        return imageByteCount;
    }

    public void setImageByteCount(int imageByteCount) {
        this.imageByteCount = imageByteCount;
    }

    public int getFrameNumberInFile() {
        return frameNumberInFile;
    }

    public void setFrameNumberInFile(int frameNumberInFile) {
        this.frameNumberInFile = frameNumberInFile;
    }

    public int getTotalFramesInFile() {
        return totalFramesInFile;
    }

    public void setTotalFramesInFile(int totalFramesInFile) {
        this.totalFramesInFile = totalFramesInFile;
    }

    @Override
    public IDecoderGettable getDecoderGettable() {
        // TODO Auto-generated method stub
        return null;
    }

    @Override
    public IHDFFilePathProvider getHDFPathProvider() {
        return NtransPathProvider.getInstance();
    }

    public static long getSerialVersionUID() {
        return serialVersionUID;
    }

    @Override
    @Column
    @Access(AccessType.PROPERTY)
    public String getDataURI() {
        return super.getDataURI();
    }

    @Override
    public String getPluginName() {
        return "ntrans";
    }
}
