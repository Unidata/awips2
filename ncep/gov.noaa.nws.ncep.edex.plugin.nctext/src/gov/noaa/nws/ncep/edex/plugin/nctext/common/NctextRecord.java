/**
 * 
 * NctextRecord
 * 
 * This java class performs the mapping to the database tables for NTEXT
 * 
 * A raw message record looks like this
 * xxx                    (some 3 digit number)
 * TTAAii CCCC YYGGgg BBB (WMO header)
 * NNNXXX                 (AWIPS Id)
 * ABCDEFG................(Data from this line)
 * ..........
 * ... ..XYZ..     ^C     (Data end with Ctl-C)
 * 
 * <pre>
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    	Engineer    Description
 * -------		------- 	-------- 	-----------
 * 10/22/2009	191		Chin Chen	Initial coding
 * 07/23/2010   191      Archana      Added DataUri annotation to productType
 * Apr 4, 2013        1846 bkowal      Added an index on refTime and forecastTime  
 * Apr 12, 2013 1857        bgonzale    Added SequenceGenerator annotation.
 * May 07, 2013 1869        bsteffen	Remove dataURI column from PluginDataObject.
 *
 * </pre>
 * 
 * @author Chin Chen
 * @version 1.0
 */
package gov.noaa.nws.ncep.edex.plugin.nctext.common;

import java.util.Calendar;

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

import org.hibernate.annotations.Index;

import com.raytheon.uf.common.dataplugin.IDecoderGettable;
import com.raytheon.uf.common.dataplugin.PluginDataObject;
import com.raytheon.uf.common.dataplugin.annotations.DataURI;
import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;
import com.raytheon.uf.common.serialization.annotations.DynamicSerializeElement;

@Entity
@SequenceGenerator(initialValue = 1, name = PluginDataObject.ID_GEN, sequenceName = "nctextseq")
@Table(name = "nctext", uniqueConstraints = { @UniqueConstraint(columnNames = { "dataURI" }) })
/*
 * Both refTime and forecastTime are included in the refTimeIndex since
 * forecastTime is unlikely to be used.
 */
@org.hibernate.annotations.Table(
		appliesTo = "nctext",
		indexes = {
				@Index(name = "nctext_refTimeIndex", columnNames = { "refTime", "forecastTime" } )
		}
)
@XmlRootElement
@XmlAccessorType(XmlAccessType.NONE)
@DynamicSerialize

public class NctextRecord extends PluginDataObject {
    private static final long serialVersionUID = 1L;

    /** product type */
    @Column(length=8)
    @DataURI(position=6)
    @XmlElement
    @DynamicSerializeElement   
    private String productType;

    
	/** TTAAii part of wmo header */
    @Column(length=6)
    @DataURI(position=1)
    @XmlElement
    @DynamicSerializeElement   
    private String wmoId;
    
    /** CCCC part of wmo header */
    @Column(length=8)
    @DataURI(position=2)
    @XmlElement
    @DynamicSerializeElement   
    private String issueSite;
    
    /** GMT date and time of report issued, combination of ingest 
     * text file name's first 8 letters which stand for yyyyMMdd and YYGG part of wmo header
     *  which stand for HHmm */
    @Column
    @XmlElement
    @DynamicSerializeElement   
    private Calendar issueTime;

    /** optional indicator BBB part of wmo header */
    @Column(length=3)
    @DataURI(position=4)
    @XmlElement
    @DynamicSerializeElement   
    private String bbbInd;

    /** AWIPS ID, NNNXXX, 2nd line of the header */
    @Column(length=6)
    @DataURI(position=3)
    @XmlElement
    @DynamicSerializeElement   
    private String awipsId;

    /** product data - the beef we are saving for a record, including header */
    @Column(columnDefinition = "text")
    //@Column(length=100000)
    @XmlElement
    @DynamicSerializeElement   
    private String rawRecord;

    /** used to identify records in a same file with same values for 
     * all other data URI fields.
     */
    @DataURI(position=5)
    //@XmlElement
    //@DynamicSerializeElement   
    //@Transient
    private int recordId; 


    /** 
     * data portion of a record, excluding first 3 lines (headers)
     */
    @Transient
    private String reportData;


	/*.
	 *  Constructor
	 */
	public NctextRecord() {
		setPluginName("nctext");
	}

	/**
	 * @param uri - dataUri=/pluginname(nctext)/dataTime/TTAAii/CCCC/AWIPSid/producttype
	 */
	public NctextRecord(String uri) {
		super(uri);
	}

	/* (non-Javadoc)
	 * @see com.raytheon.uf.common.dataplugin.PluginDataObject#getDecoderGettable()
	 */
	@Override
	public IDecoderGettable getDecoderGettable() {
		// TODO Auto-generated method stub
		return null;
	}

	public String getProductType() {
		return productType;
	}

	public void setProductType(String productType) {
		this.productType = productType;
	}
    public String getWmoId() {
		return wmoId;
	}

	public void setWmoId(String wmoId) {
		this.wmoId = wmoId;
	}

	public String getIssueSite() {
		return issueSite;
	}

	public void setIssueSite(String issueSite) {
		this.issueSite = issueSite;
	}

	public Calendar getIssueTime() {
		return issueTime;
	}

	public void setIssueTime(Calendar issueTime) {
		this.issueTime = issueTime;
	}


	public String getAwipsId() {
		return awipsId;
	}

	public void setAwipsId(String awipsid) {
		awipsId = awipsid;
	}

	public String getRawRecord() {
		return rawRecord;
	}

	public void setRawRecord(String rawRecord) {
		this.rawRecord = rawRecord;
	}
	public String getReportData() {
		return reportData;
	}

	public void setReportData(String reportData) {
		this.reportData = reportData;
	}

	public String getBbbInd() {
		return bbbInd;
	}

	public void setBbbInd(String bbbInd) {
		this.bbbInd = bbbInd;
	}

	public void setRecordId(int recordId) {
		this.recordId = recordId;
	}

	public int getRecordId() {
		return recordId;
	}
	public void printData(){
		System.out.print("\n" + rawRecord + "\n");
	}

    @Override
    @Column
    @Access(AccessType.PROPERTY)
    public String getDataURI() {
        return super.getDataURI();
    }
}
