/**
 * NtransRecord
 * 
 * This java class performs the mapping to the database table for NTRANS Metafiles
 * 
 * <pre>
 * HISTORY
 *
 * Date     	Author		Description
 * ------------	----------	-----------	--------------------------
 * 02/2013		B. Hebbard	Initial creation
 * Apr 4, 2013        1846 bkowal      Added an index on refTime and forecastTime	
 * Apr 12, 2013       1857 bgonzale    Added SequenceGenerator annotation.
 * May 07, 2013 bsteffen    Remove dataURI column from PluginDataObject.
 * 
 * </pre>
 * This code has been developed by the SIB for use in the AWIPS2 system.
 */

package gov.noaa.nws.ncep.common.dataplugin.ntrans;

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

import org.hibernate.annotations.Cache;
import org.hibernate.annotations.CacheConcurrencyStrategy;
import org.hibernate.annotations.Index;

import com.raytheon.uf.common.dataplugin.IDecoderGettable;
import com.raytheon.uf.common.dataplugin.PluginDataObject;
import com.raytheon.uf.common.dataplugin.annotations.DataURI;
import com.raytheon.uf.common.dataplugin.persist.PersistablePluginDataObject;
import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;
import com.raytheon.uf.common.serialization.annotations.DynamicSerializeElement;

@Entity
@SequenceGenerator(initialValue = 1, name = PluginDataObject.ID_GEN, sequenceName = "ntransseq")
@Table(name = "ntrans", uniqueConstraints = { @UniqueConstraint(columnNames = { "dataURI" }) })
/*
 * Both refTime and forecastTime are included in the refTimeIndex since
 * forecastTime is unlikely to be used.
 */
@org.hibernate.annotations.Table(
		appliesTo = "ntrans",
		indexes = {
				@Index(name = "ntrans_refTimeIndex", columnNames = { "refTime", "forecastTime" } )
		}
)
@Cache(usage = CacheConcurrencyStrategy.TRANSACTIONAL)
@XmlRootElement
@XmlAccessorType(XmlAccessType.NONE)
@DynamicSerialize
public class NtransRecord extends PersistablePluginDataObject {

	private static final long serialVersionUID = 1L;

	@Column
	@DataURI(position=1)
	@XmlElement
	@DynamicSerializeElement
	private String validTimeString;

	@Column
	@DataURI(position=2)
	@XmlElement
	@DynamicSerializeElement
	private String productNameString;

	@Column
	@DataURI(position=3)
	@XmlElement
	@DynamicSerializeElement
	private Calendar startTime;

	@Column
	@DataURI(position=4)
	@XmlElement
	@DynamicSerializeElement
	private Calendar endTime;

	@Column
	//@DataURI(position=5)
	@XmlElement
	@DynamicSerializeElement
	private String reportType;

	@Column
	//@DataURI(position=6)
	@XmlElement
	@DynamicSerializeElement
	private String inputFile;
	
	@Transient
	private byte[] convertedMessage;

	@Column
	//@DataURI(position=7)
	@XmlElement
	@DynamicSerializeElement
	private int recordLength;
	
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

	public String getValidTimeString() {
		return validTimeString;
	}

	public void setValidTimeString(String validTimeString) {
		this.validTimeString = validTimeString;
	}

	public String getProductNameString() {
		return productNameString;
	}

	public void setProductNameString(String productNameString) {
		this.productNameString = productNameString;
	}

	public Calendar getStartTime() {
		return startTime;
	}

	public void setStartTime(Calendar startTime) {
		this.startTime = startTime;
	}

	public Calendar getEndTime() {
		return endTime;
	}

	public void setEndTime(Calendar endTime) {
		this.endTime = endTime;
	}

	public String getReportType() {
		return reportType;
	}

	public void setReportType(String reportType) {
		this.reportType = reportType;
	}

	public String getInputFile() {
		return inputFile;
	}

	public void setInputFile(String inputFile) {
		this.inputFile = inputFile;
	}

	public byte[] getConvertedMessage() {
		return convertedMessage;
	}

	public void setConvertedMessage(byte[] convertedMessage) {
		this.convertedMessage = convertedMessage;
	}

	@Override
	public IDecoderGettable getDecoderGettable() {
		// TODO Auto-generated method stub
		return null;
	}

	public static long getSerialVersionUID() {
		return serialVersionUID;
	}

	@Override
    public void setIdentifier(Object dataURI) {
		this.identifier = dataURI;
	}

    public int getRecordLength() {
		return recordLength;
	}

	public void setRecordLength(int recordLength) {
		this.recordLength = recordLength;
	}

    @Override
    @Column
    @Access(AccessType.PROPERTY)
    public String getDataURI() {
        return super.getDataURI();
    }
}
