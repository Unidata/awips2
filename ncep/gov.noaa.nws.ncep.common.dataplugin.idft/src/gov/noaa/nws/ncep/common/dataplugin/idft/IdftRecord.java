/**
 * 
 * IdftRecord.java
 * 
 * This java class performs the mapping to the database tables for the Ice Drift
 * (IDFT) Decoder Plug-In.
 *
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date			Ticket#		Engineer		Description
 * ------------	----------- --------------	-----------------------------------
 * 05/21/09		   100		F. J. Yen		Initial creation
 * 12/08/09		   100		F. J. Yen		Modified for to11d6 from to11d3
 * 05/27/10		   100		F. J. Yen		Refactored from to11dr3 for tolldr11
 * Apr 4, 2013        1846 bkowal      Added an index on refTime and forecastTime
 * Apr 12, 2013    1857     bgonzale        Added SequenceGenerator annotation.
 * May 07, 2013	1869        bsteffen      	Remove dataURI column from         
 *                                          PluginDataObject.
 * 
 * *
 * This code has been developed by the SIB for use in the AWIPS2 system.
 * </pre>
 * 
 * @author F. J. Yen, SIB
 * @version 1
 */

package gov.noaa.nws.ncep.common.dataplugin.idft;


import gov.noaa.nws.ncep.common.tools.IDecoderConstantsN;

import java.util.Calendar;

import javax.persistence.Access;
import javax.persistence.AccessType;
import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.SequenceGenerator;
import javax.persistence.Table;
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
@SequenceGenerator(initialValue = 1, name = PluginDataObject.ID_GEN, sequenceName = "idftseq")
@Table(name = "idft", uniqueConstraints = { @UniqueConstraint(columnNames = { "dataURI" }) })
/*
 * Both refTime and forecastTime are included in the refTimeIndex since
 * forecastTime is unlikely to be used.
 */
@org.hibernate.annotations.Table(
		appliesTo = "idft",
		indexes = {
				@Index(name = "idft_refTimeIndex", columnNames = { "refTime", "forecastTime" } )
		}
)
@XmlRootElement
@XmlAccessorType(XmlAccessType.NONE)
@DynamicSerialize

public class IdftRecord extends PluginDataObject{
	
	private static final long serialVersionUID = 1L;

	/** Report type */
	@Column(length=32)
	@XmlElement
    @DataURI(position = 4)
	@DynamicSerializeElement
	private String reportType;
	
	@Column
    @DataURI(position = 1)
    @DynamicSerializeElement
	@XmlElement
	private Calendar issueTime;

	@Column
    @DataURI(position = 2)
    @DynamicSerializeElement
	@XmlElement
	private Calendar validTime;

    @XmlElement
    @DataURI(position = 3)
    @DynamicSerializeElement
	private Integer pointNum;

    @XmlElement
    @DynamicSerializeElement
	private Float lat;

    @XmlElement
    @DynamicSerializeElement
	private Float lon;

    @XmlElement
    @DynamicSerializeElement
	private Float direction;

    @XmlElement
    @DynamicSerializeElement
	private Float distanceNm;


	/**
     * Default Constructor
     */
    public IdftRecord() {
    	this.issueTime = null;
    	this.validTime = null;
    	this.pointNum = IDecoderConstantsN.INTEGER_MISSING;
    	this.lat = IDecoderConstantsN.FLOAT_MISSING;
    	this.lon = IDecoderConstantsN.FLOAT_MISSING;
    	this.direction = IDecoderConstantsN.FLOAT_MISSING;
    	this.distanceNm = IDecoderConstantsN.FLOAT_MISSING;
    }

    /**
     * Constructs an idft record from a dataURI
     * 
     * @param uri
     *            The dataURI
     */
    public IdftRecord(String uri) {
        super(uri);
    }

    @Override
    public IDecoderGettable getDecoderGettable() {
        // TODO Auto-generated method stub
        return null;
    }
    
    public String getReportType() {
    	return reportType;
    }
    public void setReportType(String reportType) {
    	this.reportType = reportType;
    }
    
	public Calendar getIssueTime(){
		return issueTime;
	}
	public void setIssueTime(Calendar issueTime){
		this.issueTime=issueTime;
	}
	
	public Calendar getValidTime(){
		return validTime;
	}
	public void setValidTime(Calendar validTime){
		this.validTime=validTime;
	}
	
	public int getPointNum(){
		return pointNum;
	}
	public void setPointNum(Integer pointNum){
		this.pointNum=pointNum;
	}
	
	public float getLat(){
		return lat;
	}
	public void setLat(float latitude){
		this.lat=latitude;
	}
	
	public float getLon(){
		return lon;
	}
	public void setLon(float longitude){
		this.lon=longitude;
	}
	
	public float getDirection(){
		return direction;
	}
	public void setDirection(float direction){
		this.direction=direction;
	}
	
	public float getDistanceNm(){
		return distanceNm;
	}
	public void setDistanceNm(float distanceNm){
		this.distanceNm=distanceNm;
	}

    @Override
    @Column
    @Access(AccessType.PROPERTY)
    public String getDataURI() {
        return super.getDataURI();
    }

}
