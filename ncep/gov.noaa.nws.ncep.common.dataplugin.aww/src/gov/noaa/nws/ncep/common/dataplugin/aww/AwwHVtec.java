/**
 * AwwHVtec
 * 
 * This java class represents the HVTEC table for an AWW record.
 * 
 * HISTORY
 *
 * Date     	Author		Description
 * ------------	----------	-----------	--------------------------
 * 12/2008		L. Lin		Initial creation	
 * 04/2009      L. Lin      Convert to to10
 * 
 * This code has been developed by the SIB for use in the AWIPS2 system.
 */


package gov.noaa.nws.ncep.common.dataplugin.aww;

import java.io.Serializable;
import java.util.Calendar;

import javax.persistence.GeneratedValue;
import javax.persistence.Id;
import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.JoinColumn;
import javax.persistence.ManyToOne;
import javax.persistence.Table;
import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;
import com.raytheon.uf.common.serialization.ISerializableObject;
import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;
import com.raytheon.uf.common.serialization.annotations.DynamicSerializeElement;


@Entity
@Table(name="aww_hvtec")
@XmlAccessorType(XmlAccessType.NONE)
@DynamicSerialize
public class AwwHVtec implements Serializable, ISerializableObject {

	private static final long serialVersionUID = 1L;
	
	@Id
    @GeneratedValue
    private Integer recordId = null;
	
	// The AWW HVTEC record this object belongs to 
	@ManyToOne
    @JoinColumn(name="parentID", nullable=false)
	private AwwVtec parentID;

	// NWS location Identifier
	@Column(length=16)
    @XmlElement
    @DynamicSerializeElement
	private String locationIdentifier;
	
	//  Flood severity such as 0, N, 1, 2, 3, U
	@Column(length=16)
    @XmlElement
    @DynamicSerializeElement
	private String floodSeverity;
	
	// Immediate Cause such as ER, SM, ...etc
	@Column(length=16)
    @XmlElement
    @DynamicSerializeElement
	private String immediateCause;
	
	// Flood record such as NO, NR, UU, OO 
	@Column(length=32)
    @XmlElement
    @DynamicSerializeElement
	private String floodRecord;
	
	// HVTEC event start time
	@Column
	@XmlElement
	@DynamicSerializeElement
	private Calendar eventStartTime;
	
	// HVTEC event crest time
	@Column
	@XmlElement
	@DynamicSerializeElement
	private Calendar eventCrestTime;
	
	// HVTEC event end time
	@Column
	@XmlElement
	@DynamicSerializeElement
	private Calendar eventEndTime;

	// Text information for HVTEC line
	@Column(length=128)
    @XmlElement
    @DynamicSerializeElement
	private String hvtecLine;


	/**
     * No-Arg Constructor.
     */
	public AwwHVtec() {
	    this.hvtecLine=null;
	    this.eventCrestTime=null;
	    this.eventEndTime=null;
	    this.eventStartTime=null;
	    this.floodRecord=null;
	    this.floodSeverity=null;
	    this.immediateCause=null;
	    this.locationIdentifier=null;
    }
	
	
	/**
	 * @return the serialVersionUID
	 */
	public static long getSerialVersionUID() {
		return serialVersionUID;
	}

	/**
     * Get the record id.
     *
     * @return The recordId. If not set returns null.
     */
	 public Integer getRecordId() {
		return recordId;
	}

	 /**
	  * Set the record id.
	  * @param record
	  */
	public void setRecordId(Integer recordId) {
		this.recordId = recordId;
	}
	
	/**
	 * @return the parentID
	 */
	public AwwVtec getParentID() {
		return parentID;
	}

	/**
	 * @param parentID the parentID to set
	 */
	public void setParentID(AwwVtec parentID) {
	    this.parentID = parentID;
	}

	/**
	 * @return the floodSeverity
	 */
	public String getFloodSeverity() {
		return floodSeverity;
	}

	/**
	 * @param floodSeverity to set
	 */
	public void setFloodSeverity(String floodSeverity) {
		this.floodSeverity = floodSeverity;
	}

	/**
	 * @return the immediateCause
	 */
	public String getImmediateCause() {
		return immediateCause;
	}

	/**
	 * @param immediateCause to set
	 */
	public void setImmediateCause(String immediateCause) {
		this.immediateCause = immediateCause;
	}

	/**
	 * @return the floodRecord
	 */
	public String getFloodRecord() {
		return floodRecord;
	}

	/**
	 * @param floodRecord to set
	 */
	public void setFloodRecord(String floodRecord) {
		this.floodRecord = floodRecord;
	}

	/**
	 * @return the locationIdentifier
	 */
	public String getLocationIdentifier() {
		return locationIdentifier;
	}

	/**
	 * @param locationIdentifier to set
	 */
	public void setLocationIdentifier(String locationIdentifier) {
		this.locationIdentifier = locationIdentifier;
	}

	/**
	 * @return the hvtecline
	 */
	public String getHvtecLine() {
		return hvtecLine;
	}

	/**
	 * @param hvtecLine to set
	 */
	public void setHvtecLine(String hvtecLine) {
		this.hvtecLine = hvtecLine;
	}

	/**
	 * @return the eventStartTime
	 */
	public Calendar getEventStartTime() {
		return eventStartTime;
	}

	/**
	 * @param eventStartTime to set
	 */
	public void setEventStartTime(Calendar eventStartTime) {
		this.eventStartTime = eventStartTime;
	}

	/**
	 * @return the eventCrestTime
	 */
	public Calendar getEventCrestTime() {
		return eventCrestTime;
	}

	/**
	 * @param eventCrestTime to set
	 */
	public void setEventCrestTime(Calendar eventCrestTime) {
		this.eventCrestTime = eventCrestTime;
	}

	/**
	 * @return the eventEndTime
	 */
	public Calendar getEventEndTime() {
		return eventEndTime;
	}

	/**
	 * @param eventEndTime to set
	 */
	public void setEventEndTime(Calendar eventEndTime) {
		this.eventEndTime = eventEndTime;
	}

}
