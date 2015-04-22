/**
 * AwwVtec
 * 
 * This java class represents the P-vtec line for an Aww record.
 * 
 * HISTORY
 *
 * Date     	Author		Description
 * ------------	----------	-----------	--------------------------
 * 12/2008		L. Lin		Initial creation	
 * 04/2009      L. Lin      Convert to to10.
 * 09/2011      Chin Chen   changed to improve purge performance and
 * 							removed xml serialization as well
 * 
 * This code has been developed by the SIB for use in the AWIPS2 system.
 */

package gov.noaa.nws.ncep.common.dataplugin.aww;

import java.io.Serializable;
import java.util.Calendar;
import java.util.HashSet;
import java.util.Set;

import javax.persistence.CascadeType;
import javax.persistence.Column;
import javax.persistence.GeneratedValue;
import javax.persistence.Id;

import javax.persistence.Entity;
import javax.persistence.FetchType;
import javax.persistence.JoinColumn;
import javax.persistence.OneToMany;
import javax.persistence.Table;

import org.hibernate.annotations.Index;

import com.raytheon.uf.common.serialization.ISerializableObject;
import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;
import com.raytheon.uf.common.serialization.annotations.DynamicSerializeElement;

@Entity
@Table(name="aww_vtec")
@DynamicSerialize
public class AwwVtec implements Serializable, ISerializableObject {

	private static final long serialVersionUID = 1L;
	
	@Id
    @GeneratedValue
    private Integer recordId = null;
	
	// The AWW VTEC record this object belongs to
	//@ManyToOne
    //@JoinColumn(name="parentID", nullable=false)
	//private AwwUgc parentID;
	
	// VTEC office ID
	@Column(length=16)
    @DynamicSerializeElement
	private String officeID;
	
	// Action such as NEW, CON, CAN, EXT, ....
	@Column(length=16)
    @DynamicSerializeElement
	private String action;
	
	// VTEC event start time
	@Column
	@DynamicSerializeElement
	private Calendar eventStartTime;
	
	// VTEC event end time
	@Column
	@DynamicSerializeElement
	private Calendar eventEndTime;
	
	// VTEC event tracking number
	@Column(length=16)
    @DynamicSerializeElement
	private String eventTrackingNumber;

	// Significance such as W-warning, A-watch, Y-advisory, S-statement
	@Column(length=16)
    @DynamicSerializeElement
	private String significance;
	
	//  product class such as O-operational product, T-test product, E-experimental...
	@Column(length=16)
    @DynamicSerializeElement
	private String productClass;
	
	// phenomena - two character string PP
	@Column(length=16)
    @DynamicSerializeElement
	private String phenomena;
	
	// Text information for VTEC
	@Column(length=128)
    @DynamicSerializeElement
	private String vtecLine;
	
	/** 
	 * AWW HVTEC Table
	 */
	@DynamicSerializeElement
	@OneToMany(cascade = CascadeType.ALL, fetch = FetchType.EAGER)
	@JoinColumn(name = "parentID", nullable = false)
    @Index(name = "awwHVtecLine_parentid_idex")
	private Set<AwwHVtec> awwHVtecLine = new HashSet<AwwHVtec>();

	/**
     * No-Arg Constructor.
     */
	public AwwVtec() {
	    this.vtecLine=null;
	    this.action=null;
	    this.eventStartTime=null;
	    this.eventEndTime=null;
	    this.eventTrackingNumber=null;
	    this.significance=null;
	    this.productClass=null;
	    this.phenomena=null;
	    
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
	 * @return the vtecLine
	 */
	public String getVtecLine() {
		return vtecLine;
	}

	
	/**
	 * @param vtecLine to set
	 */
	public void setVtecLine(String vtecLine) {
		this.vtecLine = vtecLine;
	}
	  
	  /**
	    * @return the set of H-vtecline
	    */
	   public Set<AwwHVtec> getAwwHVtecLine() {
	           return awwHVtecLine;
	   }

	   /**
	    * @param awwVtecline - the set of H-vtec lines to set
	    */
	   public void setAwwHVtecLine(Set<AwwHVtec> awwHVtecline) {
	           this.awwHVtecLine = awwHVtecline;
	   }

	   /**
	    *   Add AwwHVtec to set
	    */
	   public void addAwwHVtecLine(AwwHVtec hvtec){
	           awwHVtecLine.add(hvtec);
	   }

	   /**
	    * @return the set of officeID
	    */
	public String getOfficeID() {
		return officeID;
	}

	/**
	 * @param officeID to set
	 */
	public void setOfficeID(String officeID) {
		this.officeID = officeID;
	}

	/**
	 * @return the action
	 */
	public String getAction() {
		return action;
	}

	/**
	 * @param action to set
	 */
	public void setAction(String action) {
		this.action = action;
	}

	/**
	 * @return the significance
	 */
	public String getSignificance() {
		return significance;
	}

	/**
	 * @param significance to set
	 */
	public void setSignificance(String significance) {
		this.significance = significance;
	}

	/**
	 * @return the productClass
	 */
	public String getProductClass() {
		return productClass;
	}

	/**
	 * @param productClass to set
	 */
	public void setProductClass(String productClass) {
		this.productClass = productClass;
	}

	/**
	 * @return the phenomena
	 */
	public String getPhenomena() {
		return phenomena;
	}

	/**
	 * @param phenomena to set
	 */
	public void setPhenomena(String phenomena) {
		this.phenomena = phenomena;
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

	/**
	 * @return the eventTrackingNumber
	 */
	public String getEventTrackingNumber() {
		return eventTrackingNumber;
	}

	/**
	 * @param eventTrackingNumber to set
	 */
	public void setEventTrackingNumber(String eventTrackingNumber) {
		this.eventTrackingNumber = eventTrackingNumber;
	}
	
}
