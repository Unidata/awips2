/**
 * AwwUgc
 * 
 * This java class represents the UGC for an Aww record.
 * 
 * HISTORY
 *
 * Date     	Author		Description
 * ------------	----------	-----------	--------------------------
 * 12/2008		L. Lin		Initial creation	
 * 04/2009      L. Lin      Convert to to10
 * 09/2011      Chin Chen   changed to improve purge performance and
 * 							removed xml serialization as well
 * 
 * This code has been developed by the SIB for use in the AWIPS2 system.
 */

package gov.noaa.nws.ncep.common.dataplugin.aww;

import java.io.Serializable;
import java.util.Calendar;

import gov.noaa.nws.ncep.common.dataplugin.aww.AwwVtec;
import gov.noaa.nws.ncep.common.dataplugin.aww.AwwFips;
import gov.noaa.nws.ncep.common.dataplugin.aww.AwwLatlons;

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
@Table(name="aww_ugc")
@DynamicSerialize
public class AwwUgc implements Serializable, ISerializableObject {
	
	private static final long serialVersionUID = 1L;
	
	@Id
    @GeneratedValue
    private Integer recordId = null;
	
	// The AWW record this object belongs to
	//@ManyToOne
    //@JoinColumn(name="parentID", nullable=false)
	//private AwwRecord parentID;

	// The universal geographic code
	@Column(length=640)
    @DynamicSerializeElement
	private String ugc;
	
	// The product purge time
	@Column
	@DynamicSerializeElement
	private Calendar prodPurgeTime;
	
	// A collection of VTEC event tracking numbers under this UGC
	@Column(length=40)
    @DynamicSerializeElement
	private String eventTrackingNumber;
	
	// Text information for this segment
	@Column(length=12000)
    @DynamicSerializeElement
	private String segment;

	/** 
	 * AWW VTEC Table
	 */
	@DynamicSerializeElement
	@OneToMany(cascade = CascadeType.ALL, fetch = FetchType.EAGER)
	@JoinColumn(name = "parentID", nullable = false)
    @Index(name = "awwVtecLine_parentid_idex")
	private Set<AwwVtec> awwVtecLine = new HashSet<AwwVtec>();
	
	/** 
	 * Aww FIPS Table
	 */
	@DynamicSerializeElement
	@OneToMany(cascade = CascadeType.ALL, fetch = FetchType.EAGER)
	@JoinColumn(name = "parentID", nullable = false)
    @Index(name = "awwFIPS_parentid_idex")
	private Set<AwwFips> awwFIPS = new HashSet<AwwFips>();
	
	/** 
	 * Aww LatLon Table
	 */
	@DynamicSerializeElement
	@OneToMany(cascade = CascadeType.ALL, fetch = FetchType.EAGER)
	@JoinColumn(name = "parentID", nullable = false)
    @Index(name = "awwLatLon_parentid_idex")
	private Set<AwwLatlons> awwLatLon = new HashSet<AwwLatlons>();

	/**
	 * No-Arg Constructor.
	 */
	public AwwUgc() {
		   this.ugc=null;
		   this.prodPurgeTime=null;
		   this.segment=null;
	       //System.out.println("IN UGC constructor.....");
	       //Thread.dumpStack();
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
	 * @return the serialVersionUID
	 */
	public static long getSerialVersionUID() {
		return serialVersionUID;
	}

	
	
	/**
	 * @return the ugc
	 */
	public String getUgc() {
		return ugc;
	}

	/**
	 * @param ugc to set on the UGC record and children tables.~lukelin/bin/stopcamel

	 */
	public void setUgc(String ugc) {
		this.ugc = ugc;
	}
	
	   /**
		 * @return the set of vtecline
		 */
	   public Set<AwwVtec> getAwwVtecLine() {
	           return awwVtecLine;
	   }

	   /**
		 * @param awwVtecline - the set of vtec lines to set
		 */
	   public void setAwwVtecLine(Set<AwwVtec> awwVtecline) {
	           this.awwVtecLine = awwVtecline;
	   }

	   /**
		 * Add AwwVtecline to set
		 */
	   public void addAwwVtecLine(AwwVtec pvtec){
	           awwVtecLine.add(pvtec);
	   }
	   
	   /**
		 * @return the set of awwFIPS
		 */
	   public Set<AwwFips> getAwwFIPS() {
	           return awwFIPS;
	   }

	   /**
		 * @param awwFIPS-the set of awwFIPS to set
		 */
	   public void setAwwFIPS(Set<AwwFips> awwFips) {
	           this.awwFIPS = awwFips;
	   }

	   /**
		 * Add AwwFips to set
		 */
	   public void addAwwFIPS(AwwFips pfips){
	           awwFIPS.add(pfips);
	   }
	    
	   /**
		 * @return the set of Latlons
		 */
	   public Set<AwwLatlons> getAwwLatLon() {
	           return awwLatLon;
	   }

	   /**
		 * @param awwLatlons-the set of latlons to set
		 */
	   public void setAwwLatLon(Set<AwwLatlons> awwLatlons) {
	           this.awwLatLon = awwLatlons;
	   }

	   /**
		 * Add AwwLatlons to set
		 */
	   public void addAwwLatLon(AwwLatlons platlon){
	           awwLatLon.add(platlon);
	   }


	   /**
		 * @return the prodPurgeTime
		 */
	public Calendar getProdPurgeTime() {
		return prodPurgeTime;
	}

	/**
	 * @param prodPurgeTime to set
	 */
	public void setProdPurgeTime(Calendar prodPurgeTime) {
		this.prodPurgeTime = prodPurgeTime;
	}

	/**
	 * @return the segment
	 */
	public String getSegment() {
		return segment;
	}

	/**
	 * @param segment to set
	 */
	public void setSegment(String segment) {
		this.segment = segment;
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
