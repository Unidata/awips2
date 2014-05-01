/**
 * ConvsigmetSection
 * 
 * This java class defines the getters and setters for the 
 *      convective sigmet section table.
 * 
 * HISTORY
 *
 * Date         Ticket#         Engineer    Description
 * ------------ ----------      ----------- --------------------------
 * 03/2009      87/114			L. Lin     	Initial coding
 * 07/2009		87/114		    L. Lin		Migration to TO11
 * 09/2011      				Chin Chen   changed to improve purge performance and
 * 											removed xml serialization as well
 * </pre>
 *
 * This code has been developed by the SIB for use in the AWIPS2 system.
 */

package gov.noaa.nws.ncep.common.dataplugin.convsigmet;

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
import gov.noaa.nws.ncep.common.tools.IDecoderConstantsN;

@Entity
@Table(name="convsigmet_section")
@DynamicSerialize
public class ConvSigmetSection implements Serializable, ISerializableObject {

	private static final long serialVersionUID = 1L;
	
    @Id
    @GeneratedValue
    private Integer recordId = null;

		
	// Class type such as: LINE, AREA, ISOL, CS, and OUTLOOK
    @Column(length=8)
    @DynamicSerializeElement
	private String classType;
	
	// Sequence ID of a convective sigmet report
    @Column(length=16)
    @DynamicSerializeElement
	private String sequenceID;
    
    // Start time of the report
	@Column
	@DynamicSerializeElement
	private Calendar startTime;
	
	// End time of the report
	@Column
	@DynamicSerializeElement
	private Calendar endTime;
	
	/*
	 *  Intensity is DMSHG (weakening), DSIPTG (ending)
	 *  INTSFYG (strengthening), DVLPG (beginning/growing)
	 */
	@Column(length=32)
    @DynamicSerializeElement
	private String intensity;

	// To where flight level from tops
	@Column
	@DynamicSerializeElement
	private int flightLevel;

	// TOPS TO or TOPS ABV for flight level
	@Column(length=16)
    @DynamicSerializeElement
	private String cloudTop;

	// Direction of weather report heads to
	@Column
	@DynamicSerializeElement
	private int direction;

	// wind speed in knots
	@Column
	@DynamicSerializeElement
	private int speed;

	// Distance is the Distance or Area diameter of the area or line
	@Column
	@DynamicSerializeElement
	private int distance;

	// The information of a complete section
	@Column(length=720)
    @DynamicSerializeElement
	private String segment;
	
	
	/** 
	 * Convsigmet location
	 */
	@DynamicSerializeElement
	@OneToMany(cascade = CascadeType.ALL, fetch = FetchType.EAGER)
	@JoinColumn(name = "parentID", nullable = false)
    @Index(name = "convSigmetLocation_parentid_idex")
	private Set<ConvSigmetLocation> convSigmetLocation = new HashSet<ConvSigmetLocation>();


	/**
     * No-Arg Convstructor
     */
    public ConvSigmetSection() {
    	this.intensity = null;
    	this.cloudTop=null;
    	this.segment=null;
    	this.classType=null;
    	this.sequenceID=null;
    	
    	this.direction=IDecoderConstantsN.INTEGER_MISSING;
    	this.speed=IDecoderConstantsN.INTEGER_MISSING;
    	this.distance=IDecoderConstantsN.INTEGER_MISSING;
    	this.flightLevel=IDecoderConstantsN.INTEGER_MISSING;
    }

    /**
	 * @return the serialVersionUID
	 */
	public static long getSerialVersionUID() {
		return serialVersionUID;
	}

	/**
	 * @return the sequenceID
	 */
	public String getSequenceID() {
		return sequenceID;
		
	}

	/**
	 * @return the sequenceID
	 */
	public void setSequenceID(String sequenceID) {
		this.sequenceID = sequenceID;
	}

	/**
	 * @return the intensity
	 */
	public String getIntensity() {
		return intensity;
	}

	/**
	 * @param intensity to set
	 */
	public void setIntensity(String intensity) {
		this.intensity = intensity;
	}

	/**
	 * @return the direction
	 */
	public int getDirection() {
		return direction;
	}

	/**
	 * @param direction to set
	 */
	public void setDirection(int direction) {
		this.direction = direction;
	}

	/**
	 * @return the speed
	 */
	public int getSpeed() {
		return speed;
	}

	/**
	 * @param speed to set
	 */
	public void setSpeed(int speed) {
		this.speed = speed;
	}

	/**
	 * @return the distance
	 */
	public int getDistance() {
		return distance;
	}

	/**
	 * @param distance to set
	 */
	public void setDistance(int distance) {
		this.distance = distance;
	}
	
	/**
	 * @return the set of convective sigmet location
	 */
	   public Set<ConvSigmetLocation> getConvSigmetLocation() {
	           return convSigmetLocation;
	   }

	   /**
	    * @param convsigmet the location to set
	    */
	   public void setConvSigmetLocation(Set<ConvSigmetLocation> convLocation) {
	           this.convSigmetLocation = convLocation;
	   }

	   /**
	    * @param add conv Sigmet location to set
	    */
	   public void addConvSigmetLocation(ConvSigmetLocation plocation){
	           convSigmetLocation.add(plocation);
	   }

	   

		/**
		 * @return the classType
		 */
		public String getClassType() {
			return classType;
		}

		/**
		 * @param classType to set
		 */
		public void setClassType(String classType) {
			this.classType = classType;
		}

		/**
		 * @return the section
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
		 * @return the startTime
		 */
		public Calendar getStartTime() {
			return startTime;
		}

		/**
		 * @param startTime to set
		 */
		public void setStartTime(Calendar startTime) {
			this.startTime = startTime;
		}

		/**
		 * @return the endTime
		 */
		public Calendar getEndTime() {
			return endTime;
		}

		/**
		 * @param endTIme to set
		 */
		public void setEndTime(Calendar endTime) {
			this.endTime = endTime;
		}

		/**
		 * @return the cloudTop
		 */
		public String getCloudTop() {
			return cloudTop;
		}

		/**
		 * @param cloudTop to set
		 */
		public void setCloudTop(String cloudTop) {
			this.cloudTop = cloudTop;
		}

		/**
		 * @return the flightLevel
		 */
		public int getFlightLevel() {
			return flightLevel;
		}

		/**
		 * @param flightLevel to set
		 */
		public void setFlightLevel(int flightLevel) {
			this.flightLevel = flightLevel;
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

}
