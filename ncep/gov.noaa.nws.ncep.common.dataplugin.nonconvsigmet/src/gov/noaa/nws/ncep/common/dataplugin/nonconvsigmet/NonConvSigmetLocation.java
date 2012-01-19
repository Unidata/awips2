/**
 * NonConvsigmetLocation
 * 
 * This java class defines the getters and setters for the 
 *      convective sigmet location table.
 * 
 * HISTORY
 *
 * Date     	Author		Description
 * ------------	----------	-----------	--------------------------
 * 06/2009		Uma Josyula	Initial creation	
 * 
 * This code has been developed by the SIB for use in the AWIPS2 system.
 */

package gov.noaa.nws.ncep.common.dataplugin.nonconvsigmet;

import java.io.Serializable;

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



import gov.noaa.nws.ncep.common.tools.IDecoderConstantsN;

@Entity
@Table(name="nonconvsigmet_location")
@XmlAccessorType(XmlAccessType.NONE)
@DynamicSerialize
public class NonConvSigmetLocation implements Serializable, ISerializableObject {

	private static final long serialVersionUID = 1L;
	@Id
    @GeneratedValue
    private Integer recordId = null;
	
	// The nonconvsigmet record this object belongs to 
	@ManyToOne
    @JoinColumn(name="parentID", nullable=false)
	private NonConvSigmetRecord parentID;
	
	// Collection of locations
    @Column(length=120)
    @XmlElement
    @DynamicSerializeElement
	private String locationLine;
	
	// Each location of a nonconvective sigmet forecast area
    @Column(length=40)
    @XmlElement
    @DynamicSerializeElement
	private String location;
    
	// Each latitude of a nonconvective sigmet forecast area
    @Column
    @XmlElement
    @DynamicSerializeElement
	private double latitude;
    
	// Each longitude of a nonconvective sigmet forecast area
    @Column
    @XmlElement
    @DynamicSerializeElement
	private double longitude;   
	
	// Index for the order of a complete location set
    @Column
    @XmlElement
    @DynamicSerializeElement
	private Integer index;
			
        /**
         * No-Arg Convstructor.
         */
	public NonConvSigmetLocation() {
		this.locationLine="";
		this.location="";
		this.index=IDecoderConstantsN.INTEGER_MISSING;
    }

	 	/**
	 	 * @return the recordId. If not set returns null.
	 	 */
		public Integer getRecordId() {
			return recordId;
		}

		/**
		 * @param recordId to set
		 */
		public void setRecordId(Integer recordId) {
			this.recordId = recordId;
		}

		/**
		 * @return the parentID
		 */
		public NonConvSigmetRecord getParentID() {
			return parentID;
		}	
		
		/**
		 * @param parentID to set
		 */
		public void setParentID(NonConvSigmetRecord parentID) {
			this.parentID = parentID;
		}

		/**
		 * @return the locationLine
		 */
		public String getLocationLine() {
			return locationLine;
		}

		/**
		 * @param locationLine to set
		 */
		public void setLocationLine(String locationLine) {
			this.locationLine = locationLine;
		}

		/**
		 * @return the location
		 */
		public String getLocation() {
			return location;
		}

		/**
		 * @param location to set
		 */
		public void setLocation(String location) {
			this.location = location;
		}

		/**
		 * @return the index
		 */
		public Integer getIndex() {
			return index;
		}

		/**
		 * @param index to set
		 */
		public void setIndex(Integer index) {
			this.index = index;
		}

		public double getLatitude() {
			return latitude;
		}

		public void setLatitude(double latitude) {
			this.latitude = latitude;
		}

		public double getLongitude() {
			return longitude;
		}

		public void setLongitude(double longitude) {
			this.longitude = longitude;
		}

		/**
		 * @return the serialVersionUID
		 */
		public static long getSerialVersionUID() {
			return serialVersionUID;
		}
}