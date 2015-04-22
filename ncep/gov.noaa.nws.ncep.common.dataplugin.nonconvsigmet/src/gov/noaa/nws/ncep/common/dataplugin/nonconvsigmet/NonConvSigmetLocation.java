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
 * 09/2011      Chin Chen   changed to improve purge performance and
 * 							removed xml serialization as well
 * 
 * This code has been developed by the SIB for use in the AWIPS2 system.
 */

package gov.noaa.nws.ncep.common.dataplugin.nonconvsigmet;

import java.io.Serializable;

import javax.persistence.GeneratedValue;
import javax.persistence.Id;
import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.Table;
import com.raytheon.uf.common.serialization.ISerializableObject;
import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;
import com.raytheon.uf.common.serialization.annotations.DynamicSerializeElement;



import gov.noaa.nws.ncep.common.tools.IDecoderConstantsN;

@Entity
@Table(name="nonconvsigmet_location")
@DynamicSerialize
public class NonConvSigmetLocation implements Serializable, ISerializableObject {

	private static final long serialVersionUID = 1L;
	@Id
    @GeneratedValue
    private Integer recordId = null;
	
	
	
	// Collection of locations
    @Column(length=120)
    @DynamicSerializeElement
	private String locationLine;
	
	// Each location of a nonconvective sigmet forecast area
    @Column(length=40)
    @DynamicSerializeElement
	private String location;
    
	// Each latitude of a nonconvective sigmet forecast area
    @Column
    @DynamicSerializeElement
	private double latitude;
    
	// Each longitude of a nonconvective sigmet forecast area
    @Column
    @DynamicSerializeElement
	private double longitude;   
	
	// Index for the order of a complete location set
    @Column
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