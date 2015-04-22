/**
 * IntlsigmetLocation
 * 
 * This java class defines the getters and setters for the 
 *      international sigmet location table.
 * 
 * HISTORY
 *
 * Date         Ticket#         Engineer    Description
 * ------------	----------	-----------	--------------------------
 * 05/2009		113				L. Lin		Initial creation	
 * 07/2009		113				L. Lin		Migration to TO11
 * 09/2009		113			    L. Lin		modify lat/lon float to latitude/longitude double
 * 09/2011      				Chin Chen   changed to improve purge performance and
 * 											removed xml serialization as well
 * 
 * 
 * This code has been developed by the SIB for use in the AWIPS2 system.
 */

package gov.noaa.nws.ncep.common.dataplugin.intlsigmet;

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
@Table(name="intlsigmet_location")
@DynamicSerialize
public class IntlSigmetLocation implements Serializable, ISerializableObject {

	private static final long serialVersionUID = 1L;
	
	@Id
    @GeneratedValue
    private Integer recordId = null;
	
		
	// Collection of locations
    @Column(length=480)
    @DynamicSerializeElement
	private String locationLine;
	
	// Each location of an international sigmet forecast area
    @Column(length=48)
    @DynamicSerializeElement
	private String locationName;
    
    // Each latitude of an international sigmet forecast area
    @Column
    @DynamicSerializeElement
    private double latitude;

	// Each longitude of an international sigmet forecast area
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
	public IntlSigmetLocation() {
		this.locationLine=null;
		this.locationName=null;
		this.latitude=IDecoderConstantsN.FLOAT_MISSING;
		this.longitude=IDecoderConstantsN.FLOAT_MISSING;
		this.index=IDecoderConstantsN.INTEGER_MISSING;
    }
	

	/**
	 * @return the serialVersionUID
	 */
	public static long getSerialVersionUID() {
		return serialVersionUID;
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
	public String getLocationName() {
		return locationName;
	}

	/**
	 * @param location to set
	 */
	public void setLocationName(String location) {
		this.locationName = location;
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
	 * @return the latitude
	 */
	public double getLatitude() {
		return latitude;
	}

	/**
	 * @param latitude to set
	 */
	public void setLatitude(double latitude) {
		this.latitude = latitude;
	}

	/**
	 * @return the longitude
	 */
	public double getLongitude() {
		return longitude;
	}

	/**
	 * @param longitude to set
	 */
	public void setLongitude(double longitude) {
		this.longitude = longitude;
	}
	
}
