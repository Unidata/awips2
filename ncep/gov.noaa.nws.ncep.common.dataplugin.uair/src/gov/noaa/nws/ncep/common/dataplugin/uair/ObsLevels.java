/**
 * ObsLevels
 * 
 * This java class defines the getters and setters for the 
 *      usir observation levels.
 * 
 * HISTORY
 *
 * Date         Ticket#         Engineer    Description
 * ------------ ----------      ----------- --------------------------
 * 03/2010      210				L. Lin  	Initial coding
 * </pre>
 *
 * This code has been developed by the SIB for use in the AWIPS2 system.
 */

package gov.noaa.nws.ncep.common.dataplugin.uair;

import java.io.Serializable;
import javax.persistence.GeneratedValue;
import javax.persistence.Id;

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
@Table(name="uair_obslevels")
@XmlAccessorType(XmlAccessType.NONE)
@DynamicSerialize
public class ObsLevels implements Serializable, ISerializableObject {

	private static final long serialVersionUID = 1L;

	@Id
	@GeneratedValue
	private Integer recordId = null;

	// The CONVSIGMET record this object belongs to 
	@ManyToOne
	@JoinColumn(name="parentID", nullable=false)
	private UairRecord parentID;

	// Observation pressure in Pascals
	@XmlElement
	@DynamicSerializeElement
	private float pressure;

	// Observation geopotential height in meters.
	@XmlElement
	@DynamicSerializeElement
	private float geoHeight;

	// Observation dry air temperature in degrees Kelvin.
	@XmlElement
	@DynamicSerializeElement
	private float temp;

	// Observation dewpoint temperature in degrees Kelvin.
	@XmlElement
	@DynamicSerializeElement
	private float dwpt;

	// Observation wind direction in angular degrees. Integer
	@XmlElement
	@DynamicSerializeElement
	private float windDirection;

	// Observation wind speed in meters per second.
	// Decimal(5,2)
	@XmlElement
	@DynamicSerializeElement
	private float windSpeed;

	/**
	 * No-Arg Convstructor
	 */
	public ObsLevels() {
		windSpeed = IDecoderConstantsN.UAIR_FLOAT_MISSING;
		windDirection = IDecoderConstantsN.INTEGER_MISSING;
		dwpt = IDecoderConstantsN.UAIR_FLOAT_MISSING;
		temp = IDecoderConstantsN.UAIR_FLOAT_MISSING;
		geoHeight = IDecoderConstantsN.INTEGER_MISSING;
		pressure = IDecoderConstantsN.UAIR_FLOAT_MISSING;
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
     * Get the level pressure in Pascals.
     * 
     * @return the pressure
     */	
	public float getPressure() {
		return pressure;
	}

	/**
     * Set the level pressure in Pascals.
     * 
     * @param pressure
     *            the pressure to set
     */
	public void setPressure(float pressure) {
		this.pressure = pressure;
	}

	/**
     * Get the level geopotential height in meters.
     * 
     * @return the geoHeight
     */
	public float getGeoHeight() {
		return geoHeight;
	}

	/**
     * Set the level geopotential height in meters.
     * 
     * @param geoHeight
     *            the geoHeight to set
     */
	public void setGeoHeight(float geoHeight) {
		this.geoHeight = geoHeight;
	}

	/**
     * Get the level dry air temperature in degrees Kelvin.
     * 
     * @return the temp
     */
	public float getTemp() {
		return temp;
	}

	 /**
     * Set the level dry air temperature in degrees Kelvin.
     * 
     * @param temp
     *            the temp to set
     */
	public void setTemp(float temp) {
		this.temp = temp;
	}

	/**
     * Get the level dewpoint temperature in degrees Kelvin.
     * 
     * @return the dwpt
     */
	public float getDwpt() {
		return dwpt;
	}

	/**
     * Set the level dewpoint temperature in degrees Kelvin.
     * 
     * @param dwpt
     *            the dwpt to set
     */
	public void setDwpt(float dwpt) {
		this.dwpt = dwpt;
	}

	/**
     * Get the level wind direction in angular degrees.
     * 
     * @return the windDirection
     */
	public float getWindDirection() {
		return windDirection;
	}

	/**
     * Set the level wind direction in angular degrees.
     * 
     * @param windDirection
     *            the windDirection to set
     */
	public void setWindDirection(float windDirection) {
		this.windDirection = windDirection;
	}

	/**
     * Get the level wind speed in meters per second.
     * 
     * @return the windSpeed
     */
	public float getWindSpeed() {
		return windSpeed;
	}

	/**
     * Set the level wind speed in meters per second.
     * 
     * @param windSpeed
     *            the windSpeed to set
     */
	public void setWindSpeed(float windSpeed) {
		this.windSpeed = windSpeed;
	}

	/**
	 * @return the parentID
	 */
	public UairRecord getParentID() {

		return parentID;
	}

	public void setParentID(UairRecord parentID) {
		this.parentID = parentID;
	}

}
