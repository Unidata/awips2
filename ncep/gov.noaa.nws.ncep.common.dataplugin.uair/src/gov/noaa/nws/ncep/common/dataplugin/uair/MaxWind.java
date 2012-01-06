/**
 * MaxWind
 * 
 * This java class defines the getters and setters for the 
 *      usir maximum wind data.
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
@Table(name="uair_maxwind")
@XmlAccessorType(XmlAccessType.NONE)
@DynamicSerialize
public class MaxWind implements Serializable, ISerializableObject {

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

	// Observation wind direction in angular degrees. Integer
	@XmlElement
	@DynamicSerializeElement
	private float windDirection;

	// Observation wind speed in meters per second.
	// Decimal(5,2)
	@XmlElement
	@DynamicSerializeElement
	private float windSpeed;

	// Observation wind shear low
	@XmlElement
	@DynamicSerializeElement
	private float loShear;

	// Observation wind shear hight
	// Decimal(5,2)
	@XmlElement
	@DynamicSerializeElement
	private float hiShear;
	
	/**
	 * No-Arg Convstructor
	 */
	public MaxWind() {
		windSpeed = IDecoderConstantsN.UAIR_FLOAT_MISSING;
		windDirection = IDecoderConstantsN.INTEGER_MISSING;
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

	public float getLoShear() {
		return loShear;
	}

	public void setLoShear(float loShear) {
		this.loShear = loShear;
	}

	public float getHiShear() {
		return hiShear;
	}

	public void setHiShear(float hiShear) {
		this.hiShear = hiShear;
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
