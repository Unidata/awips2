/**
 * NcUairMaxWind
 * 
 * This java class defines the getters and setters for the maximum wind
 * data in TTAA of an upper air sounding.
 * 
 * HISTORY
 *
 * Date         Ticket#         Engineer    Description
 * ------------ ----------      ----------- --------------------------
 * 03/2010      210				L. Lin  	Initial coding
 * 04/2011		210				T. Lee		Removed table entity for H5
 * 09/2011      457             S. Gurung   Renamed H5 to Nc and h5 to nc
 * 09/2011                   	Chin Chen   support batch decoding methods for better performance and
 * 											remove xml serialization as well
 * </pre>
 *
 * This code has been developed by the SIB for use in the AWIPS2 system. 
 * @author L. Lin
 * @version 1.0 
 */

package gov.noaa.nws.ncep.common.dataplugin.ncuair;

import java.io.Serializable;

import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlAccessType;
import com.raytheon.uf.common.serialization.ISerializableObject;
import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;
import com.raytheon.uf.common.serialization.annotations.DynamicSerializeElement;
import gov.noaa.nws.ncep.common.tools.IDecoderConstantsN;
@XmlAccessorType(XmlAccessType.NONE)
@DynamicSerialize
public class NcUairMaxWind implements  ISerializableObject {

/**
	 * 
	 */
	private static final long serialVersionUID = 3322911349013884198L;

	// Observation pressure in Pascals
	
	@DynamicSerializeElement
	private float pres;

	// Observation wind direction in angular degrees.
	
	@DynamicSerializeElement
	private float drct;

	// Observation wind speed in meters per second.
	// Decimal(5,2)
	
	@DynamicSerializeElement
	private float sped;

	// Observation wind shear low
	
	@DynamicSerializeElement
	private float loShear;

	// Observation wind shear hegith
	// Decimal(5,2)
	
	@DynamicSerializeElement
	private float hiShear;
	
	/**
	 * No-Arg Constructor
	 */
	public NcUairMaxWind() {
		sped = IDecoderConstantsN.UAIR_FLOAT_MISSING;
		drct = IDecoderConstantsN.UAIR_FLOAT_MISSING;
		pres = IDecoderConstantsN.UAIR_FLOAT_MISSING;
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
	 *
	public Integer getRecordId() {
		return recordId;
	}
*/
	/**
	 * Set the record id.
	 * @param record
	 *
	public void setRecordId(Integer recordId) {
		this.recordId = recordId;
	}
*/
	/**
     * Get the level pres in Pascals.
     * 
     * @return the pres
     */	
	public float getPres() {
		return pres;
	}

	/**
     * Set the level pressure in Pascals.
     * 
     * @param pres
     *            the pres to set
     */            
 	public void setPres(float pres) {
		this.pres = pres;
	}
	
	/**
     * Get the level wind direction in angular degrees.
     * 
     * @return the drct
     */
	public float getDrct() {
		return drct;
	}

	/**
     * Set the level wind direction in angular degrees.
     * 
     * @param drct
     *            the drct to set
     */
	public void setDrct(float drct) {
		this.drct = drct;
	}

	/**
     * Get the level wind speed in meters per second.
     * 
     * @return the sped
     */
	public float getSped() {
		return sped;
	}

	/**
     * Set the level wind speed in meters per second.
     * 
     * @param sped
     *            the sped to set
     */
	public void setSped(float sped) {
		this.sped = sped;
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

}
