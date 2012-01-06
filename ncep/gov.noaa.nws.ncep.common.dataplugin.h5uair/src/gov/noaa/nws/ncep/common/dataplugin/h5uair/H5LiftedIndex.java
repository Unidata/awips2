/**
 * H5LiftedIndex
 * 
 * This java class defines the getters and setters for the Lifted Index
 * and low-level and high-level wind shear data in TTAA of an upper air
 * sounding.
 *      
 * 
 * HISTORY
 *
 * Date         Ticket#         Engineer    Description
 * ------------ ----------      ----------- --------------------------
 * 03/2010      210				L. Lin  	Initial coding
 * 04/2011		210				T. Lee		Removed table entity for H5
 * </pre>
 *
 * This code has been developed by the SIB for use in the AWIPS2 system. 
 * @author L. Lin
 * @version 1.0
 */

package gov.noaa.nws.ncep.common.dataplugin.h5uair;

import java.io.Serializable;
import javax.persistence.GeneratedValue;
import javax.persistence.Id;
import javax.persistence.JoinColumn;
import javax.persistence.ManyToOne;
import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;
import com.raytheon.uf.common.serialization.ISerializableObject;
import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;
import com.raytheon.uf.common.serialization.annotations.DynamicSerializeElement;
import gov.noaa.nws.ncep.common.tools.IDecoderConstantsN;

@XmlAccessorType(XmlAccessType.NONE)
@DynamicSerialize
public class H5LiftedIndex implements Serializable, ISerializableObject {

	private static final long serialVersionUID = 1L;

	@Id
	@GeneratedValue
	private Integer recordId = null;

	// The CONVSIGMET record this object belongs to 
	@ManyToOne
	@JoinColumn(name="parentID", nullable=false)
	private H5UairRecord parentID;

	// Observation pressure in Pascals
	@XmlElement
	@DynamicSerializeElement
	private float liTemp;

	// 
	@XmlElement
	@DynamicSerializeElement
	private float loDrct;

	// 
	// Decimal(5,2)
	@XmlElement
	@DynamicSerializeElement
	private float loSped;

	// 
	@XmlElement
	@DynamicSerializeElement
	private float hiDrct;

	// 
	// Decimal(5,2)
	@XmlElement
	@DynamicSerializeElement
	private float hiSped;
	
	/**
	 * No-Arg Convstructor
	 */
	public H5LiftedIndex() {
		liTemp = IDecoderConstantsN.UAIR_FLOAT_MISSING;
		loDrct = IDecoderConstantsN.UAIR_FLOAT_MISSING;
		loSped = IDecoderConstantsN.UAIR_FLOAT_MISSING;
		hiDrct = IDecoderConstantsN.UAIR_FLOAT_MISSING;
		loSped = IDecoderConstantsN.UAIR_FLOAT_MISSING;
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

	public float getLiTemp() {
		return liTemp;
	}

	public void setLiTemp(float liTemp) {
		this.liTemp = liTemp;
	}

	public float getLoDrct() {
		return loDrct;
	}

	public void setLoDrct(float loDrct) {
		this.loDrct = loDrct;
	}

	public float getLoSped() {
		return loSped;
	}

	public void setLoSped(float loSped) {
		this.loSped = loSped;
	}

	public float getHiDrct() {
		return hiDrct;
	}

	public void setHiDrct(float hiDrct) {
		this.hiDrct = hiDrct;
	}

	public float getHiSped() {
		return hiSped;
	}

	public void setHiSped(float hiSped) {
		this.hiSped = hiSped;
	}

	/**
	 * @return the parentID
	 */
	public H5UairRecord getParentID() {

		return parentID;
	}

	public void setParentID(H5UairRecord parentID) {
		this.parentID = parentID;
	}

}
