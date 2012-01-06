/**
 * LiftedIndex
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
@Table(name="uair_liftedindex")
@XmlAccessorType(XmlAccessType.NONE)
@DynamicSerialize
public class LiftedIndex implements Serializable, ISerializableObject {

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
	private float liTemp;

	// 
	@XmlElement
	@DynamicSerializeElement
	private float loWindDir;

	// 
	// Decimal(5,2)
	@XmlElement
	@DynamicSerializeElement
	private float loWindSpeed;

	// 
	@XmlElement
	@DynamicSerializeElement
	private float hiWindDir;

	// 
	// Decimal(5,2)
	@XmlElement
	@DynamicSerializeElement
	private float hiWindSpeed;
	
	/**
	 * No-Arg Convstructor
	 */
	public LiftedIndex() {
		liTemp = IDecoderConstantsN.UAIR_FLOAT_MISSING;
		loWindDir = IDecoderConstantsN.UAIR_FLOAT_MISSING;
		loWindSpeed = IDecoderConstantsN.UAIR_FLOAT_MISSING;
		hiWindDir = IDecoderConstantsN.UAIR_FLOAT_MISSING;
		loWindSpeed = IDecoderConstantsN.UAIR_FLOAT_MISSING;
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

	public float getLoWindDir() {
		return loWindDir;
	}

	public void setLoWindDir(float loWindDir) {
		this.loWindDir = loWindDir;
	}

	public float getLoWindSpeed() {
		return loWindSpeed;
	}

	public void setLoWindSpeed(float loWindSpeed) {
		this.loWindSpeed = loWindSpeed;
	}

	public float getHiWindDir() {
		return hiWindDir;
	}

	public void setHiWindDir(float hiWindDir) {
		this.hiWindDir = hiWindDir;
	}

	public float getHiWindSpeed() {
		return hiWindSpeed;
	}

	public void setHiWindSpeed(float hiWindSpeed) {
		this.hiWindSpeed = hiWindSpeed;
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
