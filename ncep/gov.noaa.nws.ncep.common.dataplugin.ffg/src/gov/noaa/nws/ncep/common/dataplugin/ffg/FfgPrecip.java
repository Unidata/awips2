/*
 * FfgPrecip
 * 
 * This java class defines the getters and setters for FFG precipitation data.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#         Engineer    Description
 * ------------ ----------      ----------- --------------------------
 * 08/2008      14				T. Lee     	Initial coding
 * 12/2008		14				T. Lee		Used FLOAT_MISSING
 * 03/2009		14				T. Lee		Migration to TO10
 * 11/2009		14				T. Lee		Migration to TO11D6
 * 05/2010		14				T. Lee		Migration to TO11DR11
 * 09/2011      		        Chin Chen   changed to improve purge performance and
 * 										    removed xml serialization as well
 * </pre>
 *
 * @author T.Lee
 * @version 1.0
 */

package gov.noaa.nws.ncep.common.dataplugin.ffg;

import java.io.Serializable;
import javax.persistence.GeneratedValue;
import javax.persistence.Id;
import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.Table;
import javax.xml.bind.annotation.XmlElement;
import com.raytheon.uf.common.serialization.ISerializableObject;
import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;
import com.raytheon.uf.common.serialization.annotations.DynamicSerializeElement;
import gov.noaa.nws.ncep.common.tools.IDecoderConstantsN;

@Entity
@Table(name="ffg_precip")
@DynamicSerialize
public class FfgPrecip implements Serializable, ISerializableObject {

	private static final long serialVersionUID = 1L;

	@Id
    @GeneratedValue
    private Integer recordId = null;
	
	/** The zone ID **/
	@Column(length=32)
	@XmlElement
	@DynamicSerializeElement
	private String zoneID;

	/** Raw report */
	@Column(length=72)
	@XmlElement
	@DynamicSerializeElement  
	private String report;

	/** The precipitation guidance for FFG **/
	@Column
	@XmlElement
	@DynamicSerializeElement  
	private float ff01;
	
	@Column
	@XmlElement
	@DynamicSerializeElement  
	private float ff03;
	
	@Column
	@XmlElement
	@DynamicSerializeElement  
	private float ff06;
	
	@Column
	@XmlElement
	@DynamicSerializeElement  
	private float ff12;
	
	@Column
	@XmlElement
	@DynamicSerializeElement  
	private float ff24;

    /**
     * No-Arg Constructor
     */
	public FfgPrecip() {
		this.ff01 = IDecoderConstantsN.FLOAT_MISSING;
		this.ff03 = IDecoderConstantsN.FLOAT_MISSING;
		this.ff06 = IDecoderConstantsN.FLOAT_MISSING;
		this.ff12 = IDecoderConstantsN.FLOAT_MISSING;
		this.ff24 = IDecoderConstantsN.FLOAT_MISSING;         
    }
	
	/**
	 * @return the serialVersionUID
	 */
	public static long getSerialVersionUID() {
		return serialVersionUID;
	}

	/**
	 * @return the parentID
	 *
	public FfgRecord getParentID() {
		return parentID;
	}*/

	/**
	 * @param parentID the parentID to set
	 *
	public void setParentID(FfgRecord parentID) {
		this.parentID = parentID;
	}*/

	/**
	 * @return the zoneID
	 */
	public String getZoneID() {
		return zoneID;
	}

	/**
	 * @param zoneID the zoneID to set
	 */
	public void setZoneID(String zoneID) {
		this.zoneID = zoneID;
	}

	/**
	 * @return the ff01
	 */
	public float getFf01() {
		return ff01;
	}

	/**
	 * @param ff01 the ff01 to set
	 */
	public void setFf01(float ff01) {
		this.ff01 = ff01;
	}

	/**
	 * @return the ff03
	 */
	public float getFf03() {
		return ff03;
	}

	/**
	 * @param ff03 the ff03 to set
	 */
	public void setFf03(float ff03) {
		this.ff03 = ff03;
	}

	/**
	 * @return the ff06
	 */
	public float getFf06() {
		return ff06;
	}

	/**
	 * @param ff06 the ff06 to set
	 */
	public void setFf06(float ff06) {
		this.ff06 = ff06;
	}

	/**
	 * @return the ff12
	 */
	public float getFf12() {
		return ff12;
	}

	/**
	 * @param ff12 the ff12 to set
	 */
	public void setFf12(float ff12) {
		this.ff12 = ff12;
	}

	/**
	 * @return the ff24
	 */
	public float getFf24() {
		return ff24;
	}

	/**
	 * @param ff24 the ff24 to set
	 */
	public void setFf24(float ff24) {
		this.ff24 = ff24;
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
     *
     * @param recordId
     *            The recordId.
     */
    public void setRecordId(Integer recordId) {
        this.recordId = recordId;
    }

	
	/**
	 * @return the report
	 */
	public String getReport() {
		return report;
	}
		
	/**
	 * 
	 * @param report the report to set
	 */
	public void setReport(String report) {
		this.report = report;
	}
}
