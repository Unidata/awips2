/**
 * NcUairLiftedIndex
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

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;

import com.raytheon.uf.common.serialization.ISerializableObject;
import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;
import com.raytheon.uf.common.serialization.annotations.DynamicSerializeElement;
import gov.noaa.nws.ncep.common.tools.IDecoderConstantsN;

@XmlAccessorType(XmlAccessType.NONE)
@DynamicSerialize
public class NcUairLiftedIndex implements ISerializableObject {

//	private static final long serialVersionUID = 1L;

	// Observation pressure in Pascals
	
	/**
	 * 
	 */
	private static final long serialVersionUID = 5454992542008085989L;

	@DynamicSerializeElement
	private float liTemp;

	// 
	
	@DynamicSerializeElement
	private float loDrct;

	// 
	// Decimal(5,2)
	
	@DynamicSerializeElement
	private float loSped;

	// 
	
	@DynamicSerializeElement
	private float hiDrct;

	// 
	// Decimal(5,2)
	
	@DynamicSerializeElement
	private float hiSped;
	
	/**
	 * No-Arg Convstructor
	 */
	public NcUairLiftedIndex() {
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

}
