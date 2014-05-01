/**
 * NcPafmFips
 * 
 * This java class represents the county FIPS for a PAFM record.
 * 
 * HISTORY
 *
 * Date     	Ticket # Author		Description
 * ------------	-------- ----------	-----------	--------------------------
 * 08/05/09		   126   F. J. Yen	Initial creation based on AWW Decoder
 * 12/11/09		   126   F. J. Yen	Migrated from to11d3 to to11d6
 * 01/06/10		   126	 F. J. Yen	Migrated and refactored from to11dr3 to to11dr11
 * 09/30/11		   126   B. Hebbard PafmFips becomes new NcPafmFips.  Same data,
 * 				                    but no longer persisted to database.
 * 
 * This code has been developed by the SIB for use in the AWIPS2 system.
 */

package gov.noaa.nws.ncep.common.dataplugin.ncpafm;

import gov.noaa.nws.ncep.common.tools.IDecoderConstantsN;

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


public class NcPafmFips implements Serializable, ISerializableObject {

	private static final long serialVersionUID = 1L;

    private Integer recordId = null;
	
	// The PAFM record this object belongs to.
	private NcPafmUgc parentID;
 
	// The county FIPS 
	private String fips;
	
	// The elevation
	private Float elev;	
	      
	// The latitude
	private Float lat;	
	      
	// The longitude
	private Float lon;	
	      
	/**
     * No-Arg Constructor.
     */
	public NcPafmFips() {
            this.fips = null;
            this.elev = IDecoderConstantsN.FLOAT_MISSING;
            this.lat = IDecoderConstantsN.FLOAT_MISSING;
            this.lon = IDecoderConstantsN.FLOAT_MISSING;
//            this.ugc=null;
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
	 * @return the parentID
	 */
	public NcPafmUgc getParentID() {
	    return parentID;
	}

	/**
	 * @param parentID the parentID to set
	 */
	public void setParentID(NcPafmUgc parentID) {
	    this.parentID = parentID;
	}
	
	/**
	 * @return the fips
	 */
	public String getFips() {
		return fips;
	}

	/**
	 * @param fips to set
	 */
	public void setFips(String fips) {
		this.fips = fips;
	}
	/**
	 * @return the elev
	 */
	public Float getElev() {
		return elev;
	}
	/**
	 * @param elev to set
	 */
	public void setElev(Float elev) {
		this.elev = elev;
	}
	
	/**
	 * @return the lat
	 */
	public Float getLat() {
		return lat;
	}
	/**
	 * @param lat to set
	 */
	public void setLat(Float lat) {
		this.lat = lat;
	}
	
	/**
	 * @return the lon
	 */
	public Float getLon() {
		return lon;
	}
	/**
	 * @param lon to set
	 */
	public void setLon(Float lon) {
		this.lon = lon;
	}	
	
	/**
	 * Set FIPS record.
	 */
	public static NcPafmFips setFIPS(String countyFips) {		
		// New NcPafmFips record to hold county FIPS
		NcPafmFips currentFips = new NcPafmFips();
   	    currentFips.setFips(countyFips);
	    return currentFips;     	     
	}	
}
