/**
 * AwwFips
 * 
 * This java class represents the county FIPS for an AWW record.
 * 
 * HISTORY
 *
 * Date     	Author		Description
 * ------------	----------	-----------	--------------------------
 * 12/2008		L. Lin		Initial creation	
 * 09/2011      Chin Chen   changed to improve purge performance and
 * 							removed xml serialization as well
 * 
 * This code has been developed by the SIB for use in the AWIPS2 system.
 */

package gov.noaa.nws.ncep.common.dataplugin.aww;

import java.io.Serializable;

import javax.persistence.GeneratedValue;
import javax.persistence.Id;
import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.Table;
import com.raytheon.uf.common.serialization.ISerializableObject;
import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;
import com.raytheon.uf.common.serialization.annotations.DynamicSerializeElement;


@Entity
@Table(name="aww_fips")
@DynamicSerialize
public class AwwFips implements Serializable, ISerializableObject {

	private static final long serialVersionUID = 1L;

	@Id
    @GeneratedValue
    private Integer recordId = null;
	
	// The AWW record this object belongs to.
	//@ManyToOne
    //@JoinColumn(name="parentID", nullable=false)
	//private AwwUgc parentID;

	// The county FIPS 
	@Column(length=16)
    @DynamicSerializeElement
	private String fips;

	// The universal geographic code
	@Column(length=640)
    @DynamicSerializeElement
	private String ugc;
	
        
	/**
     * No-Arg Constructor.
     */
	public AwwFips() {
            this.fips=null;
            this.ugc=null;
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
	 * @return the ugc
	 */
	public String getUgc() {
		return ugc;
	}

	/**
	 * @param ugc to set
	 */
	public void setUgc(String ugc) {
		this.ugc = ugc;
	}

	/**
	 * Set FIPS record.
	 */
	public static AwwFips setFIPS(String countyFips) {
		
		// New AwwFips record to hold county FIPS
		AwwFips currentFips = new AwwFips();
   	    currentFips.setFips(countyFips);
		 
	    return currentFips;
	     	     
	}
	
}
