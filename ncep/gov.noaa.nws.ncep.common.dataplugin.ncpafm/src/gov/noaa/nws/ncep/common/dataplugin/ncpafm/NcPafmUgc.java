/**
 * NcPafmUgc
 * 
 * This java class represents the UGC for an Pafm record.
 * 
 * HISTORY
 *
 * Date     	Ticket #    Author		Description
 * ------------	---------	-----------	--------------------------
 * 08/21/09		  126		F. J. Yen	Initial creation based on AWW Decoder
 * 01/06/10		  126		F. J. Yen	Migrated and refactored from to11dr3 to to11dr11
 * 
 * This code has been developed by the SIB for use in the AWIPS2 system.
 */

package gov.noaa.nws.ncep.common.dataplugin.ncpafm;

import java.io.Serializable;
import java.util.Calendar;
import java.util.HashSet;
import java.util.Iterator;
import java.util.Set;

import com.raytheon.uf.common.serialization.ISerializableObject;

public class NcPafmUgc implements Serializable, ISerializableObject {
	
	private static final long serialVersionUID = 1L;
	
    private Integer recordId = null;
	
	// The PAFM record this object belongs to
	private NcPafmBulletin parentID;

	// The universal geographic code
	private String ugc;
	
	// The product purge time
	private Calendar prodPurgeTime;
	
	// Text information for this segment
	private String segment;
	
	/** 
	 * Pafm FIPS Table
	 */
	private Set<NcPafmFips> pafmFIPS = new HashSet<NcPafmFips>();
	
	/** 
	 * Pafm Parameters Table
	 */
	private Set<NcPafmParameters> pafmParms = new HashSet<NcPafmParameters>();

	/**
	 * No-Arg Constructor.
	 */
	public NcPafmUgc() {
		   this.ugc=null;
		   this.prodPurgeTime=null;
		   this.segment=null;
	       //System.out.println("IN UGC constructor.....");
	       //Thread.dumpStack();
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
	 * @return the serialVersionUID
	 */
	public static long getSerialVersionUID() {
		return serialVersionUID;
	}

	/**
	 * @return the parentID
	 */
	public NcPafmBulletin getParentID() {
	    return parentID;
	}

	/**
	 * @param parentID the parentID to set
	 */
	public void setParentID(NcPafmBulletin parentID) {
	    this.parentID = parentID;
	    
	    if (this.getPafmFIPS() !=null && this.getPafmFIPS().size() >0 )
	    {
	    	for (Iterator<NcPafmFips> iter = this.getPafmFIPS().iterator(); iter.hasNext();) {
	            NcPafmFips cond = iter.next();
	            cond.setParentID(this);
	            //cond.setUgc(ugc);
	         }
	    }
	    
	    if (this.getPafmParms() !=null && this.getPafmParms().size() >0 )
	    {
	    	for (Iterator<NcPafmParameters> iter = this.getPafmParms().iterator(); iter.hasNext();) {
	            NcPafmParameters cond = iter.next();
	            cond.setParentID(this);
	         }
	    }
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
	 * @return the set of pafmFIPS
	 */
	public Set<NcPafmFips> getPafmFIPS() {
		return pafmFIPS;
	}

	/**
	 * @param pafmFIPS-the set of pafmFIPS to set
	 */
	public void setPafmFIPS(Set<NcPafmFips> ncPafmFips) {
		this.pafmFIPS = ncPafmFips;
	}

	/**
	 * Add NcPafmFips to set
	 */
	public void addPafmFIPS(NcPafmFips pfips){
		pafmFIPS.add(pfips);
	}

	/**
	 * @return the set of Parameters
	 */
	public Set<NcPafmParameters> getPafmParms() {
		return pafmParms;
	}

	/**
	 * @param pafmParameters-the set of Parmameters to set
	 */
	public void setPafmParms(Set<NcPafmParameters> pafmParams) {
		this.pafmParms = pafmParams;
	}

	/**
	 * Add NcPafmParameters to set
	 */
	public void addPafmParms(NcPafmParameters pparms){
		pafmParms.add(pparms);
	}

	/**
	 * @return the prodPurgeTime
	 */
	public Calendar getProdPurgeTime() {
		return prodPurgeTime;
	}
	/**
	 * @param prodPurgeTime to set
	 */
	public void setProdPurgeTime(Calendar prodPurgeTime) {
		this.prodPurgeTime = prodPurgeTime;
	}

	/**
	 * @return the segment
	 */
	public String getSegment() {
		return segment;
	}
	/**
	 * @param segment to set
	 */
	public void setSegment(String segment) {
		this.segment = segment;
	}
	
}
