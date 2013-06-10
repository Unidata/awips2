/**
 * This software was developed and / or modified by Raytheon Company,
 * pursuant to Contract DG133W-05-CQ-1067 with the US Government.
 * 
 * U.S. EXPORT CONTROLLED TECHNICAL DATA
 * This software product contains export-restricted data whose
 * export/transfer/disclosure is restricted by U.S. law. Dissemination
 * to non-U.S. persons whether in the United States or abroad requires
 * an export license or other authorization.
 * 
 * Contractor Name:        Raytheon Company
 * Contractor Address:     6825 Pine Street, Suite 340
 *                         Mail Stop B8
 *                         Omaha, NE 68106
 *                         402.291.0100
 * 
 * See the AWIPS II Master Rights File ("Master Rights File.pdf") for
 * further licensing information.
 **/
package com.raytheon.uf.common.dataplugin.scan.data;

import java.io.Serializable;

import com.raytheon.uf.common.serialization.ISerializableObject;
import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;
import com.raytheon.uf.common.serialization.annotations.DynamicSerializeElement;
import com.raytheon.uf.common.time.DataTime;

/**
 * 
 * SCAN Table Data, abstract for other tables
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * April 29, 2009   2037    dhladky     Initial creation
 * 02/01/13     1569        D. Hladky   removed XML where not needed
 * </pre>
 * 
 * @author dhladky
 * @version 1.0
 * 
 */

@DynamicSerialize
public abstract class ScanTableDataRow implements ISerializableObject, Serializable {
		
	/**
     * 
     */
    private static final long serialVersionUID = 1L;

    public ScanTableDataRow(DataTime time) {
		this.time = time;
	}
	
	public ScanTableDataRow() {

    }
	/**
	 * mdaSR enum
	 * @author dhladky
	 *
	 */
	public enum MDA_TYPE {
        L("Low Top/Core"), 
        S("Shallow");

        private final String mdaName;

        private MDA_TYPE(String name) {
            mdaName = name;
        }

        public String getMDAName() {
            return mdaName;
        }
    };
    
    /**
	 * TVS enum
	 * @author dhladky
	 *
	 */
	public enum TVS_TYPE {
        NONE("NONE"), 
        TVS("TVS"), 
        ETVS("ETVS");

        private final String tvsName;

        private TVS_TYPE(String name) {
            tvsName = name;
        }

        public String getTVSName() {
            return tvsName;
        }
    };
    /** time of insert **/
    @DynamicSerializeElement
    public DataTime time = null;
    /** azimuth **/
    @DynamicSerializeElement
	public Double azm = 0.0;
	/** range **/
    @DynamicSerializeElement
	public Double rng = 0.0;
	/** speed **/
    @DynamicSerializeElement
	public Double spd = 0.0;
	/** direction **/
    @DynamicSerializeElement
	public Double dir = 0.0;
	/** county of origin **/
    @DynamicSerializeElement
	public String county = "N/A";
	/** cwa **/
    @DynamicSerializeElement
    public String cwa = "N/A";
	/** latitude **/
    @DynamicSerializeElement
	public Double lat = 0.0;
	/** longitude **/
    @DynamicSerializeElement
	public Double lon = 0.0;
    @DynamicSerializeElement
	/** Ident (Cell Table - Storm ID, MESO DMDIdent, TVS FeatureID) **/
	public String ident = null;
    @DynamicSerializeElement
    /** is feature new **/
    public boolean isNew = true;
	
    /**
	 * time of data
	 * @return
	 */
	public DataTime getTime() {
		return time;
	}
	/**
	 * Sets the Time of data
	 * @param time
	 */
	public void setTime(DataTime time) {
		this.time = time;
	}
	/**
	 * azimuth radar in degress
	 * @return
	 */
	public Double getAzm() {
		return azm;
	}
	/**
	 * sets the Direction
	 * @param dir
	 */
	public void setAzm(Double azm) {
		this.azm = azm;
	}
    /** 
     * gets the direction
     * @return
     */
	public Double getDir() {
		return dir;
	}
	
	 /** 
     * sets the direction
     * @return
     */
    public void setDir(Double dir) {
        this.dir = dir;
    }
    
	
	/**
	 * speed of movement
	 * @return
	 */
	public Double getSpd() {
		return spd;
	}
	/**
	 * gets the speed
	 * @param spd
	 */
	public void setSpd(Double spd) {
		this.spd = spd;
	}
    /** 
     * gets the range
     * @return
     */
	public Double getRng() {
		return rng;
	}
	
	/**
	 * Sets the range
	 * @param rng
	 */
	public void setRng(Double rng) {
		this.rng = rng;
	}
	/**
	 * get the lat
	 * @return
	 */
	public Double getLat() {
		return lat;
	}
	/**
	 * set the lat
	 * @param lat
	 */
	public void setLat(Double lat) {
		this.lat = lat;
	}
	/**
	 * get the lon
	 * @return
	 */
	public Double getLon() {
		return lon;
	}
	/**
	 * set the lon
	 * @param lon
	 */
	public void setLon(Double lon) {
		this.lon = lon;
	}
	/**
	 * the county
	 * @return
	 */
	public String getCounty() {
		return county;
	}
	/**
	 * set the county
	 * @param county
	 */
	public void setCounty(String county) {
		this.county = county;
	}
	/**
	 * The ident (TVS and MESO) stormID for CELL
	 * @return
	 */
	public String getIdent() {
		return ident;
	}
	/**
	 * set the ident
	 * @param ident
	 */
	public void setIdent(String ident) {
		this.ident = ident;
	}
	
	 /**
     * county warning area
     * @return
     */
    public String getCwa() {
        return cwa;
    }
    
    /**
     * set cwa
     * @param cwa
     */
    public void setCwa(String cwa) {
        this.cwa = cwa;
    }
    
    /**
     * get whether new or not
     * @return
     */
    public boolean getIsNew() {
        return isNew;
    }

    /**
     * set whether new or not
     * @param isNew
     */
    public void setIsNew(boolean isNew) {
        this.isNew = isNew;
    }

  
	/**
	 * Copy row
	 */
	public abstract ScanTableDataRow copy();
	
	/**
	 * clear all non persistent data
	 */
	public abstract void clear();
	
	/**
	 * copy common objects
	 */
	public ScanTableDataRow copyCommon(ScanTableDataRow row) {
	    
	    row.setTime(this.getTime());
	    row.setAzm(this.getAzm());
	    row.setRng(this.getRng());
	    row.setSpd(this.getSpd());
	    row.setDir(this.getDir());
	    row.setCounty(this.getCounty());
	    row.setLat(this.getLat());
	    row.setLon(this.getLon());
	    row.setIdent(this.getIdent());
	    row.setCwa(this.getCwa());
	    row.setIsNew(this.getIsNew());
	    
	    return row;
	}
	
	/**
	 * clears out the common data for persistent ID's
	 */
	public void clearNonPersistantData() {
	    this.setAzm(0.0);
	    this.setRng(0.0);
	    this.setSpd(0.0);
	    this.setDir(0.0);
	    this.setCounty(null);
	    this.setLat(0.0);
	    this.setLon(0.0);
	    this.setCwa(null);
	    
	    this.clear();
	}
		
	public abstract String toString();
	
	public abstract Double getValue(String field);
}
