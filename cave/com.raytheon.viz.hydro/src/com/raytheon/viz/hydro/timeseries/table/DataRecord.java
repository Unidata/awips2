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
package com.raytheon.viz.hydro.timeseries.table;

import java.util.Date;

/**
 * Object to hold a singe data record when editing.
 * 
 * <pre>
 *
 * software history
 *
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Oct 16, 2008 #1520      mpduff     Initial creation.
 *
 * </pre>
 *
 * @author mpduff
 * @version 1.0	
 */

public class DataRecord {
    /** Location Id */
    private String lid = null;
    
    /** Observation time */
    private Date obsTime = null;
    
    /** Data value */
    private double value;
    
    /** Revised data, 1 = yes */
    private int revision;
    
    private String shefQualCode = null;
    
    /** The quality code */
    private long qualityCode;
    
    /** Product ID */
    private String productId = null;
    
    /** Product time */
    private Date productTime = null;
    
    /** Posting time */
    private Date postingTime = null;
    
    /** Physical Element */
    private String pe = null;
    
    /** Duration */
    private int dur;
    
    /** Type Source */
    private String ts = null;
    
    /** Extremum */
    private String ext = null;
    
    /** Forecast Basis Time */
    private String basisTime = null;
    
    /** Valid Time of Data */
    private Date validTime = null;

    /**
     * @return the lid
     */
    public String getLid() {
        return lid;
    }

    /**
     * @param lid the lid to set
     */
    public void setLid(String lid) {
        this.lid = lid;
    }

    /**
     * @return the obsTime
     */
    public Date getObsTime() {
        return obsTime;
    }

    /**
     * @param obsTime the obsTime to set
     */
    public void setObsTime(Date obsTime) {
        this.obsTime = obsTime;
    }

    /**
     * @return the value
     */
    public double getValue() {
        return value;
    }

    /**
     * @param value the value to set
     */
    public void setValue(double value) {
        this.value = value;
    }

    /**
     * @return the revision
     */
    public int getRevision() {
        return revision;
    }

    /**
     * @param revision the revision to set
     */
    public void setRevision(int revision) {
        this.revision = revision;
    }

    /**
     * @return the shefQualCode
     */
    public String getShefQualCode() {
        return shefQualCode;
    }

    /**
     * @param shefQualCode the shefQualCode to set
     */
    public void setShefQualCode(String shefQualCode) {
        this.shefQualCode = shefQualCode;
    }

    /**
     * @return the qualityCode
     */
    public long getQualityCode() {
        return qualityCode;
    }

    /**
     * @param qualityCode the qualityCode to set
     */
    public void setQualityCode(long qualityCode) {
        this.qualityCode = qualityCode;
    }

    /**
     * @return the productId
     */
    public String getProductId() {
        return productId;
    }

    /**
     * @param productId the productId to set
     */
    public void setProductId(String productId) {
        this.productId = productId;
    }

    /**
     * @return the productTime
     */
    public Date getProductTime() {
        return productTime;
    }

    /**
     * @param productTime the productTime to set
     */
    public void setProductTime(Date productTime) {
        this.productTime = productTime;
    }

    /**
     * @return the postingTime
     */
    public Date getPostingTime() {
        return postingTime;
    }

    /**
     * @param postingTime the postingTime to set
     */
    public void setPostingTime(Date postingTime) {
        this.postingTime = postingTime;
    }

    /**
     * @return the pe
     */
    public String getPe() {
        return pe;
    }

    /**
     * @param pe the pe to set
     */
    public void setPe(String pe) {
        this.pe = pe;
    }

    /**
     * @return the dur
     */
    public int getDur() {
        return dur;
    }

    /**
     * @param dur the dur to set
     */
    public void setDur(int dur) {
        this.dur = dur;
    }

    /**
     * @return the ts
     */
    public String getTs() {
        return ts;
    }

    /**
     * @param ts the ts to set
     */
    public void setTs(String ts) {
        this.ts = ts;
    }

    /**
     * @return the ext
     */
    public String getExt() {
        return ext;
    }

    /**
     * @param ext the ext to set
     */
    public void setExt(String ext) {
        this.ext = ext;
    }

    /**
     * @return the basisTime
     */
    public String getBasisTime() {
        return basisTime;
    }

    /**
     * @param basisTime the basisTime to set
     */
    public void setBasisTime(String basisTime) {
        this.basisTime = basisTime;
    }

    /**
     * @return the validTime
     */
    public Date getValidTime() {
        return validTime;
    }

    /**
     * @param validTime the validTime to set
     */
    public void setValidTime(Date validTime) {
        this.validTime = validTime;
    }

}
