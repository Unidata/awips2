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
package com.raytheon.viz.hydro.util;

import java.util.Date;

/**
 * TODO Add Description
 * 
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Apr 6, 2009            mpduff     Initial creation
 *
 * </pre>
 *
 * @author mpduff
 * @version 1.0	
 */

public class Forecast {
    private String lid  = null;

    private String pe  = null;
    
    private int dur;
    
    private String ts = null;
    
    private String extremum = null;

    private double probability;
    
    private Date validtime = null;

    private Date basistime = null;

    private double value;

    private String shefQualCode = null;

    private int qualityCode;

    private int revision;

    private String productId = null;
    
    private Date producttime = null;
    
    private Date postingtime = null;

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
     * @return the extremum
     */
    public String getExtremum() {
        return extremum;
    }

    /**
     * @param extremum the extremum to set
     */
    public void setExtremum(String extremum) {
        this.extremum = extremum;
    }

    /**
     * @return the probability
     */
    public double getProbability() {
        return probability;
    }

    /**
     * @param probability the probability to set
     */
    public void setProbability(double probability) {
        this.probability = probability;
    }

    /**
     * @return the validtime
     */
    public Date getValidtime() {
        return validtime;
    }

    /**
     * @param validtime the validtime to set
     */
    public void setValidtime(Date validtime) {
        this.validtime = validtime;
    }

    /**
     * @return the basistime
     */
    public Date getBasistime() {
        return basistime;
    }

    /**
     * @param basistime the basistime to set
     */
    public void setBasistime(Date basistime) {
        this.basistime = basistime;
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
    public int getQualityCode() {
        return qualityCode;
    }

    /**
     * @param qualityCode the qualityCode to set
     */
    public void setQualityCode(int qualityCode) {
        this.qualityCode = qualityCode;
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
     * @return the producttime
     */
    public Date getProducttime() {
        return producttime;
    }

    /**
     * @param producttime the producttime to set
     */
    public void setProducttime(Date producttime) {
        this.producttime = producttime;
    }

    /**
     * @return the postingtime
     */
    public Date getPostingtime() {
        return postingtime;
    }

    /**
     * @param postingtime the postingtime to set
     */
    public void setPostingtime(Date postingtime) {
        this.postingtime = postingtime;
    }
}
