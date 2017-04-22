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
package com.raytheon.viz.hydro.stationprofile;

import java.util.Date;

/**
 * Hydro Data Report object.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Mar 12, 2010            mpduff     Initial creation
 * 
 * </pre>
 * 
 * @author mpduff
 * @version 1.0
 */

public class HydroDataReport {
    private String pe;

    private int dur;

    private String ts;

    private String extremum;

    private double probability;

    private String shefQualCode;

    private long qualityCode;

    private double value = -9999.0;

    private Date validTime;

    private Date basisTime;
    
    public HydroDataReport(String pe, int dur, String ts, String extremum, double probability,
            String shefQualCode, long qualityCode, double value, Date validTime, Date basisTime) {
        this.pe = pe;
        this.dur = dur;
        this.extremum = extremum;
        this.probability = probability;
        this.shefQualCode = shefQualCode;
        this.qualityCode = qualityCode;
        this.value = value;
        this.validTime = validTime;
        this.basisTime = basisTime;
    }
    
    public HydroDataReport() {
        
    }

    /**
     * @return the pe
     */
    public String getPe() {
        return pe;
    }

    /**
     * @param pe
     *            the pe to set
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
     * @param dur
     *            the dur to set
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
     * @param ts
     *            the ts to set
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
     * @param extremum
     *            the extremum to set
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
     * @param probability
     *            the probability to set
     */
    public void setProbability(double probability) {
        this.probability = probability;
    }

    /**
     * @return the shefQualCode
     */
    public String getShefQualCode() {
        return shefQualCode;
    }

    /**
     * @param shefQualCode
     *            the shefQualCode to set
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
     * @param qualityCode
     *            the qualityCode to set
     */
    public void setQualityCode(long qualityCode) {
        this.qualityCode = qualityCode;
    }

    /**
     * @return the value
     */
    public double getValue() {
        return value;
    }

    /**
     * @param value
     *            the value to set
     */
    public void setValue(double value) {
        this.value = value;
    }

    /**
     * @return the validTime
     */
    public Date getValidTime() {
        return validTime;
    }

    /**
     * @param validTime
     *            the validTime to set
     */
    public void setValidTime(Date validTime) {
        this.validTime = validTime;
    }

    /**
     * @return the basisTime
     */
    public Date getBasisTime() {
        return basisTime;
    }

    /**
     * @param basisTime
     *            the basisTime to set
     */
    public void setBasisTime(Date basisTime) {
        this.basisTime = basisTime;
    }
}
