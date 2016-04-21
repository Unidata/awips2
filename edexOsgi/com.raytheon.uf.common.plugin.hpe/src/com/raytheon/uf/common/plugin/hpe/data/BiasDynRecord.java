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
package com.raytheon.uf.common.plugin.hpe.data;

import java.util.Date;

import com.raytheon.uf.common.util.StringUtil;

/**
 * Data structure holding data from the RWBiasDyn and the DAABiasDyn tables.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Mar 26, 2014    3026    mpduff      Initial creation
 * 
 * </pre>
 * 
 * @author mpduff
 * @version 1.0
 */

public class BiasDynRecord {
    /** Radar ID */
    private String radarId;

    /** Office ID */
    private String officeId;

    /** Observation time */
    private Date obsTime;

    /** MemspanIndex */
    private float memspanIndex;

    /** Number pairs */
    private double numPairs;

    /** Sum of Gages */
    private float sumGages;

    /** Sum of radars */
    private float sumRadars;

    private float bias;

    /**
     * @return the radarId
     */
    public String getRadarId() {
        return radarId;
    }

    /**
     * @param radarId
     *            the radarId to set
     */
    public void setRadarId(String radarId) {
        this.radarId = radarId;
    }

    /**
     * @return the officeId
     */
    public String getOfficeId() {
        return officeId;
    }

    /**
     * @param officeId
     *            the officeId to set
     */
    public void setOfficeId(String officeId) {
        this.officeId = officeId;
    }

    /**
     * @return the obsTime
     */
    public Date getObsTime() {
        return obsTime;
    }

    /**
     * @param obsTime
     *            the obsTime to set
     */
    public void setObsTime(Date obsTime) {
        this.obsTime = obsTime;
    }

    /**
     * @return the memspanIndex
     */
    public float getMemspanIndex() {
        return memspanIndex;
    }

    /**
     * @param memspanIndex
     *            the memspanIndex to set
     */
    public void setMemspanIndex(float memspanIndex) {
        this.memspanIndex = memspanIndex;
    }

    /**
     * @return the numPairs
     */
    public double getNumPairs() {
        return numPairs;
    }

    /**
     * @param numPairs
     *            the numPairs to set
     */
    public void setNumPairs(double numPairs) {
        this.numPairs = numPairs;
    }

    /**
     * @return the sumGages
     */
    public float getSumGages() {
        return sumGages;
    }

    /**
     * @param sumGages
     *            the sumGages to set
     */
    public void setSumGages(float sumGages) {
        this.sumGages = sumGages;
    }

    /**
     * @return the sumRadars
     */
    public float getSumRadars() {
        return sumRadars;
    }

    /**
     * @param sumRadars
     *            the sumRadars to set
     */
    public void setSumRadars(float sumRadars) {
        this.sumRadars = sumRadars;
    }

    /**
     * @return the bias
     */
    public float getBias() {
        return bias;
    }

    /**
     * @param bias
     *            the bias to set
     */
    public void setBias(float bias) {
        this.bias = bias;
    }

    @Override
    public String toString() {
        StringBuilder sb = new StringBuilder();
        sb.append("Bias: ").append(this.bias).append(StringUtil.NEWLINE);
        sb.append("MemSpanIndex: ").append(this.memspanIndex)
                .append(StringUtil.NEWLINE);
        sb.append("NumPairs: ").append(this.numPairs)
                .append(StringUtil.NEWLINE);
        sb.append("OfficeId: ").append(this.officeId)
                .append(StringUtil.NEWLINE);
        sb.append("RadarId: ").append(this.radarId).append(StringUtil.NEWLINE);
        sb.append("sumGages: ").append(this.sumGages)
                .append(StringUtil.NEWLINE);
        sb.append("sumRadars: ").append(this.sumRadars);

        return sb.toString();
    }
}
