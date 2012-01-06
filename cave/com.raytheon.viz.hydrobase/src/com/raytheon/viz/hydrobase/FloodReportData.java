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
package com.raytheon.viz.hydrobase;

import java.util.Date;

import com.raytheon.viz.hydrocommon.HydroConstants;

/**
 * Flood Report Data Object.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Sep 2, 2009  2259       mpduff      Initial creation
 * 
 * </pre>
 * 
 * @author mpduff
 * @version 1.0
 */

public class FloodReportData {
    /** Location ID */
    private String lid = null;

    /** Long name of location */
    private String longName = null;

    /** Crest height */
    private double crest = HydroConstants.FLOOD_REPORT_MSG;

    /** Flood stage for location */
    private double floodStage = HydroConstants.FLOOD_REPORT_MSG;

    /** Date of the crest */
    private Date crestDate = null;
    
    /** The flood event id */
    private int floodEventId = -999;

    /** Last Crest height */
    private double lastCrest = -HydroConstants.FLOOD_REPORT_MSG;

    /** Date of the last crest */
    private Date lastCrestDate = null;
    
    /**
     * Constructor
     */
    public FloodReportData() {

    }

    /**
     * Constructor
     * 
     * @param lid
     * @param longName
     * @param crest
     * @param floodStage
     * @param crestDate
     */
    public FloodReportData(String lid, String longName, double crest,
            double floodStage, Date crestDate) {
        this.lid = lid;
        this.longName = longName;
        this.crest = crest;
        this.floodStage = floodStage;
        this.crestDate = crestDate;
    }

    /**
     * @return the lid
     */
    public String getLid() {
        return lid;
    }

    /**
     * @param lid
     *            the lid to set
     */
    public void setLid(String lid) {
        this.lid = lid;
    }

    /**
     * @return the longName
     */
    public String getLongName() {
        return longName;
    }

    /**
     * @param longName
     *            the longName to set
     */
    public void setLongName(String longName) {
        this.longName = longName;
    }

    /**
     * @return the crest
     */
    public double getCrest() {
        return crest;
    }

    /**
     * @param crest
     *            the crest to set
     */
    public void setCrest(double crest) {
        this.crest = crest;
    }

    /**
     * @return the floodStage
     */
    public double getFloodStage() {
        return floodStage;
    }

    /**
     * @param floodStage
     *            the floodStage to set
     */
    public void setFloodStage(double floodStage) {
        this.floodStage = floodStage;
    }

    /**
     * @return the crestDate
     */
    public Date getCrestDate() {
        return crestDate;
    }

    /**
     * @param crestDate
     *            the crestDate to set
     */
    public void setCrestDate(Date crestDate) {
        this.crestDate = crestDate;
    }

    /**
     * @return the floodEventId
     */
    public int getFloodEventId() {
        return floodEventId;
    }

    /**
     * @param floodEventId the floodEventId to set
     */
    public void setFloodEventId(int floodEventId) {
        this.floodEventId = floodEventId;
    }

    /**
     * @return the lastCrest
     */
    public double getLastCrest() {
        return lastCrest;
    }

    /**
     * @param lastCrest the lastCrest to set
     */
    public void setLastCrest(double lastCrest) {
        this.lastCrest = lastCrest;
    }

    /**
     * @return the lastCrestDate
     */
    public Date getLastCrestDate() {
        return lastCrestDate;
    }

    /**
     * @param lastCrestDate the lastCrestDate to set
     */
    public void setLastCrestDate(Date lastCrestDate) {
        this.lastCrestDate = lastCrestDate;
    }
}
