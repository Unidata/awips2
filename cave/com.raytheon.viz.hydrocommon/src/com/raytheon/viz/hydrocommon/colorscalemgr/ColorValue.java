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
package com.raytheon.viz.hydrocommon.colorscalemgr;

/**
 * Reused from OHD.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Mar 30, 2010 4671       mpduff      Initial creation.
 * 
 * </pre>
 * 
 * @author mpduff
 * @version 1.0
 */

public class ColorValue {
    private String userID = null;

    private String applicationName = null;

    private String colorUseName = null;

    private double duration;

    private double thresholdValue;

    private String thresholdUnit = null;

    private String colorName = null;

    /**
     * @return the userID
     */
    public String getUserID() {
        return userID;
    }

    /**
     * @param userID
     *            the userID to set
     */
    public void setUserID(String userID) {
        this.userID = userID;
    }

    /**
     * @return the applicationName
     */
    public String getApplicationName() {
        return applicationName;
    }

    /**
     * @param applicationName
     *            the applicationName to set
     */
    public void setApplicationName(String applicationName) {
        this.applicationName = applicationName;
    }

    /**
     * @return the colorUseName
     */
    public String getColorUseName() {
        return colorUseName;
    }

    /**
     * @param colorUseName
     *            the colorUseName to set
     */
    public void setColorUseName(String colorUseName) {
        this.colorUseName = colorUseName;
    }

    /**
     * @return the duration
     */
    public double getDuration() {
        return duration;
    }

    /**
     * @param duration
     *            the duration to set
     */
    public void setDuration(double duration) {
        this.duration = duration;
    }

    /**
     * @return the thresholdValue
     */
    public double getThresholdValue() {
        return thresholdValue;
    }

    /**
     * @param thresholdValue
     *            the thresholdValue to set
     */
    public void setThresholdValue(double thresholdValue) {
        this.thresholdValue = thresholdValue;
    }

    /**
     * @return the thresholdUnit
     */
    public String getThresholdUnit() {
        return thresholdUnit;
    }

    /**
     * @param thresholdUnit
     *            the thresholdUnit to set
     */
    public void setThresholdUnit(String thresholdUnit) {
        this.thresholdUnit = thresholdUnit;
    }

    /**
     * @return the colorName
     */
    public String getColorName() {
        return colorName;
    }

    /**
     * @param colorName
     *            the colorName to set
     */
    public void setColorName(String colorName) {
        this.colorName = colorName;
    }

}
