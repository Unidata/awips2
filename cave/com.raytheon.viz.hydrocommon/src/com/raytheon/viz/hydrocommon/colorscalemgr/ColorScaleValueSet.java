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

import java.io.Serializable;
import java.util.ArrayList;

/**
 * Color scale value set data object.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Mar 31, 2010 4671       mpduff      Initial creation.
 * 
 * </pre>
 * 
 * @author mpduff
 * @version 1.0
 */

public class ColorScaleValueSet implements Serializable {
    private static final long serialVersionUID = 1L;

    /**
     * List of colors in the color set.
     */
    private ArrayList<String> colorSetList = new ArrayList<String>();

    /**
     * List of scale values in the color set.
     */
    private ArrayList<Double> scaleValueSetList = new ArrayList<Double>();

    /**
     * The color use string.
     */
    private String colorUseString = null;

    /**
     * Color use set duration.
     */
    private double duration = -99999;

    /**
     * The user id.
     */
    private String userID = null;

    /**
     * Default Constructor.
     */
    public ColorScaleValueSet() {

    }

    public ColorScaleValueSet(ArrayList<String> colorSetList,
            ArrayList<Double> scaleValueSetList, String colorUseString,
            double duration, String userID) {
        this.colorSetList = colorSetList;
        this.scaleValueSetList = scaleValueSetList;
        this.colorUseString = colorUseString;
        this.duration = duration;
        this.userID = userID;
    }

    /**
     * @return the colorSetList
     */
    public ArrayList<String> getColorSetList() {
        return colorSetList;
    }

    /**
     * @param colorSetList
     *            the colorSetList to set
     */
    public void setColorSetList(ArrayList<String> colorSetList) {
        this.colorSetList = colorSetList;
    }

    /**
     * @return the scaleValueSetList
     */
    public ArrayList<Double> getScaleValueSetList() {
        return scaleValueSetList;
    }

    /**
     * @param scaleValueSetList
     *            the scaleValueSetList to set
     */
    public void setScaleValueSetList(ArrayList<Double> scaleValueSetList) {
        this.scaleValueSetList = scaleValueSetList;
    }

    /**
     * @return the colorUseString
     */
    public String getColorUseString() {
        return colorUseString;
    }

    /**
     * @param colorUseString
     *            the colorUseString to set
     */
    public void setColorUseString(String colorUseString) {
        this.colorUseString = colorUseString;
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

}
