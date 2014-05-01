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
package com.raytheon.viz.hydro.timeseries.util;

import java.io.Serializable;
import java.util.Date;

/**
 * A time series point.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date			Ticket#		Engineer	Description
 * ------------	----------	-----------	--------------------------
 * Jun 25, 2008	1194		mpduff	Initial creation
 * 
 * </pre>
 * 
 * @author mpduff
 * @version 1.0
 */

public class TimeSeriesPoint implements Serializable {
    private static final long serialVersionUID = 5888351700556012922L;

    /** The X value, which is a Date in a time series graph */
    private Date x;

    /** The Y value */
    private double y;
    
    /** The x pixel value */
    private int pixelX;
    
    /** The y pixel value */
    private int pixely;

    /** The previous value if point was edited */
    private float old_value;

    /** The Mode */
    private char mode;

    /** The Revision */
    private short revision;

    /** The quality code */
    private int quality_code;

    /** The probability */
    private float probability;
    
    /** Is the point selected */
    private boolean isSelected;
    
    /** The Fcst valid time */
    private Date validtime;

    /**
     * @return the x
     */
    public Date getX() {
        return x;
    }

    /**
     * @param x
     *            the x to set
     */
    public void setX(Date x) {
        this.x = x;
    }

    /**
     * @return the y
     */
    public double getY() {
        return y;
    }

    /**
     * @param y
     *            the y to set
     */
    public void setY(double y) {
        this.y = y;
    }

    /**
     * @return the old_value
     */
    public float getOld_value() {
        return old_value;
    }

    /**
     * @param old_value
     *            the old_value to set
     */
    public void setOld_value(float old_value) {
        this.old_value = old_value;
    }

    /**
     * @return the mode
     */
    public char getMode() {
        return mode;
    }

    /**
     * @param mode
     *            the mode to set
     */
    public void setMode(char mode) {
        this.mode = mode;
    }

    /**
     * @return the revision
     */
    public short getRevision() {
        return revision;
    }

    /**
     * @param revision
     *            the revision to set
     */
    public void setRevision(short revision) {
        this.revision = revision;
    }

    /**
     * @return the quality_code
     */
    public int getQuality_code() {
        return quality_code;
    }

    /**
     * @param quality_code
     *            the quality_code to set
     */
    public void setQuality_code(int quality_code) {
        this.quality_code = quality_code;
    }

    /**
     * @return the probability
     */
    public float getProbability() {
        return probability;
    }

    /**
     * @param probability
     *            the probability to set
     */
    public void setProbability(float probability) {
        this.probability = probability;
    }

    /**
     * @return the isSelected
     */
    public boolean isSelected() {
        return isSelected;
    }

    /**
     * @param isSelected the isSelected to set
     */
    public void setSelected(boolean isSelected) {
        this.isSelected = isSelected;
    }

    /**
     * @return the pixelX
     */
    public int getPixelX() {
        return pixelX;
    }

    /**
     * @param pixelX the pixelX to set
     */
    public void setPixelX(int pixelX) {
        this.pixelX = pixelX;
    }

    /**
     * @return the pixely
     */
    public int getPixelY() {
        return pixely;
    }

    /**
     * @param pixely the pixely to set
     */
    public void setPixely(int pixely) {
        this.pixely = pixely;
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

    @Override
    public String toString() {
        return getX() + ": " + getY();
    }
}
