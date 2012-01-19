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
package com.raytheon.viz.hydro.pointdatacontrol.data;

import java.util.Calendar;
import java.util.Date;
import java.util.GregorianCalendar;

/**
 * Holds the Point Data Control file information.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Feb 4, 2009            mpduff     Initial creation
 * 
 * </pre>
 * 
 * @author mpduff
 * @version 1.0
 */

public class PDCFileInfo {
    private static PDCFileInfo instance;

    private boolean success = false;

    private int entryCount = 0;

    private Calendar startTime = new GregorianCalendar();

    private int incrementTime;

    private int valueCount = 0;

    private Date timestepFileCreationTime = null;

    public static synchronized PDCFileInfo getInstance() {
        if (instance == null) {
            instance = new PDCFileInfo();
        }
        return instance;
    }

    /**
     * Private Constructor.
     */
    private PDCFileInfo() {

    }

    /**
     * @return the success
     */
    public boolean isSuccess() {
        return success;
    }

    /**
     * @param success
     *            the success to set
     */
    public void setSuccess(boolean success) {
        this.success = success;
    }

    /**
     * @return the entryCount
     */
    public int getEntryCount() {
        return entryCount;
    }

    /**
     * @param entryCount
     *            the entryCount to set
     */
    public void setEntryCount(int entryCount) {
        this.entryCount = entryCount;
    }

    /**
     * @return the startTime
     */
    public Calendar getStartTime() {
        return startTime;
    }

    /**
     * @param startTime
     *            the startTime to set
     */
    public void setStartTime(Calendar startTime) {
        this.startTime = startTime;
    }

    /**
     * @return the incrementTime
     */
    public int getIncrementTime() {
        return incrementTime;
    }

    /**
     * @param incrementTime
     *            the incrementTime to set
     */
    public void setIncrementTime(int incrementTime) {
        this.incrementTime = incrementTime;
    }

    /**
     * @return the valueCount
     */
    public int getValueCount() {
        return valueCount;
    }

    /**
     * @param valueCount
     *            the valueCount to set
     */
    public void setValueCount(int valueCount) {
        this.valueCount = valueCount;
    }

    /**
     * @return the timestepFileCreationTime
     */
    public Date getTimestepFileCreationTime() {
        return timestepFileCreationTime;
    }

    /**
     * @param timestepFileCreationTime the timestepFileCreationTime to set
     */
    public void setTimestepFileCreationTime(Date timestepFileCreationTime) {
        this.timestepFileCreationTime = timestepFileCreationTime;
    }

    /**
     * Return the contents of this object as a String.
     * 
     * @see java.lang.Object#toString()
     * @return String
     */
    @Override
    public String toString() {
        StringBuilder sb = new StringBuilder("\n************** PDCFileInfo ***********");
        sb.append("\nEntry Count:  " + getEntryCount());
        sb.append("\nIncrement Time:  " + getIncrementTime());
        sb.append("\nValue Count:  " + getValueCount());
        sb.append("\nStart Time:  " + getStartTime().toString());

        return sb.toString();
    }
}
