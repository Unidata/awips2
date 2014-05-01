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

/**
 * This object holds the Station Profile label information.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Mar 16, 2010            mpduff     Initial creation
 * 
 * </pre>
 * 
 * @author mpduff
 * @version 1.0
 */

public class LabelPosition {
    /**
     * Lid label x pixel.
     */
    private int lidX;

    /**
     * Lid label y pixel.
     */
    private int lidY;

    /**
     * Elevation label x pixel.
     */
    private int elevX;

    /**
     * Elevation label y pixel.
     */
    private int elevY;

    /**
     * Lid label text.
     */
    private String lidBuffer;

    /**
     * Elevation label text.
     */
    private String elevBuffer;

    /**
     * @return the lidX
     */
    public int getLidX() {
        return lidX;
    }

    /**
     * @param lidX the lidX to set
     */
    public void setLidX(int lidX) {
        this.lidX = lidX;
    }

    /**
     * @return the lidY
     */
    public int getLidY() {
        return lidY;
    }

    /**
     * @param lidY the lidY to set
     */
    public void setLidY(int lidY) {
        this.lidY = lidY;
    }

    /**
     * @return the elevX
     */
    public int getElevX() {
        return elevX;
    }

    /**
     * @param elevX the elevX to set
     */
    public void setElevX(int elevX) {
        this.elevX = elevX;
    }

    /**
     * @return the elevY
     */
    public int getElevY() {
        return elevY;
    }

    /**
     * @param elevY the elevY to set
     */
    public void setElevY(int elevY) {
        this.elevY = elevY;
    }

    /**
     * @return the lidBuffer
     */
    public String getLidBuffer() {
        return lidBuffer;
    }

    /**
     * @param lidBuffer the lidBuffer to set
     */
    public void setLidBuffer(String lidBuffer) {
        this.lidBuffer = lidBuffer;
    }

    /**
     * @return the elevBuffer
     */
    public String getElevBuffer() {
        return elevBuffer;
    }

    /**
     * @param elevBuffer the elevBuffer to set
     */
    public void setElevBuffer(String elevBuffer) {
        this.elevBuffer = elevBuffer;
    }
}
