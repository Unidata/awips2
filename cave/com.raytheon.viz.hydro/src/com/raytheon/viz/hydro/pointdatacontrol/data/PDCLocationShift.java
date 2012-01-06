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

/**
 * Location Shift data object.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Feb 17, 2010 2635       mpduff      Initial creation
 * 
 * </pre>
 * 
 * @author mpduff
 * @version 1.0
 */

public class PDCLocationShift {
    private String lid = null;

    private String paramCode = null;

    private int xShift = 0;

    private int yShift = 0;

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
     * @return the paramCode
     */
    public String getParamCode() {
        return paramCode;
    }

    /**
     * @param paramCode
     *            the paramCode to set
     */
    public void setParamCode(String paramCode) {
        this.paramCode = paramCode;
    }

    /**
     * @return the xShift
     */
    public int getXShift() {
        return xShift;
    }

    /**
     * @param shift
     *            the xShift to set
     */
    public void setXShift(int shift) {
        xShift = shift;
    }

    /**
     * @return the yShift
     */
    public int getYShift() {
        return yShift;
    }

    /**
     * @param shift
     *            the yShift to set
     */
    public void setYShift(int shift) {
        yShift = shift;
    }
}
