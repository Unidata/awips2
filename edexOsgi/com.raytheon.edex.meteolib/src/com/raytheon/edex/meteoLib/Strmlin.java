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
package com.raytheon.edex.meteoLib;

/**
 * Strmlin
 * 
 * Provides set and get functions for strmlin.f outputs
 * 
 * <pre>
 *   
 *    SOFTWARE HISTORY
 *   
 *    Date          Ticket#     Engineer    Description
 *    ------------  ----------  ----------- --------------------------
 *    June 12, 2008             S Manoj    Initial Creation.
 *   
 * </pre>
 * 
 * @author chammack
 * @version 1
 */

public class Strmlin {
    private int[] LCPnt;

    private float[] IPnt;

    private float[] JPnt;

    private int kpnt1;

    private int kpnt2;

    /**
     * @return the LCPnt
     */
    public int[] getLCPnt() {
        return LCPnt;
    }

    /**
     * @param LCPnt
     * 
     */
    public void setLCPnt(int[] LCPnt) {
        this.LCPnt = LCPnt;
    }

    /**
     * @return the IPnt
     */
    public float[] getIPnt() {
        return IPnt;
    }

    /**
     * @param IPnt
     */
    public void setIPnt(float[] IPnt) {
        this.IPnt = IPnt;
    }

    /**
     * @return the JPnt
     */
    public float[] getJPnt() {
        return JPnt;
    }

    /**
     * @param JPnt
     */
    public void setJPnt(float[] JPnt) {
        this.JPnt = JPnt;
    }

    /**
     * @return the kpnt1
     */
    public int getKpnt1() {
        return kpnt1;
    }

    /**
     * @param kpnt1
     */
    public void setKpnt1(int kpnt1) {
        this.kpnt1 = kpnt1;
    }

    /**
     * @return the kpnt2
     */
    public int getKpnt2() {
        return kpnt2;
    }

    /**
     * @param kpnt2
     */
    public void setKpnt2(int kpnt2) {
        this.kpnt2 = kpnt2;
    }

}
