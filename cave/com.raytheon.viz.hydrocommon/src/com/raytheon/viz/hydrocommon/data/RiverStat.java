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
package com.raytheon.viz.hydrocommon.data;

/**
 * RiverStat data object.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Nov 17, 2008            mpduff     Initial creation.
 * 
 * </pre>
 * 
 * @author mpduff
 * @version 1.0
 */

public class RiverStat {
    /**
     * Location Id.
     */
    private String lid = null;

    /**
     * Physical element.
     */
    private String pe = null;

    /**
     * Flood flow.
     */
    private double fq;

    /**
     * Flood stage.
     */
    private double fs;

    /**
     * Action flow.
     */
    private double aq;

    /**
     * Action stage.
     */
    private double as;

    public RiverStat() {

    }

    public RiverStat(Object[] oa) {
        if (oa[0] != null) {
            setLid((String) oa[0]);
        }

        if (oa[1] != null) {
            setPe((String) oa[1]);
        }

        if (oa[2] != null) {
            setFq((Double) oa[2]);
        }

        if (oa[3] != null) {
            setFs((Double) oa[3]);
        }

        if (oa[4] != null) {
            setAq((Double) oa[4]);
        }

        if (oa[5] != null) {
            setAs((Double) oa[5]);
        }
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
     * @return the fq
     */
    public double getFq() {
        return fq;
    }

    /**
     * @param fq
     *            the fq to set
     */
    public void setFq(double fq) {
        this.fq = fq;
    }

    /**
     * @return the fs
     */
    public double getFs() {
        return fs;
    }

    /**
     * @param fs
     *            the fs to set
     */
    public void setFs(double fs) {
        this.fs = fs;
    }

    /**
     * @return the aq
     */
    public double getAq() {
        return aq;
    }

    /**
     * @param aq
     *            the aq to set
     */
    public void setAq(double aq) {
        this.aq = aq;
    }

    /**
     * @return the as
     */
    public double getAs() {
        return as;
    }

    /**
     * @param as
     *            the as to set
     */
    public void setAs(double as) {
        this.as = as;
    }

}