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
package com.raytheon.viz.mpe.ui.dialogs.hourlyradar;

import java.util.Date;

/**
 * Data object to hold the DPA Supplemental data.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Aug 5, 2009  2675       mpduff     Initial creation
 * 
 * </pre>
 * 
 * @author mpduff
 * @version 1.0
 */

public class DPASupplementalData {
    private int minoff;

    private float maxvalh;

    private float maxvald;

    private float s1_bias_value;

    private Date producttime;

    private int nisolbin;

    private int noutint;

    private int noutrep;

    private float areared;

    private float biscanr;

    private int nbadscan;

    private int nhourout;

    private int volcovpat;

    private int opermode;

    private String missper;

    private int supplmess;

    /**
     * @return the minoff
     */
    public int getMinoff() {
        return minoff;
    }

    /**
     * @param minoff
     *            the minoff to set
     */
    public void setMinoff(int minoff) {
        this.minoff = minoff;
    }

    /**
     * @return the maxvalh
     */
    public float getMaxvalh() {
        return maxvalh;
    }

    /**
     * @param maxvalh
     *            the maxvalh to set
     */
    public void setMaxvalh(float maxvalh) {
        this.maxvalh = maxvalh;
    }

    /**
     * @return the maxvald
     */
    public float getMaxvald() {
        return maxvald;
    }

    /**
     * @param maxvald
     *            the maxvald to set
     */
    public void setMaxvald(float maxvald) {
        this.maxvald = maxvald;
    }

    /**
     * @return the s1_bias_value
     */
    public float getS1_bias_value() {
        return s1_bias_value;
    }

    /**
     * @param s1_bias_value
     *            the s1_bias_value to set
     */
    public void setS1_bias_value(float s1_bias_value) {
        this.s1_bias_value = s1_bias_value;
    }

    /**
     * @return the producttime
     */
    public Date getProducttime() {
        return producttime;
    }

    /**
     * @param producttime
     *            the producttime to set
     */
    public void setProducttime(Date producttime) {
        this.producttime = producttime;
    }

    /**
     * @return the nisolbin
     */
    public int getNisolbin() {
        return nisolbin;
    }

    /**
     * @param nisolbin
     *            the nisolbin to set
     */
    public void setNisolbin(int nisolbin) {
        this.nisolbin = nisolbin;
    }

    /**
     * @return the noutint
     */
    public int getNoutint() {
        return noutint;
    }

    /**
     * @param noutint
     *            the noutint to set
     */
    public void setNoutint(int noutint) {
        this.noutint = noutint;
    }

    /**
     * @return the noutrep
     */
    public int getNoutrep() {
        return noutrep;
    }

    /**
     * @param noutrep
     *            the noutrep to set
     */
    public void setNoutrep(int noutrep) {
        this.noutrep = noutrep;
    }

    /**
     * @return the areared
     */
    public float getAreared() {
        return areared;
    }

    /**
     * @param areared
     *            the areared to set
     */
    public void setAreared(float areared) {
        this.areared = areared;
    }

    /**
     * @return the biscanr
     */
    public float getBiscanr() {
        return biscanr;
    }

    /**
     * @param biscanr
     *            the biscanr to set
     */
    public void setBiscanr(float biscanr) {
        this.biscanr = biscanr;
    }

    /**
     * @return the nbadscan
     */
    public int getNbadscan() {
        return nbadscan;
    }

    /**
     * @param nbadscan
     *            the nbadscan to set
     */
    public void setNbadscan(int nbadscan) {
        this.nbadscan = nbadscan;
    }

    /**
     * @return the nhourout
     */
    public int getNhourout() {
        return nhourout;
    }

    /**
     * @param nhourout
     *            the nhourout to set
     */
    public void setNhourout(int nhourout) {
        this.nhourout = nhourout;
    }

    /**
     * @return the volcovpat
     */
    public int getVolcovpat() {
        return volcovpat;
    }

    /**
     * @param volcovpat
     *            the volcovpat to set
     */
    public void setVolcovpat(int volcovpat) {
        this.volcovpat = volcovpat;
    }

    /**
     * @return the opermode
     */
    public int getOpermode() {
        return opermode;
    }

    /**
     * @param opermode
     *            the opermode to set
     */
    public void setOpermode(int opermode) {
        this.opermode = opermode;
    }

    /**
     * @return the missper
     */
    public String getMissper() {
        return missper;
    }

    /**
     * @param missper
     *            the missper to set
     */
    public void setMissper(String missper) {
        this.missper = missper;
    }

    /**
     * @return the supplmess
     */
    public int getSupplmess() {
        return supplmess;
    }

    /**
     * @param supplmess
     *            the supplmess to set
     */
    public void setSupplmess(int supplmess) {
        this.supplmess = supplmess;
    }
}
