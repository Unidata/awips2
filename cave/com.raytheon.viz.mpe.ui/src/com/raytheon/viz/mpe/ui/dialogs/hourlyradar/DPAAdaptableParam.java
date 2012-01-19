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

/**
 * Adaptable Parameter Data Object.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Aug 5, 2009            mpduff     Initial creation
 * 
 * </pre>
 * 
 * @author mpduff
 * @version 1.0
 */

public class DPAAdaptableParam {
    private float min_reflth;

    private float max_reflth;

    private float ref_tltest;

    private float rng_tltin;

    private float rng_tltout;

    private float max_birng;

    private float min_birng;

    private float min_echoar;

    private float min_awrefl;

    private float max_pctred;

    private float mlt_zrcoef;

    private float pwr_zrcoef;

    private float min_zrefl;

    private float max_zrefl;

    private float max_stmspd;

    private float max_timdif;

    private float min_artcon;

    private float tim_p1cont;

    private float tim_p2cont;

    private float max_ecarch;

    private float rng_cutoff;

    private float rng_e1coef;

    private float rng_e2coef;

    private float rng_e3coef;

    private float min_prate;

    private float max_prate;

    private float tim_restrt;

    private float max_timint;

    private float min_timprd;

    private float thr_hlyout;

    private float end_timgag;

    private float max_prdval;

    private float max_hlyval;

    private float tim_biest;

    private float thr_nosets;

    private float res_bias;

    private float longest_lag;

    private String bias_applied;

    /**
     * @return the min_reflth
     */
    public float getMin_reflth() {
        return min_reflth;
    }

    /**
     * @param min_reflth
     *            the min_reflth to set
     */
    public void setMin_reflth(float min_reflth) {
        this.min_reflth = min_reflth;
    }

    /**
     * @return the max_reflth
     */
    public float getMax_reflth() {
        return max_reflth;
    }

    /**
     * @param max_reflth
     *            the max_reflth to set
     */
    public void setMax_reflth(float max_reflth) {
        this.max_reflth = max_reflth;
    }

    /**
     * @return the ref_tltest
     */
    public float getRef_tltest() {
        return ref_tltest;
    }

    /**
     * @param ref_tltest
     *            the ref_tltest to set
     */
    public void setRef_tltest(float ref_tltest) {
        this.ref_tltest = ref_tltest;
    }

    /**
     * @return the rng_tltin
     */
    public float getRng_tltin() {
        return rng_tltin;
    }

    /**
     * @param rng_tltin
     *            the rng_tltin to set
     */
    public void setRng_tltin(float rng_tltin) {
        this.rng_tltin = rng_tltin;
    }

    /**
     * @return the rng_tltout
     */
    public float getRng_tltout() {
        return rng_tltout;
    }

    /**
     * @param rng_tltout
     *            the rng_tltout to set
     */
    public void setRng_tltout(float rng_tltout) {
        this.rng_tltout = rng_tltout;
    }

    /**
     * @return the max_birng
     */
    public float getMax_birng() {
        return max_birng;
    }

    /**
     * @param max_birng
     *            the max_birng to set
     */
    public void setMax_birng(float max_birng) {
        this.max_birng = max_birng;
    }

    /**
     * @return the min_birng
     */
    public float getMin_birng() {
        return min_birng;
    }

    /**
     * @param min_birng
     *            the min_birng to set
     */
    public void setMin_birng(float min_birng) {
        this.min_birng = min_birng;
    }

    /**
     * @return the min_echoar
     */
    public float getMin_echoar() {
        return min_echoar;
    }

    /**
     * @param min_echoar
     *            the min_echoar to set
     */
    public void setMin_echoar(float min_echoar) {
        this.min_echoar = min_echoar;
    }

    /**
     * @return the min_awrefl
     */
    public float getMin_awrefl() {
        return min_awrefl;
    }

    /**
     * @param min_awrefl
     *            the min_awrefl to set
     */
    public void setMin_awrefl(float min_awrefl) {
        this.min_awrefl = min_awrefl;
    }

    /**
     * @return the max_pctred
     */
    public float getMax_pctred() {
        return max_pctred;
    }

    /**
     * @param max_pctred
     *            the max_pctred to set
     */
    public void setMax_pctred(float max_pctred) {
        this.max_pctred = max_pctred;
    }

    /**
     * @return the mlt_zrcoef
     */
    public float getMlt_zrcoef() {
        return mlt_zrcoef;
    }

    /**
     * @param mlt_zrcoef
     *            the mlt_zrcoef to set
     */
    public void setMlt_zrcoef(float mlt_zrcoef) {
        this.mlt_zrcoef = mlt_zrcoef;
    }

    /**
     * @return the pwr_zrcoef
     */
    public float getPwr_zrcoef() {
        return pwr_zrcoef;
    }

    /**
     * @param pwr_zrcoef
     *            the pwr_zrcoef to set
     */
    public void setPwr_zrcoef(float pwr_zrcoef) {
        this.pwr_zrcoef = pwr_zrcoef;
    }

    /**
     * @return the min_zrefl
     */
    public float getMin_zrefl() {
        return min_zrefl;
    }

    /**
     * @param min_zrefl
     *            the min_zrefl to set
     */
    public void setMin_zrefl(float min_zrefl) {
        this.min_zrefl = min_zrefl;
    }

    /**
     * @return the max_zrefl
     */
    public float getMax_zrefl() {
        return max_zrefl;
    }

    /**
     * @param max_zrefl
     *            the max_zrefl to set
     */
    public void setMax_zrefl(float max_zrefl) {
        this.max_zrefl = max_zrefl;
    }

    /**
     * @return the max_stmspd
     */
    public float getMax_stmspd() {
        return max_stmspd;
    }

    /**
     * @param max_stmspd
     *            the max_stmspd to set
     */
    public void setMax_stmspd(float max_stmspd) {
        this.max_stmspd = max_stmspd;
    }

    /**
     * @return the max_timdif
     */
    public float getMax_timdif() {
        return max_timdif;
    }

    /**
     * @param max_timdif
     *            the max_timdif to set
     */
    public void setMax_timdif(float max_timdif) {
        this.max_timdif = max_timdif;
    }

    /**
     * @return the min_artcon
     */
    public float getMin_artcon() {
        return min_artcon;
    }

    /**
     * @param min_artcon
     *            the min_artcon to set
     */
    public void setMin_artcon(float min_artcon) {
        this.min_artcon = min_artcon;
    }

    /**
     * @return the tim_p1cont
     */
    public float getTim_p1cont() {
        return tim_p1cont;
    }

    /**
     * @param tim_p1cont
     *            the tim_p1cont to set
     */
    public void setTim_p1cont(float tim_p1cont) {
        this.tim_p1cont = tim_p1cont;
    }

    /**
     * @return the tim_p2cont
     */
    public float getTim_p2cont() {
        return tim_p2cont;
    }

    /**
     * @param tim_p2cont
     *            the tim_p2cont to set
     */
    public void setTim_p2cont(float tim_p2cont) {
        this.tim_p2cont = tim_p2cont;
    }

    /**
     * @return the max_ecarch
     */
    public float getMax_ecarch() {
        return max_ecarch;
    }

    /**
     * @param max_ecarch
     *            the max_ecarch to set
     */
    public void setMax_ecarch(float max_ecarch) {
        this.max_ecarch = max_ecarch;
    }

    /**
     * @return the rng_cutoff
     */
    public float getRng_cutoff() {
        return rng_cutoff;
    }

    /**
     * @param rng_cutoff
     *            the rng_cutoff to set
     */
    public void setRng_cutoff(float rng_cutoff) {
        this.rng_cutoff = rng_cutoff;
    }

    /**
     * @return the rng_e1coef
     */
    public float getRng_e1coef() {
        return rng_e1coef;
    }

    /**
     * @param rng_e1coef
     *            the rng_e1coef to set
     */
    public void setRng_e1coef(float rng_e1coef) {
        this.rng_e1coef = rng_e1coef;
    }

    /**
     * @return the rng_e2coef
     */
    public float getRng_e2coef() {
        return rng_e2coef;
    }

    /**
     * @param rng_e2coef
     *            the rng_e2coef to set
     */
    public void setRng_e2coef(float rng_e2coef) {
        this.rng_e2coef = rng_e2coef;
    }

    /**
     * @return the rng_e3coef
     */
    public float getRng_e3coef() {
        return rng_e3coef;
    }

    /**
     * @param rng_e3coef
     *            the rng_e3coef to set
     */
    public void setRng_e3coef(float rng_e3coef) {
        this.rng_e3coef = rng_e3coef;
    }

    /**
     * @return the min_prate
     */
    public float getMin_prate() {
        return min_prate;
    }

    /**
     * @param min_prate
     *            the min_prate to set
     */
    public void setMin_prate(float min_prate) {
        this.min_prate = min_prate;
    }

    /**
     * @return the max_prate
     */
    public float getMax_prate() {
        return max_prate;
    }

    /**
     * @param max_prate
     *            the max_prate to set
     */
    public void setMax_prate(float max_prate) {
        this.max_prate = max_prate;
    }

    /**
     * @return the tim_restrt
     */
    public float getTim_restrt() {
        return tim_restrt;
    }

    /**
     * @param tim_restrt
     *            the tim_restrt to set
     */
    public void setTim_restrt(float tim_restrt) {
        this.tim_restrt = tim_restrt;
    }

    /**
     * @return the max_timint
     */
    public float getMax_timint() {
        return max_timint;
    }

    /**
     * @param max_timint
     *            the max_timint to set
     */
    public void setMax_timint(float max_timint) {
        this.max_timint = max_timint;
    }

    /**
     * @return the min_timprd
     */
    public float getMin_timprd() {
        return min_timprd;
    }

    /**
     * @param min_timprd
     *            the min_timprd to set
     */
    public void setMin_timprd(float min_timprd) {
        this.min_timprd = min_timprd;
    }

    /**
     * @return the thr_hlyout
     */
    public float getThr_hlyout() {
        return thr_hlyout;
    }

    /**
     * @param thr_hlyout
     *            the thr_hlyout to set
     */
    public void setThr_hlyout(float thr_hlyout) {
        this.thr_hlyout = thr_hlyout;
    }

    /**
     * @return the end_timgag
     */
    public float getEnd_timgag() {
        return end_timgag;
    }

    /**
     * @param end_timgag
     *            the end_timgag to set
     */
    public void setEnd_timgag(float end_timgag) {
        this.end_timgag = end_timgag;
    }

    /**
     * @return the max_prdval
     */
    public float getMax_prdval() {
        return max_prdval;
    }

    /**
     * @param max_prdval
     *            the max_prdval to set
     */
    public void setMax_prdval(float max_prdval) {
        this.max_prdval = max_prdval;
    }

    /**
     * @return the max_hlyval
     */
    public float getMax_hlyval() {
        return max_hlyval;
    }

    /**
     * @param max_hlyval
     *            the max_hlyval to set
     */
    public void setMax_hlyval(float max_hlyval) {
        this.max_hlyval = max_hlyval;
    }

    /**
     * @return the tim_biest
     */
    public float getTim_biest() {
        return tim_biest;
    }

    /**
     * @param tim_biest
     *            the tim_biest to set
     */
    public void setTim_biest(float tim_biest) {
        this.tim_biest = tim_biest;
    }

    /**
     * @return the thr_nosets
     */
    public float getThr_nosets() {
        return thr_nosets;
    }

    /**
     * @param thr_nosets
     *            the thr_nosets to set
     */
    public void setThr_nosets(float thr_nosets) {
        this.thr_nosets = thr_nosets;
    }

    /**
     * @return the res_bias
     */
    public float getRes_bias() {
        return res_bias;
    }

    /**
     * @param res_bias
     *            the res_bias to set
     */
    public void setRes_bias(float res_bias) {
        this.res_bias = res_bias;
    }

    /**
     * @return the longest_lag
     */
    public float getLongest_lag() {
        return longest_lag;
    }

    /**
     * @param longest_lag
     *            the longest_lag to set
     */
    public void setLongest_lag(float longest_lag) {
        this.longest_lag = longest_lag;
    }

    /**
     * @return the bias_applied
     */
    public String getBias_applied() {
        return bias_applied;
    }

    /**
     * @param bias_applied
     *            the bias_applied to set
     */
    public void setBias_applied(String bias_applied) {
        this.bias_applied = bias_applied;
    }

}
