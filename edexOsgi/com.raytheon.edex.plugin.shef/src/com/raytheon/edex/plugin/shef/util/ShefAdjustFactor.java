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
package com.raytheon.edex.plugin.shef.util;

/**
 * SHEF adjust factor object holding the values required to adjust the shef
 * value.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Apr 28, 2014    3088    mpduff      Initial creation.
 * 
 * </pre>
 * 
 * @author mpduff
 * 
 */
public class ShefAdjustFactor {
    private double divisor = 1.0;

    private double base = 0.0;

    private double multiplier = 1.0;

    private double adder = 0.0;

    /**
     * Constructor.
     * 
     * @param divisor
     * @param base
     * @param multiplier
     * @param adder
     */
    public ShefAdjustFactor(double divisor, double base, double multiplier,
            double adder) {
        this.divisor = divisor;
        this.base = base;
        this.multiplier = multiplier;
        this.adder = adder;
    }

    /**
     * @return the divisor
     */
    public double getDivisor() {
        return divisor;
    }

    /**
     * @param divisor
     *            the divisor to set
     */
    public void setDivisor(double divisor) {
        this.divisor = divisor;
    }

    /**
     * @return the base
     */
    public double getBase() {
        return base;
    }

    /**
     * @param base
     *            the base to set
     */
    public void setBase(double base) {
        this.base = base;
    }

    /**
     * @return the multiplier
     */
    public double getMultiplier() {
        return multiplier;
    }

    /**
     * @param multiplier
     *            the multiplier to set
     */
    public void setMultiplier(double multiplier) {
        this.multiplier = multiplier;
    }

    /**
     * @return the adder
     */
    public double getAdder() {
        return adder;
    }

    /**
     * @param adder
     *            the adder to set
     */
    public void setAdder(double adder) {
        this.adder = adder;
    }
}
