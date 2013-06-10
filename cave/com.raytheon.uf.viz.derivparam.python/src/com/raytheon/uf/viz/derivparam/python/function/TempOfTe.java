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
package com.raytheon.uf.viz.derivparam.python.function;

import static com.raytheon.uf.viz.derivparam.python.function.AdiabeticTemperature.adiabatic_te;
import static java.lang.Math.sqrt;

/**
 * This routine calculates the saturation tempurature of an equivalent
 * temperature at given pressure using the adiabatic definition
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jun 3, 2013  2043       bsteffen    Ported from meteolib C
 * 
 * </pre>
 * 
 * @author bsteffen
 * @version 1.0
 */

public class TempOfTe {

    private static final int tmin = 193;

    private static final int tmax = 333;

    private static final int nval = 1 + tmax - tmin;

    private static final double[] Te1000 = new double[nval];

    private static final double[] Te850 = new double[nval];

    private static final double[] Te700 = new double[nval];

    private static final double[] Te600 = new double[nval];

    private static final double[] Te500 = new double[nval];

    private static final double[] Te350 = new double[nval];

    private static final double[] Te200 = new double[nval];
    static {
        int i = 0;
        for (int t = tmin; t <= tmax; t += 1) {
            Te1000[i] = adiabatic_te(t, 1000);
            Te850[i] = adiabatic_te(t, 850);
            Te700[i] = adiabatic_te(t, 700);
            Te600[i] = adiabatic_te(t, 600);
            Te500[i] = adiabatic_te(t, 500);
            Te350[i] = adiabatic_te(t, 350);
            Te200[i] = adiabatic_te(t, 200);
            i += 1;
        }
    }

    public static double temp_of_te(double te, double press) {
        double[] TeLookup = null;
        double base;
        /* find correct table, check for beyond bounds of table */
        if (press <= 250) {
            TeLookup = Te200;
            base = 200;
        } else if (press <= 400) {
            TeLookup = Te350;
            base = 350;
        } else if (press <= 550) {
            TeLookup = Te500;
            base = 500;
        } else if (press <= 650) {
            TeLookup = Te600;
            base = 600;
        } else if (press <= 750) {
            TeLookup = Te700;
            base = 700;
        } else if (press <= 900) {
            TeLookup = Te850;
            base = 850;
        } else {
            TeLookup = Te1000;
            base = 1000;
        }
        if (te < TeLookup[1]) {
            return te;
        }
        if (te >= TeLookup[nval - 1]) {
            return Double.NaN;
        }
        /* use table to get first guesses for value of temp */
        double t1 = tmin;
        double t2 = (int) te;
        if (t2 > tmax) {
            t2 = tmax;
        }
        double t;
        while (t2 - t1 >= 3) {
            t = (int) ((t1 + t2) / 2);
            if (TeLookup[(int) t - tmin] > te) {
                t2 = t;
            } else if (TeLookup[(int) t - tmin] < te) {
                t1 = t;
            } else {
                if (t1 < t - 1) {
                    t1 = t - 1;
                }
                if (t2 > t + 1) {
                    t2 = t + 1;
                }
                break;
            }
        }

        double w = sqrt(base / press);
        t1 = (1 - w) * TeLookup[(int) t1 - tmin] + w * t1;
        t2 = (1 - w) * TeLookup[(int) t2 - tmin] + w * t2;

        /* Iterate to find the exact solution */
        double d1 = te - adiabatic_te(t1, press);
        double d2 = adiabatic_te(t2, press) - te;
        w = d2 / (d1 + d2);
        t = w * t1 + (1 - w) * t2;
        double d = adiabatic_te(t, press) - te;
        int i = 0;
        while (i++ < 10) {
            if (d > 0.01) {
                d2 = d;
                t2 = t;
            } else if (d < -0.01) {
                d1 = -d;
                t1 = t;
            } else {
                break;
            }
            w = d2 / (d1 + d2);
            t = w * t1 + (1 - w) * t2;
            d = adiabatic_te(t, press) - te;
        }
        return t;
    }
}
