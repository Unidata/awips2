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
package com.raytheon.uf.common.wxmath;

import static com.raytheon.uf.common.wxmath.Constants.c0;
import static com.raytheon.uf.common.wxmath.Constants.c1;
import static com.raytheon.uf.common.wxmath.Constants.c2;
import static com.raytheon.uf.common.wxmath.Constants.c_1;
import static com.raytheon.uf.common.wxmath.Constants.c_2;
import static com.raytheon.uf.common.wxmath.Constants.f;
import static java.lang.Math.abs;
import static java.lang.Math.exp;
import static java.lang.Math.log;
import static java.lang.Math.max;
import static java.lang.Math.min;
import static java.lang.Math.sqrt;

/**
 * Routine to calculate wetbulb from temperature, and relative humidity.
 * 
 * <pre>
 * Inputs/Outputs:
 * 
 *      Variable     Var Type     I/O   Description
 *     ----------   ----------   ----- -------------
 *      p               RA         I    Pressure (mb)
 *      t               RA         I    Temperature (K)
 *      rh              RA         I    Relative humidity [range: 0. - 100.]
 *      tw              RA         O    wet-bulb temp (K)
 *   
 *   
 *   User Notes:
 * 
 *   1.  No quality control is performed in this routine.
 * </pre>
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jun 06, 2013 2043       bsteffen    Ported from meteolib fortran
 * Aug 13, 2013 2262       njensen     Moved from deriv params
 * 
 * </pre>
 * 
 * @author bsteffen
 * @version 1.0
 */

public class CalcTw {

    public static double calctw(double p, double t, double rh) {
        double rhqc = min(100.0, max(1.0, rh));
        double b = c1 * t + c2 / t - log(rhqc / 100.0);
        double td = (b - sqrt(b * b - c_1)) / c_2;
        return mytw(t, td, p);
    }

    /*
     * This function takes temperature in degrees K, dewpoint in degrees K and
     * pressure in millibars and returns the isobaric wet-bulb temperature in
     * degrees K using an iterative technique. For a given guess for the wet
     * bulb temp, one tries to do an energy balance, matching cp*(T-Tw) to
     * (esat(Tw)-esat(Td))*eps*L/p*.
     */
    public static double mytw(double k, double kd, double p) {
        // Special cases of Td >= T or a ridiculously low T.
        if (kd >= k) {
            return (k + kd) / 2;
        } else if (k < 100) {
            return k;
        }

        // Special case of a ridiculously high saturation vapor pressure.
        double ew = c0 - c1 * k - c2 / k;
        if (ew > 10.0) {
            return (k + kd) / 2;
        }
        ew = exp(ew);

        // Kw is our current guess for wet-bulb, ed the vapor pressure
        // corresponding to the depoint. Deal with case of a ridiculously small
        // dewpoint vapor pressure.
        double kdx = kd;
        double ed = c0 - c1 * kdx - c2 / kdx;
        while (ed < -50.0) {
            kdx = kdx + 10;
            ed = c0 - c1 * kdx - c2 / kdx;
        }
        ed = exp(ed);
        double fp = p * f;
        double s = (ew - ed) / (k - kdx);
        double kw = (k * fp + kdx * s) / (fp + s);

        // At each step of the iteration, esat(Tw)-esat(Td) is compared to
        // (T-Tw)*p/(eps*L). When that difference is less than one part in
        // 10000 of esat(Tw), or ten iterations have been done, the iteration
        // stops.
        // This is basically trying to find the value of Kw where de is 0. The
        // value s is the derivative of de with respect to Kw, a fairly standard
        // numerical technique for finding the zero value of a function.
        for (int l = 1; l <= 10; l += 1) {
            ew = c0 - c1 * kw - c2 / kw;
            if (ew < -50.0 || ew > 10.0) {
                break;
            }
            ew = exp(ew);
            double de = fp * (k - kw) + ed - ew;
            if (abs(de / ew) < 1e-5) {
                continue;
            }
            s = ew * (c1 - c2 / (kw * kw)) - fp;
            kw = kw - de / s;
        }
        return kw;

    }
}
