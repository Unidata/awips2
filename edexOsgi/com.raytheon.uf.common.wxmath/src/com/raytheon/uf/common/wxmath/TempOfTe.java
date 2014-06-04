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

import static com.raytheon.uf.common.wxmath.AdiabeticTemperature.adiabatic_te;
import static java.lang.Math.sqrt;

import java.util.Arrays;

/**
 * This routine calculates the saturation tempurature of an equivalent
 * temperature at given pressure using the adiabatic definition
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date          Ticket#  Engineer    Description
 * ------------- -------- ----------- --------------------------
 * Jun 03, 2013  2043     bsteffen    Ported from meteolib C
 * Aug 13, 2013  2262     njensen     Moved from deriv params
 * Aug 21, 2013  2289     bsteffen    Add more pressure levels to TeTable.
 *                                    Remove redundant adiabatic_te calls.
 *                                    Use binary search in Arrays class.
 *                                    Return table values when possible.
 * May 12, 2014  2289     bsteffen    Change pmin to 200 because adiabetic_te
 *                                    is not reliable for all temperatures 
 *                                    for smaller pressures.
 * 
 * </pre>
 * 
 * @author bsteffen
 * @version 1.0
 */

public class TempOfTe {

    private static final int tmin = 193;

    private static final int tmax = 333;

    private static final int nt = 1 + tmax - tmin;

    private static final int pmin = 200;

    private static final int pmax = 1000;

    private static final int pspace = 25;

    private static final int np = 1 + (pmax - pmin) / pspace;

    private static final float[][] TeTable = new float[np][nt];

    static {
        int i = 0;
        for (int p = pmin; p <= pmax; p += pspace) {
            int j = 0;
            for (int t = tmin; t <= tmax; t += 1) {
                TeTable[i][j] = (float) adiabatic_te(t, p);
                j += 1;
            }
            i += 1;
        }
    }

    public static double temp_of_te(double te, double press) {
        /* find the closest table row */
        int pIndex = (int) ((press - pmin) / pspace);
        if (pIndex < 0) {
            pIndex = 0;
        }
        if (pIndex >= np) {
            pIndex = np - 1;
        }
        float[] TeLookup = TeTable[pIndex];

        if (te < TeLookup[1]) {
            return te;
        }
        if (te >= TeLookup[nt - 1]) {
            return Double.NaN;
        }
        double base = pmin + pIndex * pspace;

        /* use table to get first guesses for value of temp */
        int indexHigh = (int) te - tmin + 1;
        if (indexHigh > nt) {
            indexHigh = nt - 1;
        }

        int indexLow = Arrays.binarySearch(TeLookup, 0, indexHigh, (float) te);
        if (indexLow >= 0) {
            if (base == press) {
                return tmin + indexLow;
            } else {
                indexHigh = indexLow + 1;
                indexLow = indexLow - 1;
            }
        } else {
            indexHigh = -(indexLow) - 1;
            indexLow = -(indexLow) - 2;
        }

        double tempLow = indexLow + tmin;
        double tempHigh = indexHigh + tmin;
        double diffLow = te - TeLookup[indexLow];
        double diffHigh = TeLookup[indexHigh] - te;

        if (base != press) {
            /* use weight to take into account pressure difference */
            double weight = sqrt(base / press);

            tempLow = (1 - weight) * TeLookup[indexLow] + weight * tempLow;
            diffLow = te - adiabatic_te(tempLow, press);

            tempHigh = (1 - weight) * TeLookup[indexHigh] + weight * tempHigh;
            diffHigh = adiabatic_te(tempHigh, press) - te;

        }

        if (diffLow < 0.01 && diffLow > -0.01) {
            return tempLow;
        }

        if (diffHigh < 0.01 && diffHigh > -0.01) {
            return tempHigh;
        }

        /* Iterate to find the exact solution */
        double weight = diffHigh / (diffLow + diffHigh);
        double temp = weight * tempLow + (1 - weight) * tempHigh;
        double diff = adiabatic_te(temp, press) - te;
        for (int i = 0; i <= 10; i += 1) {
            if (diff > 0.01) {
                diffHigh = diff;
                tempHigh = temp;
            } else if (diff < -0.01) {
                diffLow = -diff;
                tempLow = temp;
            } else {
                break;
            }
            weight = diffHigh / (diffLow + diffHigh);
            temp = weight * tempLow + (1 - weight) * tempHigh;
            diff = adiabatic_te(temp, press) - te;
        }
        return temp;
    }
}
