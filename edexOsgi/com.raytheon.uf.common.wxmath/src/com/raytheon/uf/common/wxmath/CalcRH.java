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

/**
 * Calculate relative humidity from temperature and dewpoint.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Aug 13, 2013  #2262     dgilling     Ported from calcrh.f.
 * 
 * </pre>
 * 
 * @author dgilling
 * @version 1.0
 */

public class CalcRH {

    private static final double flg = 99998.0;

    // Never allow this class to be directly instantiated
    private CalcRH() {
        throw new AssertionError();
    }

    /**
     * Routine to calculate relative humidity from temperature and dewpoint.
     * 
     * @param temperature
     *            Temperature (in C or K)
     * @param dewpoint
     *            Dewpoint (same as temp)
     * @return Relative humidity [range: 0. - 100.]
     */
    public static float calcrh(float temperature, float dewpoint) {
        float rh;
        if (temperature > flg || dewpoint > flg) {
            rh = Float.NaN;
        } else {
            double t1 = (temperature < 80.0) ? temperature + Constants.k0
                    : temperature;
            double td1 = (temperature < 80.0) ? dewpoint + Constants.k0
                    : dewpoint;
            rh = (float) (100.0 * Math.exp(Constants.b * (t1 - td1)
                    + Constants.c / t1 - Constants.c / td1));
        }

        return rh;
    }
}
