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
 * Converts a pressure in milibars into a height in a standard atmosphere in
 * meters.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Aug 13, 2013  #2262     dgilling     Ported from ptozsa.f.
 * 
 * </pre>
 * 
 * @author dgilling
 * @version 1.0
 */

public class PToZsa {

    private static final double Flg = 1e10;

    // Never allow this class to be directly instantiated
    private PToZsa() {
        throw new AssertionError();
    }

    /**
     * This routine converts a pressure in milibars into a height in a standard
     * atmosphere in meters.
     * 
     * @param pressure
     *            Pressure (in mb)
     * @return Height (in m)
     */
    public static float ptozsa(float pressure) {
        float PtoZsa;
        if (pressure > Flg || pressure < 1.0) {
            PtoZsa = Float.NaN;
        } else if (pressure > Constants.p11) {
            PtoZsa = (float) ((Constants.T0 - (Constants.T0 * Math.pow(
                    (pressure / Constants.p0), (1 / Constants.HGT_PRES_c1)))) / Constants.gamma);
        } else {
            PtoZsa = (float) (Constants.HGT_PRES_c2
                    * Math.log10(Constants.p11 / pressure) + Constants.z11);
        }

        return PtoZsa;
    }
}
