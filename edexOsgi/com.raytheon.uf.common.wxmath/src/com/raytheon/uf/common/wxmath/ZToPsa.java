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
 * Converts a height in meters into a pressure in a standard atmosphere in
 * milibars.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Aug 13, 2013  #2262     dgilling     Ported from ztopsa.f.
 * 
 * </pre>
 * 
 * @author dgilling
 * @version 1.0
 */

public class ZToPsa {

    private static final double Flg = 1e10;

    // Never allow this class to be directly instantiated
    private ZToPsa() {
        throw new AssertionError();
    }

    /**
     * This routine converts a height in meters into a pressure in a standard
     * atmosphere in milibars.
     * 
     * @param height
     *            Height (in m)
     * @return Pressure (in mb)
     */
    public static float ztopsa(float height) {
        float ZtoPsa;
        if (height > Flg) {
            ZtoPsa = Float.NaN;
        } else if (height < Constants.z11) {
            ZtoPsa = (float) (Constants.p0 * Math.pow(
                    ((Constants.T0 - Constants.gamma * height) / Constants.T0),
                    Constants.HGT_PRES_c1));
        } else {
            ZtoPsa = (float) (Constants.p11 * Math.pow(10,
                    ((Constants.z11 - height) / Constants.HGT_PRES_c2)));
        }

        return ZtoPsa;
    }
}
