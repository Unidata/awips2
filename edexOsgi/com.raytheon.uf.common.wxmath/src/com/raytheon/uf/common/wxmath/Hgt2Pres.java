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
 * Calculate pressure from height based on a standard atmosphere.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Aug 13, 2013  #2262     dgilling     Ported from hgt2pres.f.
 * 
 * </pre>
 * 
 * @author dgilling
 * @version 1.0
 */

public class Hgt2Pres {

    private static final double bad = 1e06 - 2;

    // Never allow this class to be directly instantiated
    private Hgt2Pres() {
        throw new AssertionError();
    }

    /**
     * Routine to calculate pressure from height based on a standard atmosphere.
     * 
     * @param height
     *            Height (in m)
     * @return Pressure (in mb)
     */
    public static float hgt2pres(float height) {
        float pres;
        if (height > bad) {
            pres = Float.NaN;
        } else if (height < Constants.z11) {
            pres = (float) (Constants.p0 * Math.pow(
                    ((Constants.T0 - Constants.gamma * height) / Constants.T0),
                    Constants.HGT_PRES_c1));
        } else {
            pres = (float) (Constants.p11 * Math.pow(10,
                    ((Constants.z11 - height) / Constants.HGT_PRES_c2)));
        }

        return pres;
    }
}
