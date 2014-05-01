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

import jep.INumpyable;

/**
 * Calls {@link com.raytheon.uf.common.wxmath.CapeFunc} and transforms the
 * output into an INumpyable.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Aug 13, 2013            njensen     Initial creation
 * 
 * </pre>
 * 
 * @author njensen
 * @version 1.0
 */

public class CapeFuncPythonAdapter {

    public static class CapeCinPair implements INumpyable {

        private final int nx;

        private final int ny;

        private final float[] cape;

        private final float[] cin;

        public CapeCinPair(int nx, int ny, float[] cape, float[] cin) {
            this.nx = nx;
            this.ny = ny;
            this.cape = cape;
            this.cin = cin;
        }

        @Override
        public Object[] getNumpy() {
            return new Object[] { cape, cin };
        }

        @Override
        public int getNumpyX() {
            return nx;
        }

        @Override
        public int getNumpyY() {
            return ny;
        }

    }

    public static INumpyable capeFunc(float usetv, float[] p_dat,
            float[] tve_dat, float[] p0, float[] th0, float[] sh0, int nx,
            int ny, int nz) {
        float[][] result = com.raytheon.uf.common.wxmath.CapeFunc.capeFunc(
                usetv, p_dat, tve_dat, p0, th0, sh0, nx, ny, nz);
        return new CapeCinPair(ny, nx, result[0], result[1]);
    }

    public static INumpyable capeFuncTop(float usetv, float[] p_dat,
            float[] tve_dat, float[] p0, float[] th0, float[] sh0,
            float[] ptop, int nx, int ny, int nz) {
        float[][] result = com.raytheon.uf.common.wxmath.CapeFunc.capeFuncTop(
                usetv, p_dat, tve_dat, p0, th0, sh0, ptop, nx, ny, nz);
        return new CapeCinPair(ny, nx, result[0], result[1]);
    }

}
