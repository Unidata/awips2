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

import com.raytheon.uf.common.python.PythonNumpyFloatArray;

/**
 * Calls {@link com.raytheon.uf.common.wxmath.DCapeFunc} and transforms the
 * output into an INumpyable for python.
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

public class DCapeFuncPythonAdapter {

    public static INumpyable dcapeFunc(float usetv, float[] p_dat,
            float[] t_dat, float[] td_dat, float[] p0, float[] th0,
            float[] sh0, int nx, int ny, int nz, float max_evap, float max_rh) {
        float[] result = com.raytheon.uf.common.wxmath.DCapeFunc.dcapeFunc(
                usetv, p_dat, t_dat, td_dat, p0, th0, sh0, nx, ny, nz,
                max_evap, max_rh);
        return new PythonNumpyFloatArray(result, ny, nx);
    }

}
