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

import java.util.Arrays;

import jep.INumpyable;

import com.raytheon.uf.common.python.PythonNumpyFloatArray;

/**
 * TODO Add Description
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Aug 10, 2011            rjpeter     Initial creation
 * 
 * </pre>
 * 
 * @author rjpeter
 * @version 1.0
 */

public class DistFilter {
    private static final int MAX_WAVE_NUMBER = 15;

    private static float weights[] = null;

    private static float lastNpts = -Float.MAX_VALUE;

    /**
     * Ported from meteoLib/dist_filter.c. For our case mnx=nx since subgridding
     * is handled elsewhere.
     * 
     * @param input
     * @param npts
     * @param nx
     * @param ny
     * @param times
     * @return
     */
    public static INumpyable filter(float[] input, float npts, int nx, int ny,
            int times) {
        float[] output = new float[input.length];

        // copy the data if needed
        if (npts <= 1.0) {
            System.arraycopy(input, 0, output, 0, input.length);
            return new PythonNumpyFloatArray(output, nx, ny);
        }

        int i, j, ii, jj;
        int i1, i2, j1, j2;
        int d, n, m, dd;
        int offset;
        int dwgtx, dnx, dskip;
        int fpiMid;
        int fpi;
        int fpo;
        int wp;
        float waveno;
        float tot;

        fpo = 0;
        Arrays.fill(output, Float.NaN);

        n = (int) (npts + 0.99);
        if (n < 2) {
            n = 2;
        } else if (n > MAX_WAVE_NUMBER) {
            n = MAX_WAVE_NUMBER;
        }

        d = (n + 1) / 2;
        dd = d + d;
        m = dd + 1;
        float[] myWeights = null;

        // calculate weight table if needed.
        if (lastNpts != npts) {
            waveno = 3.14159265f / (npts);
            myWeights = new float[m * m];
            tot = 0;
            float waveVal = 0;

            for (wp = 0, j = -d; j <= d; j++) {
                for (i = -d; i <= d; i++, wp++) {
                    if (i != 0) {
                        waveVal = waveno * i;
                        myWeights[wp] = (float) Math.sin(waveVal) / (waveVal);
                    } else {
                        myWeights[wp] = 1;
                    }

                    if (j != 0) {
                        waveVal = waveno * j;
                        myWeights[wp] *= Math.sin(waveVal) / (waveVal);
                    }

                    tot += myWeights[wp];
                }
            }

            // divide by sum of the weights
            for (wp = 0, j = -d; j <= d; j++) {
                for (i = -d; i <= d; i++, wp++) {
                    myWeights[wp] /= tot;
                }
            }

            synchronized (DistFilter.class) {
                weights = myWeights;
                lastNpts = npts;
            }
        } else {
            myWeights = weights;
        }

        // loop for doing those where weights fall totally within grid
        for (int time = 0; time < times; time++) {
            dwgtx = nx - m;
            dnx = dd;
            offset = (nx * d) + d;
            fpiMid = offset;
            fpo = offset;
            for (jj = dd; jj < ny; jj++, fpiMid += dnx, fpo += dnx) {
                for (ii = dd; ii < nx; ii++, fpiMid++, fpo++) {
                    if (input[fpiMid] > 99998.0) {
                        output[fpo] = 1e37f;
                        continue;
                    }

                    output[fpo] = 0;
                    tot = 1;
                    wp = 0;
                    fpi = fpiMid - offset;

                    for (j = 0; j < m; j++, fpi += dwgtx) {
                        for (i = 0; i < m; i++, wp++, fpi++) {
                            if (input[fpi] > 99998.0) {
                                tot -= myWeights[wp];
                            } else {
                                output[fpo] += input[fpi] * myWeights[wp];
                            }
                        }
                    }

                    if (tot < 0.95) {
                        output[fpo] = 1e37f;
                    } else {
                        output[fpo] /= tot;
                    }
                } // for ii
            } // jj

            // loop for doing those where weights fall partially outside grid
            fpiMid = 0;
            fpo = 0;
            // dnx = *mnx-*nx; -- 0
            dskip = nx - dd;
            ii = nx + d;
            jj = ny + d;
            j1 = -d;
            for (j2 = d; j2 < jj; j1++, j2++) { // , fpiMid+=dnx,fpo+=dnx) dnx=0
                i1 = -d;
                for (i2 = d; i2 < ii; i1++, i2++, fpiMid++, fpo++) {
                    if (i1 == 0 && j1 >= 0 && j2 < ny) {
                        i1 += dskip;
                        i2 += dskip;
                        fpiMid += dskip;
                        fpo += dskip;
                    }

                    if (input[fpiMid] > 99998.0) {
                        output[fpo] = 1e37f;
                        continue;
                    }

                    output[fpo] = 0;
                    tot = 1;
                    wp = 0;
                    fpi = fpiMid - offset;

                    for (j = j1; j <= j2; j++, fpi += dwgtx) {
                        for (i = i1; i <= i2; i++, wp++, fpi++) {
                            if (i >= 0 && i < nx && j >= 0 && j < ny
                                    && input[fpi] < 99998.0) {
                                output[fpo] += input[fpi] * myWeights[wp];
                            } else {
                                tot -= myWeights[wp];
                            }
                        }
                    }

                    if (tot < 0.95) {
                        output[fpo] = 1e37f;
                    } else {
                        output[fpo] /= tot;
                    }
                }// for i2
            }// for j2

            // reset input in case of multiple loops over filter
            input = output;
        }

        return new PythonNumpyFloatArray(output, nx, ny);
    }
}
