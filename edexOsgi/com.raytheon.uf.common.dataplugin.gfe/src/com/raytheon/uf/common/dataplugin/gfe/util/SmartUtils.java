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
package com.raytheon.uf.common.dataplugin.gfe.util;

import jep.INumpyable;

import com.raytheon.uf.common.python.PythonNumpyFloatArray;

/**
 * Java port of python utility functions. Ported to Java to boost performance to
 * surpass python's poor looping performance.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jan 11, 2013            njensen     Initial creation
 * 
 * </pre>
 * 
 * @author njensen
 * @version 1.0
 */

public class SmartUtils {

    /**
     * Define a method to fill the specified edit area. Ported from python.
     * 
     * @param grid
     * @param gridNx
     * @param gridNy
     * @param editPointsX
     * @param editPointsY
     * @param borderPointsX
     * @param borderPointsY
     * @return
     */
    public static INumpyable fillEditArea(float[] grid, int gridNx, int gridNy,
            int[] editPointsX, int[] editPointsY, int[] borderPointsX,
            int[] borderPointsY) {
        // edit points and border points are a list of (x,y) indices
        int[] e = new int[2];
        int[] b = new int[2];

        for (int i = 0; i < editPointsX.length; i++) {
            e[0] = editPointsX[i];
            e[1] = editPointsY[i];
            double numSum = 0.0;
            double denomSum = 0.0;

            for (int k = 0; k < borderPointsX.length; k++) {
                b[0] = borderPointsX[k];
                b[1] = borderPointsY[k];

                // points in the same row, column or diagonal
                int xdiff = e[0] - b[0];
                int ydiff = e[1] - b[1];
                int absXdiff = (xdiff < 0) ? -xdiff : xdiff;
                int absYdiff = (ydiff < 0) ? -ydiff : ydiff;
                if (e[0] == b[0] || e[1] == b[1] || absXdiff == absYdiff) {

                    double xdist = xdiff;
                    double ydist = ydiff;

                    // calculate the distance to the border point
                    double dist = Math.sqrt(xdist * xdist + ydist * ydist);

                    // value = grid[b[0], b[1]]
                    float value = grid[b[0] + (gridNx * b[1])];

                    // Accumulate the distance-weighted average
                    numSum = numSum + value / dist;
                    denomSum = denomSum + 1 / dist;
                }
            }

            int eIndex = e[0] + (gridNx * e[1]);
            if (denomSum > 0.0f) {
                // grid[e[0], e[1]] = numSum / denomSum;
                grid[eIndex] = (float) (numSum / denomSum);
            } else {
                // grid[e[0], e[1]] = 0.0;
                grid[eIndex] = 0.0f;
            }
        }

        // Return completed grid
        return new PythonNumpyFloatArray(grid, gridNx, gridNy);
    }
}
