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

import static org.junit.Assert.assertEquals;

import org.junit.Test;

/**
 * Tests the Scaleless Analysis function.
 * 
 * Note that the inputs were generated using a random number generator and then
 * thrown against the original compiled C to get the expected outputs. No idea
 * if the inputs would be considered valid values that are normally passed into
 * scaleless_analysis.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Aug 16, 2013            njensen     Initial creation
 * 
 * </pre>
 * 
 * @author njensen
 * @version 1.0
 */

public class TestScaleless {

    private static float[] randomXind = { 23, 34, 8, 48, 33, 40, 32, 24, 41,
            12, 9, 4, 2, 36, 30, 45, 5, 18, 1, 27, 13, 15, 25, 11, 0, 39, 37,
            17, 43, 49, 26, 6, 31, 22, 14, 7, 16, 3, 29, 20, 44, 46, 19, 10,
            21, 38, 35, 47, 28, 42 };

    private static float[] randomYind = { 45, 38, 24, 48, 32, 46, 8, 9, 4, 27,
            47, 41, 17, 1, 13, 28, 31, 34, 14, 29, 42, 21, 6, 19, 20, 40, 44,
            49, 15, 16, 11, 23, 7, 39, 2, 0, 25, 26, 43, 12, 36, 30, 33, 37,
            22, 5, 18, 3, 10, 35 };

    private static float[] randomValues = { 73, 70, 99, 28, 48, 52, 58, 98, 72,
            14, 34, 38, 12, 21, 66, 82, 86, 41, 62, 40, 0, 11, 56, 57, 95, 15,
            78, 9, 39, 27, 71, 61, 63, 8, 7, 84, 50, 44, 54, 91, 67, 37, 30,
            59, 2, 96, 53, 26, 32, 5 };

    private static float[] randomExpected = { 59.24f, 58.61f, 57.02f, 56.65f,
            56.23f, 60.07f, 59.44f, 58.24f, 56.93f, 54.91f, 61.86f, 60.63f,
            59.08f, 57.93f, 55.30f, 63.19f, 62.30f, 60.40f, 59.55f, 55.34f,
            65.30f, 64.34f, 63.38f, 63.61f, 63.54f, 67.14f, 66.15f, 65.96f,
            67.74f, 72.88f, 67.91f, 67.48f, 68.24f, 69.68f, 71.17f, 72.22f,
            70.04f, 69.74f, 68.91f, 69.15f, 79.85f, 73.60f, 70.69f, 69.57f,
            67.02f, 77.09f, 74.62f, 72.06f, 71.51f, 69.78f };

    @Test
    public void testRandomlyGeneratedData() {
        int nx = 5;
        int ny = 10;
        int nv = nx * ny;

        float[] grid = ScalelessAnalysis.scaleless_analysis(randomXind,
                randomYind, randomValues, nv, nx, ny);
        for (int k = 0; k < nv; k++) {
            assertEquals(randomExpected[k], grid[k], 0.01f);
        }
    }

}
