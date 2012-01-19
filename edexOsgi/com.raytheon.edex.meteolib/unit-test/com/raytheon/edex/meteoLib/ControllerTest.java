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
package com.raytheon.edex.meteoLib;

import static org.junit.Assert.fail;

import org.junit.Test;

public class ControllerTest {
    static {
        System.loadLibrary("meteoLib");
    }

    @Test
    public void testScaleless_analysis() {

        float[] xind = { 3, 16, 16, 3 };
        float[] yind = { 3, 3, 16, 16 };
        float[] values = { 1, 2, 3, 4 };

        int nx = 20;
        int ny = 20;
        float[] grid = new float[nx * ny];

        Controller.scaleless_analysis(xind, yind, values, values.length, nx,
                ny, grid);

        for (int j = 0; j < ny; j++) {
            for (int i = 0; i < nx; i++) {
                System.out.print(String.format("%.3f ", grid[j * nx + i]));
            }
            System.out.println();
        }
        fail("Need known test inputs/outputs");
    }

    @Test
    public void testDist_filter() {

        int mnx = 20;
        int mny = 20;
        float[] input = new float[mnx * mny];
        for (int j = 0; j < mny; j++) {
            for (int i = 0; i < mnx; i++) {
                input[j * mnx + i] = (i == j ? i : 0);
            }
        }

        int nx = mnx - 10;
        int ny = mny - 10;
        int xoff = 5;
        int yoff = 5;
        float[] output = Controller.dist_filter(input, 1, mnx, xoff, yoff, nx,
                ny);

        System.out.println();
        for (int j = 0; j < mny; j++) {
            for (int i = 0; i < mnx; i++) {
                System.out.print(String.format("%.3g ", output[j * mnx + i]));
            }
            System.out.println();
        }

        nx = mnx;
        ny = mny;
        xoff = 0;
        yoff = 0;
        output = Controller.dist_filter(input, 3, mnx, xoff, yoff, nx, ny);

        System.out.println();
        for (int j = 0; j < mny; j++) {
            for (int i = 0; i < mnx; i++) {
                System.out.print(String.format("%.3g ", output[j * mnx + i]));
            }
            System.out.println();
        }
        fail("Need known test inputs/outputs");
    }
}
