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
package com.raytheon.viz.core.gl.internal.images;

import junit.framework.Assert;

import org.junit.Test;

public class GLFloatColormappedImageTest {

    @Test
    public void testConvertToFloat16() {
        float[] testInput = new float[] { 1, -2, 65504,
                (float) Math.pow(2.0, -14), 0, Float.POSITIVE_INFINITY,
                Float.NEGATIVE_INFINITY, 0.333333333333f, Float.NaN };

        short[] expected = new short[] { 0x3c00, (short) 0xc000, 0x7bff,
                0x0400, 0x0000, 0x7c00, (short) (0xfc00), 0x3555, 0x7fff };

        short[] testOutput = new short[testInput.length];
        int i = 0;
        for (float f : testInput) {
            System.out.println("TODO: Fix test class");
        }

        for (i = 0; i < testOutput.length; i++) {
            Assert.assertEquals(expected[i], testOutput[i]);
        }
    }
}
