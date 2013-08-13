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
import static org.junit.Assert.assertTrue;

import org.junit.BeforeClass;
import org.junit.Test;

import com.raytheon.edex.meteoLib.Controller;

/**
 * Test for {@link com.raytheon.uf.common.wxmath.CalcRH}
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Aug 14, 2013  #2262     dgilling     Initial creation
 * 
 * </pre>
 * 
 * @author dgilling
 * @version 1.0
 */

public class TestCalcRH {

    @BeforeClass
    public static void setUpBeforeClass() throws Exception {
        System.loadLibrary("meteoLib");
    }

    @Test
    public void testDegreesC() {
        float[] tempArray = new float[1];
        float[] dewptArray = new float[1];
        int iDim = 1;
        int jDim = 1;
        int nDim = 1;

        for (float temp = -10.0f; temp <= 80.0f; temp += 5.5f) {
            for (float dewpoint = 0.0f; dewpoint <= 80.0f; dewpoint += 1.25f) {
                tempArray[0] = temp;
                dewptArray[0] = dewpoint;
                float[] legacyResult = Controller.calcrh(tempArray, dewptArray,
                        nDim, iDim, jDim);
                float portResult = CalcRH.calcrh(temp, dewpoint);

                assertEquals("Ported calcrh does not match legacy for temp= "
                        + temp + ", dewpt= " + dewpoint, legacyResult[0],
                        portResult, 0.005f);
            }
        }
    }

    @Test
    public void testDegreesK() {
        float[] tempArray = new float[1];
        float[] dewptArray = new float[1];
        int iDim = 1;
        int jDim = 1;
        int nDim = 1;

        for (float temp = 263.15f; temp <= 373.15f; temp += 5.5f) {
            for (float dewpoint = 273.15f; dewpoint <= 333.15f; dewpoint += 1.25f) {
                tempArray[0] = temp;
                dewptArray[0] = dewpoint;
                float[] legacyResult = Controller.calcrh(tempArray, dewptArray,
                        nDim, iDim, jDim);
                float portResult = CalcRH.calcrh(temp, dewpoint);

                assertEquals("Ported calcrh does not match legacy for temp= "
                        + temp + ", dewpt= " + dewpoint, legacyResult[0],
                        portResult, 0.005f);
            }
        }
    }

    @Test
    public void testTempOutsideRange() {
        float temperature = 1.0e06f;
        float dewpoint = 40f;
        float portResult = CalcRH.calcrh(temperature, dewpoint);
        assertTrue(Float.isNaN(portResult));
    }

    @Test
    public void testDewPtOutsideRange() {
        float temperature = 40f;
        float dewpoint = 1.0e06f;
        float portResult = CalcRH.calcrh(temperature, dewpoint);
        assertTrue(Float.isNaN(portResult));
    }
}
