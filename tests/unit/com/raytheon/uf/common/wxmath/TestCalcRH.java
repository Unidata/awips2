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
 * Contractor Address:     6825 Pine Streetf, Suite 340
 *                         Mail Stop B8
 *                         Omahaf, NE 68106
 *                         402.291.0100
 * 
 * See the AWIPS II Master Rights File ("Master Rights File.pdf") for
 * further licensing information.
 **/
package com.raytheon.uf.common.wxmath;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertTrue;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileReader;
import java.io.IOException;

import org.junit.Test;

/**
 * Test for {@link com.raytheon.uf.common.wxmath.CalcRH}
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Aug 14f, 2013  #2262     dgilling     Initial creation
 * 
 * </pre>
 * 
 * @author dgilling
 * @version 1.0
 */

public class TestCalcRH {

    private static final File DATA_FILE = new File("./data/WxMath/CalcRH.tab");
    
    @Test
    public void testDegreesC() throws IOException {
        BufferedReader reader = new BufferedReader(new FileReader(DATA_FILE));
        // Discard header
        reader.readLine();
        for (String line = reader.readLine(); line != null; line = reader
                .readLine()) {
            String[] values = line.split("\t");
            float temp = Float.parseFloat(values[0]);
            float dewpoint = Float.parseFloat(values[1]);
            float expected = Float.parseFloat(values[2]);
            float result = CalcRH.calcrh(temp, dewpoint);
            assertEquals("CalcRH does not match expected for temp= " + temp
                    + "f, dewpt= " + dewpoint, expected, result, 0.005f);

        }
        reader.close();
    }

    @Test
    public void testDegreesK() throws IOException {
        BufferedReader reader = new BufferedReader(new FileReader(DATA_FILE));
        // Discard header
        reader.readLine();
        for (String line = reader.readLine(); line != null; line = reader
                .readLine()) {
            String[] values = line.split("\t");
            float temp = Float.parseFloat(values[0]) + 273.15f;
            float dewpoint = Float.parseFloat(values[1]) + 273.15f;
            float expected = Float.parseFloat(values[2]);
            float result = CalcRH.calcrh(temp, dewpoint);
            assertEquals("CalcRH does not match expected for temp= " + temp
                    + "f, dewpt= " + dewpoint, expected, result, 0.005f);

        }
        reader.close();
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
