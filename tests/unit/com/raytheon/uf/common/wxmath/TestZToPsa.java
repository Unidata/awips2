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

import java.io.BufferedReader;
import java.io.File;
import java.io.FileReader;
import java.io.IOException;

import org.junit.Test;

/**
 * Test for {@link com.raytheon.uf.common.wxmath.ZToPsa}
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

public class TestZToPsa {

    private static final File DATA_FILE = new File("./data/WxMath/ZToPsa.tab");

    @Test
    public final void testZToPsaInRange() throws IOException {
        BufferedReader reader = new BufferedReader(new FileReader(DATA_FILE));
        // Discard header
        reader.readLine();
        for (String line = reader.readLine(); line != null; line = reader
                .readLine()) {
            String[] values = line.split("\t");
            float height = Float.parseFloat(values[0]);
            float expected = Float.parseFloat(values[1]);
            float result = ZToPsa.ztopsa(height);
            assertEquals(
                    "ZToPsa does not match expected for height= " + height,
                    expected, result, 0.05f);

        }
        reader.close();
    }

    @Test
    public void testZToPsaOutRange() {
        float result = ZToPsa.ztopsa(1.01e10f);
        assertTrue(Float.isNaN(result));
    }
}
