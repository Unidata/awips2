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
 * Test for {@link com.raytheon.uf.common.wxmath.PToZsa}
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

public class TestPToZsa {

    private static final File DATA_FILE = new File("./data/WxMath/PToZsa.tab");

    @Test
    public final void testPToZsaInRange() throws IOException {
        BufferedReader reader = new BufferedReader(new FileReader(DATA_FILE));
        // Discard header
        reader.readLine();
        for (String line = reader.readLine(); line != null; line = reader
                .readLine()) {
            String[] values = line.split("\t");
            float pressure = Float.parseFloat(values[0]);
            float expected = Float.parseFloat(values[1]);
            float result = PToZsa.ptozsa(pressure);
            assertEquals("PToZsa does not match expected for pressure= "
                    + pressure, expected, result, 0.005 + pressure / 100000);

        }
        reader.close();
    }

    @Test
    public void testPToZsaOutRange() {
        float result = PToZsa.ptozsa(1.01e10f);
        assertTrue("PToZsa failed upper limit check.", Float.isNaN(result));

        result = PToZsa.ptozsa(0.999f);
        assertTrue("PToZsa failed lower limit check.", Float.isNaN(result));
    }
}
