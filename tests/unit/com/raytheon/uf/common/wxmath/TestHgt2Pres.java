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
 * Test for {@link com.raytheon.uf.common.wxmath.Hgt2Pres}
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

public class TestHgt2Pres {

    private static final File DATA_FILE = new File("./data/WxMath/Hgt2Pres.tab");

    @Test
    public void testHgt2PresInRange() throws IOException {
        BufferedReader reader = new BufferedReader(new FileReader(DATA_FILE));
        // Discard header
        reader.readLine();
        for (String line = reader.readLine(); line != null; line = reader
                .readLine()) {
            String[] values = line.split("\t");
            float height = Float.parseFloat(values[0]);
            float expected = Float.parseFloat(values[1]);
            float result = Hgt2Pres.hgt2pres(height);
            assertEquals("Hgt2Pres does not match expected for height= "
                    + height, expected, result, 0.05f);

        }
        reader.close();
    }

    @Test
    public void testHgt2PresOutRange() {
        float result = Hgt2Pres.hgt2pres(1.0e06f);
        assertTrue(Float.isNaN(result));
    }
}
