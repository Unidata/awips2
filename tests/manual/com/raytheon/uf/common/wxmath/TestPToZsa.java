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

    @BeforeClass
    public static void setUpBeforeClass() throws Exception {
        System.loadLibrary("meteoLib");
    }

    @Test
    public final void testPToZsaInRangeLow() {
        for (float pressure = 1.0f; pressure <= 1e05f; pressure += 12.25f) {
            float legacyResult = Controller.ptozsa(pressure);
            float portResult = PToZsa.ptozsa(pressure);

            assertEquals("Ported ptozsa does not match legacy for pressure= "
                    + pressure, legacyResult, portResult, 0.05f);
        }
    }

    @Test
    public final void testPToZsaInRangeHigh() {
        for (float pressure = 1e05f; pressure <= 1e10f; pressure += 2253.33f) {
            float legacyResult = Controller.ptozsa(pressure);
            float portResult = PToZsa.ptozsa(pressure);

            assertEquals("Ported ptozsa does not match legacy for pressure= "
                    + pressure, legacyResult, portResult, 0.5f);
        }
    }

    @Test
    public void testPToZsaOutRange() {
        float result = PToZsa.ptozsa(1.01e10f);
        assertTrue("PToZsa failed upper limit check.", Float.isNaN(result));

        result = PToZsa.ptozsa(0.999f);
        assertTrue("PToZsa failed lower limit check.", Float.isNaN(result));
    }
}
