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

    @BeforeClass
    public static void setUpBeforeClass() throws Exception {
        System.loadLibrary("meteoLib");
    }

    @Test
    public final void testZToPsaInRangeLow() {
        for (float height = -1000; height < 1.0e06f; height += 12.25f) {
            float legacyResult = Controller.ztopsa(height);
            float portResult = ZToPsa.ztopsa(height);

            assertEquals("Ported ptozsa does not match legacy for height= "
                    + height, legacyResult, portResult, 0.05f);
        }
    }

    @Test
    public final void testZToPsaInRangeHigh() {
        for (float height = 1.0e06f; height < 1.0e10f; height += 2253.33f) {
            float legacyResult = Controller.ztopsa(height);
            float portResult = ZToPsa.ztopsa(height);

            assertEquals("Ported ptozsa does not match legacy for height= "
                    + height, legacyResult, portResult, 0.5f);
        }
    }

    @Test
    public void testZToPsaOutRange() {
        float result = ZToPsa.ztopsa(1.01e10f);
        assertTrue(Float.isNaN(result));
    }
}
