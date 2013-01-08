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
package com.raytheon.uf.common.time;

import static org.junit.Assert.assertEquals;

import java.util.Date;

import org.junit.After;
import org.junit.Test;

import com.raytheon.uf.common.util.TestUtil;

/**
 * Test {@link SimulatedTime}.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Aug 24, 2012            djohnson     Initial creation
 * 
 * </pre>
 * 
 * @author djohnson
 * @version 1.0
 */

public class SimulatedTimeTest {

    @After
    public void cleanUp() {
        SimulatedTime.getSystemTime().setRealTime();
    }

    @Test
    public void testFreezeTimeReturnsFrozenTime() throws Exception {
        SimulatedTime simulatedTime = SimulatedTime.getSystemTime();
        simulatedTime.setFrozen(true);
        Date start = simulatedTime.getTime();
        Thread.sleep(10L);
        Date end = simulatedTime.getTime();

        assertEquals("Time should have been frozen!", start, end);
    }

    @Test
    public void testUnfrozenTimeReturnsRealTime() throws Exception {
        SimulatedTime simulatedTime = SimulatedTime.getSystemTime();
        simulatedTime.setFrozen(false);
        Date start = simulatedTime.getTime();
        Thread.sleep(10L);
        Date end = simulatedTime.getTime();

        TestUtil.assertNotEquals(start, end);
    }
}
