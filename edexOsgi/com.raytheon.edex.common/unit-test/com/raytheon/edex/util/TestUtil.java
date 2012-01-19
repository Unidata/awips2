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
package com.raytheon.edex.util;


import static org.junit.Assert.assertTrue;

import java.util.Calendar;

import org.junit.After;
import org.junit.Before;
import org.junit.Test;

/**
 * JUnit test case for the {@link com.raytheon.edex.util.Util} class.
 * 
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date       	Ticket#		Engineer	Description
 * ------------	----------	-----------	--------------------------
 * 19Jun2007    345         MW Fegan    Initial creation. Testing limited
 *                                       to formatDate(float).
 * 
 * </pre>
 *
 * @author mfegan
 * @version 1
 */

public class TestUtil {
    private String pattern = "^\\d{4}-\\d{2}-\\d{2}_\\d{2}:\\d{2}:\\d{2}\\.\\d{1,3}$";
    /**
     * @throws java.lang.Exception
     */
    @Before
    public void setUp() throws Exception {
        System.out.println("Setup");
    }

    /**
     * @throws java.lang.Exception
     */
    @After
    public void tearDown() throws Exception {
    }
    /**
     * Test method for {@link com.raytheon.edex.util.Util#formatDate(float)}.
     * Will error out if {@code formatDate(float)} throws an exception. Will
     * fail if the date is incorrectly formatted.
     */
    @Test
    public void testFormatDate() {
        float time = Calendar.getInstance().getTimeInMillis()/1000;
        String result = Util.formatDate(time);
        assertTrue("Checking date format",result.matches(pattern));
    }
}
