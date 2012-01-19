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
package com.raytheon.edex.subscription;

import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertSame;

import org.junit.After;
import org.junit.Before;
import org.junit.Test;

/**
 * JUnit Test case for the {@link com.raytheon.edex.subscription.Script Script} 
 * class. Includes the following tests:
 * <dl>
 * <dt><b>{@link #basicScriptTest() basic Script test}:</b>
 * <dd>Tests the creation of a Script.
 * </dl>
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date       	Ticket#		Engineer	Description
 * ------------	----------	-----------	--------------------------
 * 02May2007    266         MW Fegan    Initial creation.
 * 
 * </pre>
 *
 * @author mfegan
 * @version 1
 */

public class TestScript {
    String scriptID;
    String script;
    /**
     * Initialized test data set.
     * 
     * @throws java.lang.Exception
     */
    @Before
    public void setUp() throws Exception {
        scriptID = "test";
        script = "This is a test.";
    }

    /**
     * Performs test cleanup.
     * 
     * @throws java.lang.Exception
     */
    @After
    public void tearDown() throws Exception {
        /* intentionally empty */
    }

    /**
     * Test method for {@link com.raytheon.edex.subscription.Script#Script(java.lang.String, java.lang.Object)}.
     * Verifies basic creation of a Script.
     */
    @Test
    public void basicScriptTest() {
        System.out.println("Testing basic Script creation.");
        Script s = new Script(scriptID,script);
        assertNotNull("Checking object creation",s);
        assertSame("Checking script ID",scriptID,s.getScriptid());
        assertSame("Schecking script",script,s.getScript());
    }

}
