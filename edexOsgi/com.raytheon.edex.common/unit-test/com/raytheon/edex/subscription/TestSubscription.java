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

import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertSame;
import static org.junit.Assert.assertTrue;

import org.junit.After;
import org.junit.Before;
import org.junit.Test;

/**
 * JUnit Test case for the {@link com.raytheon.edex.subscription.Subscription 
 * Subscription} class. Includes the following tests:
 * <dl>
 * <dt><b>{@link #basicSubscriptionTest() basic subscription test}:</b>
 * <dd>Tests the creation of a Subscription with a single script attached.
 * <dt><b>{@link #advancedSubscriptionTest() advanced subscription test}:</b>
 * <dd>Tests ability of a single subscription to manage multiple scripts. 
 * <dt><b>{@link #addScript() test addScript()}:</b>
 * <dd>Tests basic functionality of the addScript(String,Object) and
 *     addScript(Script) methods.
 * <dt><b>{@link #getScript() test getScript()}:</b>
 * <dd>Tests the basic functionality of the getScript(String) method.
 * <dt><b>{@link #removeScript() test removeScript()}:</b>
 * <dd>Tests the basic functionality of the removeScript(String) method.
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

public class TestSubscription {
    /* test data values */
    private String dataURI;
    private String testID1;
    private String testID2;
    private String testScript1;
    private String testScript2;
    /* the test fixture */
    private Subscription subscription;

    /**
     * Initialized test data fixture.
     * 
     * @throws java.lang.Exception
     */
    @Before
    public void setUp() throws Exception {
        dataURI     = "test";
        testID1     = "subscription1";
        testID2     = "subscription2";
        testScript1 = "test script 1";
        testScript2 = "test script 2";
        subscription = new Subscription();
        subscription.setIdentifier(dataURI);
    }

    /**
     * Performs test cleanup.
     * @throws java.lang.Exception
     */
    @After
    public void tearDown() throws Exception {
        subscription = null;
    }

    /**
     * Basic test method for {@link com.raytheon.edex.subscription.Subscription#Subscription()}.
     * Verifies successful creation of a subscription from a data URI, script ID and script.
     * Note: does not use the test fixture.
     */
    @Test
    public void basicSubscriptionTest() {
        System.out.println("Testing basic Subscription creation.");
        Subscription s = new Subscription(dataURI,testID1,testScript1);
        assertSame("Checking subscription URI.",dataURI,(String)s.getIdentifier());
        assertTrue("Checking script count.", s.getCount() == 1);
        assertNotNull("Checking for non-null script",s.getScript(testID1));
        assertSame("Checking script value.",testScript1,s.getScript(testID1).getScript());
    }

    /**
     * Advanced test method for {@link com.raytheon.edex.subscription.Subscription#Subscription()}.
     * Verifies that a single subscription can contain multiple scripts, that the scripts can
     * be retrieved individually, and that the scripts can be deleted individulally.
     */
    @Test
    public void advancedSubscriptionTest() {
        System.out.println("Testing multiple script Subscription functionality.");
        subscription.addScript(testID1, testScript1);        
        subscription.addScript(testID2, testScript2);
        
        assertSame("Checking subscription URI.",dataURI,(String)subscription.getIdentifier());
        assertTrue("Checking script count.", subscription.getCount() == 2);
        assertNotNull("Checking for non-null script - script 1.",subscription.getScript(testID1));
        assertSame("Checking script value - script 1.",testScript1,subscription.getScript(testID1).getScript());
        assertNotNull("Checking for non-null script - script 2.",subscription.getScript(testID2));
        assertSame("Checking script value - script 2.",testScript2,subscription.getScript(testID2).getScript());
        subscription.removeScript(testID1);
        assertTrue("Checking script count.", subscription.getCount() == 1);
        assertNull("Checking script value.",subscription.getScript(testID1));
        subscription.removeScript(testID2);
        assertTrue("Checking script count.", subscription.getCount() == 0);
        assertNull("Checking script value.",subscription.getScript(testID2));
    }
    /**
     * Test method for {@link com.raytheon.edex.subscription.Subscription#addScript(Script)} and
     * {@link com.raytheon.edex.subscription.Subscription#addScript(String,Object)}.
     * Verifies a new script is added to a subscription. Verifies no script is added when the
     * script ID is reused.
     */
    @Test
    public void addScript() {
        System.out.println("Testing addScript(String,Object) and addScript(Script)");
        boolean r;
        r = subscription.addScript(testID1, testScript1);
        assertTrue("Checking result of adding script - script 1.",r);
        r = subscription.addScript(testID1, testScript1);
        assertFalse("Checking result of re-adding script - script 1.",r);
        assertTrue("Checking scriptcount.",subscription.getCount() == 1);
        
        Script t = new Script(testID2, testScript2);
        r = subscription.addScript(t);
        assertTrue("Checking result of adding script - script 2.",r);
        r = subscription.addScript(t);
        assertFalse("Checking result of re-adding script - script 2.",r);
        assertTrue("Checking scriptcount.",subscription.getCount() == 2);
        
    }
    /**
     * Test method for {@link com.raytheon.edex.subscription.Subscription#getScript(String)}.
     * Verifies that the requested Script is returned. Verifies that {@code} is returned
     * when the requested Script does not exist.
     *
     */
    @Test
    public void getScript() {
        System.out.println("Testing getScript(String)");
        Script t;
        t = subscription.getScript(testID1);
        assertNull("Checking Script retrieval - empty Subscription",t);
        
        subscription.addScript(testID1, testScript1);
        t = subscription.getScript(testID1);
        assertNotNull("Checking Script retrieval - script 1",t);
        assertSame("Checking script ID - script 1",testID1,t.getScriptid());
        assertSame("Checking script contents - script 1",testScript1,t.getScript());
        
        t = subscription.getScript(testID2);
        assertNull("Checking Script retrieval - invalid script ID",t);
    }
    /**
     * Test method for {@link com.raytheon.edex.subscription.Subscription#removeScript(String)}.
     *
     */
    @Test
    public void removeScript() {
        System.out.println("Testing removeScript(String)");
        boolean r;
        
        r = subscription.removeScript(testID1);
        assertFalse("Checking Script removal - empty Subscription",r);
        
        subscription.addScript(testID1, testScript1);
        assertTrue("Checking script count - prior to remove.", subscription.getCount() == 1);
        r = subscription.removeScript(testID2);
        assertTrue("Checking script count - after remove.", subscription.getCount() == 1);
        assertFalse("Checking Script removal - no such script",r);
        
        subscription.addScript(testID2, testScript2);
        assertTrue("Checking script count - prior to remove.", subscription.getCount() == 2);
        r = subscription.removeScript(testID1);
        assertTrue("Checking script count - after remove.", subscription.getCount() == 1);
        assertTrue("Checking Script removal - script exists",r);
        Script t = subscription.getScript(testID1);
        assertNull("Checking that Script has been removed",t);
    }
}
