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

import static com.raytheon.edex.subscription.AddScriptAction.addScript;
import static com.raytheon.edex.subscription.DeleteScriptAction.deleteScript;
import static org.hamcrest.Matchers.instanceOf;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertSame;

import org.jmock.Expectations;
import org.jmock.Mockery;
import org.jmock.integration.junit4.JMock;
import org.jmock.integration.junit4.JUnit4Mockery;
import org.junit.After;
import org.junit.Before;
import org.junit.Test;
import org.junit.runner.RunWith;

import com.raytheon.edex.db.dao.ISubscriber;
import com.raytheon.edex.exception.SubscriptionException;
import com.raytheon.uf.edex.database.DataAccessLayerException;



/**
 * 
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date       	Ticket#		Engineer	Description
 * ------------	----------	-----------	--------------------------
 * 	
 * 
 * </pre>
 *
 * @author mfegan
 * @version 1
 */
@RunWith(JMock.class)
public class TestSubscriptionManager {
    Mockery context = new JUnit4Mockery();
    /* the test fixture */
    SubscriptionManager manager;
    String goodKey = "/.+?/METAR/KDBN/.+";
    String badKey = "/.+?/METAR/KQKQ/.+";
    String scriptID1 = "TEST 1";
    String scriptID2 = "TEST 2";
    StringBuffer script1 = new StringBuffer("This is test script 1");
    StringBuffer script2 = new StringBuffer("This is test script 2");
    String subNotFound = "No subscriptions exist for requested URI";
    String[] subscriptions = {goodKey};
    Subscription goodScript;
    
    /**
     * @throws java.lang.Exception
     */
    @Before
    public void setUp() throws Exception {
        manager = new SubscriptionManager();
        final ISubscriber dataLayer = context.mock(ISubscriber.class);
        goodScript = new Subscription(goodKey,scriptID1,script1);
        // expections
        context.checking(new Expectations() {{
            allowing (dataLayer).getSubscriptions();
                will(returnValue(subscriptions));
            allowing (dataLayer).getSubscription(goodKey);
                will(returnValue(goodScript));
            allowing (dataLayer).getSubscription(badKey);
                will(throwException(new DataAccessLayerException(subNotFound)));
            allowing (dataLayer).manageSubscriptions(with(instanceOf(Subscription.class)), 
                                                     with(equal(ISubscriber.SUBSCRIBE_MODE_SAVE)));
                will(addScript(goodScript,scriptID2,script2));
            allowing (dataLayer).manageSubscriptions(with(instanceOf(Subscription.class)), 
                                                     with(equal(ISubscriber.SUBSCRIBE_MODE_DELETE)));
                will(deleteScript(goodScript,scriptID2));
            allowing (dataLayer).manageSubscriptions(with(instanceOf(Subscription.class)), 
                                                     with(equal(ISubscriber.SUBSCRIBE_MODE_UPDATE)));
                will(returnValue(null));
        }});
       // manager.setDataLayer(dataLayer);
        
    }

    /**
     * @throws java.lang.Exception
     */
    @After
    public void tearDown() throws Exception {
        manager = null;
        
        
    }

    /**
     * Test method for {@link com.raytheon.edex.subscription.SubscriptionManager#subscribe(java.lang.String, java.lang.String, java.lang.Object)}.
     */
    @Test
    public void testSubscribe() throws SubscriptionException {
        manager.subscribe(goodKey, scriptID2, script2);
        Subscription s = manager.getSubscription(goodKey);
        assertNotNull("Checking Subscription retrieval - good subscription",s);
        assertEquals("Checking Subscription script count",2,s.getCount());
        Script t = s.getScript(scriptID1);
        assertSame("Checkin script 1",script1,t.getScript());
        t = s.getScript(scriptID2);
        assertSame("Checking script 2",script2,t.getScript());
    }

    /**
     * Test method for {@link com.raytheon.edex.subscription.SubscriptionManager#subscribe(java.lang.String, java.lang.String, java.lang.Object)}.
     */
    @Test(expected=SubscriptionException.class)
    public void testSubscribe_bad() throws SubscriptionException {
        manager.subscribe(goodKey, scriptID1, script1);
    }
    
    /**
     * Test method for {@link com.raytheon.edex.subscription.SubscriptionManager#getSubscription(java.lang.String)}.
     */
    @Test
    public void testGetSubscription() {
        Subscription s = manager.getSubscription(goodKey);
        assertNotNull("Checking Subscription retrieval - good subscription",s);
        assertEquals("Checking Subscription script count",1,s.getCount());
        
        
    }
    /**
     * Test method for {@link com.raytheon.edex.subscription.SubscriptionManager#getSubscription(java.lang.String)}.
     */
    @Test
    public void testGetSubscription_error() {
        Subscription s = manager.getSubscription(badKey);
        assertNull("Checking Subscription retrieval - bad subscription",s);
    }
}
