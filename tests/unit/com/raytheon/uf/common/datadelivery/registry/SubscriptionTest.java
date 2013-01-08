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
package com.raytheon.uf.common.datadelivery.registry;

import static org.hamcrest.CoreMatchers.equalTo;
import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.CoreMatchers.not;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertThat;

import java.util.Calendar;
import java.util.Date;

import org.junit.After;
import org.junit.Before;
import org.junit.Test;

import com.raytheon.uf.common.datadelivery.registry.Utils.SubscriptionStatus;
import com.raytheon.uf.common.time.util.TimeUtil;
import com.raytheon.uf.common.time.util.TimeUtilTest;

/**
 * Test {@link Subscription}.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Aug 27, 2012 0743       djohnson     Initial creation
 * Jan 02, 2012 1345       djohnson     Fix broken assertion that id matches copied object.
 * 
 * </pre>
 * 
 * @author djohnson
 * @version 1.0
 */
public class SubscriptionTest {

    @Before
    public void setUp() {
        Calendar cal = TimeUtil.newGmtCalendar();
        cal.set(Calendar.MONTH, Calendar.JANUARY);
        cal.set(Calendar.DAY_OF_MONTH, 20);
        cal.set(Calendar.YEAR, 2012);
        TimeUtilTest.freezeTime(cal.getTimeInMillis());
    }

    @After
    public void tearDown() {
        TimeUtilTest.resumeTime();
    }

    @Test
    public void testCopyConstructorSetsSpecifiedName() throws Exception {
        Subscription subscription = SubscriptionFixture.INSTANCE.get();

        Subscription copied = new Subscription(subscription, "newName");

        assertEquals("Expected the new name to be set on the subscription!",
                "newName", copied.getName());
    }

    @Test
    public void testCopyConstructorSetsValuesFromSourceSubscription()
            throws Exception {
        Subscription subscription = SubscriptionFixture.INSTANCE.get();

        Subscription copied = new Subscription(subscription, "newName");

        assertEquals(subscription.getActivePeriodEnd(),
                copied.getActivePeriodEnd());
        assertEquals(subscription.getActivePeriodStart(),
                copied.getActivePeriodStart());
        assertEquals(subscription.getCoverage(), copied.getCoverage());
        assertEquals(subscription.getDataSetName(), copied.getDataSetName());
        assertEquals(subscription.getDataSetSize(), copied.getDataSetSize());
        assertEquals(subscription.getDataSetType(), copied.getDataSetType());
        assertEquals(subscription.getDescription(), copied.getDescription());
        assertEquals(subscription.getGroupName(), copied.getGroupName());

        assertThat(copied.getId(), is(not(equalTo(subscription.getId()))));

        assertEquals(subscription.getOfficeID(), copied.getOfficeID());
        assertEquals(subscription.getPriority(), copied.getPriority());
        assertEquals(subscription.getProvider(), copied.getProvider());
        assertEquals(subscription.getStatus(), copied.getStatus());
        assertEquals(subscription.getSubscriptionEnd(),
                copied.getSubscriptionEnd());
        assertEquals(subscription.getSubscriptionStart(),
                copied.getSubscriptionStart());
        assertEquals(subscription.getSubscriptionId(),
                copied.getSubscriptionId());
        assertEquals(subscription.getTime(), copied.getTime());
        assertEquals(subscription.getUrl(), copied.getUrl());
    }

    @Test
    public void testGetStatusReturnsActiveForSubscriptionWithinActiveWindowOfCurrentYear() {
        final Date today = TimeUtil.newGmtCalendar().getTime();
        final Date fiveDaysFromNow = new Date(today.getTime()
                + (TimeUtil.MILLIS_PER_DAY * 5));

        Subscription subscription = new SubscriptionBuilder()
                .withActivePeriodStart(today)
                .withActivePeriodEnd(fiveDaysFromNow).build();

        assertThat(subscription.getStatus(),
                is(equalTo(SubscriptionStatus.ACTIVE.toString())));
    }

    @Test
    public void testGetStatusReturnsInactiveForSubscriptionOutsideActiveWindowOfCurrentYear() {
        final Date fiveDaysAgo = new Date(TimeUtil.currentTimeMillis()
                - (TimeUtil.MILLIS_PER_DAY * 5));
        final Date yesterday = new Date(TimeUtil.currentTimeMillis()
                - TimeUtil.MILLIS_PER_DAY);

        Subscription subscription = new SubscriptionBuilder()
                .withActivePeriodStart(fiveDaysAgo)
                .withActivePeriodEnd(yesterday).build();

        assertThat(subscription.getStatus(),
                is(equalTo(SubscriptionStatus.INACTIVE.toString())));
    }

    @Test
    public void testGetStatusReturnsActiveForSubscriptionWithinActiveWindowOfStoredYear() {
        Calendar cal = TimeUtil.newGmtCalendar();
        cal.set(Calendar.YEAR, 1970);

        final Date today1970 = cal.getTime();
        final Date fiveDaysFromNow1970 = new Date(today1970.getTime()
                + (TimeUtil.MILLIS_PER_DAY * 5));

        Subscription subscription = new SubscriptionBuilder()
                .withActivePeriodStart(today1970)
                .withActivePeriodEnd(fiveDaysFromNow1970).build();

        assertThat(subscription.getStatus(),
                is(equalTo(SubscriptionStatus.ACTIVE.toString())));
    }

    @Test
    public void testGetStatusReturnsInactiveForSubscriptionOutsideActiveWindowOfStoredYear() {
        Calendar cal = TimeUtil.newGmtCalendar();
        cal.set(Calendar.YEAR, 1970);
        cal.add(Calendar.DAY_OF_YEAR, -5);

        final Date fiveDaysAgo1970 = cal.getTime();
        cal.add(Calendar.DAY_OF_YEAR, 4);
        final Date yesterday1970 = cal.getTime();

        Subscription subscription = new SubscriptionBuilder()
                .withActivePeriodStart(fiveDaysAgo1970)
                .withActivePeriodEnd(yesterday1970).build();

        assertThat(subscription.getStatus(),
                is(equalTo(SubscriptionStatus.INACTIVE.toString())));
    }
}
