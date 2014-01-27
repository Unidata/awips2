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
package com.raytheon.uf.edex.datadelivery.retrieval.util;

import static org.junit.Assert.assertTrue;

import java.util.ArrayList;
import java.util.Calendar;
import java.util.Date;
import java.util.List;

import org.junit.Test;

import com.raytheon.uf.common.datadelivery.registry.Subscription;
import com.raytheon.uf.common.datadelivery.registry.SubscriptionBuilder;
import com.raytheon.uf.common.time.CalendarBuilder;
import com.raytheon.uf.common.time.util.TimeUtil;

/**
 * JUnit test class for {@link SubscriptionChecker}.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Oct 29, 2013   2450     mpduff     Initial creation
 * 
 * </pre>
 * 
 * @author mpduff
 * @version 1.0
 */

public class SubscriptionCheckerTest {

    @Test
    public void checkNonExpiredSubscriptionIsNotFlaggedAsExpired() {
        CalendarBuilder calBuilder = new CalendarBuilder();

        Calendar cal = calBuilder.build();
        cal.add(Calendar.MONTH, 1);
        final Date januaryFirstTwelveZ = cal.getTime();

        // Create subscription with an end date one month from now so it isn't
        // expired
        List<Subscription> subList = new ArrayList<Subscription>();

        subList.add(new SubscriptionBuilder().withSubscriptionEnd(
                januaryFirstTwelveZ).build());

        SubscriptionChecker checker = new SubscriptionChecker();
        List<Subscription> expired = checker.getNewlyExpiredSubscriptions(
                subList, TimeUtil.MILLIS_PER_HOUR);

        assertTrue("Non-expired subscription marked as expired",
                expired.isEmpty());
    }

    @Test
    public void checkExpiredSubscriptionIsFlaggedAsExpired() {
        CalendarBuilder calBuilder = new CalendarBuilder();

        Calendar cal = calBuilder.build();
        cal.add(Calendar.MINUTE, -10);
        final Date januaryFirstTwelveZ = cal.getTime();

        // Create a subscription with an end date ten minutes in the past so it
        // is expired
        List<Subscription> subList = new ArrayList<Subscription>();

        subList.add(new SubscriptionBuilder().withSubscriptionEnd(
                januaryFirstTwelveZ).build());

        SubscriptionChecker checker = new SubscriptionChecker();
        List<Subscription> expired = checker.getNewlyExpiredSubscriptions(
                subList, TimeUtil.MILLIS_PER_HOUR);

        assertTrue("Expired subscription marked as Not Expired",
                !expired.isEmpty());
    }

    @Test
    public void checkNonEndingSubscriptionIsNotFlaggedAsExpired() {
        List<Subscription> subList = new ArrayList<Subscription>();

        subList.add(new SubscriptionBuilder().withSubscriptionEnd(null).build());

        SubscriptionChecker checker = new SubscriptionChecker();
        List<Subscription> expired = checker.getNewlyExpiredSubscriptions(
                subList, TimeUtil.MILLIS_PER_HOUR);

        assertTrue("Non-expired subscription marked as expired",
                expired.isEmpty());
    }

    @Test
    public void checkSubsNearingEndOfActivePeriodAreFlaggedAsSuch() {
        List<Subscription> subList = new ArrayList<Subscription>();

        CalendarBuilder calBuilder = new CalendarBuilder();

        Calendar cal = calBuilder.build();
        cal.add(Calendar.HOUR, 5);

        subList.add(new SubscriptionBuilder()
                .withActivePeriodEnd(cal.getTime()).build());

        SubscriptionChecker checker = new SubscriptionChecker();
        List<Subscription> subs = checker
                .getSubscriptionsNearingEndOfActivePeriod(subList);
        assertTrue("Sub nearing end of active period not flagged",
                !subs.isEmpty());
    }

    @Test
    public void checkSubsPastEndOfActivePeriodAreNotFlagged() {
        List<Subscription> subList = new ArrayList<Subscription>();

        CalendarBuilder calBuilder = new CalendarBuilder();

        Calendar cal = calBuilder.build();
        cal.add(Calendar.DAY_OF_MONTH, -5);

        subList.add(new SubscriptionBuilder()
                .withActivePeriodEnd(cal.getTime()).build());

        SubscriptionChecker checker = new SubscriptionChecker();
        List<Subscription> subs = checker
                .getSubscriptionsNearingEndOfActivePeriod(subList);
        assertTrue("Sub past end of active period flagged", subs.isEmpty());
    }

    @Test
    public void checkSubsWithNoActivePeriodAreNotFlagged() {
        List<Subscription> subList = new ArrayList<Subscription>();

        subList.add(new SubscriptionBuilder().withActivePeriodEnd(null).build());

        SubscriptionChecker checker = new SubscriptionChecker();
        List<Subscription> subs = checker
                .getSubscriptionsNearingEndOfActivePeriod(subList);
        assertTrue("Sub past end of active period flagged", subs.isEmpty());
    }
}
