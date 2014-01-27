package com.raytheon.uf.common.datadelivery.service.subscription;

import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;

import org.geotools.geometry.jts.ReferencedEnvelope;
import org.junit.Test;

import com.raytheon.uf.common.datadelivery.registry.Coverage;
import com.raytheon.uf.common.datadelivery.registry.DataType;
import com.raytheon.uf.common.datadelivery.registry.ParameterFixture;
import com.raytheon.uf.common.datadelivery.registry.PointTime;
import com.raytheon.uf.common.datadelivery.registry.SiteSubscription;
import com.raytheon.uf.common.datadelivery.registry.SiteSubscriptionFixture;
import com.raytheon.uf.common.geospatial.MapUtil;
import com.raytheon.uf.edex.datadelivery.service.services.overlap.PointOverlapData;

/**
 * PointOverlapData object test class.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Oct 18, 2013   2292     mpduff      Initial creation
 * 
 * </pre>
 * 
 * @author mpduff
 * @version 1.0
 */

public class PointOverlapDataTest {
    private static final PointSubscriptionOverlapConfig FIFTY_PERCENT_MATCH_ALL = new PointSubscriptionOverlapConfig(
            50, 50, 50, SubscriptionOverlapMatchStrategy.MATCH_ALL);

    private static final PointSubscriptionOverlapConfig FIFTY_PERCENT_MATCH_ANY = new PointSubscriptionOverlapConfig(
            50, 50, 50, SubscriptionOverlapMatchStrategy.MATCH_ANY);

    private static final PointSubscriptionOverlapConfig FIFTY_PERCENT_MATCH_HALF = new PointSubscriptionOverlapConfig(
            50, 50, 50, SubscriptionOverlapMatchStrategy.AT_LEAST_HALF);

    /*
     * Duplicate subs
     */

    @Test
    public void testFiftyPercentMatchAllWithDuplicates() {
        SiteSubscription dupeSub1 = getSubscription(1);
        SiteSubscription dupeSub2 = new SiteSubscription(dupeSub1);
        dupeSub2.getCoverage().setRequestEnvelope(
                dupeSub1.getCoverage().getRequestEnvelope());
        PointOverlapData<PointTime, Coverage> od = new PointOverlapData<PointTime, Coverage>(
                dupeSub1, dupeSub2, FIFTY_PERCENT_MATCH_ALL);
        assertTrue("These should be duplicates", od.isDuplicate());
        assertTrue("These should overlap", od.isOverlapping());
    }

    @Test
    public void testFiftyPercentMatchAnyWithDuplicates() {
        SiteSubscription dupeSub1 = getSubscription(1);
        SiteSubscription dupeSub2 = new SiteSubscription(dupeSub1);
        dupeSub2.getCoverage().setRequestEnvelope(
                dupeSub1.getCoverage().getRequestEnvelope());
        PointOverlapData<PointTime, Coverage> od = new PointOverlapData<PointTime, Coverage>(
                dupeSub1, dupeSub2, FIFTY_PERCENT_MATCH_ALL);
        assertTrue("These should be duplicates", od.isDuplicate());
        assertTrue("These should overlap", od.isOverlapping());
    }

    @Test
    public void testFiftyPercentMatchHalfWithDuplicates() {
        SiteSubscription dupeSub1 = getSubscription(1);
        SiteSubscription dupeSub2 = new SiteSubscription(dupeSub1);
        dupeSub2.getCoverage().setRequestEnvelope(
                dupeSub1.getCoverage().getRequestEnvelope());
        PointOverlapData<PointTime, Coverage> od = new PointOverlapData<PointTime, Coverage>(
                dupeSub1, dupeSub2, FIFTY_PERCENT_MATCH_ALL);
        assertTrue("These should be duplicates", od.isDuplicate());
        assertTrue("These should overlap", od.isOverlapping());
    }

    /*
     * Non matching subs
     */

    @Test
    public void testFiftyPercentMatchAllWithNoMatches() {
        SiteSubscription sub1 = getSubscription(1);
        SiteSubscription sub2 = getSubscription(2);
        ReferencedEnvelope env = new ReferencedEnvelope(20, 25, 20, 25,
                MapUtil.LATLON_PROJECTION);
        sub2.getCoverage().setRequestEnvelope(env);
        sub2.getParameter().clear();
        sub2.addParameter(ParameterFixture.INSTANCE.get(4));
        PointTime time = new PointTime();
        time.setInterval(100);
        sub2.setTime(time);

        PointOverlapData<PointTime, Coverage> od = new PointOverlapData<PointTime, Coverage>(
                sub1, sub2, FIFTY_PERCENT_MATCH_ALL);
        assertFalse("These should not be duplicates", od.isDuplicate());
        assertFalse("These should not overlap", od.isOverlapping());
    }

    @Test
    public void testFiftyPercentMatchAnyWithNoMatches() {
        SiteSubscription sub1 = getSubscription(1);
        SiteSubscription sub2 = getSubscription(2);
        ReferencedEnvelope env = new ReferencedEnvelope(20, 25, 20, 25,
                MapUtil.LATLON_PROJECTION);
        sub2.getCoverage().setRequestEnvelope(env);
        sub2.getParameter().clear();
        sub2.addParameter(ParameterFixture.INSTANCE.get(4));
        PointTime time = new PointTime();
        time.setInterval(100);
        sub2.setTime(time);

        PointOverlapData<PointTime, Coverage> od = new PointOverlapData<PointTime, Coverage>(
                sub1, sub2, FIFTY_PERCENT_MATCH_ANY);
        assertFalse("These should not be duplicates", od.isDuplicate());
        assertFalse("These should not overlap", od.isOverlapping());
    }

    @Test
    public void testFiftyPercentMatchHalfWithNoMatches() {
        SiteSubscription sub1 = getSubscription(1);
        SiteSubscription sub2 = getSubscription(2);
        ReferencedEnvelope env = new ReferencedEnvelope(20, 25, 20, 25,
                MapUtil.LATLON_PROJECTION);
        sub2.getCoverage().setRequestEnvelope(env);
        sub2.getParameter().clear();
        sub2.addParameter(ParameterFixture.INSTANCE.get(4));
        PointTime time = new PointTime();
        time.setInterval(100);
        sub2.setTime(time);

        PointOverlapData<PointTime, Coverage> od = new PointOverlapData<PointTime, Coverage>(
                sub1, sub2, FIFTY_PERCENT_MATCH_HALF);
        assertFalse("These should not be duplicates", od.isDuplicate());
        assertFalse("These should not overlap", od.isOverlapping());
    }

    /*
     * Half matching subs - match parameter and interval
     */

    @Test
    public void testFiftyPercentMatchAllWithHalfMatches() {
        SiteSubscription sub1 = getSubscription(1);
        SiteSubscription sub2 = getSubscription(2);
        ReferencedEnvelope env = new ReferencedEnvelope(20, 25, 20, 25,
                MapUtil.LATLON_PROJECTION);
        sub2.getCoverage().setRequestEnvelope(env);

        PointOverlapData<PointTime, Coverage> od = new PointOverlapData<PointTime, Coverage>(
                sub1, sub2, FIFTY_PERCENT_MATCH_ALL);
        assertFalse("These should not be duplicates", od.isDuplicate());
        assertFalse("These should not overlap", od.isOverlapping());
    }

    @Test
    public void testFiftyPercentMatchAnyWithHalfMatches() {
        SiteSubscription sub1 = getSubscription(1);
        SiteSubscription sub2 = getSubscription(2);
        ReferencedEnvelope env = new ReferencedEnvelope(20, 25, 20, 25,
                MapUtil.LATLON_PROJECTION);
        sub2.getCoverage().setRequestEnvelope(env);

        PointOverlapData<PointTime, Coverage> od = new PointOverlapData<PointTime, Coverage>(
                sub1, sub2, FIFTY_PERCENT_MATCH_ANY);
        assertFalse("These should not be duplicates", od.isDuplicate());
        assertTrue("These should not overlap", od.isOverlapping());
    }

    @Test
    public void testFiftyPercentMatchHalfWithHalfMatches() {
        SiteSubscription sub1 = getSubscription(1);
        SiteSubscription sub2 = getSubscription(2);
        ReferencedEnvelope env = new ReferencedEnvelope(20, 25, 20, 25,
                MapUtil.LATLON_PROJECTION);
        sub2.getCoverage().setRequestEnvelope(env);

        PointOverlapData<PointTime, Coverage> od = new PointOverlapData<PointTime, Coverage>(
                sub1, sub2, FIFTY_PERCENT_MATCH_HALF);
        assertFalse("These should not be duplicates", od.isDuplicate());
        assertTrue("These should overlap", od.isOverlapping());
    }

    private SiteSubscription getSubscription(int seed) {
        SiteSubscription sub = SiteSubscriptionFixture.INSTANCE.get(seed,
                DataType.POINT);
        ReferencedEnvelope env = new ReferencedEnvelope(0, 15, 0, 15,
                MapUtil.LATLON_PROJECTION);

        sub.getCoverage().setRequestEnvelope(env);
        return sub;
    }

}
