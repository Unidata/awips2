package com.raytheon.uf.edex.datadelivery.service.services.overlap;

import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;

import java.util.ArrayList;
import java.util.List;

import org.geotools.geometry.jts.ReferencedEnvelope;
import org.junit.BeforeClass;
import org.junit.Test;

import com.raytheon.uf.common.datadelivery.registry.Coverage;
import com.raytheon.uf.common.datadelivery.registry.DataType;
import com.raytheon.uf.common.datadelivery.registry.Parameter;
import com.raytheon.uf.common.datadelivery.registry.ParameterFixture;
import com.raytheon.uf.common.datadelivery.registry.PointTime;
import com.raytheon.uf.common.datadelivery.registry.PointTimeFixture;
import com.raytheon.uf.common.datadelivery.registry.SiteSubscription;
import com.raytheon.uf.common.datadelivery.registry.SiteSubscriptionFixture;
import com.raytheon.uf.common.datadelivery.registry.Time;
import com.raytheon.uf.common.datadelivery.service.subscription.PointSubscriptionOverlapConfig;
import com.raytheon.uf.common.datadelivery.service.subscription.SubscriptionOverlapMatchStrategy;
import com.raytheon.uf.common.geospatial.MapUtil;
import com.vividsolutions.jts.geom.Envelope;

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
 * Feb 13, 2014   2386     bgonzale    Added test cases to match ticket 2771 test procedures.
 *                                     Fixed areaLessThan50PercentOverlap bounds.
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

    private static SiteSubscription matchPoints;

    private static Envelope areaMatchPoints;

    private static Envelope areaLessThan50PercentOverlap;

    private static Envelope areaGreaterThan50PercentOverlap;

    private static Envelope areaWithNoOverlap;

    @BeforeClass
    public static void setup() {
        areaMatchPoints = new Envelope(0, 10, 0, 20);
        areaLessThan50PercentOverlap = new Envelope(5, 25, 0, 15);
        areaGreaterThan50PercentOverlap = new Envelope(0, 10, 10, 25);
        areaWithNoOverlap = new Envelope(0, 30, 20, 20);

        matchPoints = getSubscription(1, areaMatchPoints, createPointTime(30));
    }

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

    @Test
    public void testMatchAllNotEnoughSpatialOverlap() {
        int parameter = 100;
        int spatial = 50;
        int time = 100;
        final PointSubscriptionOverlapConfig overlap = new PointSubscriptionOverlapConfig(
                parameter, time, spatial,
                SubscriptionOverlapMatchStrategy.MATCH_ALL);
        SiteSubscription sub2 = getSubscription(1,
                areaLessThan50PercentOverlap, createPointTime(30));

        PointOverlapData<PointTime, Coverage> overlapResult = new PointOverlapData<PointTime, Coverage>(
                matchPoints, sub2, overlap);
        assertFalse("These should not be duplicates",
                overlapResult.isDuplicate());
        assertFalse("These should not overlap", overlapResult.isOverlapping());
    }

    @Test
    public void testMatchAllSpatialOverlap() {
        int parameter = 100;
        int spatial = 50;
        int time = 100;
        int cycles = 100;
        final PointSubscriptionOverlapConfig overlap = new PointSubscriptionOverlapConfig(
                parameter, time, spatial,
                SubscriptionOverlapMatchStrategy.MATCH_ALL);
        SiteSubscription sub3 = getSubscription(1,
                areaGreaterThan50PercentOverlap, createPointTime(30));

        PointOverlapData<PointTime, Coverage> overlapResult = new PointOverlapData<PointTime, Coverage>(
                matchPoints, sub3, overlap);
        assertFalse("These should not be duplicates",
                overlapResult.isDuplicate());
        assertTrue("These should overlap", overlapResult.isOverlapping());
    }

    @Test
    public void testMatchAnyParametersOnly() {
        int parameter = 100;
        int spatial = 100;
        int time = 100;
        final PointSubscriptionOverlapConfig overlap = new PointSubscriptionOverlapConfig(
                parameter, time, spatial,
                SubscriptionOverlapMatchStrategy.MATCH_ANY);

        SiteSubscription sub2 = getSubscription(1, areaWithNoOverlap,
                createPointTime(5));

        PointOverlapData<PointTime, Coverage> overlapResult = new PointOverlapData<PointTime, Coverage>(
                matchPoints, sub2, overlap);
        assertFalse("These should not be duplicates",
                overlapResult.isDuplicate());
        assertTrue("These should overlap", overlapResult.isOverlapping());
        assertFalse("These should not overlap",
                overlapResult.timeDuplicationPass);
        assertFalse("These should not overlap", overlapResult.spatialPass);
    }

    @Test
    public void testMatchAtLeastHalfParametersAndSpatial() {
        int parameter = 100;
        int spatial = 50;
        int time = 100;
        final PointSubscriptionOverlapConfig overlap = new PointSubscriptionOverlapConfig(
                parameter, time, spatial,
                SubscriptionOverlapMatchStrategy.AT_LEAST_HALF);

        SiteSubscription sub2 = getSubscription(1, areaMatchPoints,
                createPointTime(5));

        PointOverlapData<PointTime, Coverage> overlapResult = new PointOverlapData<PointTime, Coverage>(
                matchPoints, sub2, overlap);
        assertFalse("These should not be duplicates",
                overlapResult.isDuplicate());
        assertTrue("These should overlap", overlapResult.isOverlapping());
        assertFalse("These should not overlap",
                overlapResult.timeDuplicationPass);
        assertTrue("These should overlap", overlapResult.spatialPass);
    }

    @Test
    public void testMatchAtLeastHalfSpatialAndTime() {
        int parameter = 100;
        int spatial = 50;
        int time = 100;
        final PointSubscriptionOverlapConfig overlap = new PointSubscriptionOverlapConfig(
                parameter, time, spatial,
                SubscriptionOverlapMatchStrategy.AT_LEAST_HALF);

        SiteSubscription sub2 = getSubscription(1,
                areaGreaterThan50PercentOverlap, createPointTime(30));

        PointOverlapData<PointTime, Coverage> overlapResult = new PointOverlapData<PointTime, Coverage>(
                matchPoints, sub2, overlap);
        assertFalse("These should not be duplicates",
                overlapResult.isDuplicate());
        assertTrue("These should overlap", overlapResult.isOverlapping());
        assertTrue("These should overlap", overlapResult.timeDuplicationPass);
        assertTrue("These should overlap", overlapResult.spatialPass);
    }

    @Test
    public void testMatchAtLeastHalfParametersAndTime() {
        int parameter = 100;
        int spatial = 50;
        int time = 100;
        final PointSubscriptionOverlapConfig overlap = new PointSubscriptionOverlapConfig(
                parameter, time, spatial,
                SubscriptionOverlapMatchStrategy.AT_LEAST_HALF);

        SiteSubscription sub2 = getSubscription(1, areaWithNoOverlap,
                createPointTime(30));

        PointOverlapData<PointTime, Coverage> overlapResult = new PointOverlapData<PointTime, Coverage>(
                matchPoints, sub2, overlap);
        assertFalse("These should not be duplicates",
                overlapResult.isDuplicate());
        assertTrue("These should overlap", overlapResult.isOverlapping());
        assertTrue("These should overlap", overlapResult.timeDuplicationPass);
        assertFalse("These should not overlap", overlapResult.spatialPass);
    }

    @Test
    public void testMatchAtLeastHalfOneMatch() {
        int parameter = 100;
        int spatial = 50;
        int time = 100;
        final PointSubscriptionOverlapConfig overlap = new PointSubscriptionOverlapConfig(
                parameter, time, spatial,
                SubscriptionOverlapMatchStrategy.AT_LEAST_HALF);

        SiteSubscription sub2 = getSubscription(1, areaWithNoOverlap,
                createPointTime(10));

        PointOverlapData<PointTime, Coverage> overlapResult = new PointOverlapData<PointTime, Coverage>(
                matchPoints, sub2, overlap);
        assertFalse("These should not be duplicates",
                overlapResult.isDuplicate());
        assertFalse("These should not overlap", overlapResult.isOverlapping());
        assertFalse("These should not overlap",
                overlapResult.timeDuplicationPass);
        assertFalse("These should not overlap", overlapResult.spatialPass);
    }

    private static Time createPointTime(int interval) {
        PointTime time = PointTimeFixture.INSTANCE.get();
        time.setInterval(interval);
        return time;
    }

    private static SiteSubscription getSubscription(int seed) {
        List<Parameter> parameters = new ArrayList<Parameter>();
        parameters.add(ParameterFixture.INSTANCE.get());
        return getSubscription(seed, 0, 0, 15, 15, parameters,
                PointTimeFixture.INSTANCE.get());
    }

    private static SiteSubscription getSubscription(int seed, double x1,
            double y1, double x2, int y2, List<Parameter> parameters, Time time) {
        SiteSubscription sub = SiteSubscriptionFixture.INSTANCE.get(seed,
                DataType.POINT);
        ReferencedEnvelope env = new ReferencedEnvelope(x1, x2, y1, y2,
                MapUtil.LATLON_PROJECTION);

        sub.getCoverage().setRequestEnvelope(env);
        sub.setParameter(parameters);
        sub.setTime(time);
        return sub;
    }

    private static SiteSubscription getSubscription(int seed,
            Envelope envelope, Time time) {
        SiteSubscription sub = SiteSubscriptionFixture.INSTANCE.get(seed,
                DataType.POINT);
        ReferencedEnvelope env = new ReferencedEnvelope(envelope,
                MapUtil.LATLON_PROJECTION);
        ArrayList<Parameter> parametersMatchPoints = new ArrayList<Parameter>();
        parametersMatchPoints.add(ParameterFixture.INSTANCE.get(1));

        sub.getCoverage().setRequestEnvelope(env);
        sub.setParameter(parametersMatchPoints);
        sub.setTime(time);
        return sub;
    }
}
