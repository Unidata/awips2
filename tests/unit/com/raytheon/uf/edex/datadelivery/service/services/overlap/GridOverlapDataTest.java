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
package com.raytheon.uf.edex.datadelivery.service.services.overlap;

import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

import org.geotools.geometry.jts.ReferencedEnvelope;
import org.junit.BeforeClass;
import org.junit.Test;
import org.opengis.referencing.crs.CoordinateReferenceSystem;

import com.raytheon.uf.common.datadelivery.registry.DataType;
import com.raytheon.uf.common.datadelivery.registry.GriddedCoverage;
import com.raytheon.uf.common.datadelivery.registry.GriddedTime;
import com.raytheon.uf.common.datadelivery.registry.GriddedTimeFixture;
import com.raytheon.uf.common.datadelivery.registry.Parameter;
import com.raytheon.uf.common.datadelivery.registry.ParameterFixture;
import com.raytheon.uf.common.datadelivery.registry.SiteSubscription;
import com.raytheon.uf.common.datadelivery.registry.SiteSubscriptionFixture;
import com.raytheon.uf.common.datadelivery.registry.Time;
import com.raytheon.uf.common.datadelivery.service.subscription.GridSubscriptionOverlapConfig;
import com.raytheon.uf.common.datadelivery.service.subscription.SubscriptionOverlapMatchStrategy;
import com.raytheon.uf.common.geospatial.MapUtil;
import com.vividsolutions.jts.geom.Envelope;

/**
 * GridOverlapData object test class.
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
 *                                     Added Test case to match issue of a less than
 *                                     50% overlap for an existing, but more than 50%
 *                                     overlap for a new subscription, but still not passing
 *                                     a 50% overlap rule.
 * 
 * </pre>
 * 
 * @author mpduff
 * @version 1.0
 */

public class GridOverlapDataTest {
    private static final GridSubscriptionOverlapConfig FIFTY_PERCENT_MATCH_ALL = new GridSubscriptionOverlapConfig(
            50, 50, 50, 50, SubscriptionOverlapMatchStrategy.MATCH_ALL);

    private static final GridSubscriptionOverlapConfig FIFTY_PERCENT_MATCH_ANY = new GridSubscriptionOverlapConfig(
            50, 50, 50, 50, SubscriptionOverlapMatchStrategy.MATCH_ANY);

    private static final GridSubscriptionOverlapConfig FIFTY_PERCENT_MATCH_HALF = new GridSubscriptionOverlapConfig(
            50, 50, 50, 50, SubscriptionOverlapMatchStrategy.AT_LEAST_HALF);

    private static ArrayList<Parameter> matchMeParameters;

    private static SiteSubscription matchMe;

    private static Envelope areaMatchMe;

    private static Envelope areaLessThan50PercentOverlap;

    private static Envelope areaGreaterThan50PercentOverlap;

    private static Envelope areaWithNoOverlap;

    @BeforeClass
    public static void setup() {
        matchMeParameters = new ArrayList<Parameter>();
        matchMeParameters.add(ParameterFixture.INSTANCE.get(1));

        areaMatchMe = new Envelope(0, 10, 0, 20);
        areaLessThan50PercentOverlap = new Envelope(5, 25, 0, 15);
        areaGreaterThan50PercentOverlap = new Envelope(0, 10, 10, 25);
        areaWithNoOverlap = new Envelope(0, 30, 20, 20);

        matchMe = getSubscription(
                1,
                areaMatchMe,
                matchMeParameters,
                createGriddedTime(Arrays.asList(0, 1, 2, 3),
                        Arrays.asList(0, 1, 2, 3)));
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
        GridOverlapData<GriddedTime, GriddedCoverage> od = new GridOverlapData<GriddedTime, GriddedCoverage>(
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
        GridOverlapData<GriddedTime, GriddedCoverage> od = new GridOverlapData<GriddedTime, GriddedCoverage>(
                dupeSub1, dupeSub2, FIFTY_PERCENT_MATCH_ANY);
        assertTrue("These should be duplicates", od.isDuplicate());
        assertTrue("These should overlap", od.isOverlapping());
    }

    @Test
    public void testFiftyPercentMatchHalfWithDuplicates() {
        SiteSubscription dupeSub1 = getSubscription(1);
        SiteSubscription dupeSub2 = new SiteSubscription(dupeSub1);
        dupeSub2.getCoverage().setRequestEnvelope(
                dupeSub1.getCoverage().getRequestEnvelope());
        GridOverlapData<GriddedTime, GriddedCoverage> od = new GridOverlapData<GriddedTime, GriddedCoverage>(
                dupeSub1, dupeSub2, FIFTY_PERCENT_MATCH_HALF);
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
        GriddedTime time = new GriddedTime();
        time.addCycleTime(0);
        List<Integer> indices = new ArrayList<Integer>(1);
        indices.add(50);
        time.setSelectedTimeIndices(indices);
        sub2.setTime(time);

        GridOverlapData<GriddedTime, GriddedCoverage> od = new GridOverlapData<GriddedTime, GriddedCoverage>(
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
        GriddedTime time = new GriddedTime();
        time.addCycleTime(0);
        List<Integer> indices = new ArrayList<Integer>(1);
        indices.add(50);
        time.setSelectedTimeIndices(indices);
        sub2.setTime(time);

        GridOverlapData<GriddedTime, GriddedCoverage> od = new GridOverlapData<GriddedTime, GriddedCoverage>(
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
        GriddedTime time = new GriddedTime();
        time.addCycleTime(0);
        List<Integer> indices = new ArrayList<Integer>(1);
        indices.add(50);
        time.setSelectedTimeIndices(indices);
        sub2.setTime(time);

        GridOverlapData<GriddedTime, GriddedCoverage> od = new GridOverlapData<GriddedTime, GriddedCoverage>(
                sub1, sub2, FIFTY_PERCENT_MATCH_HALF);
        assertFalse("These should not be duplicates", od.isDuplicate());
        assertFalse("These should not overlap", od.isOverlapping());
    }

    /*
     * Half matching subs - match parameter and cycle times
     */

    @Test
    public void testFiftyPercentMatchAllWithHalfMatches() {
        SiteSubscription sub1 = getSubscription(1);
        SiteSubscription sub2 = getSubscription(2);
        ReferencedEnvelope env = new ReferencedEnvelope(20, 25, 20, 25,
                MapUtil.LATLON_PROJECTION);
        sub2.getCoverage().setRequestEnvelope(env);
        GriddedTime time = new GriddedTime((GriddedTime) sub1.getTime());
        List<Integer> indices = new ArrayList<Integer>(1);
        indices.add(50);
        time.setSelectedTimeIndices(indices);
        sub2.setTime(time);

        GridOverlapData<GriddedTime, GriddedCoverage> od = new GridOverlapData<GriddedTime, GriddedCoverage>(
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
        GriddedTime time = new GriddedTime((GriddedTime) sub1.getTime());
        List<Integer> indices = new ArrayList<Integer>(1);
        indices.add(50);
        time.setSelectedTimeIndices(indices);
        sub2.setTime(time);

        GridOverlapData<GriddedTime, GriddedCoverage> od = new GridOverlapData<GriddedTime, GriddedCoverage>(
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
        GriddedTime time = new GriddedTime((GriddedTime) sub1.getTime());
        List<Integer> indices = new ArrayList<Integer>(1);
        indices.add(50);
        time.setSelectedTimeIndices(indices);
        sub2.setTime(time);

        GridOverlapData<GriddedTime, GriddedCoverage> od = new GridOverlapData<GriddedTime, GriddedCoverage>(
                sub1, sub2, FIFTY_PERCENT_MATCH_HALF);
        assertFalse("These should not be duplicates", od.isDuplicate());
        assertTrue("These should overlap", od.isOverlapping());
    }

    @Test
    public void testMatchAllNotEnoughSpatialOverlap() {
        int parameter = 100;
        int spatial = 50;
        int forecastHours = 100;
        int cycles = 100;
        final GridSubscriptionOverlapConfig overlap = new GridSubscriptionOverlapConfig(
                parameter, forecastHours, cycles, spatial,
                SubscriptionOverlapMatchStrategy.MATCH_ALL);
        List<Parameter> parameters = new ArrayList<Parameter>();
        parameters.add(ParameterFixture.INSTANCE.get(1));

        SiteSubscription sub2 = getSubscription(
                1,
                areaLessThan50PercentOverlap,
                parameters,
                createGriddedTime(Arrays.asList(0, 1, 2, 3),
                        Arrays.asList(0, 1, 2, 3)));

        GridOverlapData<GriddedTime, GriddedCoverage> overlapResult = new GridOverlapData<GriddedTime, GriddedCoverage>(
                matchMe, sub2, overlap);
        assertFalse("These should not be duplicates",
                overlapResult.isDuplicate());
        assertFalse("These should not overlap", overlapResult.isOverlapping());
    }

    @Test
    public void testMatchAllSpatialOverlap() {
        int parameter = 100;
        int spatial = 50;
        int forecastHours = 100;
        int cycles = 100;
        final GridSubscriptionOverlapConfig overlap = new GridSubscriptionOverlapConfig(
                parameter, forecastHours, cycles, spatial,
                SubscriptionOverlapMatchStrategy.MATCH_ALL);
        SiteSubscription sub3 = getSubscription(
                1,
                areaGreaterThan50PercentOverlap,
                matchMeParameters,
                createGriddedTime(Arrays.asList(0, 1, 2, 3),
                        Arrays.asList(0, 1, 2, 3)));

        GridOverlapData<GriddedTime, GriddedCoverage> overlapResult = new GridOverlapData<GriddedTime, GriddedCoverage>(
                matchMe, sub3, overlap);
        assertFalse("These should not be duplicates",
                overlapResult.isDuplicate());
        assertTrue("These should overlap", overlapResult.isOverlapping());
    }

    @Test
    public void testMatchAnyParametersOnly() {
        int parameter = 50;
        int spatial = 50;
        int forecastHours = 100;
        int cycles = 100;
        final GridSubscriptionOverlapConfig overlap = new GridSubscriptionOverlapConfig(
                parameter, forecastHours, cycles, spatial,
                SubscriptionOverlapMatchStrategy.MATCH_ANY);

        List<Parameter> parameters2 = new ArrayList<Parameter>();
        parameters2.addAll(matchMeParameters);
        parameters2.add(ParameterFixture.INSTANCE.get(20));

        SiteSubscription sub2 = getSubscription(
                1,
                areaWithNoOverlap,
                parameters2,
                createGriddedTime(Arrays.asList(4, 5, 6),
                        Arrays.asList(4, 5, 6)));

        GridOverlapData<GriddedTime, GriddedCoverage> overlapResult = new GridOverlapData<GriddedTime, GriddedCoverage>(
                matchMe, sub2, overlap);
        assertFalse("These should not be duplicates",
                overlapResult.isDuplicate());
        assertTrue("These should overlap", overlapResult.isOverlapping());
        assertFalse("These should not overlap", overlapResult.cyclePass);
        assertFalse("These should not overlap", overlapResult.fcstHrPass);
        assertTrue("These should overlap", overlapResult.parameterPass);
        assertFalse("These should not overlap", overlapResult.spatialPass);
    }

    @Test
    public void testMatchAnySpatialOnly() {
        int parameter = 50;
        int spatial = 50;
        int forecastHours = 100;
        int cycles = 100;
        final GridSubscriptionOverlapConfig overlap = new GridSubscriptionOverlapConfig(
                parameter, forecastHours, cycles, spatial,
                SubscriptionOverlapMatchStrategy.MATCH_ANY);

        List<Parameter> parameters2 = new ArrayList<Parameter>();
        parameters2.addAll(matchMeParameters);
        parameters2.add(ParameterFixture.INSTANCE.get(20));

        List<Parameter> parameters3 = new ArrayList<Parameter>();
        parameters3.add(ParameterFixture.INSTANCE.get(30));

        SiteSubscription sub3 = getSubscription(
                1,
                areaGreaterThan50PercentOverlap,
                parameters3,
                createGriddedTime(Arrays.asList(7, 8, 9),
                        Arrays.asList(7, 8, 9)));
        GridOverlapData<GriddedTime, GriddedCoverage> overlapResult = new GridOverlapData<GriddedTime, GriddedCoverage>(
                matchMe, sub3, overlap);
        assertFalse("These should not be duplicates",
                overlapResult.isDuplicate());
        assertTrue("These should overlap", overlapResult.isOverlapping());
        assertFalse("These should not overlap", overlapResult.cyclePass);
        assertFalse("These should not overlap", overlapResult.fcstHrPass);
        assertFalse("These should not overlap", overlapResult.parameterPass);
        assertTrue("These should overlap", overlapResult.spatialPass);
    }

    @Test
    public void testMatchAnySpatialOnlyLessThan50PercentOfExistingGreaterThan50PercentOfNew() {
        int parameter = 50;
        int spatial = 50;
        int forecastHours = 100;
        int cycles = 100;
        final GridSubscriptionOverlapConfig overlap = new GridSubscriptionOverlapConfig(
                parameter, forecastHours, cycles, spatial,
                SubscriptionOverlapMatchStrategy.MATCH_ANY);

        List<Parameter> parameters2 = new ArrayList<Parameter>();
        parameters2.addAll(matchMeParameters);
        parameters2.add(ParameterFixture.INSTANCE.get(20));

        List<Parameter> parameters3 = new ArrayList<Parameter>();
        parameters3.add(ParameterFixture.INSTANCE.get(30));

        CoordinateReferenceSystem crs = MapUtil.LATLON_PROJECTION;
        ReferencedEnvelope matchEnv = new ReferencedEnvelope(
                -353287.9146908128, 499474.38945139456, 4064387.3713984187,
                4442030.0353220245, crs);
        ReferencedEnvelope otherEnvelope = new ReferencedEnvelope(
                -584748.4738489623, 487298.1073337787, 4113114.739653571,
                4783132.733000439, crs);

        SiteSubscription match = getSubscription(
                1,
                matchEnv,
                matchMeParameters,
                createGriddedTime(Arrays.asList(0, 1, 2, 3),
                        Arrays.asList(0, 1, 2, 3)));
        SiteSubscription otherSub = getSubscription(
                1,
                otherEnvelope,
                parameters3,
                createGriddedTime(Arrays.asList(7, 8, 9),
                        Arrays.asList(7, 8, 9)));
        GridOverlapData<GriddedTime, GriddedCoverage> overlapResult = new GridOverlapData<GriddedTime, GriddedCoverage>(
                match, otherSub, overlap);

        assertFalse("These should not be duplicates",
                overlapResult.isDuplicate());
        assertTrue("These should overlap", overlapResult.isOverlapping());
        assertFalse("These should not overlap", overlapResult.cyclePass);
        assertFalse("These should not overlap", overlapResult.fcstHrPass);
        assertFalse("These should not overlap", overlapResult.parameterPass);
        assertTrue("These should overlap", overlapResult.spatialPass);
    }

    @Test
    public void testMatchAnyForecastHoursOnly() {
        int parameter = 50;
        int spatial = 50;
        int forecastHours = 100;
        int cycles = 100;
        final GridSubscriptionOverlapConfig overlap = new GridSubscriptionOverlapConfig(
                parameter, forecastHours, cycles, spatial,
                SubscriptionOverlapMatchStrategy.MATCH_ANY);

        List<Parameter> parameters2 = new ArrayList<Parameter>();
        parameters2.add(ParameterFixture.INSTANCE.get(20));

        SiteSubscription sub2 = getSubscription(
                1,
                areaWithNoOverlap,
                parameters2,
                createGriddedTime(Arrays.asList(0, 1, 2, 3),
                        Arrays.asList(10, 11, 12)));

        GridOverlapData<GriddedTime, GriddedCoverage> overlapResult = new GridOverlapData<GriddedTime, GriddedCoverage>(
                matchMe, sub2, overlap);
        assertFalse("These should not be duplicates",
                overlapResult.isDuplicate());
        assertTrue("These should overlap", overlapResult.isOverlapping());
        assertFalse("These should not overlap", overlapResult.cyclePass);
        assertTrue("These should overlap", overlapResult.fcstHrPass);
        assertFalse("These should not overlap", overlapResult.parameterPass);
        assertFalse("These should not overlap", overlapResult.spatialPass);
    }

    @Test
    public void testMatchAnyCyclesOnly() {
        int parameter = 50;
        int spatial = 50;
        int forecastHours = 100;
        int cycles = 100;
        final GridSubscriptionOverlapConfig overlap = new GridSubscriptionOverlapConfig(
                parameter, forecastHours, cycles, spatial,
                SubscriptionOverlapMatchStrategy.MATCH_ANY);

        List<Parameter> parameters2 = new ArrayList<Parameter>();
        parameters2.add(ParameterFixture.INSTANCE.get(20));

        SiteSubscription sub2 = getSubscription(
                1,
                areaWithNoOverlap,
                parameters2,
                createGriddedTime(Arrays.asList(10, 11, 12),
                        Arrays.asList(0, 1, 2, 3)));

        GridOverlapData<GriddedTime, GriddedCoverage> overlapResult = new GridOverlapData<GriddedTime, GriddedCoverage>(
                matchMe, sub2, overlap);
        assertFalse("These should not be duplicates",
                overlapResult.isDuplicate());
        assertTrue("These should overlap", overlapResult.isOverlapping());
        assertTrue("These should overlap", overlapResult.cyclePass);
        assertFalse("These should not overlap", overlapResult.fcstHrPass);
        assertFalse("These should not overlap", overlapResult.parameterPass);
        assertFalse("These should not overlap", overlapResult.spatialPass);
    }

    @Test
    public void testMatchAnyNoMatches() {
        int parameter = 50;
        int spatial = 50;
        int forecastHours = 100;
        int cycles = 100;
        final GridSubscriptionOverlapConfig overlap = new GridSubscriptionOverlapConfig(
                parameter, forecastHours, cycles, spatial,
                SubscriptionOverlapMatchStrategy.MATCH_ANY);

        List<Parameter> parameters2 = new ArrayList<Parameter>();
        parameters2.add(ParameterFixture.INSTANCE.get(20));

        SiteSubscription sub2 = getSubscription(
                1,
                areaWithNoOverlap,
                parameters2,
                createGriddedTime(Arrays.asList(13, 14, 15),
                        Arrays.asList(13, 14, 15)));

        GridOverlapData<GriddedTime, GriddedCoverage> overlapResult = new GridOverlapData<GriddedTime, GriddedCoverage>(
                matchMe, sub2, overlap);
        assertFalse("These should not be duplicates",
                overlapResult.isDuplicate());
        assertFalse("These should not overlap", overlapResult.isOverlapping());
        assertFalse("These should not overlap", overlapResult.cyclePass);
        assertFalse("These should not overlap", overlapResult.fcstHrPass);
        assertFalse("These should not overlap", overlapResult.parameterPass);
        assertFalse("These should not overlap", overlapResult.spatialPass);
    }

    @Test
    public void testMatchAtLeastHalfParametersAndSpatial() {
        int parameter = 50;
        int spatial = 50;
        int forecastHours = 100;
        int cycles = 100;
        final GridSubscriptionOverlapConfig overlap = new GridSubscriptionOverlapConfig(
                parameter, forecastHours, cycles, spatial,
                SubscriptionOverlapMatchStrategy.AT_LEAST_HALF);

        List<Parameter> parameters2 = new ArrayList<Parameter>();
        parameters2.addAll(matchMeParameters);
        parameters2.add(ParameterFixture.INSTANCE.get(20));

        SiteSubscription sub2 = getSubscription(
                1,
                areaMatchMe,
                parameters2,
                createGriddedTime(Arrays.asList(4, 5, 6),
                        Arrays.asList(4, 5, 6)));

        GridOverlapData<GriddedTime, GriddedCoverage> overlapResult = new GridOverlapData<GriddedTime, GriddedCoverage>(
                matchMe, sub2, overlap);
        assertFalse("These should not be duplicates",
                overlapResult.isDuplicate());
        assertTrue("These should overlap", overlapResult.isOverlapping());
        assertFalse("These should not overlap", overlapResult.cyclePass);
        assertFalse("These should not overlap", overlapResult.fcstHrPass);
        assertTrue("These should overlap", overlapResult.parameterPass);
        assertTrue("These should overlap", overlapResult.spatialPass);
    }

    @Test
    public void testMatchAtLeastHalfForecastHoursCycles() {
        int parameter = 50;
        int spatial = 50;
        int forecastHours = 100;
        int cycles = 100;
        final GridSubscriptionOverlapConfig overlap = new GridSubscriptionOverlapConfig(
                parameter, forecastHours, cycles, spatial,
                SubscriptionOverlapMatchStrategy.AT_LEAST_HALF);

        List<Parameter> parameters2 = new ArrayList<Parameter>();
        parameters2.add(ParameterFixture.INSTANCE.get(20));

        SiteSubscription sub2 = getSubscription(
                1,
                areaWithNoOverlap,
                parameters2,
                createGriddedTime(Arrays.asList(0, 1, 2, 3),
                        Arrays.asList(0, 1, 2, 3)));

        GridOverlapData<GriddedTime, GriddedCoverage> overlapResult = new GridOverlapData<GriddedTime, GriddedCoverage>(
                matchMe, sub2, overlap);
        assertFalse("These should not be duplicates",
                overlapResult.isDuplicate());
        assertTrue("These should overlap", overlapResult.isOverlapping());
        assertTrue("These should overlap", overlapResult.cyclePass);
        assertTrue("These should overlap", overlapResult.fcstHrPass);
        assertFalse("These should not overlap", overlapResult.parameterPass);
        assertFalse("These should not overlap", overlapResult.spatialPass);
    }

    @Test
    public void testMatchAtLeastHalfParametersForecastHours() {
        int parameter = 50;
        int spatial = 50;
        int forecastHours = 100;
        int cycles = 100;
        final GridSubscriptionOverlapConfig overlap = new GridSubscriptionOverlapConfig(
                parameter, forecastHours, cycles, spatial,
                SubscriptionOverlapMatchStrategy.AT_LEAST_HALF);

        List<Parameter> parameters2 = new ArrayList<Parameter>();
        parameters2.addAll(matchMeParameters);
        parameters2.add(ParameterFixture.INSTANCE.get(20));

        SiteSubscription sub2 = getSubscription(
                1,
                areaWithNoOverlap,
                parameters2,
                createGriddedTime(Arrays.asList(0, 1, 2, 3),
                        Arrays.asList(7, 8, 9)));

        GridOverlapData<GriddedTime, GriddedCoverage> overlapResult = new GridOverlapData<GriddedTime, GriddedCoverage>(
                matchMe, sub2, overlap);
        assertFalse("These should not be duplicates",
                overlapResult.isDuplicate());
        assertTrue("These should overlap", overlapResult.isOverlapping());
        assertFalse("These should not overlap", overlapResult.cyclePass);
        assertTrue("These should overlap", overlapResult.fcstHrPass);
        assertTrue("These should overlap", overlapResult.parameterPass);
        assertFalse("These should not overlap", overlapResult.spatialPass);
    }

    @Test
    public void testMatchAtLeastHalfSpatialAndCycles() {
        int parameter = 50;
        int spatial = 50;
        int forecastHours = 100;
        int cycles = 100;
        final GridSubscriptionOverlapConfig overlap = new GridSubscriptionOverlapConfig(
                parameter, forecastHours, cycles, spatial,
                SubscriptionOverlapMatchStrategy.AT_LEAST_HALF);

        List<Parameter> parameters2 = new ArrayList<Parameter>();
        parameters2.add(ParameterFixture.INSTANCE.get(20));

        SiteSubscription sub2 = getSubscription(
                1,
                areaMatchMe,
                parameters2,
                createGriddedTime(Arrays.asList(10, 11, 12),
                        Arrays.asList(0, 1, 2, 3)));

        GridOverlapData<GriddedTime, GriddedCoverage> overlapResult = new GridOverlapData<GriddedTime, GriddedCoverage>(
                matchMe, sub2, overlap);
        assertFalse("These should not be duplicates",
                overlapResult.isDuplicate());
        assertTrue("These should overlap", overlapResult.isOverlapping());
        assertTrue("These should overlap", overlapResult.cyclePass);
        assertFalse("These should not overlap", overlapResult.fcstHrPass);
        assertFalse("These should not overlap", overlapResult.parameterPass);
        assertTrue("These should overlap", overlapResult.spatialPass);
    }

    @Test
    public void testMatchAtLeastHalfOneMatch() {
        int parameter = 50;
        int spatial = 50;
        int forecastHours = 100;
        int cycles = 100;
        final GridSubscriptionOverlapConfig overlap = new GridSubscriptionOverlapConfig(
                parameter, forecastHours, cycles, spatial,
                SubscriptionOverlapMatchStrategy.AT_LEAST_HALF);

        List<Parameter> parameters2 = new ArrayList<Parameter>();
        parameters2.addAll(matchMeParameters);
        parameters2.add(ParameterFixture.INSTANCE.get(20));

        SiteSubscription sub2 = getSubscription(
                1,
                areaWithNoOverlap,
                parameters2,
                createGriddedTime(Arrays.asList(13, 14, 15),
                        Arrays.asList(13, 14, 15)));

        GridOverlapData<GriddedTime, GriddedCoverage> overlapResult = new GridOverlapData<GriddedTime, GriddedCoverage>(
                matchMe, sub2, overlap);
        assertFalse("These should not be duplicates",
                overlapResult.isDuplicate());
        assertFalse("These should not overlap", overlapResult.isOverlapping());
        assertFalse("These should not overlap", overlapResult.cyclePass);
        assertFalse("These should not overlap", overlapResult.fcstHrPass);
        assertTrue("These should overlap", overlapResult.parameterPass);
        assertFalse("These should not overlap", overlapResult.spatialPass);
    }

    @Test
    public void testMatchAtLeastHalfNoMatches() {
        int parameter = 50;
        int spatial = 50;
        int forecastHours = 100;
        int cycles = 100;
        final GridSubscriptionOverlapConfig overlap = new GridSubscriptionOverlapConfig(
                parameter, forecastHours, cycles, spatial,
                SubscriptionOverlapMatchStrategy.AT_LEAST_HALF);

        List<Parameter> parameters1 = new ArrayList<Parameter>();
        parameters1.add(ParameterFixture.INSTANCE.get(10));

        List<Parameter> parameters2 = new ArrayList<Parameter>();
        parameters2.add(ParameterFixture.INSTANCE.get(20));

        SiteSubscription sub2 = getSubscription(
                1,
                areaWithNoOverlap,
                parameters2,
                createGriddedTime(Arrays.asList(16, 17, 18),
                        Arrays.asList(16, 17, 18)));

        GridOverlapData<GriddedTime, GriddedCoverage> overlapResult = new GridOverlapData<GriddedTime, GriddedCoverage>(
                matchMe, sub2, overlap);
        assertFalse("These should not be duplicates",
                overlapResult.isDuplicate());
        assertFalse("These should not overlap", overlapResult.isOverlapping());
        assertFalse("These should not overlap", overlapResult.cyclePass);
        assertFalse("These should not overlap", overlapResult.fcstHrPass);
        assertFalse("These should not overlap", overlapResult.parameterPass);
        assertFalse("These should not overlap", overlapResult.spatialPass);
    }

    private static Time createGriddedTime(List<Integer> selectedTimeIndices,
            List<Integer> cycleTimes) {
        GriddedTime time = GriddedTimeFixture.INSTANCE.get();
        time.setCycleTimes(cycleTimes);
        time.setSelectedTimeIndices(selectedTimeIndices);
        return time;
    }

    private static SiteSubscription getSubscription(int seed) {
        List<Parameter> parameters = new ArrayList<Parameter>();
        parameters.add(ParameterFixture.INSTANCE.get());
        return getSubscription(seed, 0, 0, 15, 15, parameters,
                GriddedTimeFixture.INSTANCE.get());
    }

    private static SiteSubscription getSubscription(int seed, double x1,
            double y1, double x2, int y2, List<Parameter> parameters, Time time) {
        SiteSubscription sub = SiteSubscriptionFixture.INSTANCE.get(seed,
                DataType.GRID);
        ReferencedEnvelope env = new ReferencedEnvelope(x1, x2, y1, y2,
                MapUtil.LATLON_PROJECTION);

        sub.getCoverage().setRequestEnvelope(env);
        sub.setParameter(parameters);
        sub.setTime(time);
        return sub;
    }

    private static SiteSubscription getSubscription(int seed,
            Envelope envelope, List<Parameter> parameters, Time time) {
        SiteSubscription sub = SiteSubscriptionFixture.INSTANCE.get(seed,
                DataType.GRID);
        ReferencedEnvelope env = new ReferencedEnvelope(envelope,
                MapUtil.LATLON_PROJECTION);

        sub.getCoverage().setRequestEnvelope(env);
        sub.setParameter(parameters);
        sub.setTime(time);
        return sub;
    }
}
