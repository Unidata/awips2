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
package com.raytheon.uf.common.datadelivery.service.subscription;

import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;

import java.util.ArrayList;
import java.util.List;

import org.geotools.geometry.jts.ReferencedEnvelope;
import org.junit.Test;

import com.raytheon.uf.common.datadelivery.registry.DataType;
import com.raytheon.uf.common.datadelivery.registry.GriddedCoverage;
import com.raytheon.uf.common.datadelivery.registry.GriddedTime;
import com.raytheon.uf.common.datadelivery.registry.ParameterFixture;
import com.raytheon.uf.common.datadelivery.registry.SiteSubscription;
import com.raytheon.uf.common.datadelivery.registry.SiteSubscriptionFixture;
import com.raytheon.uf.common.geospatial.MapUtil;
import com.raytheon.uf.edex.datadelivery.service.services.overlap.GridOverlapData;

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

    private SiteSubscription getSubscription(int seed) {
        SiteSubscription sub = SiteSubscriptionFixture.INSTANCE.get(seed,
                DataType.GRID);
        ReferencedEnvelope env = new ReferencedEnvelope(0, 15, 0, 15,
                MapUtil.LATLON_PROJECTION);

        sub.getCoverage().setRequestEnvelope(env);
        return sub;
    }
}
