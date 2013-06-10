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

import static com.raytheon.uf.common.datadelivery.service.subscription.SubscriptionOverlapMatchStrategy.AT_LEAST_HALF;
import static com.raytheon.uf.common.datadelivery.service.subscription.SubscriptionOverlapMatchStrategy.MATCH_ALL;
import static com.raytheon.uf.common.datadelivery.service.subscription.SubscriptionOverlapMatchStrategy.MATCH_ANY;
import static org.hamcrest.Matchers.is;
import static org.junit.Assert.assertThat;

import org.junit.Test;

/**
 * Test {@link SubscriptionOverlapMatchStrategy}.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * May 14, 2013 2000       djohnson     Initial creation
 * 
 * </pre>
 * 
 * @author djohnson
 * @version 1.0
 */
public class SubscriptionOverlapMatchStrategyTest {

    private static final SubscriptionOverlapConfig MUST_EXCEED_FIFTY_PERCENT = new SubscriptionOverlapConfig(
            50, 50, 50, 50, null);

    @Test
    public void matchAnyReturnsTrueIfAnyExceedMaxAllowed() {

        assertThat(MATCH_ANY.isOverlapping(MUST_EXCEED_FIFTY_PERCENT, 55, 45,
                45, 45), is(true));
        assertThat(MATCH_ANY.isOverlapping(MUST_EXCEED_FIFTY_PERCENT, 45, 55,
                45, 45), is(true));
        assertThat(MATCH_ANY.isOverlapping(MUST_EXCEED_FIFTY_PERCENT, 45, 45,
                55, 45), is(true));
        assertThat(MATCH_ANY.isOverlapping(MUST_EXCEED_FIFTY_PERCENT, 45, 45,
                45, 55), is(true));
    }

    @Test
    public void matchAnyReturnsFalseIfNoneExceedMaxAllowed() {

        assertThat(MATCH_ANY.isOverlapping(MUST_EXCEED_FIFTY_PERCENT, 50, 50,
                50, 50), is(false));
        assertThat(
                MATCH_ANY.isOverlapping(MUST_EXCEED_FIFTY_PERCENT, 0, 0, 0, 0),
                is(false));
        assertThat(MATCH_ANY.isOverlapping(MUST_EXCEED_FIFTY_PERCENT, 25, 25,
                25, 25), is(false));
    }

    @Test
    public void matchAllReturnsTrueIfAllExceedMaxAllowed() {

        assertThat(MATCH_ALL.isOverlapping(MUST_EXCEED_FIFTY_PERCENT, 55, 55,
                55, 55), is(true));
        assertThat(MATCH_ALL.isOverlapping(MUST_EXCEED_FIFTY_PERCENT, 100, 100,
                100, 100), is(true));
    }

    @Test
    public void matchAllReturnsFalseIfNotAllExceedMaxAllowed() {

        assertThat(MATCH_ALL.isOverlapping(MUST_EXCEED_FIFTY_PERCENT, 55, 55,
                55, 45), is(false));
        assertThat(MATCH_ALL.isOverlapping(MUST_EXCEED_FIFTY_PERCENT, 55, 55,
                45, 55), is(false));
        assertThat(MATCH_ALL.isOverlapping(MUST_EXCEED_FIFTY_PERCENT, 55, 45,
                55, 55), is(false));
        assertThat(MATCH_ALL.isOverlapping(MUST_EXCEED_FIFTY_PERCENT, 45, 55,
                55, 55), is(false));
    }

    @Test
    public void atLeastHalfReturnsTrueIfAllExceedMaxAllowed() {

        assertThat(AT_LEAST_HALF.isOverlapping(MUST_EXCEED_FIFTY_PERCENT, 55,
                55, 55, 55), is(true));
        assertThat(AT_LEAST_HALF.isOverlapping(MUST_EXCEED_FIFTY_PERCENT, 100,
                100, 100, 100), is(true));
    }

    @Test
    public void atLeastHalfReturnsTrueIfHalfExceedMaxAllowed() {

        assertThat(AT_LEAST_HALF.isOverlapping(MUST_EXCEED_FIFTY_PERCENT, 45,
                45, 55, 55), is(true));
        assertThat(AT_LEAST_HALF.isOverlapping(MUST_EXCEED_FIFTY_PERCENT, 55,
                55, 45, 45), is(true));
    }

    @Test
    public void atLeastHalfReturnsFalseIfLessThanHalfExceedMaxAllowed() {

        assertThat(AT_LEAST_HALF.isOverlapping(MUST_EXCEED_FIFTY_PERCENT, 45,
                45, 45, 55), is(false));
        assertThat(AT_LEAST_HALF.isOverlapping(MUST_EXCEED_FIFTY_PERCENT, 45,
                45, 55, 45), is(false));
        assertThat(AT_LEAST_HALF.isOverlapping(MUST_EXCEED_FIFTY_PERCENT, 45,
                55, 45, 45), is(false));
        assertThat(AT_LEAST_HALF.isOverlapping(MUST_EXCEED_FIFTY_PERCENT, 55,
                45, 45, 45), is(false));
    }

}
