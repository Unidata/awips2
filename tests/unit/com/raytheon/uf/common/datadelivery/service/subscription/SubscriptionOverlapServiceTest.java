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

import static com.raytheon.uf.common.datadelivery.service.subscription.SubscriptionOverlapMatchStrategy.MATCH_ALL;
import static com.raytheon.uf.common.datadelivery.service.subscription.SubscriptionOverlapMatchStrategy.MATCH_ANY;
import static org.hamcrest.Matchers.is;
import static org.junit.Assert.assertThat;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

import org.junit.Before;
import org.junit.Test;
import org.opengis.referencing.operation.TransformException;

import com.raytheon.uf.common.datadelivery.registry.Subscription;
import com.raytheon.uf.common.datadelivery.registry.SiteSubscriptionFixture;
import com.raytheon.uf.common.localization.PathManagerFactoryTest;
import com.raytheon.uf.common.localization.exception.LocalizationException;

/**
 * Test {@link SubscriptionOverlapService}.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * May 07, 2013 2000       djohnson     Initial creation
 * 
 * </pre>
 * 
 * @author djohnson
 * @version 1.0
 */

public class SubscriptionOverlapServiceTest {

    private static final SubscriptionOverlapConfig ANY_MUST_EXCEED_65_PERCENT = new SubscriptionOverlapConfig(
            65, 65, 65, 65, MATCH_ANY);

    private static final SubscriptionOverlapConfig ALL_MUST_EXCEED_65_PERCENT = new SubscriptionOverlapConfig(
            65, 65, 65, 65, MATCH_ALL);

    private final ISubscriptionDuplicateChecker duplicateChecker = mock(ISubscriptionDuplicateChecker.class);

    private final SubscriptionOverlapService service = new SubscriptionOverlapService(
            duplicateChecker);

    private final Subscription sub1 = SiteSubscriptionFixture.INSTANCE.get(1);

    private final Subscription sub2 = SiteSubscriptionFixture.INSTANCE.get(2);

    @Before
    public void setUp() {
        PathManagerFactoryTest.initLocalization();
    }

    @Test
    public void moreParametersInCommonThanAllowedOverlaps() {
        when(duplicateChecker.getParameterDuplicationPercent(sub1, sub2))
                .thenReturn(66);

        assertThat(service.isOverlapping(sub1, sub2).isOverlapping(), is(true));
    }

    @Test
    public void lessParametersInCommonThanAllowedDoesNotOverlap() {
        when(duplicateChecker.getParameterDuplicationPercent(sub1, sub2))
                .thenReturn(64);

        assertThat(service.isOverlapping(sub1, sub2).isOverlapping(), is(false));
    }

    @Test
    public void moreForecastHoursInCommonThanAllowedOverlaps() {
        when(duplicateChecker.getForecastHourDuplicationPercent(sub1, sub2))
                .thenReturn(66);

        assertThat(service.isOverlapping(sub1, sub2).isOverlapping(), is(true));
    }

    @Test
    public void lessForecastHoursInCommonThanAllowedDoesNotOverlap() {
        when(duplicateChecker.getForecastHourDuplicationPercent(sub1, sub2))
                .thenReturn(64);

        assertThat(service.isOverlapping(sub1, sub2).isOverlapping(), is(false));
    }

    @Test
    public void moreCyclesInCommonThanAllowedOverlaps() {
        when(duplicateChecker.getCycleDuplicationPercent(sub1, sub2))
                .thenReturn(66);

        assertThat(service.isOverlapping(sub1, sub2).isOverlapping(), is(true));
    }

    @Test
    public void lessCyclesInCommonThanAllowedDoesNotOverlap() {
        when(duplicateChecker.getCycleDuplicationPercent(sub1, sub2))
                .thenReturn(64);

        assertThat(service.isOverlapping(sub1, sub2).isOverlapping(), is(false));
    }

    @Test
    public void moreSpatialInCommonThanAllowedOverlaps()
            throws TransformException {
        when(duplicateChecker.getSpatialDuplicationPercent(sub1, sub2))
                .thenReturn(66);

        assertThat(service.isOverlapping(sub1, sub2).isOverlapping(), is(true));
    }

    @Test
    public void lessSpatialInCommonThanAllowedDoesNotOverlap()
            throws TransformException {
        when(duplicateChecker.getSpatialDuplicationPercent(sub1, sub2))
                .thenReturn(64);

        assertThat(service.isOverlapping(sub1, sub2).isOverlapping(), is(false));
    }

    @Test
    public void matchesAnyTrueWillConsiderOneExceededValueAsOverlaps()
            throws LocalizationException {
        service.writeConfig(ANY_MUST_EXCEED_65_PERCENT);

        when(duplicateChecker.getCycleDuplicationPercent(sub1, sub2))
                .thenReturn(66);

        assertThat(service.isOverlapping(sub1, sub2).isOverlapping(), is(true));
    }

    @Test
    public void matchesAnyFalseWillNotConsiderOneExceededValueAsOverlaps()
            throws LocalizationException {
        service.writeConfig(ALL_MUST_EXCEED_65_PERCENT);

        when(duplicateChecker.getCycleDuplicationPercent(sub1, sub2))
                .thenReturn(66);

        assertThat(service.isOverlapping(sub1, sub2).isOverlapping(), is(false));
    }

    @Test
    public void matchesAnyTrueWillConsiderAllExceededValuesAsOverlaps()
            throws LocalizationException, TransformException {
        service.writeConfig(ANY_MUST_EXCEED_65_PERCENT);

        when(duplicateChecker.getCycleDuplicationPercent(sub1, sub2))
                .thenReturn(66);
        when(duplicateChecker.getForecastHourDuplicationPercent(sub1, sub2))
                .thenReturn(66);
        when(duplicateChecker.getParameterDuplicationPercent(sub1, sub2))
                .thenReturn(66);
        when(duplicateChecker.getSpatialDuplicationPercent(sub1, sub2))
                .thenReturn(66);

        assertThat(service.isOverlapping(sub1, sub2).isOverlapping(), is(true));
    }

    @Test
    public void matchesAnyFalseWillConsiderAllExceededValuesAsOverlaps()
            throws LocalizationException, TransformException {
        service.writeConfig(ALL_MUST_EXCEED_65_PERCENT);

        when(duplicateChecker.getCycleDuplicationPercent(sub1, sub2))
                .thenReturn(66);
        when(duplicateChecker.getForecastHourDuplicationPercent(sub1, sub2))
                .thenReturn(66);
        when(duplicateChecker.getParameterDuplicationPercent(sub1, sub2))
                .thenReturn(66);
        when(duplicateChecker.getSpatialDuplicationPercent(sub1, sub2))
                .thenReturn(66);

        assertThat(service.isOverlapping(sub1, sub2).isOverlapping(), is(true));
    }

    @Test
    public void whenAllComparisonsReturnOneHundredPercentReturnsDuplicate()
            throws LocalizationException, TransformException {
        when(duplicateChecker.getCycleDuplicationPercent(sub1, sub2))
                .thenReturn(100);
        when(duplicateChecker.getForecastHourDuplicationPercent(sub1, sub2))
                .thenReturn(100);
        when(duplicateChecker.getParameterDuplicationPercent(sub1, sub2))
                .thenReturn(100);
        when(duplicateChecker.getSpatialDuplicationPercent(sub1, sub2))
                .thenReturn(100);

        assertThat(service.isOverlapping(sub1, sub2).isDuplicate(), is(true));
    }

    @Test
    public void whenAllComparisonsDontReturnOneHundredPercentReturnsNotDuplicate()
            throws LocalizationException, TransformException {
        when(duplicateChecker.getCycleDuplicationPercent(sub1, sub2))
                .thenReturn(100);
        when(duplicateChecker.getForecastHourDuplicationPercent(sub1, sub2))
                .thenReturn(100);
        when(duplicateChecker.getParameterDuplicationPercent(sub1, sub2))
                .thenReturn(100);
        when(duplicateChecker.getSpatialDuplicationPercent(sub1, sub2))
                .thenReturn(99);

        assertThat(service.isOverlapping(sub1, sub2).isDuplicate(), is(false));
    }

}
