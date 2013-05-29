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

import static org.hamcrest.Matchers.is;
import static org.junit.Assert.assertThat;

import java.util.Arrays;
import java.util.Collections;
import java.util.List;

import org.geotools.geometry.jts.ReferencedEnvelope;
import org.junit.Test;
import org.opengis.referencing.operation.TransformException;

import com.raytheon.uf.common.datadelivery.registry.GriddedCoverage;
import com.raytheon.uf.common.datadelivery.registry.Parameter;
import com.raytheon.uf.common.datadelivery.registry.ParameterFixture;
import com.raytheon.uf.common.datadelivery.registry.SiteSubscriptionFixture;
import com.raytheon.uf.common.datadelivery.registry.SiteSubscription;
import com.raytheon.uf.common.geospatial.MapUtil;
import com.vividsolutions.jts.geom.Coordinate;
import com.vividsolutions.jts.geom.Envelope;

/**
 * TesT {@link SubscriptionDuplicateChecker}.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * May 02, 2013 2000       djohnson     Initial creation
 * 
 * </pre>
 * 
 * @author djohnson
 * @version 1.0
 */
public class SubscriptionDuplicateCheckerTest {

    private static final ISubscriptionDuplicateChecker dupeChecker = new SubscriptionDuplicateChecker();

    @Test
    public void returnsPercentOfParametersThatAreTheSame() {
        final SiteSubscription sub1 = SiteSubscriptionFixture.INSTANCE.get(1);
        final SiteSubscription sub2 = SiteSubscriptionFixture.INSTANCE.get(2);

        sub1.getParameter().clear();
        sub1.addParameter(ParameterFixture.INSTANCE.get(1));

        sub2.getParameter().clear();
        sub2.addParameter(sub1.getParameter().iterator().next());
        sub2.addParameter(ParameterFixture.INSTANCE.get(2));

        assertThat(dupeChecker.getParameterDuplicationPercent(sub1, sub2),
                is(50));
        assertThat(dupeChecker.getParameterDuplicationPercent(sub2, sub1),
                is(100));
    }

    @Test
    public void returnsZeroPercentOfParametersForNullsOrEmpties() {
        final SiteSubscription sub1 = SiteSubscriptionFixture.INSTANCE.get(1);
        final SiteSubscription sub2 = SiteSubscriptionFixture.INSTANCE.get(2);

        sub1.setParameter(null);

        sub2.getParameter().clear();
        sub2.addParameter(ParameterFixture.INSTANCE.get(2));

        assertThat(dupeChecker.getParameterDuplicationPercent(sub1, sub2),
                is(0));
        assertThat(dupeChecker.getParameterDuplicationPercent(sub2, sub1),
                is(0));

        sub1.setParameter(Collections.<Parameter> emptyList());
        assertThat(dupeChecker.getParameterDuplicationPercent(sub1, sub2),
                is(0));
        assertThat(dupeChecker.getParameterDuplicationPercent(sub2, sub1),
                is(0));
    }

    @Test
    public void returnsPercentOfForecastHoursThatAreTheSame() {
        final SiteSubscription sub1 = SiteSubscriptionFixture.INSTANCE.get(1);
        final SiteSubscription sub2 = SiteSubscriptionFixture.INSTANCE.get(2);

        final List<Integer> sub1SelectedTimes = Arrays.asList(0, 1);
        sub1.getTime().setSelectedTimeIndices(sub1SelectedTimes);
        final List<Integer> sub2SelectedTimes = Arrays.asList(0, 3, 4);
        sub2.getTime().setSelectedTimeIndices(sub2SelectedTimes);

        assertThat(dupeChecker.getForecastHourDuplicationPercent(sub1, sub2),
                is(33));
        assertThat(dupeChecker.getForecastHourDuplicationPercent(sub2, sub1),
                is(50));
    }

    @Test
    public void returnsZeroPercentOfForecastHoursForNullsOrEmpties() {
        final SiteSubscription sub1 = SiteSubscriptionFixture.INSTANCE.get(1);
        final SiteSubscription sub2 = SiteSubscriptionFixture.INSTANCE.get(2);

        sub1.getTime().setSelectedTimeIndices(null);
        final List<Integer> sub2SelectedTimes = Arrays.asList(0, 3, 4);
        sub2.getTime().setSelectedTimeIndices(sub2SelectedTimes);

        assertThat(dupeChecker.getForecastHourDuplicationPercent(sub1, sub2),
                is(0));
        assertThat(dupeChecker.getForecastHourDuplicationPercent(sub2, sub1),
                is(0));

        sub1.getTime()
                .setSelectedTimeIndices(Collections.<Integer> emptyList());
        assertThat(dupeChecker.getForecastHourDuplicationPercent(sub1, sub2),
                is(0));
        assertThat(dupeChecker.getForecastHourDuplicationPercent(sub2, sub1),
                is(0));
    }

    @Test
    public void returnsPercentOfCyclesThatAreTheSame() {
        final SiteSubscription sub1 = SiteSubscriptionFixture.INSTANCE.get(1);
        final SiteSubscription sub2 = SiteSubscriptionFixture.INSTANCE.get(2);

        final List<Integer> sub1CycleTimes = Arrays.asList(0, 6);
        sub1.getTime().setCycleTimes(sub1CycleTimes);
        final List<Integer> sub2CycleTimes = Arrays.asList(0, 12, 18);
        sub2.getTime().setCycleTimes(sub2CycleTimes);

        assertThat(dupeChecker.getCycleDuplicationPercent(sub1, sub2),
                is(33));
        assertThat(dupeChecker.getCycleDuplicationPercent(sub2, sub1),
                is(50));
    }

    @Test
    public void returnsZeroPercentOfCyclesForNullsOrEmpties() {
        final SiteSubscription sub1 = SiteSubscriptionFixture.INSTANCE.get(1);
        final SiteSubscription sub2 = SiteSubscriptionFixture.INSTANCE.get(2);

        sub1.getTime().setCycleTimes(null);
        final List<Integer> cycleTimes = Arrays.asList(0, 3, 4);
        sub2.getTime().setCycleTimes(cycleTimes);

        assertThat(dupeChecker.getCycleDuplicationPercent(sub1, sub2), is(0));
        assertThat(dupeChecker.getCycleDuplicationPercent(sub2, sub1), is(0));

        sub1.getTime().setCycleTimes(Collections.<Integer> emptyList());
        assertThat(dupeChecker.getCycleDuplicationPercent(sub1, sub2), is(0));
        assertThat(dupeChecker.getCycleDuplicationPercent(sub2, sub1), is(0));
    }

    @Test
    public void returnsPercentOfSpatialThatIsTheSame()
            throws TransformException {
        final SiteSubscription sub1 = SiteSubscriptionFixture.INSTANCE.get(1);
        final SiteSubscription sub2 = SiteSubscriptionFixture.INSTANCE.get(2);

        ReferencedEnvelope envelope1 = new ReferencedEnvelope(new Envelope(
                new Coordinate(-5, 0), new Coordinate(0, 5)),
                MapUtil.LATLON_PROJECTION);

        // dx = 6, dy = 3, area = 18
        ReferencedEnvelope envelope2 = new ReferencedEnvelope(new Envelope(
                new Coordinate(-3, 3), new Coordinate(3, 6)),
                MapUtil.LATLON_PROJECTION);

        // The intersection should have coordinates: (-3, 3) (0, 5)
        // dx = 3, dy = 2, area = 6

        sub1.getCoverage().setRequestEnvelope(envelope1);
        sub2.getCoverage().setRequestEnvelope(envelope2);

        // The expected percent overlap: 6 / 18 = .33
        assertThat(dupeChecker.getSpatialDuplicationPercent(sub1, sub2), is(33));
    }

    @Test
    public void returnsZeroPercentOfSpatialWhenNoOverlap()
            throws TransformException {
        final SiteSubscription sub1 = SiteSubscriptionFixture.INSTANCE.get(1);
        final SiteSubscription sub2 = SiteSubscriptionFixture.INSTANCE.get(2);

        ReferencedEnvelope envelope1 = new ReferencedEnvelope(new Envelope(
                new Coordinate(-5, 0), new Coordinate(0, 5)),
                MapUtil.LATLON_PROJECTION);

        ReferencedEnvelope envelope2 = new ReferencedEnvelope(new Envelope(
                new Coordinate(-10, -20), new Coordinate(-6, -15)),
                MapUtil.LATLON_PROJECTION);

        sub1.getCoverage().setRequestEnvelope(envelope1);
        sub2.getCoverage().setRequestEnvelope(envelope2);

        assertThat(dupeChecker.getSpatialDuplicationPercent(sub1, sub2), is(0));
    }

    @Test
    public void returnsZeroPercentOfSpatialForNulls() throws TransformException {
        final SiteSubscription sub1 = SiteSubscriptionFixture.INSTANCE.get(1);
        final SiteSubscription sub2 = SiteSubscriptionFixture.INSTANCE.get(2);

        sub1.setCoverage(null);

        assertThat(dupeChecker.getSpatialDuplicationPercent(sub1, sub2), is(0));
        assertThat(dupeChecker.getSpatialDuplicationPercent(sub2, sub1), is(0));

        // No envelope set
        sub1.setCoverage(new GriddedCoverage());
        assertThat(dupeChecker.getSpatialDuplicationPercent(sub1, sub2), is(0));
        assertThat(dupeChecker.getSpatialDuplicationPercent(sub2, sub1), is(0));
    }

}
