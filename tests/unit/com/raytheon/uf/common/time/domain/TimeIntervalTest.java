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
package com.raytheon.uf.common.time.domain;

import static com.raytheon.uf.common.serialization.SerializationUtil.transformFromThrift;
import static com.raytheon.uf.common.serialization.SerializationUtil.transformToThrift;
import static org.hamcrest.CoreMatchers.equalTo;
import static org.hamcrest.CoreMatchers.is;
import static org.junit.Assert.assertThat;
import static org.junit.Assert.assertTrue;

import java.util.Arrays;
import java.util.List;
import java.util.concurrent.TimeUnit;

import javax.xml.bind.JAXBException;

import org.junit.Test;

import com.raytheon.uf.common.serialization.JAXBManager;
import com.raytheon.uf.common.serialization.SerializationException;
import com.raytheon.uf.common.time.domain.api.ITimeInterval;
import com.raytheon.uf.common.time.domain.api.ITimePoint;
import com.raytheon.uf.common.util.TestUtil;

/**
 * Test {@link TimeInterval}.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jan 14, 2013 1286       djohnson     Initial creation
 * 
 * </pre>
 * 
 * @author djohnson
 * @version 1.0
 */
public class TimeIntervalTest {

    private final ITimePoint intervalStart = TimePoints.fromMillis(100L);

    private final ITimePoint intervalEnd = TimePoints.fromMillis(500L);

    private final ITimeInterval interval = new TimeInterval(intervalStart,
            intervalEnd);

    @Test
    public void testConversionToAndFromDynamicSerialize()
            throws SerializationException {
        UsesTimeInterval usesTimeInterval = new UsesTimeInterval();
        usesTimeInterval.setTimeInterval(interval);

        ITimeInterval restored = transformFromThrift(UsesTimeInterval.class,
                transformToThrift(usesTimeInterval)).getTimeInterval();

        assertThat(restored, is(equalTo(interval)));
    }

    @Test
    public void testConversionToAndFromJaxb() throws SerializationException,
            JAXBException {
        UsesTimeInterval usesTimeInterval = new UsesTimeInterval();
        usesTimeInterval.setTimeInterval(interval);

        JAXBManager manager = new JAXBManager(UsesTimeInterval.class);
        final String xml = manager.marshalToXml(usesTimeInterval);

        ITimeInterval restored = ((UsesTimeInterval) manager
                .unmarshalFromXml(xml)).getTimeInterval();

        assertThat(restored, is(equalTo(interval)));
    }

    @Test
    public void testGetStartReturnsCorrectTimePoint() {
        assertThat(interval.getStart(), is(equalTo(intervalStart)));
    }

    @Test
    public void testGetEndReturnsCorrectTimePoint() {
        assertThat(interval.getEnd(), is(equalTo(intervalEnd)));
    }

    @Test
    public void testContainsTimePointReturnsTrueForMidPointWithinInterval() {
        final ITimePoint withinInterval = TimePoints.fromMillis((intervalStart
                .asMilliseconds() + intervalEnd.asMilliseconds()) / 2);

        assertTrue(
                "The interval should have returned true for a time point contained in it!",
                interval.containsTimePoint(withinInterval));
    }

    @Test
    public void testContainsTimePointReturnsTrueForIntervalStart() {
        assertTrue(
                "The interval should have returned true for a time point contained in it!",
                interval.containsTimePoint(intervalStart));
    }

    @Test
    public void testContainsTimePointReturnsTrueForIntervalEnd() {
        assertTrue(
                "The interval should have returned true for a time point contained in it!",
                interval.containsTimePoint(intervalEnd));
    }

    @Test
    public void testGetDurationReturnsCorrectAmount() {
        assertThat(interval.getDuration(), is(equalTo(Durations.of(
                intervalEnd.asMilliseconds() - intervalStart.asMilliseconds(),
                TimeUnit.MILLISECONDS))));
    }

    @Test
    public void testEqualsAndHashcodeContract() {
        ITimeInterval sameInterval = new TimeInterval(
                TimePoints.fromMillis(intervalStart.asMilliseconds()),
                TimePoints.fromMillis(intervalEnd.asMilliseconds()));
        List<ITimeInterval> equalObjects = Arrays
                .<ITimeInterval> asList(sameInterval);

        ITimeInterval startOneMillisecondEarlier = new TimeInterval(
                TimePoints.fromMillis(intervalStart.asMilliseconds() - 1L),
                TimePoints.fromMillis(intervalEnd.asMilliseconds()));
        ITimeInterval endOneMillisecondLater = new TimeInterval(
                TimePoints.fromMillis(intervalStart.asMilliseconds()),
                TimePoints.fromMillis(intervalEnd.asMilliseconds() + 1L));

        List<ITimeInterval> unequalObjects = Arrays.<ITimeInterval> asList(
                startOneMillisecondEarlier, endOneMillisecondLater);

        TestUtil.assertEqualsAndHashcodeContract(interval, equalObjects,
                unequalObjects);
    }
}
