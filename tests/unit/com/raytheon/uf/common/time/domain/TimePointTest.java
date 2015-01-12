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
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertThat;
import static org.junit.Assert.assertTrue;

import java.util.Arrays;
import java.util.Date;
import java.util.List;

import javax.xml.bind.JAXBException;

import org.junit.Test;

import com.raytheon.uf.common.serialization.JAXBManager;
import com.raytheon.uf.common.serialization.SerializationException;
import com.raytheon.uf.common.time.domain.api.ITimeInterval;
import com.raytheon.uf.common.time.domain.api.ITimePoint;
import com.raytheon.uf.common.time.util.TimeUtil;
import com.raytheon.uf.common.util.TestUtil;

/**
 * Test {@link TimePoint}.
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

public class TimePointTest {

    private final ITimePoint earlierPoint = new TimePoint(1L);

    private final ITimePoint laterPoint = new TimePoint(2L);

    private final ITimePoint sameAsEarlierPoint = new TimePoint(earlierPoint);

    @Test
    public void testConversionToAndFromDynamicSerialize()
            throws SerializationException {
        UsesTimePoint usesTimePoint = new UsesTimePoint();
        usesTimePoint.setTimePoint(laterPoint);

        ITimePoint restored = transformFromThrift(UsesTimePoint.class,
                transformToThrift(usesTimePoint)).getTimePoint();

        assertThat(restored, is(equalTo(laterPoint)));
    }

    @Test
    public void testConversionToAndFromJaxb() throws SerializationException,
            JAXBException {
        UsesTimePoint usesTimePoint = new UsesTimePoint();
        usesTimePoint.setTimePoint(laterPoint);

        JAXBManager manager = new JAXBManager(UsesTimePoint.class);
        final String xml = manager.marshalToXml(usesTimePoint);

        ITimePoint restored = ((UsesTimePoint) manager.unmarshalFromXml(xml))
                .getTimePoint();

        assertThat(restored, is(equalTo(laterPoint)));
    }

    @Test
    public void testAsDateReturnsSameInstance() {
        Date date = new Date();

        TimePoint timePoint = new TimePoint(date.getTime());

        assertThat(timePoint.asDate(), is(equalTo(date)));
    }

    @Test
    public void testAsMillisecondsReturnsOriginalValue() {
        final long originalMillis = TimeUtil.currentTimeMillis();
        TimePoint timePoint = new TimePoint(originalMillis);

        assertThat(timePoint.asMilliseconds(), is(equalTo(originalMillis)));
    }

    @Test
    public void testIsAfterReturnsTrueWhenLater() {
        assertTrue("The later point should have been recognized as later!",
                laterPoint.isAfter(earlierPoint));
    }

    @Test
    public void testIsAfterReturnsFalseWhenEarlier() {
        assertFalse(
                "The earlier point should have been recognized as not later!",
                earlierPoint.isAfter(laterPoint));
    }

    @Test
    public void testIsAfterReturnsFalseWhenSameTime() {
        assertFalse(
                "The earlier point should have been recognized as not later!",
                sameAsEarlierPoint.isAfter(laterPoint));
    }

    @Test
    public void testIsBeforeReturnsTrueWhenEarlier() {
        assertTrue("The earlier point should have been recognized as earlier!",
                earlierPoint.isBefore(laterPoint));
    }

    @Test
    public void testIsBeforeReturnsFalseWhenLater() {
        assertFalse(
                "The later point should have been recognized as not earlier!",
                laterPoint.isBefore(earlierPoint));
    }

    @Test
    public void testIsBeforeReturnsFalseWhenSameTime() {
        assertFalse(
                "The same time point should have been recognized as not earlier!",
                sameAsEarlierPoint.isBefore(earlierPoint));
    }

    @Test
    public void testIsSameReturnsFalseWhenEarlier() {
        assertFalse("The earlier point should not have been the same!",
                earlierPoint.isSame(laterPoint));
    }

    @Test
    public void testIsSameReturnsFalseWhenLater() {
        assertFalse("The later point should not have been the same!",
                laterPoint.isSame(earlierPoint));
    }

    @Test
    public void testIsSameReturnsTrueWhenSameTime() {
        assertTrue(
                "The same time point should have been recognized as the same time!",
                sameAsEarlierPoint.isSame(earlierPoint));
    }

    @Test
    public void testIsWithinReturnsTrueForTimeIntervalThatContainsIt() {
        ITimeInterval interval = TimeIntervals.fromTimePoints(earlierPoint,
                laterPoint);
        assertTrue(
                "The time point should have returned true for being within the interval",
                earlierPoint.isWithin(interval));
    }

    @Test
    public void testIsWithinReturnsFalseForTimeIntervalThatDoesntContainIt() {
        ITimeInterval interval = TimeIntervals.fromTimePoints(earlierPoint,
                laterPoint);

        assertFalse(
                "The time point should have returned false for being within the interval",
                new TimePoint(earlierPoint.asMilliseconds() - 1L)
                        .isWithin(interval));
    }

    @Test
    public void testEqualsAndHashcodeContract() {
        List<ITimePoint> equalObjects = Arrays.asList(sameAsEarlierPoint);
        List<ITimePoint> unequalObjects = Arrays.asList(laterPoint,
                TimePoints.now());

        TestUtil.assertEqualsAndHashcodeContract(earlierPoint, equalObjects,
                unequalObjects);
    }
}
