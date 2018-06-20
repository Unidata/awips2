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

import java.util.Arrays;
import java.util.List;
import java.util.concurrent.TimeUnit;

import javax.xml.bind.JAXBException;

import org.junit.Test;

import com.raytheon.uf.common.serialization.JAXBManager;
import com.raytheon.uf.common.serialization.SerializationException;
import com.raytheon.uf.common.time.domain.api.IDuration;
import com.raytheon.uf.common.util.TestUtil;

/**
 * Test {@link Duration}.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jan 10, 2013 1286       djohnson     Initial creation
 * 
 * </pre>
 * 
 * @author djohnson
 * @version 1.0
 */

public class DurationTest {

    private static final long TWO_DAYS_IN_NANOS = TimeUnit.DAYS.toNanos(2);

    private static final long TWO_DAYS_IN_MICROS = TimeUnit.DAYS.toMicros(2);

    private static final long TWO_DAYS_IN_MILLIS = TimeUnit.DAYS.toMillis(2);

    private static final long TWO_DAYS_IN_SECONDS = TimeUnit.DAYS.toSeconds(2);

    private static final long TWO_DAYS_IN_MINUTES = TimeUnit.DAYS.toMinutes(2);

    private static final long TWO_DAYS_IN_HOURS = TimeUnit.DAYS.toHours(2);

    private static final long TWO_DAYS_IN_DAYS = TimeUnit.DAYS.toDays(2);

    @Test
    public void testNanosecondsConvertToEntireRange() {
        IDuration duration = new Duration(TWO_DAYS_IN_NANOS,
                TimeUnit.NANOSECONDS);

        assertThat(TWO_DAYS_IN_NANOS, is(equalTo(duration.getNanos())));
        assertThat(TWO_DAYS_IN_MICROS, is(equalTo(duration.getMicros())));
        assertThat(TWO_DAYS_IN_MILLIS, is(equalTo(duration.getMillis())));
        assertThat(TWO_DAYS_IN_SECONDS, is(equalTo(duration.getSeconds())));
        assertThat(TWO_DAYS_IN_MINUTES, is(equalTo(duration.getMinutes())));
        assertThat(TWO_DAYS_IN_HOURS, is(equalTo(duration.getHours())));
        assertThat(TWO_DAYS_IN_DAYS, is(equalTo(duration.getDays())));
    }

    @Test
    public void testNanosecondsConvertToZeroWhenNotEnoughOfTargetUnit() {
        final long startValue = 5L;
        IDuration duration = new Duration(startValue, TimeUnit.NANOSECONDS);

        assertThat(startValue, is(equalTo(duration.getNanos())));
        assertThat(0L, is(equalTo(duration.getMicros())));
        assertThat(0L, is(equalTo(duration.getMillis())));
        assertThat(0L, is(equalTo(duration.getSeconds())));
        assertThat(0L, is(equalTo(duration.getMinutes())));
        assertThat(0L, is(equalTo(duration.getHours())));
        assertThat(0L, is(equalTo(duration.getDays())));
    }

    @Test
    public void testMicrosecondsConvertToEntireRange() {
        IDuration duration = new Duration(TWO_DAYS_IN_MICROS,
                TimeUnit.MICROSECONDS);

        assertThat(TWO_DAYS_IN_NANOS, is(equalTo(duration.getNanos())));
        assertThat(TWO_DAYS_IN_MICROS, is(equalTo(duration.getMicros())));
        assertThat(TWO_DAYS_IN_MILLIS, is(equalTo(duration.getMillis())));
        assertThat(TWO_DAYS_IN_SECONDS, is(equalTo(duration.getSeconds())));
        assertThat(TWO_DAYS_IN_MINUTES, is(equalTo(duration.getMinutes())));
        assertThat(TWO_DAYS_IN_HOURS, is(equalTo(duration.getHours())));
        assertThat(TWO_DAYS_IN_DAYS, is(equalTo(duration.getDays())));
    }

    @Test
    public void testMicrosecondsConvertToZeroWhenNotEnoughOfTargetUnit() {
        final long startValue = 5L;
        IDuration duration = new Duration(startValue, TimeUnit.MICROSECONDS);

        assertThat(startValue, is(equalTo(duration.getMicros())));
        assertThat(0L, is(equalTo(duration.getMillis())));
        assertThat(0L, is(equalTo(duration.getSeconds())));
        assertThat(0L, is(equalTo(duration.getMinutes())));
        assertThat(0L, is(equalTo(duration.getHours())));
        assertThat(0L, is(equalTo(duration.getDays())));
    }

    @Test
    public void testMillisecondsConvertToEntireRange() {
        IDuration duration = new Duration(TWO_DAYS_IN_MILLIS,
                TimeUnit.MILLISECONDS);

        assertThat(TWO_DAYS_IN_NANOS, is(equalTo(duration.getNanos())));
        assertThat(TWO_DAYS_IN_MICROS, is(equalTo(duration.getMicros())));
        assertThat(TWO_DAYS_IN_MILLIS, is(equalTo(duration.getMillis())));
        assertThat(TWO_DAYS_IN_SECONDS, is(equalTo(duration.getSeconds())));
        assertThat(TWO_DAYS_IN_MINUTES, is(equalTo(duration.getMinutes())));
        assertThat(TWO_DAYS_IN_HOURS, is(equalTo(duration.getHours())));
        assertThat(TWO_DAYS_IN_DAYS, is(equalTo(duration.getDays())));
    }

    @Test
    public void testMillisecondsConvertToZeroWhenNotEnoughOfTargetUnit() {
        final long startValue = 5L;
        IDuration duration = new Duration(startValue, TimeUnit.MILLISECONDS);

        assertThat(startValue, is(equalTo(duration.getMillis())));
        assertThat(0L, is(equalTo(duration.getSeconds())));
        assertThat(0L, is(equalTo(duration.getMinutes())));
        assertThat(0L, is(equalTo(duration.getHours())));
        assertThat(0L, is(equalTo(duration.getDays())));
    }

    @Test
    public void testSecondsConvertToEntireRange() {
        IDuration duration = new Duration(TWO_DAYS_IN_SECONDS, TimeUnit.SECONDS);

        assertThat(TWO_DAYS_IN_NANOS, is(equalTo(duration.getNanos())));
        assertThat(TWO_DAYS_IN_MICROS, is(equalTo(duration.getMicros())));
        assertThat(TWO_DAYS_IN_MILLIS, is(equalTo(duration.getMillis())));
        assertThat(TWO_DAYS_IN_SECONDS, is(equalTo(duration.getSeconds())));
        assertThat(TWO_DAYS_IN_MINUTES, is(equalTo(duration.getMinutes())));
        assertThat(TWO_DAYS_IN_HOURS, is(equalTo(duration.getHours())));
        assertThat(TWO_DAYS_IN_DAYS, is(equalTo(duration.getDays())));
    }

    @Test
    public void testSecondsConvertToZeroWhenNotEnoughOfTargetUnit() {
        final long startValue = 5L;
        IDuration duration = new Duration(startValue, TimeUnit.SECONDS);

        assertThat(startValue, is(equalTo(duration.getSeconds())));
        assertThat(0L, is(equalTo(duration.getMinutes())));
        assertThat(0L, is(equalTo(duration.getHours())));
        assertThat(0L, is(equalTo(duration.getDays())));
    }

    @Test
    public void testMinutesConvertToEntireRange() {
        IDuration duration = new Duration(TWO_DAYS_IN_MINUTES, TimeUnit.MINUTES);

        assertThat(TWO_DAYS_IN_NANOS, is(equalTo(duration.getNanos())));
        assertThat(TWO_DAYS_IN_MICROS, is(equalTo(duration.getMicros())));
        assertThat(TWO_DAYS_IN_MILLIS, is(equalTo(duration.getMillis())));
        assertThat(TWO_DAYS_IN_SECONDS, is(equalTo(duration.getSeconds())));
        assertThat(TWO_DAYS_IN_MINUTES, is(equalTo(duration.getMinutes())));
        assertThat(TWO_DAYS_IN_HOURS, is(equalTo(duration.getHours())));
        assertThat(TWO_DAYS_IN_DAYS, is(equalTo(duration.getDays())));
    }

    @Test
    public void testMinutesConvertToZeroWhenNotEnoughOfTargetUnit() {
        final long startValue = 5L;
        IDuration duration = new Duration(startValue, TimeUnit.MINUTES);

        assertThat(startValue, is(equalTo(duration.getMinutes())));
        assertThat(0L, is(equalTo(duration.getHours())));
        assertThat(0L, is(equalTo(duration.getDays())));
    }

    @Test
    public void testHoursConvertToEntireRange() {
        IDuration duration = new Duration(TWO_DAYS_IN_HOURS, TimeUnit.HOURS);

        assertThat(TWO_DAYS_IN_NANOS, is(equalTo(duration.getNanos())));
        assertThat(TWO_DAYS_IN_MICROS, is(equalTo(duration.getMicros())));
        assertThat(TWO_DAYS_IN_MILLIS, is(equalTo(duration.getMillis())));
        assertThat(TWO_DAYS_IN_SECONDS, is(equalTo(duration.getSeconds())));
        assertThat(TWO_DAYS_IN_MINUTES, is(equalTo(duration.getMinutes())));
        assertThat(TWO_DAYS_IN_HOURS, is(equalTo(duration.getHours())));
        assertThat(TWO_DAYS_IN_DAYS, is(equalTo(duration.getDays())));
    }

    @Test
    public void testHoursConvertToZeroWhenNotEnoughOfTargetUnit() {
        final long startValue = 5L;
        IDuration duration = new Duration(startValue, TimeUnit.HOURS);

        assertThat(startValue, is(equalTo(duration.getHours())));
        assertThat(0L, is(equalTo(duration.getDays())));
    }

    @Test
    public void testDaysConvertToEntireRange() {
        IDuration duration = new Duration(TWO_DAYS_IN_DAYS, TimeUnit.DAYS);

        assertThat(TWO_DAYS_IN_NANOS, is(equalTo(duration.getNanos())));
        assertThat(TWO_DAYS_IN_MICROS, is(equalTo(duration.getMicros())));
        assertThat(TWO_DAYS_IN_MILLIS, is(equalTo(duration.getMillis())));
        assertThat(TWO_DAYS_IN_SECONDS, is(equalTo(duration.getSeconds())));
        assertThat(TWO_DAYS_IN_MINUTES, is(equalTo(duration.getMinutes())));
        assertThat(TWO_DAYS_IN_HOURS, is(equalTo(duration.getHours())));
        assertThat(TWO_DAYS_IN_DAYS, is(equalTo(duration.getDays())));
    }

    @Test
    public void testAddDurationReturnsCorrectAmount() {
        IDuration dur1 = new Duration(2, TimeUnit.HOURS);
        IDuration dur2 = new Duration(2, TimeUnit.DAYS);
        
        assertThat(new Duration(50, TimeUnit.HOURS),
                is(equalTo(dur1.plus(dur2))));
    }

    @Test
    public void testSubtractDurationReturnsCorrectAmount() {
        IDuration dur1 = new Duration(2, TimeUnit.DAYS);
        IDuration dur2 = new Duration(2, TimeUnit.HOURS);

        assertThat(new Duration(46, TimeUnit.HOURS),
                is(equalTo(dur1.minus(dur2))));
    }

    @Test
    public void testConversionToAndFromDynamicSerialize()
            throws SerializationException {
        IDuration original = new Duration(2, TimeUnit.DAYS);
        UsesDuration usesDuration = new UsesDuration();
        usesDuration.setDuration(original);

        IDuration restored = transformFromThrift(UsesDuration.class,
                transformToThrift(usesDuration)).getDuration();

        assertThat(restored, is(equalTo(original)));
    }

    @Test
    public void testConversionToAndFromJaxb() throws SerializationException,
            JAXBException {
        IDuration original = new Duration(2, TimeUnit.DAYS);
        UsesDuration usesDuration = new UsesDuration();
        usesDuration.setDuration(original);

        JAXBManager manager = new JAXBManager(UsesDuration.class);
        final String xml = manager.marshalToXml(usesDuration);

        IDuration restored = ((UsesDuration) manager.unmarshalFromXml(xml))
                .getDuration();

        assertThat(restored, is(equalTo(original)));
    }

    @Test
    public void testEqualsAndHashcodeContract() {
        IDuration objectUnderTest = Durations.of(TWO_DAYS_IN_DAYS,
                TimeUnit.DAYS);

        List<IDuration> equalObjects = Arrays.asList(
                Durations.of(TWO_DAYS_IN_HOURS, TimeUnit.HOURS),
                Durations.of(TWO_DAYS_IN_MINUTES, TimeUnit.MINUTES),
                Durations.of(TWO_DAYS_IN_SECONDS, TimeUnit.SECONDS));

        List<IDuration> unequalObjects = Arrays.asList(
                Durations.of(TWO_DAYS_IN_HOURS + 1, TimeUnit.HOURS),
                Durations.of(TWO_DAYS_IN_MINUTES + 1, TimeUnit.MINUTES),
                Durations.of(TWO_DAYS_IN_SECONDS + 1, TimeUnit.SECONDS));

        TestUtil.assertEqualsAndHashcodeContract(objectUnderTest, equalObjects,
                unequalObjects);
    }
}
