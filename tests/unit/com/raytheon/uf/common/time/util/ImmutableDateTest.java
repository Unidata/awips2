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
package com.raytheon.uf.common.time.util;

import static org.junit.Assert.assertEquals;

import java.util.Date;

import org.junit.Test;

import com.raytheon.uf.common.serialization.SerializationException;
import com.raytheon.uf.common.serialization.SerializationUtil;

/**
 * Test {@link ImmutableDate}.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Aug 15, 2012 0743       djohnson     Initial creation
 * 
 * </pre>
 * 
 * @author djohnson
 * @version 1.0
 */

public class ImmutableDateTest {

    private final Date date = new ImmutableDate();

    @Test
    public void testConstructorAcceptingDateSetsCorrectTime() {
        Date date = new Date();
        Date immutable = new ImmutableDate(date);

        assertEquals("The two date objects should have the same time!",
                date.getTime(), immutable.getTime());
    }

    @Test
    public void testConstructorAcceptingLongSetsCorrectTime() {
        long time = System.currentTimeMillis();
        Date immutable = new ImmutableDate(time);

        assertEquals("The date object should have the specified time!", time,
                immutable.getTime());
    }

    @Test(expected = UnsupportedOperationException.class)
    public void testSetYearThrowsUnsupportedOperationException() {
        date.setYear(2012);
    }

    @Test(expected = UnsupportedOperationException.class)
    public void testSetMonthThrowsUnsupportedOperationException() {
        date.setMonth(1);
    }

    @Test(expected = UnsupportedOperationException.class)
    public void testSetDateThrowsUnsupportedOperationException() {
        date.setDate(1);
    }

    @Test(expected = UnsupportedOperationException.class)
    public void testSetHoursThrowsUnsupportedOperationException() {
        date.setHours(1);
    }

    @Test(expected = UnsupportedOperationException.class)
    public void testSetMinutesThrowsUnsupportedOperationException() {
        date.setMinutes(1);
    }

    @Test(expected = UnsupportedOperationException.class)
    public void testSetSecondsThrowsUnsupportedOperationException() {
        date.setSeconds(1);
    }

    @Test(expected = UnsupportedOperationException.class)
    public void testSetTimeThrowsUnsupportedOperationException() {
        date.setTime(1L);
    }

    @Test
    public void testCanBeThriftedAndRestoredAsImmutableDate()
            throws SerializationException {
        ImmutableDate date = new ImmutableDate();
        byte[] serialized = SerializationUtil.transformToThrift(date);
        ImmutableDate unserialized = (ImmutableDate) SerializationUtil
                .transformFromThrift(serialized);

        assertEquals(
                "The unserialized version should have the same time as the serialized version!",
                date.getTime(), unserialized.getTime());
    }
}
