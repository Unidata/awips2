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

import static org.hamcrest.CoreMatchers.equalTo;
import static org.hamcrest.CoreMatchers.is;
import static org.junit.Assert.assertThat;

import java.util.concurrent.TimeUnit;

import org.junit.Test;

import com.raytheon.uf.common.time.domain.api.IDuration;

/**
 * Test {@link Durations}.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jan 11, 2013 1286       djohnson     Initial creation
 * 
 * </pre>
 * 
 * @author djohnson
 * @version 1.0
 */

public class DurationsTest {

    @Test
    public void testParseFromString() {
        IDuration duration = Durations.fromString("3 SECONDS");

        assertThat(duration, is(equalTo(Durations.of(3, TimeUnit.SECONDS))));
    }

    @Test(expected = IllegalArgumentException.class)
    public void testUnparseableStringThrowsException() {
        Durations.fromString("unparseable");
    }

    @Test
    public void testToStringWritesOutLargestUnitNotLosingPrecision() {
        final String original = "3 SECONDS";
        IDuration duration = Durations.fromString(original);

        assertThat(Durations.toString(duration), is(equalTo(original)));
    }

    @Test
    public void testToStringForSmallestUnit() {
        final String original = "1 NANOSECONDS";
        IDuration duration = Durations.fromString(original);

        assertThat(Durations.toString(duration), is(equalTo(original)));
    }

    @Test
    public void testToStringForLargestUnit() {
        final String original = "7 DAYS";
        IDuration duration = Durations.fromString(original);

        assertThat(Durations.toString(duration), is(equalTo(original)));
    }

    @Test
    public void test128MinutesDoesNotUseHours() {
        final String original = "128 MINUTES";
        IDuration duration = Durations.fromString(original);

        assertThat(Durations.toString(duration), is(equalTo(original)));
    }
}
