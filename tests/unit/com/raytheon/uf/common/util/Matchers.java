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
package com.raytheon.uf.common.util;

import java.text.DateFormat;
import java.text.SimpleDateFormat;
import java.util.Date;
import java.util.TimeZone;

import org.hamcrest.Description;
import org.hamcrest.Matcher;
import org.junit.internal.matchers.TypeSafeMatcher;

/**
 * Custom Hamcrest matcher implementations.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jan 04, 2013 1453       djohnson     Initial creation
 * 
 * </pre>
 * 
 * @author djohnson
 * @version 1.0
 */

public final class Matchers {

    private static class DateFormatMatcher extends TypeSafeMatcher<Date> {

        private final DateFormat format;

        private final String expectedDateFormat;

        private DateFormatMatcher(DateFormat format, Date expected) {
            this.format = format;
            this.expectedDateFormat = format.format(expected);
        }

        /**
         * {@inheritDoc}
         */
        @Override
        public void describeTo(Description description) {
            description.appendText("a formatted date of ").appendText(
                    expectedDateFormat);
        }

        /**
         * {@inheritDoc}
         */
        @Override
        public boolean matchesSafely(Date item) {
            return format.format(item).equals(expectedDateFormat);
        }
    }

    private static final SimpleDateFormat YYYY_MM_DD_FORMAT = new SimpleDateFormat(
            "yyyyMMdd");
    static {
        YYYY_MM_DD_FORMAT.setTimeZone(TimeZone.getTimeZone("GMT"));
    }

    private Matchers() {
    }

    public static <T> Matcher<Date> yyyyMmDdMatches(Date expected) {
        return new DateFormatMatcher(YYYY_MM_DD_FORMAT, expected);
    }

}
