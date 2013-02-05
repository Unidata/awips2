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
import java.util.Date;

import org.hamcrest.Description;
import org.hamcrest.TypeSafeMatcher;
import org.junit.Ignore;

/**
 * Date matchers for JUnit/Hamcrest, intentionally package-private.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Feb 01, 2013 1543       djohnson     Initial creation
 * 
 * </pre>
 * 
 * @author djohnson
 * @version 1.0
 */
@Ignore
final class DateMatchers {
    /**
     * Prevent construction.
     */
    private DateMatchers() {
    }

    static class DateFormatMatcher extends TypeSafeMatcher<Date> {

        private final DateFormat format;

        private final String expectedDateFormat;

        private String actualFormatted;

        DateFormatMatcher(DateFormat format, Date expected) {
            this.format = format;
            this.expectedDateFormat = format.format(expected);
        }

        /**
         * {@inheritDoc}
         */
        @Override
        public void describeTo(Description description) {
            description.appendText("a formatted date of ");
            description.appendValue(expectedDateFormat);
            description.appendText(" not ");
            description.appendValue(actualFormatted);
        }

        /**
         * {@inheritDoc}
         */
        @Override
        public boolean matchesSafely(Date item) {
            actualFormatted = format.format(item);
            return expectedDateFormat.equals(actualFormatted);
        }
    }
}
