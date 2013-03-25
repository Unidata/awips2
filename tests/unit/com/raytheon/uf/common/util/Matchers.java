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

import java.io.File;
import java.text.SimpleDateFormat;
import java.util.Date;
import java.util.TimeZone;

import org.hamcrest.Matcher;
import org.junit.Ignore;

import com.raytheon.uf.common.util.DateMatchers.DateFormatMatcher;
import com.raytheon.uf.common.util.FileMatchers.DirectoryNumberOfFilesMatcher;

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
@Ignore
public final class Matchers {

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

    public static <T> Matcher<File> hasNumberOfFiles(final int numberOfFiles) {
        return new DirectoryNumberOfFilesMatcher(numberOfFiles);
    }

    public static <T> Matcher<File> hasNoFiles() {
        return hasNumberOfFiles(0);
    }

}
