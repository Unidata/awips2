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
package com.raytheon.viz.hydrocommon.textreport;

import java.util.ArrayList;
import java.util.List;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

/**
 * Text utilities.
 * 
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Sep 22, 2009 2260       mpduff      Initial creation
 * Feb 23, 2018 6862       mpduff      Return an empty string rather than null for null inputs.
 *
 * </pre>
 *
 * @author mpduff
 */

public class TextUtil {
    /**
     * Wrap the text to the number of columns.
     * 
     * @param str
     *            The string to wrap
     * @param numCols
     *            The number of columns per line
     * @param leftMargin
     *            The number of blank spaces before the text
     * @return String array of lines. IF str is null returns an array with one
     *         element of ""
     */
    public static String[] wordWrap(String str, int numCols, int leftMargin) {
        if (str == null) {
            str = "";
        }

        String pattern = ".{0," + numCols + "}([ $|\\s$]|$)";
        String margin = "";
        /* Build the left margin String */
        for (int i = 0; i < leftMargin; i++) {
            margin = margin.concat(" ");
        }

        Pattern wrapPattern = Pattern.compile(pattern);

        List<String> list = new ArrayList<>();

        Matcher m = wrapPattern.matcher(str);

        while (m.find()) {
            if (leftMargin > 0) {
                list.add(margin + m.group());
            } else {
                list.add(m.group());
            }
        }

        return list.toArray(new String[list.size()]);
    }

    /**
     * Remove any embedded new lines.
     * 
     * @param text
     *            The text to remove the new lines from.
     * @return The text without the new lines. Null input is returned as ""
     */
    public static String removeEmbeddedNewlines(String text) {
        if ((text == null) || (text.length() <= 0)) {
            return "";
        }

        /* replace newline with a space */
        text = text.replace("\n", " ");

        /* add a 2nd space after a period */
        text = text.replace("\\.", ". ");

        return text;
    }
}
