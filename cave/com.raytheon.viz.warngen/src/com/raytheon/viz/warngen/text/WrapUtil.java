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
package com.raytheon.viz.warngen.text;

import java.util.regex.Matcher;
import java.util.regex.Pattern;

/**
 * Wraps lines in the warning text that exceed the max width.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Sep 24, 2012    15322   jsanchez     Initial creation
 * Oct 19, 2012    15332   jsanchez     Created a local pattern listOfAreaNamesPtrn.
 * 
 * </pre>
 * 
 * @author jsanchez
 * @version 1.0
 */
public class WrapUtil implements ICommonPatterns {

    /** Maximum width of a warning */
    private static final int MAX_WIDTH = 69;

    private static final String INDENT = "  ";

    private static final String BULLET_START = "* ";

    private static final Pattern wrapUgcPtrn = Pattern.compile("(\\S{1,"
            + (MAX_WIDTH - 1) + "}-)");

    private static final Pattern wrapListOfNamesPtrn = Pattern.compile("(.{1,"
            + (MAX_WIDTH - 4) + "} \\w{2}-)");

    // Locations in 4th bullet or locations paragraph of followup
    // ex: ellipsis, spaces, period
    private static final Pattern wrapDefaultPtrn = Pattern
            .compile("(\\w{1,}\\.\\.\\.)|(AND \\w+\\.\\.\\.)|(\\w+\\.\\.\\.\\s{1,2})|"
                    + "("
                    + LOCK_START
                    + "\\w+"
                    + LOCK_END
                    + "\\.\\.\\.)|\\S+\\.\\.\\."
                    + LOCK_END
                    + "|\\S+\\.\\.\\.|"
                    + "(\\s*\\S+\\s+)|(.+\\.)|(\\S+$)");

    // ugc pattern
    private static final Pattern ugcPtrn = Pattern.compile(ugc);

    // list of areas pattern
    private static final Pattern listOfAreaNamePtrn = Pattern
            .compile(listOfAreaName);

    /**
     * Wraps the text independent of being locked before or after.
     * 
     * @param text
     * @return
     */
    public static String wrap(String text) {
        StringBuffer sb = new StringBuffer();

        boolean inBullet = false;
        String addLine = "";
        String[] lines = text.split("\n");
        for (int i = 0; i < lines.length; i++) {
            String line = lines[i];
            String unlocked = line.replaceAll(LOCK_START + "|" + LOCK_END, "");

            if (unlocked.trim().length() == 0) { // BLANK LINE
                inBullet = false;
                addLine = line;
            } else if (unlocked.length() <= MAX_WIDTH) { // LESS THAN MAX
                // Add indenting if the template didn't account for it yet.
                if (inBullet && !unlocked.startsWith(INDENT)) {
                    sb.append(INDENT);
                }

                if (unlocked.startsWith(BULLET_START)) {
                    inBullet = true;
                }

                addLine = line;
            } else { // NEEDS TO BE WRAPPED
                addLine = wrapLongLines(line, inBullet);
            }

            sb.append(addLine);
            if (i != lines.length - 1) {
                sb.append("\n");
            }
        }

        return sb.toString();
    }

    /**
     * Wraps lines longer than the MAX WIDTH
     * 
     * @param line
     * @param inBullet
     * @return
     */
    private static String wrapLongLines(String line, boolean inBullet) {
        StringBuffer sb = new StringBuffer(line.length());
        String unlockedLine = line.replaceAll(LOCK_START + "|" + LOCK_END, "");

        if (unlockedLine.startsWith(BULLET_START)) {
            inBullet = true;
        }

        Pattern p = getPattern(line);
        Matcher m = p.matcher(line.trim());
        String tmp = inBullet && !unlockedLine.startsWith(BULLET_START) ? INDENT
                : "";

        while (m.find()) {
            String group = m.group();
            String unlocked = group.replaceAll(LOCK_START + "|" + LOCK_END, "");

            int len = tmp.replaceAll(LOCK_START + "|" + LOCK_END, "").length();
            if (len + unlocked.length() > MAX_WIDTH && tmp.trim().length() > 0) {
                sb.append(tmp + "\n");
                tmp = inBullet ? INDENT : "";
                tmp += group;
            } else {
                tmp += group;
            }
        }

        if (tmp.trim().length() > 0) {
            sb.append(tmp);
        }

        return sb.toString();
    }

    /**
     * Helper method to return matcher object for wrapping text.
     * 
     * @param line
     * @return
     */
    private static Pattern getPattern(String line) {

        Matcher m = ugcPtrn.matcher(line);
        // UGC line or FIPS line
        if (m.find()) {
            return wrapUgcPtrn;
        }

        m = listOfAreaNamePtrn.matcher(line);
        // List of area names.
        if (m.find() && !line.startsWith(BULLET_START)) {
            return wrapListOfNamesPtrn;
        }

        return wrapDefaultPtrn;
    }

}
