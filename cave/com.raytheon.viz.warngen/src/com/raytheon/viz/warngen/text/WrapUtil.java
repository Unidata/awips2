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
 * Apr 18, 2013 DR 16055   D. Friedman  Fix word wrapping of long area names.
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

    // Guess at the amount of extra characters added by wrapping
    private static final int WRAP_COUNT_GUESS = 16 * 3; // *3 for bullet indent + \n

    private static final String DELIM_GROUP = "\0";
    private static final String[] NORMAL_DELIMS = { " ", "...", DELIM_GROUP, "-" };
    private static final String[] AREA_DELIMS = { "-", "...", DELIM_GROUP, " " };
    private static final String[] UGC_DELIMS = AREA_DELIMS;

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
            } else {
                boolean wasInBullet = inBullet;

                if (unlocked.startsWith(BULLET_START)) {
                    inBullet = true;
                }

                // Add indenting if the template didn't account for it yet.
                int add = wasInBullet && !unlocked.startsWith(INDENT) ? INDENT
                        .length() : 0;
                if (unlocked.length() <= MAX_WIDTH - add) { // LESS THAN MAX
                    if (add > 0) {
                        sb.append(INDENT);
                    }
                    addLine = line;
                } else { // NEEDS TO BE WRAPPED
                    addLine = wrapLongLines(line, inBullet);
                }
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
        String[] delims = getDelimiters(line);
        return wrapLongLine(line, inBullet, delims, MAX_WIDTH);
    }

    /**
     * Wraps a line of text to the given maximum width, if possible, by breaking
     * at the given delimiters.
     * 
     * @param line
     * @param inBullet
     * @param delims
     *            a list of delimiters divided into groups by the object
     *            instance DELIM_GROUP. A delimiter in an earlier group will be
     *            used before using any delimiter from a later group. Within a
     *            group, the delimiter closest to the margin is used.
     * @param maxLength
     * @return
     */
    public static String wrapLongLine(String line, boolean inBullet, String[] delims, int maxLength) {
        StringBuilder sb = new StringBuilder(line.length() + WRAP_COUNT_GUESS);
        int start = 0;
        int allowLength = maxLength;
        boolean failed = false;

        /*
         * Skip over initial bullet point and/or indent OR add indentation if
         * missing.
         */
        if (inBullet) {
            char[] cb = new char[2];
            int i = indexIgnoringLockMarkers(line, start, 0);
            int o = 0;
            while (i < line.length() && o < 2) {
                cb[o++] = line.charAt(i);
                i = indexIgnoringLockMarkers(line, i, 1);
            }
            if (o == 2) {
                String lead = new String(cb, 0, o);
                if (INDENT.equals(lead) || BULLET_START.equals(lead)) {
                    allowLength -= lead.length();
                    start = i;
                    sb.append(line, 0, i);
                } else {
                    allowLength -= INDENT.length();
                    sb.append(INDENT);
                }
            }
        }

        while (start < line.length()) {
            int limit = indexIgnoringLockMarkers(line, start, allowLength);
            if (limit >= line.length()) {
                appendRTrim(line, start, line.length(), sb);
                break;
            }

            /*
             * Find delimiter in a group of delimiters that is closest to the
             * wrap margin. If not found, try again with the next group.
             * 
             * Searches before the limit (or at the limit if the delimiter is a
             * space). If the whole process has failed once, searches after the
             * limit.
             */
            String bestDelim = null;
            int bestP = -1;
            for (String delim : delims) {
                if (delim == DELIM_GROUP) {
                    if (bestDelim != null)
                        break;
                    continue;
                }
                int backup = " ".equals(delim) ? 0 : delim.length();
                int p = !failed ? line.lastIndexOf(delim, limit - backup)
                        : line.indexOf(delim, limit - backup);
                if (p >= start && (bestDelim == null
                        || !failed ? p > bestP : p < bestP)) {
                    bestDelim = delim;
                    bestP = p;
                }
            }

            if (bestDelim != null) {
                failed = false;
                int next = bestP + bestDelim.length();
                int segmentEnd = " ".equals(bestDelim) ? bestP : next;
                appendRTrim(line, start, segmentEnd, sb);
                start = splitEndOfLine(line, next, inBullet, sb);

                if (inBullet) {
                    allowLength = maxLength - INDENT.length();
                }
            } else if (! failed) {
                /*
                 * Failed to wrap before the margin. Try again, wrapping the
                 * line after the margin, but still as close to it as possible.
                 */
                failed = true;
                continue;
            } else {
                /*
                 * Failed to find any kind of break. Just dump the rest of the
                 * text.
                 */
                appendRTrim(line, start, line.length(), sb);
                break;
            }
        }

        return sb.toString();
    }

    /**
     * Helper method to return matcher object for wrapping text.
     * 
     * @param line
     * @return
     */
    private static String[] getDelimiters(String line) {
        Matcher m = ugcPtrn.matcher(line);
        // UGC line or FIPS line
        if (m.find()) {
            return UGC_DELIMS;
        }

        m = listOfAreaNamePtrn.matcher(line);
        // List of area names.
        if (m.find() && !line.startsWith(BULLET_START)) {
            return AREA_DELIMS;
        }

        return NORMAL_DELIMS;
    }

    /**
     * Returns the index in {@code text} that is {@code count} characters after
     * {@code start}, skipping past any lock markers. This includes any lock
     * markers at the desired index. This greediness prevents wrapping to a new
     * line that would only contain lock markers.
     * 
     * @param text
     * @param start
     *            0 <= start < text.length()
     * @param count
     *            Number of characters to skip. May be greater than the number
     *            of characters in text past start.
     * @return
     */
    private static int indexIgnoringLockMarkers(String text, int start, int count) {
        int i = start;
        do {
            if (text.startsWith(LOCK_START, i))
                i += LOCK_START.length();
            else if (text.startsWith(LOCK_END, i))
                i += LOCK_END.length();
            else if (count > 0) {
                if (i >= text.length())
                    break;
                ++i;
                --count;
            } else
                break;
        } while (true);
        return i;
    }

    /**
     * Append text.substring(start, end) to sb, removing any trailing
     * whitespace. The trailing whitespace may have have embedded lock marks.
     */
    private static void appendRTrim(String text, int start, int end, StringBuilder sb) {
        int sbStart = sb.length();
        sb.append(text, start, end);
        int i = sb.length();
        while (i > sbStart) {
            if (Character.isWhitespace(sb.charAt(i - 1))) {
                sb.deleteCharAt(--i);
            } else if (i - sbStart >= LOCK_START.length() &&
                    LOCK_START.equals(sb.substring(i - LOCK_START.length(), i))) {
                i -= LOCK_START.length();
            } else if (i - sbStart >= LOCK_END.length() &&
                    LOCK_END.equals(sb.substring(i - LOCK_END.length(), i))) {
                i -= LOCK_END.length();
            } else
                break;
        }
    }

    /**
     * Handle whitespace and lock markers between line breaks. Adds line break
     * and indentation for the next line as necessary.
     * 
     * <ul>
     * <li>If there is nothing but whitespace and lock markers remaining, append
     * all of it to the current line so as to not create an extra empty blank
     * line.</li>
     * <li>If there is an end-lock marker, include it on the current line and
     * break after it.</li>
     * <li>If there is a start-lock marker, break before it.
     * </ul>
     * 
     * @return The index in text at which processing for the next line should
     *         begin.
     */
    private static int splitEndOfLine(String text, int start, boolean inBullet, StringBuilder sb) {
        int goodBreak = start;
        int i = start;

        while (i < text.length()) {
            if (Character.isWhitespace(text.charAt(i))) {
                ++i;
            } else if (text.startsWith(LOCK_START, i)) {
                goodBreak = i;
                i += LOCK_START.length();
                break;
            } else if (text.startsWith(LOCK_END, i)) {
                i += LOCK_END.length();
                goodBreak = i;
                break;
            } else
                break;
        }

        if (i >= text.length())
            goodBreak = i;
        if (goodBreak >= start) {
            appendRTrim(text, start, goodBreak, sb);
        }
        if (i < text.length()) {
            sb.append('\n');
            if (inBullet) {
                sb.append(INDENT);
            }
            appendRTrim(text, goodBreak, i, sb);
        }
        return i;
    }
}
