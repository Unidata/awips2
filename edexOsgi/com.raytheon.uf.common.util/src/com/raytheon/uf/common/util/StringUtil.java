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

import java.util.ArrayList;
import java.util.Collection;
import java.util.Iterator;
import java.util.List;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import org.apache.commons.lang.StringUtils;

/**
 * This class is for static methods that manipulate strings.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Oct 20, 2011            rferrel     Initial creation
 * Jul 13, 2012 740        djohnson    Add join.
 * Nov 09, 2012 1322       djohnson    Add NEWLINE, createMessage.
 * Mar 02, 2013 1970       bgonzale    Added fast string replacement method.
 * Apr 02, 2014 2915       dgilling    Added left and right trim methods.
 * 
 * </pre>
 * 
 * @author rferrel
 * @version 1.0
 */
public final class StringUtil {

    public static final String NEWLINE = System.getProperty("line.separator");

    private StringUtil() {

    }

    /**
     * Splits a string using given separator characters; strings are trimmed and
     * empty entries removed.
     * 
     * @see org.apache.commons.lang.StringUtils#split
     * 
     * @param str
     *            the string to split
     * @param separatorChar
     *            Characters to use as separators
     * @return An array of trimmed non-empty strings.
     * 
     */
    public static String[] split(final String str, final String separatorChar) {
        String[] result = null;
        if (str != null) {
            result = StringUtils
                    .stripAll(StringUtils.split(str, separatorChar));
            List<String> list = new ArrayList<String>();

            for (String s : result) {
                if (s.isEmpty() == false) {
                    list.add(s);
                }
            }

            if (result.length != list.size()) {
                result = new String[list.size()];
                list.toArray(result);
            }
        }
        return result;
    }

    /**
     * Concatenate an array of object into a single string with each array
     * element's toString() value separated by the joinCharacter.
     * 
     * @param portions
     *            the array of objects
     * @param joinCharacter
     *            the character to join them with
     * @return the concatenated string
     */
    public static <T> String join(final T[] portions, final char joinCharacter) {
        StringBuilder stringBuilder = new StringBuilder();

        if (CollectionUtil.isNullOrEmpty(portions)) {
            return null;
        }

        for (T portion : portions) {
            stringBuilder.append(portion);
            stringBuilder.append(joinCharacter);
        }
        stringBuilder.deleteCharAt(stringBuilder.length() - 1);

        return stringBuilder.toString();
    }

    /**
     * Concatenate a collection of objects into a single string with each
     * object's toString() value separated by the joinCharacter.
     * 
     * @param portions
     *            the collections of objects
     * @param joinCharacter
     *            the character to join them with
     * @return the concatenated string
     */
    public static <T> String join(final Collection<T> portions,
            final char joinCharacter) {
        StringBuilder stringBuilder = new StringBuilder();

        if (CollectionUtil.isNullOrEmpty(portions)) {
            return null;
        }

        for (T portion : portions) {
            stringBuilder.append(portion);
            stringBuilder.append(joinCharacter);
        }
        stringBuilder.deleteCharAt(stringBuilder.length() - 1);

        return stringBuilder.toString();
    }

    /**
     * Creates a message based on the preamble and the provided iterables, each
     * iterable will be displayed on its own line.
     * 
     * @param preamble
     *            the preamble message, such as
     *            <code>String preamble = "The following are numbers:"</code>
     * @param iterables
     *            the iterable to retrieve items from, such as<br>
     *            <code>List&lt;String&gt; iterables = Arrays.asList("one", "two");</code>
     * @return the message
     */
    public static String createMessage(String preamble, Iterable<?> iterables) {
        return createMessage(preamble, iterables, 0);
    }

    /**
     * Creates a message based on the preamble and the provided iterables, each
     * iterable will be displayed on its own line.
     * 
     * @param preamble
     *            the preamble message, such as
     *            <code>String preamble = "The following are numbers:"</code>
     * @param iterables
     *            the iterable to retrieve items from, such as<br>
     *            <code>List&lt;String&gt; iterables = Arrays.asList("one", "two");</code>
     * @param iterableIndent
     *            the number of spaces to indent each iterable
     * @return the message
     */
    public static String createMessage(String preamble, Iterable<?> iterables,
            int iterableIndent) {
        StringBuilder msg = new StringBuilder(preamble)
                .append(StringUtil.NEWLINE);
        for (Iterator<?> iter = iterables.iterator(); iter.hasNext();) {
            for (int i = 0; i < iterableIndent; i++) {
                msg.append(' ');
            }
            msg.append(iter.next());

            if (iter.hasNext()) {
                msg.append(StringUtil.NEWLINE);
            }
        }

        return msg.toString();
    }

    /**
     * Fast replacement of all String target elements in String source with
     * String replacement.
     * 
     * @param source
     *            String that instances will be replaced in.
     * @param target
     * @param replacement
     * @return a new String equivalent to source with target Strings replaced by
     *         String replacement
     */
    public static String replace(final String source, final String target,
            final String replacement) {
        int targetIndex = 0;
        StringBuilder sb = new StringBuilder(source);

        while ((targetIndex = sb.indexOf(target, targetIndex)) > -1) {
            sb.replace(targetIndex, targetIndex + target.length(), replacement);
            targetIndex += replacement.length();
        }
        return sb.toString();
    }

    /**
     * Get a string as a separated list showing up to the limit of items.
     * 
     * @param list
     *            List of items to put in the "list"
     * @param delimiter
     *            Delimiting String
     * @param limit
     *            number of items to display
     * @return the list
     */
    public static String getDisplayList(Collection<String> list,
            String delimiter, int limit) {
        StringBuilder sb = new StringBuilder();
        if (list.size() < limit) {
            limit = list.size();
        }

        int count = 0;
        for (String s : list) {
            if (count < limit) {
                sb.append(s).append(delimiter);
            } else {
                // remove the trailing space
                sb.replace(sb.length() - 1, sb.length(), "");
                sb.append("...");
                break;
            }
            count++;
        }

        return sb.toString();
    }

    /**
     * Create a list with all the lines except the first indented.
     * 
     * @param list
     *            list of items
     * @param indent
     *            String of spaces making up the indent
     * @return the list
     */
    public static String getIndentedList(Collection<String> list,
            final String indent) {
        StringBuilder sb = new StringBuilder();

        int count = 0;
        for (String id : list) {
            if (count == 10) {
                sb.append(StringUtil.NEWLINE);
                sb.append(indent);
                count = 0;
            }
            sb.append(id).append(" ");
            count++;
        }

        return sb.toString();

    }

    /**
     * Wraps input Object.toString (if non-null) in [] for display (helps show
     * empty string in output).<br>
     * Example: printString("test") would display "[test]"<br>
     * printString(null) would display "[null]"
     * 
     * @param obj
     *            An object instance
     * @return The object's {@link Object#toString()} value
     */
    public static final String printString(Object obj) {
        return "[" + (obj == null ? "null" : obj.toString()) + "]";
    }

    /**
     * Simple check if str is null or empty.
     * 
     * @param str
     *            A string to check
     * @return true if string is null or empty, false otherwise
     */
    public static final boolean isEmptyString(String str) {
        return (str == null) || ("".equals(str));
    }

    /**
     * Determines if the given string is all alpha-numeric characters
     * 
     * @param str
     *            The string to test
     * @return True if the string is alpha-numeric
     */
    public static boolean isAlnum(String str) {
        int count = 0;
        Pattern pat = Pattern.compile("\\p{Alnum}");
        Matcher mat = pat.matcher(str);

        while (mat.find()) {
            count++;
        }

        if (count == str.length()) {
            return true;
        } else {
            return false;
        }

    }

    /**
     * Returns a copy of the string, with only leading whitespace omitted.
     * <p>
     * Like String.trim(), whitespace is defined as any character with a code
     * less than or equal to <code>'&#92;u0020'</code> (the space character).
     * 
     * @param s
     *            The <code>String</code> to trim.
     * @return A copy of this string with leading white space removed, or the
     *         passed in string if it has no leading white space.
     */
    public static String ltrim(String s) {
        int i = 0;
        while ((i < s.length()) && (s.charAt(i) <= ' ')) {
            i++;
        }
        return (i > 0) ? s.substring(i) : s;
    }

    /**
     * Returns a copy of the string, with only trailing whitespace omitted.
     * <p>
     * Like String.trim(), whitespace is defined as any character with a code
     * less than or equal to <code>'&#92;u0020'</code> (the space character).
     * 
     * @param s
     *            The <code>String</code> to trim.
     * @return A copy of this string with trailing white space removed, or the
     *         passed in string if it has no trailing white space.
     */
    public static String rtrim(String s) {
        int i = s.length();
        while ((i > 0) && (s.charAt(i - 1) <= ' ')) {
            i--;
        }
        return (i < s.length()) ? s.substring(0, i) : s;
    }
}
