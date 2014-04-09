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
package com.raytheon.openfire.plugin.detailedfeedlog;

import java.text.DateFormat;
import java.text.ParseException;
import java.util.Date;
import java.util.regex.Pattern;

/**
 * Contains all the necessary information to be stored in memory for useful
 * logging
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jul 27, 2012            mnash       Initial creation
 * Apr 07, 2014 2937       bgonzale    Changed to immutable. Use predefined patterns
 *                                     and a constant for pipe.
 * 
 * </pre>
 * 
 * @author mnash
 * @version 1.0
 */

public class LogEntry {

    private static final String PIPE = "|";

    private static final Pattern PIPE_PATTERN = Pattern.compile("\\|");

    private static final Pattern AT_PATTERN = Pattern.compile("@");

    private static final Pattern DATE_PATTERN = Pattern.compile("\\(|\\)");

    private final Date date;

    private final String site;

    private final String username;

    private final String message;

    /**
     * Initialization Constructor.
     */
    public LogEntry(Date date, String site, String username, String message) {
        this.date = date;
        this.site = site;
        this.username = username;
        this.message = message;
    }

    /**
     * @return the date
     */
    public Date getDate() {
        return date;
    }

    /**
     * @return the username
     */
    public String getUsername() {
        return username;
    }

    /**
     * @return the user without host information
     */
    public String getUser() {
        return AT_PATTERN.split(username)[0];
    }

    /**
     * @return the message
     */
    public String getMessage() {
        return message;
    }

    /**
     * @return the site
     */
    public String getSite() {
        return site;
    }

    /**
     * Parse a String and create a LineEntry.
     * 
     * @param line
     * @param dateFormat
     * @return a LineEntry
     * @throws ParseException
     * @throws ArrayIndexOutOfBoundsException
     */
    public static LogEntry fromString(String line, DateFormat dateFormat)
            throws ParseException, ArrayIndexOutOfBoundsException {
        String[] splitLine = PIPE_PATTERN.split(line);
        String dateString = splitLine[0];
        // get the string date without parenthesis
        dateString = DATE_PATTERN.split(dateString)[1];
        Date date = dateFormat.parse(dateString);
        String username = splitLine[1];
        String site = splitLine[2];
        String message = splitLine[3];

        return new LogEntry(date, site, username, message);
    }

    /**
     * Generate a formatted LineEntry String that is readable by the fromString
     * method.
     * 
     * @param dateFormat
     * @return formated String representing a LineEntry
     */
    public String toString(DateFormat dateFormat) {
        StringBuilder sb = new StringBuilder("(")
                .append(dateFormat.format(date)).append(")").append(PIPE)
                .append(username).append(PIPE).append(site).append(PIPE)
                .append(message);
        return sb.toString();
    }

}
