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

package com.raytheon.edex.plugin.ccfp;

import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;
import java.util.NoSuchElementException;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;

import com.raytheon.edex.esb.Headers;
import com.raytheon.edex.plugin.AbstractRecordSeparator;
import com.raytheon.edex.util.Util;

/**
 * 
 * CCFP Separator
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#     Engineer    Description
 * ------------ ----------  ----------- --------------------------
 * 03/03/2007   908         bwoodle     initial creation
 * 12/03/2008               chammack    Camel refactor
 * 09/16/2009   3027      njensen        Static patterns
 * 01/02/2013	DCS 135		tk			use \\r* for testing end of line
 * 
 * </pre>
 * 
 * @author bwoodle
 * @version 1
 */

public class CcfpSeparator extends AbstractRecordSeparator {

    private final Log theLogger = LogFactory.getLog(getClass());

    /** Regex used for separating multi-record files */
    private static final Pattern DATASET = Pattern
            .compile("CCFP \\d{8}_\\d{4} \\d{8}_\\d{4}([\\r*\\n]+(AREA|LINE).*)*");

    /** Regex to pull AWIPS Header */
    private static final Pattern AWIPSHEADER = Pattern
            .compile("[\\r*\\n]+(TAF|MTR|CFP)([\\p{Alnum} ]{3})[\\r*\\n]+");

    /** Regex used for extracting the header */
    private static final Pattern HEADERREGEX = Pattern
            .compile("[A-Z]{4}[0-9]{1,2} [A-Z]{4} [0-9]{6}(?: [A-Z]{3})?");

    /** Regex used for extracting a line */
    private static final Pattern TIMEREGEX = Pattern
            .compile("CCFP \\d{8}_\\d{4} \\d{8}_\\d{4}");

    /** Regex used for extracting a line */
    private static final Pattern LINEREGEX = Pattern.compile("AREA.*|LINE.*");

    private static final Pattern CANADA_FLAG = Pattern
            .compile("CANADA ON|CANADA OFF");

    /** The WMO header */
    private String header;

    /** The AWIPS header */
    private String awipsheader;

    private String canadaflag;

    /** List of individual areas in file */
    private List<String> splitAreas;

    /** List of records contained in file */
    private List<String> bodyRecords;

    /** List of records contained in file */
    private List<String> records;

    private Iterator<String> iterator = null;

    public static CcfpSeparator separate(byte[] data, Headers headers) {
        CcfpSeparator sep = new CcfpSeparator();
        sep.setData(data, headers);
        return sep;
    }

    public CcfpSeparator() {
        bodyRecords = new ArrayList<String>();
        records = new ArrayList<String>();
    }

    @Override
    public String next() {
        try {
            String temp = iterator.next();
            if (Util.isEmptyString(temp)) {
                return "";
            } else {
                return temp;
            }
        } catch (NoSuchElementException e) {
            return "";
        }
    }

    @Override
    public boolean hasNext() {
        if (iterator == null) {
            return false;
        } else {
            return iterator.hasNext();
        }
    }

    @Override
    public void setData(byte[] data, Headers headers) {
        this.separate(new String(data));
        iterator = records.iterator();
    }

    /**
     * 
     * @param message
     */
    private void separate(String message) {
        message = message.replaceAll("=", "");

        try {
            // Extracts the header
            Matcher matcher = HEADERREGEX.matcher(message);

            if (matcher.find()) {
                header = matcher.group();
            }

            // Extracts the AWIPS header
            matcher = AWIPSHEADER.matcher(message);

            if (matcher.find()) {
                awipsheader = matcher.group(1) + matcher.group(2);
            }

            matcher = CANADA_FLAG.matcher(message);
            if (matcher.find()) {
                canadaflag = matcher.group();
            }

            matcher = DATASET.matcher(message);

            // Extracts all the matches out of the message. Looks for ICAO/date
            // pairs. Does not allow duplicate entries.
            while (matcher.find()) {
                if (!bodyRecords.contains(matcher.group())) {
                    bodyRecords.add(matcher.group());
                }
            }

            // Assigns records
            for (int i = 0; i < bodyRecords.size(); i++) {
                String observation = null;
                if (i < bodyRecords.size() - 1) {
                    observation = message
                            .substring(message.indexOf(bodyRecords.get(i)),
                                    message.indexOf(bodyRecords.get(i + 1)))
                            .trim().replaceAll("\\r\\r\\n", "\n");
                    ;
                } else {
                    observation = bodyRecords.get(i).trim()
                            .replaceAll("\\r\\r\\n", "\n");
                }
                splitAreas = splitRecord(observation);
                for (int j = 0; j < splitAreas.size(); j++) {
                    String record = header + "\n" + awipsheader + "\n"
                            + splitAreas.get(j) + "\n" + canadaflag;
                    records.add(record);
                }
            }
        } catch (Exception e) {
            e.printStackTrace();
            theLogger.warn("No valid CCFP records found.");
        }
        return;
    }

    private List<String> splitRecord(String obs) {
        List<String> rval = new ArrayList<String>();
        String timeline = null;

        Matcher matcher = TIMEREGEX.matcher(obs);
        if (matcher.find()) {
            timeline = matcher.group();
        }

        matcher = LINEREGEX.matcher(obs);
        while (matcher.find()) {
            rval.add(timeline + "\n" + matcher.group());
        }

        return rval;
    }

}
