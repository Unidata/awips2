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

package com.raytheon.edex.plugin.obs.metar;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.NoSuchElementException;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.raytheon.edex.esb.Headers;
import com.raytheon.edex.exception.DecoderException;
import com.raytheon.edex.plugin.AbstractRecordSeparator;
import com.raytheon.uf.common.util.StringUtil;
import com.raytheon.uf.common.wmo.WMOHeader;

/**
 * Separator implementation for METAR/SPECI files.
 *
 * <pre>
 *
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#     Engineer    Description
 * ------------ ----------  ----------- --------------------------
 * Jul ??, 2006 3 &amp; 14  Phillippe   Initial Creation
 * Nov 14, 2006 71          Rockwood    Implemented filter for NIL observations
 * Apr 14, 2008 1093        jkorman     Added filter for Alaskan &quot;Airways&quot;
 *                                      observations.
 * May 14, 2014 2536        bclement    moved WMO Header to common, removed unused HEADERREGEX
 * Oct 02, 2014 3693        mapeters    Changed pattern String constants to Pattern constants.
 * Dec 15, 2015 5166        kbisanz     Update logging to use SLF4J
 * Jan 29, 2016 5265        nabowle     Update type replaceAll. General cleanup.
 * </pre>
 *
 * @author bphillip
 * @version 1
 */
public class MetarSeparator extends AbstractRecordSeparator {

    private final Logger theLogger = LoggerFactory.getLogger(getClass());

    /** Regex used for separating multi-record files */
    private static final Pattern ICAODATEPAIR = Pattern
            .compile("\\p{Alnum}{4} (\\d{6}Z |(RMK )?NIL)");

    /** Regex used for determining metar type */
    private static final Pattern METARTYPE = Pattern.compile("METAR|SPECI");

    /** Regex to check for Alaskan Airways observations. */
    private static final Pattern AIRWAYS = Pattern
            .compile("[A-Z][A-Z,0-9]{3} (SP|SA) \\d{4} AWOS");

    /** Regex used to search for NIL messages */
    private static final Pattern NILREGEX = Pattern.compile("NIL");

    /** The WMO header */
    private WMOHeader header;

    /** The type of METAR (METAR or SPECI) */
    private String type;

    /** List of record bodies contained in file */
    private List<String> records;

    /** Iterator over the records. */
    private Iterator<String> iterator = null;

    public MetarSeparator() {
        records = new ArrayList<String>();
    }

    public static MetarSeparator separate(byte[] data, Headers headers)
            throws DecoderException {
        MetarSeparator ms = new MetarSeparator();
        ms.setData(data, headers);
        return ms;
    }

    /**
     * Get the WMO Header found within this data.
     *
     * @return The message WMO Header.
     */
    public WMOHeader getWMOHeader() {
        return header;
    }

    @Override
    public void setData(byte[] data, Headers headers) {
        this.doSeparate(new String(data));
        iterator = records.iterator();
    }

    public boolean hasNext() {
        if (iterator == null) {
            return false;
        } else {
            return iterator.hasNext();
        }
    }

    public byte[] next() {
        try {
            String temp = iterator.next();
            if (StringUtil.isEmptyString(temp)) {
                return new byte[0];
            } else {
                return temp.getBytes();
            }
        } catch (NoSuchElementException e) {
            return new byte[0];
        }
    }

    /**
     * Separates the composite message into the individual records.
     *
     * @param message
     *            The message.
     */
    private void doSeparate(String message) {

        message = message.replaceAll("=", "");

        try {
            // Extracts the header
            WMOHeader wmoHeader = new WMOHeader(message.getBytes());
            if (wmoHeader.isValid()) {
                header = wmoHeader;
            }

            // Determines the type
            Matcher matcher = METARTYPE.matcher(message);
            if (matcher.find()) {
                type = matcher.group();
                message = matcher.replaceAll("");
            } else {
                type = "METAR";
            }

            matcher = ICAODATEPAIR.matcher(message);

            List<Integer> bodyIndex = new ArrayList<Integer>();
            Map<String, String> bodyMap = new HashMap<String, String>();
            /*
             * Extracts all the matches out of the message. Looks for ICAO/date
             * pairs. Does not allow duplicate entries.
             */
            if (matcher.find()) {
                bodyIndex.add(matcher.start());
                String obsKey = matcher.group();
                obsKey = matcher.group();
                if (!bodyMap.containsKey(obsKey)) {
                    bodyMap.put(obsKey, obsKey);
                }
                while (matcher.find()) {
                    bodyIndex.add(matcher.start());
                    bodyIndex.add(matcher.start());
                    obsKey = matcher.group();
                    if (!bodyMap.containsKey(obsKey)) {
                        bodyMap.put(obsKey, obsKey);
                    }
                }
                bodyIndex.add(message.length());
            }
            List<String> bodyRecords = new ArrayList<String>();
            // Now get each observation, checking that it should be kept.
            for (int i = 0; i < bodyIndex.size() - 1; i += 2) {
                String observation = message.substring(bodyIndex.get(i),
                        bodyIndex.get(i + 1));
                matcher = ICAODATEPAIR.matcher(observation);
                if (matcher.find()) {
                    // Get the key i.e. {ICAO|Date}
                    String obsKey = matcher.group();
                    // If it exists in the map then keep this observation.
                    if (bodyMap.containsKey(obsKey)) {
                        bodyRecords.add(observation);
                        /*
                         * now that we have data for this key, remove it from
                         * the map so we skip any subsequent observations with
                         * the same key.
                         */
                        bodyMap.remove(obsKey);
                    }
                }
            }
            bodyIndex.clear();
            // Perform a some more checks on the data.
            for (int i = 0; i < bodyRecords.size(); i++) {
                String observation = bodyRecords.get(i);
                /*
                 * 20080418 - 1093 Check for old style AIRWAYS data from Alaskan
                 * stations. This data will be at the end of valid METAR/SPECI
                 * data so just remove it.
                 */
                matcher = AIRWAYS.matcher(observation);
                if (matcher.find()) {
                    observation = observation.substring(0, matcher.start());
                }

                // Check for NIL observations and, if found, throw out
                matcher = NILREGEX.matcher(observation);
                if (!matcher.find()) {
                    String record = header.getWmoHeader() + "\n" + type + " "
                            + observation;
                    records.add(record);
                }
            }
        } catch (Exception e) {
            theLogger.warn("Invalid METAR message received.", e);
        }
        return;
    }
}
