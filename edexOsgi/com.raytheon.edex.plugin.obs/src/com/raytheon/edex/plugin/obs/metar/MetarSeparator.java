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

import java.io.BufferedReader;
import java.io.FileReader;
import java.io.IOException;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.NoSuchElementException;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;

import com.raytheon.edex.esb.Headers;
import com.raytheon.edex.exception.DecoderException;
import com.raytheon.edex.plugin.AbstractRecordSeparator;
import com.raytheon.uf.common.util.StringUtil;
import com.raytheon.uf.common.wmo.WMOHeader;

/**
 * 
 * 
 * Separator implementation for metar record
 * 
 * <pre>
 * 
 * 
 * SOFTWARE HISTORY
 * 
 * Date       	Ticket#		Engineer	Description
 * ------------	----------	-----------	--------------------------
 * July2006		3 &amp; 14		Phillippe	Initial Creation
 * 14Nov2006	71			Rockwood	Implemented filter for NIL observations	
 * 20080418           1093  jkorman     Added filter for Alaskan &quot;Airways&quot;
 *                                      observations.
 * May 14, 2014 2536       bclement    moved WMO Header to common, removed unused HEADERREGEX
 * </pre>
 * 
 * @author bphillip
 * @version 1
 */

/**
 * Implementation of file separator for METAR/SPECI files
 * 
 * @author bphillip
 * 
 */
public class MetarSeparator extends AbstractRecordSeparator {

    private final Log theLogger = LogFactory.getLog(getClass());

    /** Regex used for separating multi-record files */
    private static final String ICAODATEPAIR = "\\p{Alnum}{4} (\\d{6}Z |(RMK )?NIL)";

    /** Regex used for determining metar type */
    private static final String METARTYPE = "METAR|SPECI";

    private static final String AIRWAYS = "[A-Z][A-Z,0-9]{3} (SP|SA) \\d{4} AWOS";

    /** Regex used to search for NIL messages */
    private static final String NILREGEX = "NIL";

    /** The WMO header */
    private WMOHeader header;

    /** The type of METAR (METAR or SPECI) */
    private String type;

    /** List of record bodies contained in file */
    private List<String> records;

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
     * @return The message WMO Header.
     */
    public WMOHeader getWMOHeader() {
        return header;
    }
    
    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.edex.plugin.AbstractRecordSeparator#setData(byte[])
     */
    @Override
    public void setData(byte[] data, Headers headers) {
        this.doSeparate(new String(data));
        iterator = records.iterator();
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.edex.plugin.AbstractRecordSeparator#nextRecord()
     */
    public boolean hasNext() {
        if (iterator == null) {
            return false;
        } else {
            return iterator.hasNext();
        }
    }

    /*
     * (non-Javadoc)
     * 
     * @see java.util.Iterator#next()
     */
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
     * 
     * @param message
     */
    private void doSeparate(String message) {

        message = message.replaceAll("=", "");

        try {
            // Extracts the header
            WMOHeader wmoHeader = new WMOHeader(message.getBytes());
            if(wmoHeader.isValid()) {
                header = wmoHeader;
            }
            
//            Pattern pattern = Pattern.compile(HEADERREGEX);
//            Matcher matcher = pattern.matcher(message);
//
//            if (matcher.find()) {
//                header = matcher.group();
//            }
            // Determines the type
            Pattern pattern = Pattern.compile(METARTYPE);
            Matcher matcher = pattern.matcher(message);

            if (matcher.find()) {
                type = matcher.group();
            } else {
                type = "METAR";
            }
            message = message.replaceAll(type, "");

            pattern = Pattern.compile(ICAODATEPAIR);
            matcher = pattern.matcher(message);

            List<Integer> bodyIndex = new ArrayList<Integer>();
            Map<String, String> bodyMap = new HashMap<String, String>();
            // Extracts all the matches out of the message. Looks for ICAO/date
            // pairs. Does not allow duplicate entries.
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
                matcher = pattern.matcher(observation);
                if (matcher.find()) {
                    // Get the key i.e. {ICAO|Date}
                    String obsKey = matcher.group();
                    // If it exists in the map then keep this observation.
                    if (bodyMap.containsKey(obsKey)) {
                        bodyRecords.add(observation);
                        // now that we have data for this key,
                        // remove it from the map so we skip any
                        // subsequent observations with the same key.
                        bodyMap.remove(obsKey);
                    }
                }
            }
            bodyIndex.clear();
            // Perform a some more checks on the data.
            for (int i = 0; i < bodyRecords.size(); i++) {
                String observation = bodyRecords.get(i);
                // 20080418 - 1093
                // Check for old style AIRWAYS data from Alaskan stations. This
                // data will be at the end of valid METAR/SPECI data so just
                // remove it.
                pattern = Pattern.compile(AIRWAYS);
                matcher = pattern.matcher(observation);
                if (matcher.find()) {
                    observation = observation.substring(0, matcher.start());
                }

                // Check for NIL observations and, if found, throw out
                pattern = Pattern.compile(NILREGEX);
                matcher = pattern.matcher(observation);
                if (!matcher.find()) {
                    String record = header.getWmoHeader() + "\n" + type + " " + observation;
                    records.add(record);
                }
            }
        } catch (Exception e) {
            e.printStackTrace();
            theLogger.warn("No valid METAR records found.");
        }
        return;
    }

    /**
     * Test function
     * 
     * @param args
     */
    public static void main(String[] args) {

        String CRCRLF = "\r\r\n";

        String fileName = "/common/jkorman/data_store/obs/"
                + "SAUS70_KWBC_241218.obs";
        String str;
        MetarSeparator ben = new MetarSeparator();
        System.out.println(fileName);

        StringBuilder sb = new StringBuilder();
        try {
            BufferedReader in = new BufferedReader(new FileReader(fileName));
            while ((str = in.readLine()) != null) {
                sb.append(CRCRLF);
                sb.append(str);
            }
            in.close();
        } catch (IOException e) {
            e.printStackTrace();
        }
        ben.setData(sb.toString().getBytes(), null);
        int i = 0;
        while (ben.hasNext()) {

            byte[] tData = ben.next();

            System.out.println("Record # " + (++i) + " [" + tData.length
                    + "]: \n{" + new String(tData) + "}");
        }
    }
}
