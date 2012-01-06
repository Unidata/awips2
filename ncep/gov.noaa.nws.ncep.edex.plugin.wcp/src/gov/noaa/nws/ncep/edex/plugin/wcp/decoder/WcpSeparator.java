/**
 * 
 * WcpSeparator
 * 
 * Separator implementation for Watch Corner Point WCP Plug-In
 * 
 * 12 December 2008
 * 
 * This code has been developed by the SIB for use in the AWIPS2 system.
 * 
 */
package gov.noaa.nws.ncep.edex.plugin.wcp.decoder;

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
 * WcpSeparator
 * 
 * Separator implementation for Watch Corner Point WCP Plug-In
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date       	Ticket#		Engineer	Description
 * ------------	----------	-----------	--------------------------
 * 02Dec2008		37			F. J. Yen	Initial creation
 * 17Apr2009		37			F. J. Yen	Refactored for TO10
 * 
 * </pre>
 * 
 * @author Fee Jing Yen, SIB
 * @version 1
 */

public class WcpSeparator extends AbstractRecordSeparator {

    private final Log theLogger = LogFactory.getLog(getClass());

    /** Regex used for separating multi-bulletin files */
    private static final String BULLETINSEPARATOR = "\\d{3} \\x0d\\x0d\\x0aWWUS60 KWNS "
            + "([0-9]{6}).*\\x0d\\x0d\\x0aSEVSPC";

    /** Regex matcher */
    private Matcher matcher;

    /** Pattern object for regex search */
    private Pattern pattern;

    private String header;

    /** List of records contained in file */
    private List<String> records;

    private Iterator<String> iterator = null;

    public static WcpSeparator separate(byte[] data, Headers headers) {
        WcpSeparator wcpSeparator = new WcpSeparator();
        wcpSeparator.setData(data, headers);
        return wcpSeparator;
    }

    /**
     * Constructor.
     * 
     */
    public WcpSeparator() {
        /*
         * 8888888888888888 The following pattern stmt may not be needed--check
         * this later 8888
         */
        // pattern = Pattern.compile(BULLETINSEPARATOR);
        records = new ArrayList<String>();
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.edex.plugin.IRecordSeparator#setData(byte[])
     */
    public void setData(byte[] data, Headers headers) {
        doSeparate(new String(data));
        iterator = records.iterator();
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.edex.plugin.IRecordSeparator#hasNext()
     */
    public boolean hasNext() {
        if (iterator == null) {
            return false;
        } else {
            return iterator.hasNext();
        }
    }

    /*
     * Get record.
     */
    public byte[] next() {
        try {
            String temp = iterator.next();
            if (Util.isEmptyString(temp)) {
                return (byte[]) null;
            } else {
                return temp.getBytes();
            }
        } catch (NoSuchElementException e) {
            return (byte[]) null;
        }
    }

    /**
     * 
     * @param message
     *            separate bulletins
     */
    private void doSeparate(String message) {
        try {
            pattern = Pattern.compile(BULLETINSEPARATOR);
            matcher = pattern.matcher(message);

            while (matcher.find()) {
                if (!records.contains(matcher.group())) {
                    records.add(matcher.group());
                }
            }
            /*
             * Append the raw data files to the records
             */
            for (int i = 0; i < records.size(); i++) {
                if (i < records.size() - 1) {
                    records.set(
                            i,
                            header
                                    + "\n"
                                    + message.substring(
                                            message.indexOf(records.get(i)),
                                            message.indexOf(records.get(i + 1))));
                } else {
                    records.set(
                            i,
                            header
                                    + "\n"
                                    + message.substring(
                                            message.indexOf(records.get(i)))
                                            .trim());
                }
            }
        } catch (Exception e) {
            e.printStackTrace();
            theLogger.warn("====in separate: No valid WCP records found.");
        }
        return;
    }
}