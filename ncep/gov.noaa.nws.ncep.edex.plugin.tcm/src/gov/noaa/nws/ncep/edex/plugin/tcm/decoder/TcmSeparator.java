/*
 * TcmSeparator
 * 
 * This class sets the raw data to an array of String records based
 * on a uniquely identified separator. 
 * 
 * <pre>
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    	Engineer    Description
 * -------		-------		-------- 	-----------
 * 06/2009		128			T. Lee		Creation
 * </pre>
 * 
 * @author T.Lee
 * @version 1.0       
 */

package gov.noaa.nws.ncep.edex.plugin.tcm.decoder;

import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;
import java.util.NoSuchElementException;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import org.apache.log4j.Logger;

import com.raytheon.edex.esb.Headers;
import com.raytheon.edex.plugin.AbstractRecordSeparator;
import com.raytheon.edex.util.Util;
import com.raytheon.uf.edex.decodertools.core.IDecoderConstants;

public class TcmSeparator extends AbstractRecordSeparator {
    private final Logger log = Logger.getLogger(getClass().getName());

    /** Regex used for separate the bulletins */
    private static final String BULLSEPARATOR = "\\d{3} \\r\\r\\n"
            + IDecoderConstants.WMO_HEADER;

    /** Regex matcher */
    private Matcher matcher;

    /** Pattern object for regex search */
    private Pattern pattern;

    /** List of records contained in a bulletin file */
    private List<String> records;

    /** Record iterator */
    private Iterator<String> iterator = null;

    /**
     * No arg constructor
     */
    public TcmSeparator() {
        records = new ArrayList<String>();
    }

    /**
     * Separator driver
     * 
     * @param data
     *            The raw data
     */
    public static TcmSeparator separate(byte[] data, Headers headers) {
        TcmSeparator ds = new TcmSeparator();
        ds.setData(data, headers);
        return ds;
    }

    /**
     * Set the raw data and invoke the internal message separation process
     * 
     * @param data
     *            The raw text message
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
     * Get next record
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
     * Execute bulletin separation.
     * 
     * @param message
     *            The raw text data
     */
    private void doSeparate(String message) {
        try {
            pattern = Pattern.compile(BULLSEPARATOR);
            matcher = pattern.matcher(message);

            /*
             * Set number of bulletins to records only if the bulletin separator
             * is not the same. At the point, only separators are stored in
             * "records"
             */
            while (matcher.find()) {
                if (!records.contains(matcher.group())) {
                    records.add(matcher.group());
                }
            }

            /*
             * Append the raw data file to the records.
             */
            for (int i = 0; i < records.size(); i++) {
                if (i < records.size() - 1) {
                    records.set(
                            i,
                            "\n"
                                    + message.substring(
                                            message.indexOf(records.get(i)),
                                            message.indexOf(records.get(i + 1))));
                } else {
                    records.set(
                            i,
                            "\n"
                                    + message.substring(message.indexOf(records
                                            .get(i))));
                }
            }
        } catch (Exception e) {
            if (log.isInfoEnabled()) {
                log.info("No valid records found!");
            }
        }
        return;
    }
}