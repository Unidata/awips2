/**
 * NonConvsigmetSeparator
 *
 * This class sets the raw data to an Arraylist, records, of
 * String based on a uniquely identified separator.
 *
 * <pre>
 * Uma Josyula                               06/2009         Creation
 * </pre>
 *
 * This code has been developed by the SIB for use in the AWIPS system.
 */

package gov.noaa.nws.ncep.edex.plugin.nonconvsigmet.decoder;

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

public class NonConvSigmetSeparator extends AbstractRecordSeparator {
    private final Log logger = LogFactory.getLog(getClass());

    /** Regex used for separate the bulletins */
    private static final String BULLSEPARATOR = "([0-9]{3})( )*\\x0d\\x0d\\x0a([A-Z]{4}[0-9]{2}) ([A-Z]{4}) ([0-9]{6})( [A-Z]{3})?\\x0d\\x0d\\x0a";

    /** Regex matcher */
    private Matcher matcher;

    /** Pattern object for regex search */
    private Pattern pattern;

    /** List of records contained in file */
    private List<String> records;

    private Iterator<String> iterator = null;

    /**
     * Constructor.
     * 
     */
    public NonConvSigmetSeparator() {
        records = new ArrayList<String>();
    }

    public static NonConvSigmetSeparator separate(byte[] data, Headers headers) {
        NonConvSigmetSeparator ds = new NonConvSigmetSeparator();
        ds.setData(data, headers);
        return ds;
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.edex.plugin.AbstractRecordSeparator#setData(byte[],
     * com.raytheon.edex.esb.Headers)
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

    /**
     * Get record
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
     * @param message
     *            separate bulletins
     */
    private void doSeparate(String message) {
        /* Regex used for separate the bulletins */
        
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
            logger.warn("No valid records found!", e);
        }
        return;
    }
}