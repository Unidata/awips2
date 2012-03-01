/**
 * 
 * NcPafmSeparator
 * 
 * Separator implementation for Point/Area Forecast Matrices PAFM Decoder Plug-In
 *
 * <pre>
 * SOFTWARE HISTORY
 * Date       	Ticket#		Engineer	Description
 * ------------	----------	-----------	--------------------------
 * 05Aug2009	126			F. J. Yen	Initial creation
 * 11Feb2010	126			F. J. Yen	Handle missing ending segment delimeter "$$"
 * 30Sep2011	126			B. Hebbard	PafmSeparator becomes new NcPafmSeparator
 * 
 * </pre>
 *   
 * This code has been developed by the SIB for use in the AWIPS2 system.
 * 
 */
package gov.noaa.nws.ncep.edex.plugin.ncpafm.decoder;

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

public class NcPafmSeparator extends AbstractRecordSeparator {

    private final Log theLogger = LogFactory.getLog(getClass());

    /** Regex used for separating multi-bulletin files */
    private static final String BULLETINSEPARATOR = "\\d{3} \\x0d\\x0d\\x0aFOUS5[1-5] ([A-Z]{4}) "
            + "([0-9]{6}).*\\x0d\\x0d\\x0a";

    /** Regex matcher */
    private Matcher matcher;

    /** Pattern object for regex search */
    private Pattern pattern;

    private String header;

    /** List of records contained in file */
    private List<String> records;

    private Iterator<String> iterator = null;

    public static NcPafmSeparator separate(byte[] data, Headers headers) {
        NcPafmSeparator pafmSeparator = new NcPafmSeparator();
        pafmSeparator.setData(data, headers);
        return pafmSeparator;
    }

    /**
     * Constructor.
     * 
     */
    public NcPafmSeparator() {
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

    /**
     * Get next record.
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
                    /*
                     * trim() is not done to the bulletin message as in other
                     * decoder plugins because the end of segment delimeter of
                     * "$$" could be missing in some PAFM bulletins. If it were
                     * trimmed, without "$$", the length of the last record
                     * could be truncated which would cause a problem in
                     * decoding.
                     */
                    records.set(
                            i,
                            header
                                    + "\n"
                                    + message.substring(message.indexOf(records
                                            .get(i))));
                }
            }
        } catch (Exception e) {
            e.printStackTrace();
            theLogger.warn("====in separate: No valid PAFM records found.");
        }
        return;
    }
}