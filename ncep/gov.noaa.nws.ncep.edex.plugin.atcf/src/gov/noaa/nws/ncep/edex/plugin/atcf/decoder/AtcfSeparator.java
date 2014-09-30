/**
 * 
 * AtcfSeparator
 * 
 * Separator implementation for Automated Tropical Cyclone Forecast ATCF Plug-In
 * 
 * 06 January 2010
 * 
 * This code has been developed by the SIB for use in the AWIPS2 system.
 * 
 */
package gov.noaa.nws.ncep.edex.plugin.atcf.decoder;

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
import com.raytheon.uf.common.util.StringUtil;

/**
 * 
 * AtcfSeparator
 * 
 * Separator implementation for Automated Tropical Cyclone Forecast ATCF Plug-In
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date       	Ticket#		Engineer	Description
 * 06/23/10		283		    F. J. Yen	Initial creation
 * 6/2014                   T. Lee      Batch processing to improve performance
 * 
 * </pre>
 * 
 * @author Fee Jing Yen, SIB
 * @version 1
 */

public class AtcfSeparator extends AbstractRecordSeparator {

    private final Log theLogger = LogFactory.getLog(getClass());

    /** Regex used for separating multi-record file */
    private static final String BULLETINSEPARATOR = "(WP|IO|SH|CP|EP|AL), +\\d{1,2}, +\\d{10}, +\\d{1,2}, +\\w{1,4}, +(-|\\d)\\d{0,2}, +\\d{1,3}(N|S), +\\d{1,4}(E|W), +.*?\\x0a";

    /** Regex matcher */
    private Matcher matcher;

    /** Pattern object for regex search */
    private Pattern pattern;

    /** List of records contained in file */
    private List<String> records;

    private Iterator<String> iterator = null;

    /** Number of records in batch processing */
    private static int MAX_RECORD = 100;

    public static AtcfSeparator separate(byte[] data, Headers headers) {
        AtcfSeparator atcfSeparator = new AtcfSeparator();
        atcfSeparator.setData(data, headers);
        return atcfSeparator;
    }

    public static AtcfSeparator batchSeparate(byte[] data, Headers headers) {
        AtcfSeparator atcfSeparator = new AtcfSeparator();
        atcfSeparator.setBatchData(data, headers);
        return atcfSeparator;
    }

    /**
     * AtcfSeparator() Constructor.
     * 
     */
    public AtcfSeparator() {
        records = new ArrayList<String>();
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.edex.plugin.IRecordSeparator#setData(byte[])
     */
    @Override
    public void setData(byte[] data, Headers headers) {
        doSeparate(new String(data));
        iterator = records.iterator();
    }

    public void setBatchData(byte[] data, Headers headers) {
        doBatchSeparate(data);
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
            if (StringUtil.isEmptyString(temp)) {
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
        } catch (Exception e) {
            e.printStackTrace();
            theLogger.warn("====in separate: No valid ATCF records found.");
        }
        return;
    }

    private void doBatchSeparate(byte[] message) {
        try {
            pattern = Pattern.compile(BULLETINSEPARATOR);
            matcher = pattern.matcher(new String(message));
            Integer counter;
            String dataStream;
            counter = 0;
            dataStream = "";
            Integer nfile = 0;
            while (matcher.find()) {
                if (counter <= MAX_RECORD) {
                    dataStream += matcher.group();
                    counter++;
                } else {
                    dataStream += matcher.group();
                    records.add(dataStream);
                    counter = 0;
                    dataStream = "";
                    nfile++;
                }
            }
            records.add(dataStream);
        } catch (Exception e) {
            e.printStackTrace();
            theLogger.warn("====in separate: No valid ATCF records found.");
        }
        return;
    }
}