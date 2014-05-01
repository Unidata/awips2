/**
 * 
 * IdftSeparator
 * 
 * Separator implementation for Ice Drift IDFT Decoder Plug-In.
 * This allows for the creation of just one table for an IDFT text
 * bulletin which has several records.  This separator also handles
 * more than one bulletin in a file.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date       	Ticket#		Engineer	Description
 * ------------	----------	-----------	--------------------------
 * 02/12/2009				F. J. Yen	Initial creation for to9.
 * 05/27/2009		100		F. J. Yen	Migrate for to10.
 * 
 * </pre>
 * 
 * @author Fee Jing Yen, SIB
 * @version 1
 * 
 */

package gov.noaa.nws.ncep.edex.plugin.idft.decoder;

import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;
import java.util.NoSuchElementException;
import java.util.Scanner;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import org.apache.log4j.Logger;

import com.raytheon.edex.esb.Headers;
import com.raytheon.edex.plugin.AbstractRecordSeparator;
import com.raytheon.uf.common.util.StringUtil;
import com.raytheon.uf.edex.decodertools.core.IDecoderConstants;

public class IdftSeparator extends AbstractRecordSeparator {

    private final Logger theLogger = Logger.getLogger(getClass().getName());

    /** Regex used for separating multi-record bulletins */
    final String DATAPNT = "((\\d{1,3}) *(\\d{1,3}) *(\\d{1,3})\\.(\\d) \\r\\r\\n)|"
            + "(( *\\d{1,4}) *(\\d{0,2})\\.(\\d)(N|S) *(\\d{0,3})\\.(\\d)(W|E) *(\\d{1,3}) *(\\d{1,4})\\.(\\d)\\r\\r\\n)";

    /**
     * Regex used for extracting the header and for separating multi-bulletin
     * files as in archived data
     */
    private static final String HEADERREGEX = "FZXX41 KWNO "
            + "([0-9]{6})\\x0d\\x0d\\x0a.*\\x0d\\x0d\\x0a.*\\x0d\\x0d\\x0a.*\\r\\r\\n.*HR FORECAST"
            + " VT (\\d{2})/(\\d{2})/(\\d{2}) (\\d{2})(\\d{2}) UTC";

    /** Pattern object for regex search */
    Pattern pattern;

    /** Regex matcher */
    private Matcher matcher;

    /** The WMO header */
    private String header;

    /** List of record bodies contained in file */
    private List<String> records;

    private Iterator<String> iterator = null;

    /** List of bulletins contained in file */
    private List<String> bulletins;

    private Iterator<String> bulIterator = null;

    public IdftSeparator() {
        records = new ArrayList<String>();
        bulletins = new ArrayList<String>();
    }

    public static IdftSeparator separate(byte[] data, Headers headers) {
        IdftSeparator ds = new IdftSeparator();
        ds.setData(data, headers);
        return ds;
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
     * @see com.raytheon.edex.plugin.IRecordSeparator#nextRecord()
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

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.edex.plugin.IRecordSeparator#nextRecord()
     */
    public boolean bulHasNext() {
        if (bulIterator == null) {
            return false;
        } else {
            return bulIterator.hasNext();
        }
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.edex.plugin.IRecordSeparator#getRecord()
     */
    public byte[] getRecord() {
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

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.edex.plugin.IRecordSeparator#getRecord()
     */
    public byte[] getBulletin() {
        try {
            String temp = bulIterator.next();
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
     */
    private void doSeparate(String message) {
        String bulMessage;
        message = message.replaceAll("=", "");

        try {
            // Extracts the header
            pattern = Pattern.compile(HEADERREGEX);
            matcher = pattern.matcher(message);

            while (matcher.find()) {
                header = matcher.group();
                if (!bulletins.contains(matcher.group())) {
                    bulletins.add(matcher.group());
                }
            }
            /*
             * Append the raw data file to bulletins.
             */

            for (int i = 0; i < bulletins.size(); i++) {
                if (i < bulletins.size() - 1) {
                    bulletins.set(
                            i,
                            "\n"
                                    + message.substring(message
                                            .indexOf(bulletins.get(i)), message
                                            .indexOf(bulletins.get(i + 1))));
                } else {
                    bulletins.set(
                            i,
                            "\n"
                                    + message.substring(message
                                            .indexOf(bulletins.get(i))));
                }
                /*
                 * Exclude any duplicate report after the first ^c
                 */
                Scanner cc = new Scanner(bulletins.get(i))
                        .useDelimiter(IDecoderConstants.ETX);
                if (cc.hasNext()) {
                    bulletins.set(i, cc.next());
                }
            }
        } catch (Exception e) {
            e.printStackTrace();
            theLogger.warn("No valid Idft record found.");
            return;
        }
        bulIterator = bulletins.iterator();
        for (int j = 0; j < bulletins.size(); j++) {

            byte[] bulData = null;
            if (bulHasNext()) {
                bulData = (byte[]) getBulletin();
            } else {
                // System.out.println ("IdftSeparator separate In for loop j= "
                // + j + " Out of bulletin data");
            }

            bulMessage = new String(bulData);

            // Extracts the header from the bulletin message
            pattern = Pattern.compile(HEADERREGEX);
            matcher = pattern.matcher(bulMessage);

            if (matcher.find()) {
                header = matcher.group();
            }

            pattern = Pattern.compile(DATAPNT);
            matcher = pattern.matcher(bulMessage);

            // Extracts all the matches out of the bulMessage. Looks for an ice
            // drift
            // point record. Does not allow duplicate entries. Prepend it with
            // header.
            String record = null;
            while (matcher.find()) {
                record = header + "\n" + " " + matcher.group();
                if (!records.contains(matcher.group())) {
                    records.add(record);
                }
            }
        }
        return;
    }
}
