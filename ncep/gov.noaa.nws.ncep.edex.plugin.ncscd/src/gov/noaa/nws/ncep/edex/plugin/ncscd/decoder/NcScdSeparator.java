/**
 * NcScdSeparator
 * 
 * This class sets the raw data to an Arraylist, records, of
 * String based on a uniquely identified separator. 
 *
 *<pre>
 * SOFTWARE HISTORY
 *
 * Date    		Ticket#		Engineer	Description
 * -------		-------  	--------	-----------
 * 12/2008		41			T. Lee		Initial Creation
 * 04/2009		41			T. Lee		Migrated to TO10
 * 04/2009		41			T. Lee		Fixed ScdSeparator for multiple reports
 * 07/2009		41			T. Lee		Migrated to TO11
 * 11/2009		41			T. Lee		Migrated to TO11D6
 * 06/2011		41			F. J. Yen	Renamed ScdSeparator and converted to HDF5
 * 09/2011      457         S. Gurung   Renamed H5 to Nc and h5 to nc
 * </pre>
 *
 * @author T. Lee
 * @version 1
 *         
 */

package gov.noaa.nws.ncep.edex.plugin.ncscd.decoder;

import gov.noaa.nws.ncep.common.tools.IDecoderConstantsN;

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

public class NcScdSeparator extends AbstractRecordSeparator {
    private final Logger log = Logger.getLogger(getClass().getName());

    /** List of bulletins contained in file */
    private List<String> bulletins;

    /** List of records contained in file */
    private List<String> records;

    /** Iterator for records */
    private Iterator<String> iterator = null;

    /**
     * Constructor.
     */
    public NcScdSeparator() {
        records = new ArrayList<String>();
        bulletins = new ArrayList<String>();
    }

    public static NcScdSeparator separate(byte[] data, Headers headers) {
        NcScdSeparator ds = new NcScdSeparator();
        ds.setData(data, headers);
        return ds;
    }

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
     * @param message
     *            separate bulletins
     */
    private void doSeparate(String message) {
        /* Regex used for separate the bulletins */
        final String BULLSEPARATOR = "\\d{3} \\r\\r\\n"
                + IDecoderConstants.WMO_HEADER + "(SCD[A-Z]{2}).\\r\\r\\n";
        Matcher matcher;
        Pattern bullPattern;
        bullPattern = Pattern.compile(BULLSEPARATOR);

        try {
            matcher = bullPattern.matcher(message);

            /*
             * Set number of bulletins to records only if the bulletin separator
             * is not the same. At the point, only separators are stored in
             * "records"
             */
            while (matcher.find()) {
                if (!bulletins.contains(matcher.group())) {
                    bulletins.add(matcher.group());
                }
            }

            /*
             * Append the raw data to the bulletin arrays.
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
            }
        } catch (Exception e) {
            if (log.isInfoEnabled()) {
                log.info("No valid bulletins found!");
            }
        }

        /*
         * Extract all the SCD reports out of the bulletin array list. Add the
         * "header" (i.e., WMO header) to each report so that it can be decoded
         * and write to a single database table.
         */
        String header = null;
        Pattern reportPattern = Pattern.compile(IDecoderConstantsN.SCD_REPORT);
        for (String bull : bulletins) {
            matcher = bullPattern.matcher(bull);
            if (matcher.find()) {
                header = matcher.group();
                matcher = reportPattern.matcher(bull);
                String report;
                while (matcher.find()) {
                    report = header + matcher.group();
                    if (!records.contains(matcher.group())) {
                        records.add(report);
                    }
                }
            }
        }
    }
}