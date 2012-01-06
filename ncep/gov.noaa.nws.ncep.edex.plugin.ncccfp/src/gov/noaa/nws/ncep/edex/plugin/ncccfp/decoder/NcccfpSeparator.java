package gov.noaa.nws.ncep.edex.plugin.ncccfp.decoder;

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
 * NCCCFP Separator
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#     Engineer    Description
 * ------------ ----------  ----------- --------------------------
 * 09/03/2009   155         F. J. Yen   From Raytheon's CCFP; modify for NCCCFP
 * 
 * </pre>
 * 
 * @author F. J. Yen
 * @version 1
 */

public class NcccfpSeparator extends AbstractRecordSeparator {

    private final Log theLogger = LogFactory.getLog(getClass());

    /** Regex used for separating multi-record files */
    private static final String DATASET = "CCFP \\d{8}_\\d{4} \\d{8}_\\d{4}([\\r\\n]+(AREA|LINE).*)*";

    /** Regex to pull AWIPS Header */
    private static final String AWIPSHEADER = "[\\r\\n]+(TAF|MTR|CFP)([\\p{Alnum} ]{3})[\\r\\n]+";

    /** Regex used for extracting the header */
    private static final String HEADERREGEX = "[A-Z]{4}[0-9]{1,2} [A-Z]{4} [0-9]{6}(?: [A-Z]{3})?";

    /** Regex used for extracting a line */
    private static final String TIMEREGEX = "CCFP \\d{8}_\\d{4} \\d{8}_\\d{4}";

    /** Regex used for extracting a line */
    private static final String LINEREGEX = "AREA.*|LINE.*";

    private static final String CANADA_FLAG = "CANADA ON|CANADA OFF";

    /** Pattern object for regex search */
    Pattern pattern;

    /** Regex matcher */
    private Matcher matcher;

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

    public static NcccfpSeparator separate(byte[] data, Headers headers) {
        NcccfpSeparator sep = new NcccfpSeparator();
        sep.setData(data, headers);
        return sep;
    }

    public NcccfpSeparator() {
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
            pattern = Pattern.compile(HEADERREGEX);
            matcher = pattern.matcher(message);

            if (matcher.find()) {
                header = matcher.group();
            }

            // Extracts the AWIPS header
            pattern = Pattern.compile(AWIPSHEADER);
            matcher = pattern.matcher(message);

            if (matcher.find()) {
                awipsheader = matcher.group(1) + matcher.group(2);
            }

            pattern = Pattern.compile(CANADA_FLAG);
            matcher = pattern.matcher(message);
            if (matcher.find()) {
                canadaflag = matcher.group();
            }

            pattern = Pattern.compile(DATASET);
            matcher = pattern.matcher(message);

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
            theLogger.warn("No valid NCCCFP records found.");
        }
        return;
    }

    private List<String> splitRecord(String obs) {
        List<String> rval = new ArrayList<String>();
        String timeline = null;

        pattern = Pattern.compile(TIMEREGEX);
        matcher = pattern.matcher(obs);
        if (matcher.find()) {
            timeline = matcher.group();
        }

        pattern = Pattern.compile(LINEREGEX);
        matcher = pattern.matcher(obs);
        while (matcher.find()) {
            rval.add(timeline + "\n" + matcher.group());
        }

        return rval;
    }

}
