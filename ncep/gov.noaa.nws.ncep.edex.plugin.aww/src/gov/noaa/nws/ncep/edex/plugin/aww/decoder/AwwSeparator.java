/**
 * AwwSeparator
 *
 * This class sets the raw data to an Arraylist, records, of
 * String based on a uniquely identified separator.
 *
 * <pre>
 * * Date         Ticket#         Engineer    Description
 * ------------ ----------      ----------- --------------------------
 * 03/2009                       L. Lin       Creation
 * 01/26/2011   N/A				 M. Gao 	  Refactor: 
 * 											   change the BULLSEPARATOR regular expression more flexible. 
 * </pre>
 *
 * This code has been developed by the SIB for use in the AWIPS system.
 */

package gov.noaa.nws.ncep.edex.plugin.aww.decoder;

import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;
import java.util.NoSuchElementException;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.apache.log4j.Logger;

import com.raytheon.edex.esb.Headers;
import com.raytheon.edex.plugin.AbstractRecordSeparator;
import com.raytheon.edex.util.Util;

public class AwwSeparator extends AbstractRecordSeparator {
    private final Logger log = Logger.getLogger(getClass().getName());

    private final Log theLogger = LogFactory.getLog(getClass());

    /** Regex used for separate the bulletins */
    // private static final String BULLSEPARATOR =
    // "([0-9]{3})( )*\\x0d\\x0d\\x0a([A-Z]{4}[0-9]{2}) ([A-Z]{4}) ([0-9]{6})( [A-Z]{3})?\\x0d\\x0d\\x0a";
    // private static final String BULLSEPARATOR =
    // "\\x0d\\x0d\\x0a([A-Z]{4}[0-9]{2}) ([A-Z]{4}) ([0-9]{6})( [A-Z]{3})?\\x0d\\x0d\\x0a([A-Z]{6}|[A-Z]{3}[0-9]{1})";
    // private static final String BULLSEPARATOR =
    // "\\x0d\\x0d\\x0a([A-Z]{4}[0-9]{2}) +([A-Z]{4}) +([0-9]{6})( +[A-Z]{3})? *\\x0d\\x0d\\x0a([A-Z]{6}|[A-Z]{3}[0-9]{1}|[A-Z]{4}[0-9]{2}|[A-Z]{5}[[0-9]{1}| ])";
    private static final String BULLSEPARATOR = "\\x0d\\x0d\\x0a([A-Z]{4}[0-9]{2}) +([A-Z]{4}) +([0-9]{6})( +[A-Z]{3})? *\\x0d\\x0d\\x0a *([A-Z]{6}|[A-Z]{3}[0-9]{1}|[A-Z]{4}[0-9]{2}|[A-Z]{5}[0-9]{1}|[A-Z]{5} *)?";

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
    public AwwSeparator() {
//    	System.out.println("============================== At the beginning of AwwSeparator constructor method"); 
       records = new ArrayList<String>();
    }

    public static AwwSeparator separate(byte[] data, Headers headers) {
//    	System.out.println("============================== At the beginning of separate method, input data[]="+new String(data)); 
        AwwSeparator ds = new AwwSeparator();
        ds.setData(data, headers);
        return ds;
    }

    public void setData(byte[] data, Headers headers) {
//    	System.out.println("============================== At the beginning of setData method, input data[]="+new String(data)); 
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
//System.out.println("============================== At the beginning of doSeparate method, input message="+message); 
        try {
            pattern = Pattern.compile(BULLSEPARATOR);
            matcher = pattern.matcher(message);

            /*
             * Set number of bulletins to records only if the bulletin separator
             * is not the same. At the point, only separators are stored in
             * "records"
             */
            while (matcher.find()) {
            	String matcherGroupString = matcher.group(); 
                if (!records.contains(matcherGroupString)) {
                    records.add(matcher.group());
                }
            }

            /*
             * Append the raw data file to the records.
             */
//            System.out.println("########, before do reset on the string list, the original list content is:"); 
//            displayStringList(records); 
            for (int i = 0; i < records.size(); i++) {
                if (i < records.size() - 1) {
//                	String modifiedString = "\n"
//                        + message.substring(
//                                message.indexOf(records.get(i)),
//                                message.indexOf(records.get(i + 1)));
//                	System.out.println("====, with i="+i+", the modifiedString="+modifiedString); 
                    records.set(
                            i,
                            "\n"
                                    + message.substring(
                                            message.indexOf(records.get(i)),
                                            message.indexOf(records.get(i + 1))));
                } else {
//                	String modifiedString = "\n"
//                        + message.substring(message.indexOf(records
//                                .get(i))); 
//                	System.out.println("====*****, with i="+i+", the modifiedString="+modifiedString); 
                    records.set(
                            i,
                            "\n"
                                    + message.substring(message.indexOf(records
                                            .get(i))));
                }
            }
        } catch (Exception e) {
            e.printStackTrace();
            if (log.isInfoEnabled()) {
                log.info("No valid records found!");
            }
            theLogger.warn("No valid records found!");
        }
        return;
    }
    
    private void displayStringList(List<String> stringList) {
    	int index = 1; 
    	for(String eachString : stringList) {
    		System.out.println("=====, String Item No."+index+" ="+eachString); 
    		index++; 
    	}
    }
}
