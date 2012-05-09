/**
 * This software was modified from Raytheon's airep plugin by
 * NOAA/NWS/NCEP/NCO to order to output point data in HDF5.
 **/
package gov.noaa.nws.ncep.edex.plugin.ncairep;

import java.util.ArrayList;
import java.util.List;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;

import com.raytheon.edex.esb.Headers;
import com.raytheon.edex.plugin.AbstractRecordSeparator;
import com.raytheon.uf.edex.wmo.message.WMOHeader;
import gov.noaa.nws.ncep.edex.plugin.ncairep.NcAirepDecoder.NcAirepDecoderInput;

/**
 * The NcAirepSeparator takes a potential weather message and attempts to
 * determine the WMO header and data type of the enclosed data. Normal usage is
 * to create an instance and set the message data using the setData method. When
 * complete the separator contains the WMO header, the message data with all
 * leading data up to and including the WMO header removed. In addition all
 * extraneous spaces and carriage control has been removed. The message reports
 * are available using hasNext to determine if data is available, and the
 * getRecord method to retrieve the actual report data. Note that this separator
 * implementation should not be used on mixed text/binary weather messages.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 04/27/2011              F.J.Yen       Initial creation from airep.
 * 09/19/2011    286       Q.Zhou      Put new regex pattern. Separate message according to new patterns.
 * 									   Modified separate(), separateNcAIREP() and separateARPARS().
 * 									   Added addDayInTime(). 
 * 									   Removed DecoderTools.cleanData(), stripHeader().
 * 									   Handle amdar in the airep separator.		
 * 10/04/2011    286       Q.Zhou      Move addDayInTime() to the parser	
 * </pre>
 * 
 * @author F. J. Yen
 * @version 1.0
 */
public class NcAirepSeparator extends AbstractRecordSeparator {
    /** The logger */
    private Log logger = LogFactory.getLog(getClass());

//    private static final String SPLITCHARS = "[=;$][^\\r\\n]*[\\r\\n]+";
//    private static final String AIREP_HDR = "AIREP[\\r\\n]+";
    private static final String SPLITMSG = "\\r\\n\\x03"; // \\x03
    private static final String AIREP_MSG_LINE = "(ARP|ARS)";
    private static final String HEADER_TIME = "[0-9]{6}";
    
    private WMOHeader wmoHeader = null;

    private byte[] messageData = null;

    private List<String> reports = null;

    private int currentReport = -1;

    public static NcAirepSeparator separate(byte[] data, Headers headers) {
    	NcAirepSeparator ncAirepSeparator = new NcAirepSeparator();
        ncAirepSeparator.setData(data, headers);
        return ncAirepSeparator;
    }

    /**
     * Get the next record. This implementation returns the record as a String.
     * 
     * @return The next observation record as a String.
     */
    public NcAirepDecoderInput next() {
        NcAirepDecoderInput data = null;
        if (hasNext()) {
            data = new NcAirepDecoderInput();
            data.report = reports.get(currentReport++);
            data.wmoHeader = wmoHeader;
        }
        return data;
    }

    /**
     * Is there another record available?
     * 
     * @return Is there another record available?
     */
    @Override
    public boolean hasNext() {
        return ((reports != null) && (reports.size() > 0) && (currentReport < reports
                .size()));
    }

    /**
     * Set the raw message data and invoke the internal message separation
     * process.
     * 
     * @param rawMessage
     *            The raw weather text message.
     */
    @Override
    public void setData(byte[] rawMessage, Headers headers) {
        currentReport = -1;
        reports = null;   
        
        //rawMessage = DecoderTools.cleanData(rawMessage);        	
        if (rawMessage != null) {
            wmoHeader = new WMOHeader(rawMessage);
            if (wmoHeader.isValid()) {
            	//messageData = DecoderTools.stripWMOHeader(rawMessage, IDecoderConstants.WMO_HEADER);
                 
                separate(new String(rawMessage));               
            }
        }

        if ((reports != null) && (reports.size() > 0)) {
            currentReport = 0;
        } else {
            logger.info("No reports found in data");
        }
    }

    /**
     * Get the message data.
     * 
     * @return The cleaned message data.
     */
    public byte[] getMessage() {
        return messageData;
    }

    /**
     * Get the WMO header associated with the message data.
     * 
     * @return The WMO header.
     */
    public WMOHeader getWmoHeader() {
        return wmoHeader;
    }

    private void separate(String message) {
    
    	reports = new ArrayList<String>();
        int start = 0;
        int stop = message.length();
        
    	Pattern pattern = Pattern.compile(SPLITMSG);
    	Matcher matcher = pattern.matcher(message);
    	
    	while  (matcher.find()) {
    		stop = matcher.start() + 1;
            String subMsg = message.substring(start, stop);
            //System.out.println("***subMsg "+subMsg);
            if (subMsg.contains("AIREP")) {           
    			separateNcAIREP(subMsg); 
    		} 
            else if (subMsg.contains("AMDAR")) {           
    			separateNcAMDAR(subMsg); 
    		} 
            else {
    			separateARPARS(subMsg); //amdar also goes to here
    		}
    		start = matcher.end();
    	}
    }

    /**
     * Separate amdar data that comes in with AMDAR header line. 
     * 097 ^M
	 * UDAS02 BABJ 190113^M
	 * AMDAR 1901^M
	 * LVR CNFNXL 3315N 11850E 190113 F226 MS123 278/038 TB/ S//1=^M
     */
    private void separateNcAMDAR(String message) {
    	// find header time. Later append it to observation
    	String headerTime = findHeaderTime(message);
    	
    	// find body msg
    	Pattern pattern = Pattern.compile("=");
    	Matcher matcher = pattern.matcher(message);

        int start = 0;
        int stop = message.length();
        while (matcher.find()) {
            stop = matcher.start() + 1;
            String observation = message.substring(start, stop);
            
            // remove 'AIREP' on first AIREP record
            if (observation.contains("AMDAR")) {
            	int iAir = observation.indexOf("AMDAR");
            	observation = observation.substring(iAir + 10);          
    		}
            
            reports.add( headerTime + " AMDAR " +observation);
            
            start = matcher.end();
        }
    }

    /**
     * Separate airep data that comes in with AIREP header line. 
     * 968 ^M
	 * UAFJ01 NFFN 200000^M
	 * AIREP^M
	 * JST4 0050N17838W 2319 F360 MS46 302/025=^M
     */
    private void separateNcAIREP(String message) {
    	// find header time. Later append it to observation
    	String headerTime = findHeaderTime(message);
    	
    	// find body msg
    	Pattern pattern = Pattern.compile("=");
    	Matcher matcher = pattern.matcher(message);

        int start = 0;
        int stop = message.length();
        while (matcher.find()) {
            stop = matcher.start() + 1;
            String observation = message.substring(start, stop);
            
            // remove 'AIREP' on first AIREP record
            if (observation.contains("AIREP")) {
            	int iAir = observation.indexOf("AIREP");           
            	observation = observation.substring(iAir + 5);          
    		}
            
            reports.add( headerTime + " ARP " + observation);
            
            start = matcher.end();
        }
    }

    /**
     * Separate airep data that does not have the AIREP header line. This data
     * is checked to ensure that it contains ARP/ARS report start data.
     * The ending of the ARP/ARS report could be "=", or only has one ARP/ARS report.
     * 706 ^M
	 * UAPA01 KWBC 200000^M
	 * ARP UAL559 3401N 13421W 2351 F360 MS52 260/025KT=
     */
    private void separateARPARS(String message) {
    	// find header time. Later append it to observation
    	String headerTime = findHeaderTime(message);
    	
    	// find body msg    	
    	ArrayList<Integer> bodyRecords = new ArrayList<Integer>();
        Pattern pattern = Pattern.compile(AIREP_MSG_LINE);
        Matcher matcher = pattern.matcher(message);

        while (matcher.find()) {
            bodyRecords.add(matcher.start());
        }

        for (int i = 0; i < bodyRecords.size(); i++) {
            String observation = null;
            if (i < bodyRecords.size() - 1) {
                observation = message.substring(bodyRecords.get(i),
                        bodyRecords.get(i + 1)).trim();
            } else {
                observation = message.substring(bodyRecords.get(i)).trim();
            }
            
            reports.add( headerTime +" " +observation);
        }
        bodyRecords = null;
    }

    /*
     *  find header time for each report
     */
    private String findHeaderTime(String message) {    	
    	String headerTime = "";
        
        Pattern pat = Pattern.compile(HEADER_TIME);
    	Matcher mat = pat.matcher(message);
    	if (mat.find())
    		headerTime = message.substring(mat.start(), mat.end());
    	
    	return headerTime;
    }
    

}

