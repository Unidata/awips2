/**
 * This software was modified from Raytheon's pirep plugin by
 * NOAA/NWS/NCEP/NCO to order to output point data in HDF5.
 **/
package gov.noaa.nws.ncep.edex.plugin.ncpirep;

import java.util.ArrayList;
import java.util.List;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;

import com.raytheon.edex.esb.Headers;
import com.raytheon.edex.plugin.AbstractRecordSeparator;
import com.raytheon.uf.edex.decodertools.core.DecoderInput;
import com.raytheon.uf.edex.decodertools.core.IDecoderInput;
import com.raytheon.uf.edex.wmo.message.WMOHeader;

/**
 * The NcPirepSeparator takes a potential weather message and attempts to
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
 * 04/28/2011              F.J.Yen       Initial creation from pirep.
 * 09/22/2011   286        qzhou       Put new regex pattern to get header time. Do separate message accordingly.
 * 									   Modified doSeparate and added separate()
 * </pre>
 * 
 * @author jkorman
 * @version 1.0
 */
public class NcPirepSeparator extends AbstractRecordSeparator {
    /** The logger */
    private Log logger = LogFactory.getLog(getClass());

    //private static final String PIREP_HDR = "[\\r\\n]*.*(UA|UUA) +/OV";
    private static final String SPLITMSG = "\\r\\n\\x03";
    private static final String PIREP = "[A-Z]{3}[ ](UA|UUA)";
    private static final String HEADER_TIME = "[0-9]{6}";
    
    private WMOHeader wmoHeader = null;

    private byte[] messageData = null;

    private List<String> reports =  new ArrayList<String>();;

    private int currentReport = -1;
    
    public static NcPirepSeparator separate(byte[] data, Headers headers) {
        NcPirepSeparator ncPirepSeparator = new NcPirepSeparator();
        ncPirepSeparator.setData(data, headers);
        return ncPirepSeparator;
    }

    /**
     * Get the next record. This implementation returns the record as a String.
     * 
     * @return The next observation record as a String.
     */
    @Override
    public IDecoderInput next() {
        
        IDecoderInput data = null;
        if (hasNext()) {
            data = new DecoderInput(wmoHeader,reports.get(currentReport++));
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
        System.out.println("========== Begin =============== ");
        //rawMessage = DecoderTools.cleanData(rawMessage);       
        if (rawMessage != null) {
            wmoHeader = new WMOHeader(rawMessage);
            if (wmoHeader.isValid()) {
//                messageData = DecoderTools.stripWMOHeader(rawMessage,
//                        WMO_HEADER);               
//                doSeparate(new String(messageData));
            	doSeparate(new String(rawMessage));
            }
        }

        if ((reports != null) && (reports.size() > 0)) {
            currentReport = 0;
        } else {
            String msg = (wmoHeader != null) ? wmoHeader.getWmoHeader() : "";
            
            logger.info("No reports in data in " + msg);
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

    /**
     * Separate the candidate message into individual pirep reports based on the
     * report header .
     * 
     * @param message
     *            The message data to separate.
     */
    private void doSeparate(String message) {
    	
    	reports = new ArrayList<String>();
        int start = 0;
        int stop = message.length();
    	
    	Pattern pattern = Pattern.compile(SPLITMSG);
    	Matcher matcher = pattern.matcher(message);
    	
    	while  (matcher.find()) {
    		stop = matcher.start() +1;
            String subMsg = message.substring(start, stop);
            //System.out.println("***subMsg "+subMsg);
            separate( subMsg);
            
    		start = matcher.end();
    	}    	
    }

    /*
     * 046 ^M
	 * UBUS01 KMSC 190000^M
	 * ENA UA /OV PDN288051/TM 2348/FL340/TP B737/TA M56/WV 218035/TB CONT ^M
	 * LGHT OCNL MOD/RM CONT CHOP FL350 ZAN =^M
     */
    private void separate(String message) {
    	int start = 0;
        String headerTime = "";
        
        // find header time. Later append it to observation
    	Pattern pat = Pattern.compile(HEADER_TIME);
    	Matcher mat = pat.matcher(message);
    	if (mat.find())
    		headerTime = message.substring(mat.start(), mat.end());
    	
    	// find body records
        Pattern p = Pattern.compile("="); //PIREP_HDR);
        Matcher m = p.matcher(message);
        
        while (m.find()) {  
        	String sub = message.substring(start, m.end()); 
            Pattern pattern = Pattern.compile(PIREP);
        	Matcher matcher = pattern.matcher(sub); //only one head in the message
        	String observation ="";
        	
        	if (matcher.find()) {
        		observation = headerTime +" "+ sub.substring(matcher.start());
        		reports.add(observation);
        	}      		
       	
        	start = m.end();
        	
        }
    }
 
    
    public static void main(String [] args) {
    	String message = "ENA UA /OV PDN288051/TM 2348/FL340/TP B737/TA M56/WV 218035/TB CONT LGHT OCNL MOD/RM CONT CHOP FL350 ZAN =";
    	//doSeparate(message);
    	
    }
}
