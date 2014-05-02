/**
 * This software was developed and / or modified by HSEB, OHD
 
 **/
package gov.noaa.nws.ohd.edex.plugin.hydrodualpol;

import java.util.Date;
import java.util.regex.Pattern;

//import com.raytheon.edex.msg.DataURINotificationMessage;
import com.raytheon.uf.common.dataplugin.message.DataURINotificationMessage;
import com.raytheon.edex.urifilter.URIFilter;
import com.raytheon.edex.urifilter.URIGenerateMessage;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;

/**
 * TODO Add Description
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * May 13, 2013            jtDeng     Initial creation
 * Construct the DSA/DPR/DAA URI filter pattern.
 * </pre>
 * 
 * @author deng2
 * @version 1.0
 */

public class HydroDualPolURIFilter extends URIFilter {

	 private static final transient IUFStatusHandler statusHandler = UFStatus
     .getHandler(HydroDualPolGenerator.class);
	
    private static final long serialVersionUID = 1346354654L;

    /** an ICAO you may wish to use in matching */
    protected String icao = null;

    public static double tiltAngle = 0.0;

    public static double layer = 0.0;

    /** Dual pol DSA/DPR?DAA prod ID */
    
    public static String daa = "170";

    public static String dsa = "172";

    public static String dpr = "176";
    
    private String productType = null;

    /** URI pattern for DSA/DPR/DAA radar */
    public Pattern dsaURIpattern = null;

    public Pattern dhrURIpattern = null;

    /** URI pattern for DPR radar */
    public Pattern dprURIpattern = null;

    /** URI pattern for DSA radar */
    public Pattern daaURIpattern = null;

    public HydroDualPolURIFilter(String name, String productType) {
        super(name);
        logger.debug("HydroDualPolFilter " + name + " Filter construction...");
        
        setProductType(productType);
        setDataTypes(new String[] { "radar" });
        setIcao(name);
        setExclude(false);
        setMatchURIs();
    }

    /**
     * Filter By ICAO
     * 
     * @return
     */
    public String getIcao() {
        return icao;
    }

    /**
     * Set the filtering ICAOs
     * 
     * @param icaos
     */
    public void setIcao(String icao) {
        this.icao = icao;
    }

    @Override
    protected String applyWildCards(String key) {

        String[] comps = key.split(uriSeperator);
        StringBuffer newKey = new StringBuffer();

        for (int i = 1; i < comps.length; i++) {
            if (i == 2) {
                // second comp is time, wildcarded for most
                newKey.append(uriSeperator + wildCard);
            } else if (i == 0) {
                // do nothing
            } else {
                newKey.append(uriSeperator + comps[i]);
            }
        }
        return newKey.toString();
    }

    @Override
    protected String removeWildCards(String key) {

        String[] comps = key.split(uriSeperator);
        StringBuffer newKey = new StringBuffer();
        String time = getDateFormatter().format(
                getMatchedURIs().get(key).getTime());

        for (int i = 1; i < comps.length; i++) {
            if (i == 2) {
                // second comp is time, wildcarded for most
                newKey.append(uriSeperator + time);
            } else if (i == 0) {
                // do nothing
            } else {
                newKey.append(uriSeperator + comps[i]);
            }
        }
        return newKey.toString();
    }

    @Override
    public void setMatchURIs() {
        // Duration doesn't matter as we are just keying on DSA,DPR,DAA products
        
        long duration = 60 * 1000l * 0; // 5 mins
       
        if (getProductType().equalsIgnoreCase(daa))
        {
        	 setDAAURIPattern();
        	 getMatchURIs().put(getDAAURIPattern(), duration);
        }
        else if (getProductType().equalsIgnoreCase(dpr))
        {
        	 setDPRURIPattern();
        	 getMatchURIs().put(getDPRURIPattern(), duration);
        }
        else if (getProductType().equalsIgnoreCase(dsa))
        {
        	 setDSAURIPattern();
        	 getMatchURIs().put(getDSAURIPattern(), duration);
        }   
    }

    /**
     * Sets the DSA URI Pattern
     * 
     * @return
     */
    public void setDSAURIPattern() {

        dsaURIpattern = Pattern.compile("/radar/" + ".*" + uriSeperator
                + getIcao() + uriSeperator + dsa + uriSeperator + tiltAngle
                + uriSeperator + layer);        
    }

    /**
     * Gets the DSA URI Pattern
     * 
     * @return
     */
    public Pattern getDSAURIPattern() {
        return dsaURIpattern;
    }

    /**
     * Sets the DPR URI Pattern
     * 
     * @return
     */
    public void setDPRURIPattern() {

        dprURIpattern = Pattern.compile("/radar/" + ".*" + uriSeperator
                + getIcao() + uriSeperator + dpr + uriSeperator + tiltAngle
                + uriSeperator + layer);
    }

    /**
     * Gets the DPR URI Pattern
     * 
     * @return
     */
    public Pattern getDPRURIPattern() {
        return dprURIpattern;
    }

    /**
     * Sets the DAA URI Pattern
     * 
     * @return
     */
    public void setDAAURIPattern() {
    
        daaURIpattern = Pattern.compile("/radar/" + ".*" + uriSeperator
                + getIcao() + uriSeperator + daa + uriSeperator + "0.0" + uriSeperator +"0.0");
   
        
    }

    /**
     * Gets the DAA URI Pattern
     * 
     * @return
     */
    public Pattern getDAAURIPattern() {
        return daaURIpattern;
    }

    @Override
    public URIGenerateMessage createGenerateMessage() {
        return new HydroDualPolURIGenerateMessage(this);
    }

    
    /**
     * This is the real deal where you look for matches based on your
     * suppositions. It keeps a list called matchedURIs, when all matchURIs have
     * been satisfied with a matchedURI, this will return true.
     * 
     * @param message
     * @return boolean
     */
    
    private void  log(String textMessage)
    {
    	  statusHandler.handle(Priority.INFO, textMessage);
    }
    
    
    public boolean isMatched(DataURINotificationMessage message) {

    	String header = "HydroDualPolURIFilter.isMatched(): ";
   
    	boolean debug = logger.isDebugEnabled();
    	//debug = true;  //force it true
    	
    	
        setCurrentTime(new Date(System.currentTimeMillis()));
        // process all the product messages
        if ((message != null) && (message.getDataURIs().length > 0))
        {
            if (debug) {
          //  	log(header + name + ": Filtering Messages..."
          //              + message.getDataURIs().length);
            }
            
            
            for (String key : message.getDataURIs()) {
            	
            	//debug = true;
            	
                // add your pattern checks to the key
                for (Pattern pattern : getMatchURIs().keySet()) {
                	
                    if (debug) {
                    	
                    	if (key.contains("/" + dpr + "/") && (key.contains("radar")))
                    	{
                    		log(header + "********** This should match! ****************");
                    		log(header + "\nhPattern: " + pattern.toString()
                                    + "\nKey: " + key);
                      	}                    	
                    }
                    
                    if (pattern.matcher(key).matches()) {
                        // extract times, used later
                        setValidTime(getTime(key, getDateFormatter()));
                        long duration = 0l;
                        if (debug) {
                        	log(header + name + ": match Found: " + key);
                        }

                        if (!getMatchedURIs().containsKey(pattern.toString())) {
                            // brand spanking new
                            getMatchedURIs().put(pattern.toString(),
                                    getValidTime());
                            getMatchTimes().put(pattern.toString(),
                                    getCurrentTime());
                            if (debug) {
                            	log(header + name + ": new product. " + key);
                            }
                        } else {
                            duration = getValidTime().getTime()
                                    - getMatchedURIs().get(pattern.toString())
                                            .getTime();
                            if (debug) {
                            	log(header + name + ": not new. " + key
                                        + " Age of MatchedURI: " + duration
                                        / (1000 * 60) + " minutes");

                                // got a replacement
                            	log(header + name
                                        + ": not new. "
                                        + key
                                        + " Age of MatchURI: "
                                        + getMatchURIs().get(pattern)
                                                .longValue() / (1000 * 60)
                                        + " minutes");
                            }

                            if (duration <= getMatchURIs().get(pattern)
                                    .longValue()) {
                                // replace
                                getMatchedURIs().remove(pattern.toString());
                                getMatchTimes().remove(pattern.toString());
                                getMatchedURIs().put(pattern.toString(),
                                        getValidTime());
                                getMatchTimes().put(pattern.toString(),
                                        getCurrentTime());
                                if (debug) {
                                	log(header + name
                                            + ": in range: replaced in matchedURIs. "
                                            + key);
                                }
                            } else if (debug) {
                            	log(header + name
                                        + ": not in range: discarded. " + key);
                            }
                        }
                    }
                }
            }
            if (debug) {
            	log(header + name + ": matchedURIs size: "
                        + getMatchedURIs().size());
            }
        }

        // check for repeats and equality of size
        if ((getMatchedURIs().size() == getMatchURIs().size()) && !isRepeat()) {
            // this is your green light condition
            if (debug) {
            	log(header + name + ": Matched all products! ");
            }
            match = true;

        } else {
            if (getMatchedURIs().size() > 0) {
                ageTimes();
            }
        }

        return match;
    }

	public void setProductType(String productType) {
		this.productType = productType;
	}

	public String getProductType() {
		return productType;
	}



}
