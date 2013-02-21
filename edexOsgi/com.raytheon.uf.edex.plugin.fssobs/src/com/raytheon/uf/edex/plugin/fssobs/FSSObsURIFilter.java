package com.raytheon.uf.edex.plugin.fssobs;

import java.text.SimpleDateFormat;
import java.util.Date;
import java.util.HashMap;
import java.util.List;
import java.util.TimeZone;
import java.util.regex.Pattern;

import com.raytheon.edex.msg.DataURINotificationMessage;
import com.raytheon.edex.site.SiteUtil;
import com.raytheon.edex.urifilter.URIFilter;
import com.raytheon.edex.urifilter.URIGenerateMessage;
import com.vividsolutions.jts.geom.Coordinate;

/**
 * FSSObs URI Filter
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Dec 5, 2012  #1351      skorolev    Cleaned code
 * 
 * </pre>
 * 
 * @author skorolev
 * @version 1.0
 */
public class FSSObsURIFilter extends URIFilter {

    /** Station ID **/
    private String stn;

    /** CWA **/
    private String cwa;

    /** Monitor's name **/
    private String monitorName;

    /** Station coordinates **/
    private Coordinate stationCoor = null;

    /** Patterns used for matching URI's **/
    private HashMap<String, Pattern> patternKeys = null;

    /** METAR Pattern **/
    private Pattern MetarPattern = null;

    /** Maritime Pattern **/
    private Pattern MaritimePattern = null;

    /** Mesowest Pattern **/
    private Pattern MesowestPattern = null;

    /** Current data type #METAR, #Maritime or #Mesonet **/
    private String dataType;

    /** Current Site **/
    private String currentSite = SiteUtil.getSite();

    /** Date format **/
    private static String datePattern = "yyyy-MM-dd_HH:mm:ss.S";

    /** Station type **/
    private enum StnType {
        METAR, MARITIME, MESONET
    };

    /**
     * Constructor
     * 
     * @param name
     *            Monitor name
     */
    public FSSObsURIFilter(String name) {
        super(name);
        logger.info("FSSObsFilter " + name + " Filter construction...");
        setDataTypes(new String[] { "obs", "sfcobs", "ldadmesonet" });
        setExclude(false);
        SimpleDateFormat datef = new SimpleDateFormat(datePattern);
        datef.setTimeZone(TimeZone.getTimeZone("Zulu"));
        setDateFormatter(datef);
        setCwa(currentSite);
        // Which monitor should use: fog, ss or snow
        this.setMonitorName(name);
        setMatchURIs();
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.edex.urifilter.URIFilter#setMatchURIs()
     */
    @Override
    public void setMatchURIs() {
        List<String> stns = FSSObsUtils.getStations(name);
        Pattern pat = Pattern.compile("#");
        for (String st : stns) {
            String[] tokens = pat.split(st);
            setStn(tokens[0]);
            setDataType(tokens[1]);
            if (getDataType().equals(StnType.METAR.name())) {
                setMetarPattern();
                getMatchURIs().put(getMetarPattern(), 0l);
            }
            if (getDataType().equals(StnType.MARITIME.name())) {
                setMaritimePattern();
                getMatchURIs().put(getMaritimePattern(), 0l);
            }
            if (getDataType().equals(StnType.MESONET.name())) {
                setMesowestPattern();
                getMatchURIs().put(getMesowestPattern(), 0l);
            }
        }
    }

    /**
     * @param message
     * @return boolean
     */
    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.edex.urifilter.URIFilter#isMatched(com.raytheon.edex.msg
     * .DataURINotificationMessage)
     */
    @Override
    public boolean isMatched(DataURINotificationMessage message) {
        setCurrentTime(new Date(System.currentTimeMillis()));
        if ((message != null) && (message.getDataURIs().length > 0)) {
            for (String dataUri : message.getDataURIs()) {
                // add your pattern checks to the key
                for (Pattern pattern : getMatchURIs().keySet()) {
                    if (pattern.matcher(dataUri).find()) {
                        // extract times, used later
                        setValidTime(getTime(dataUri, getDateFormatter()));
                        // duration value
                        long duration = 0l;
                        if (!getMatchedURIs().containsKey(pattern.toString())) {
                            // brand spanking new
                            getMatchedURIs().put(dataUri, getValidTime());
                            getMatchTimes().put(dataUri, getCurrentTime());
                            match = true;
                        } else {
                            duration = getValidTime().getTime()
                                    - getMatchedURIs().get(pattern.toString())
                                            .getTime();
                            logger.info(name + ": not new. " + dataUri
                                    + " Age of MatchedURI: " + duration
                                    / (1000 * 60) + " minutes");

                            if (logger.isDebugEnabled()) {
                                // got a replacement
                                logger.debug(name
                                        + ": not new. "
                                        + dataUri
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
                                logger.info(name
                                        + ": in range: replaced in matchedURIs. "
                                        + dataUri);
                            } else {
                                logger.info(name
                                        + ": not in range: discarded. "
                                        + dataUri);
                            }
                        }
                    }
                }
            }
        }
        return match;
    }

    /**
     * Gets the matching key for a matching pattern
     * 
     * @param pattern
     * @return key
     */
    public String getPatternName(Pattern pattern) {
        for (String key : patternKeys.keySet()) {
            if (patternKeys.get(key).pattern().equals(pattern.toString())) {
                return key;
            }
        }

        return null;
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.edex.urifilter.URIFilter#applyWildCards(java.lang.String)
     */
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

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.edex.urifilter.URIFilter#removeWildCards(java.lang.String)
     */
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

    /**
     * Gets CWA
     * 
     * @return cwa
     */
    public String getCwa() {
        return cwa;
    }

    /**
     * Sets CWA
     * 
     * @param cwa
     */
    public void setCwa(String cwa) {
        this.cwa = cwa;
    }

    /**
     * Gets station coordinates
     * 
     * @return stationCoor
     */
    public Coordinate getStationCoor() {
        return stationCoor;
    }

    /**
     * Gets Metar Pattern.
     * 
     * @return MetarPattern
     */
    public Pattern getMetarPattern() {
        return MetarPattern;
    }

    /**
     * Sets Metar Pattern.
     */
    public void setMetarPattern() {
        // "/obs/2010-11-01_14:15:00.0/METAR<SPECI???>/null/K0A9/36.371/-82.173"
        MetarPattern = Pattern.compile("/obs/" + wildCard + uriSeperator
                + wildCard + uriSeperator + "null" + uriSeperator + getStn()
                + uriSeperator);
    }

    /**
     * Gets Maritime Pattern.
     * 
     * @return MaritimePattern
     */
    public Pattern getMaritimePattern() {
        return MaritimePattern;
    }

    /**
     * Sets Maritime Pattern
     */
    public void setMaritimePattern() {
        // /sfcobs/2010-10-28_10:36:00.0/1004/null/BEPB6/32.373/-64.703
        MaritimePattern = Pattern.compile("/sfcobs/" + wildCard + uriSeperator
                + wildCard + uriSeperator + "null" + uriSeperator + getStn()
                + uriSeperator);
    }

    /**
     * Gets Mesowest Pattern.
     * 
     * @return the mesowestPattern
     */
    public Pattern getMesowestPattern() {
        return MesowestPattern;
    }

    /**
     * Sets Mesowest Pattern.
     * 
     * @param mesowestPattern
     *            the mesowestPattern to set
     */
    public void setMesowestPattern() {
        // /ldadmesonet/2011-06-29_22:10:00.0/mesonet/NWSRAWS/RINN4/41.1181/-74.2403
        MesowestPattern = Pattern.compile("/ldadmesonet/" + wildCard
                + uriSeperator + wildCard + uriSeperator + wildCard
                + uriSeperator + getStn());
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.edex.urifilter.URIFilter#createGenerateMessage()
     */
    @Override
    public URIGenerateMessage createGenerateMessage() {
        return new FSSObsURIGenrtMessage(this);
    }

    /**
     * Gets station name
     * 
     * @return the stn
     */
    public String getStn() {
        return stn;
    }

    /**
     * Sets station name.
     * 
     * @param stn
     *            the stn to set
     */
    public void setStn(String stn) {
        this.stn = stn;
    }

    /**
     * Sets data type.
     * 
     * @param the
     *            dataType to set
     */
    public void setDataType(String type) {
        this.dataType = type;
    }

    /**
     * Gets data type.
     * 
     * @return the dataType
     */
    public String getDataType() {
        return dataType;
    }

    /**
     * Gets Monitor Name.
     * 
     * @return the monitorName
     */
    public String getMonitorName() {
        return monitorName;
    }

    /**
     * Sets Monitor Name.
     * 
     * @param monitorName
     *            the monitorName to set
     */
    public void setMonitorName(String monitorName) {
        this.monitorName = monitorName;
    }

}
