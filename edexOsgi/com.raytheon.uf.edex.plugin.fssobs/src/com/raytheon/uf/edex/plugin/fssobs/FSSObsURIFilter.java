package com.raytheon.uf.edex.plugin.fssobs;

import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Date;
import java.util.HashMap;
import java.util.TimeZone;
import java.util.regex.Pattern;

import com.raytheon.edex.msg.DataURINotificationMessage;
import com.raytheon.edex.site.SiteUtil;
import com.raytheon.edex.urifilter.URIFilter;
import com.raytheon.edex.urifilter.URIGenerateMessage;
import com.vividsolutions.jts.geom.Coordinate;

public class FSSObsURIFilter extends URIFilter {

    public String stn; // station

    public String cwa;

    public String monitorName;

    public Coordinate stationCoor = null;

    /** patterns used for matching URI's **/
    private HashMap<String, Pattern> patternKeys = null;

    public Pattern MetarPattern = null;

    public Pattern MaritimePattern = null;

    public Pattern MesowestPattern = null;

    // IDecoderConstants

    // callback to the generator
    public FSSObsGenerator fssgen = null;

    // Current data type #METAR, #Maritime or #Mesonet
    private String dataType;

    private String currentSite = SiteUtil.getSite();

    // dataTypes to process
    private HashMap<String, Pattern> dataTypes;

    public static String datePattern = "yyyy-MM-dd_HH:mm:ss.S";

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

    @Override
    public void setMatchURIs() {
        ArrayList<String> stns = FSSObsUtils.getStations(name);
        for (String st : stns) {
            String[] tokens = st.split("#");
            setStn(tokens[0]);
            setDataType(tokens[1]);
            if (getDataType().equals("METAR")) {
                setMetarPattern();
                getMatchURIs().put(getMetarPattern(), 0l);
            }
            if (getDataType().equals("MARITIME")) {
                setMaritimePattern();
                getMatchURIs().put(getMaritimePattern(), 0l);
            }
            if (getDataType().equals("MESONET")) {
                setMesowestPattern();
                getMatchURIs().put(getMesowestPattern(), 0l);
            }
        }
    }

    /**
     * @param message
     * @return boolean
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

                            // got a replacement
                            logger.debug(name + ": not new. " + dataUri
                                    + " Age of MatchURI: "
                                    + getMatchURIs().get(pattern).longValue()
                                    / (1000 * 60) + " minutes");
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
     * gets the matching key for a matching pattern
     * 
     * @param pattern
     * @return
     */
    public String getPatternName(Pattern pattern) {
        for (String key : patternKeys.keySet()) {
            if (patternKeys.get(key).pattern().equals(pattern.toString())) {
                return key;
            }
        }

        return null;
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

    public String getCwa() {
        return cwa;
    }

    public void setCwa(String cwa) {
        this.cwa = cwa;
    }

    public Coordinate getStationCoor() {
        return stationCoor;
    }

    public Pattern getMetarPattern() {
        return MetarPattern;
    }

    public void setMetarPattern() {
        // "/obs/2010-11-01_14:15:00.0/METAR<SPECI???>/null/K0A9/36.371/-82.173"
        MetarPattern = Pattern.compile("/obs/" + wildCard + uriSeperator
                + wildCard + uriSeperator + "null" + uriSeperator + getStn()
                + uriSeperator);
    }

    public Pattern getMaritimePattern() {
        return MaritimePattern;
    }

    public void setMaritimePattern() {
        // /sfcobs/2010-10-28_10:36:00.0/1004/null/BEPB6/32.373/-64.703
        MaritimePattern = Pattern.compile("/sfcobs/" + wildCard + uriSeperator
                + wildCard + uriSeperator + "null" + uriSeperator + getStn()
                + uriSeperator);
    }

    /**
     * @return the mesowestPattern
     */
    public Pattern getMesowestPattern() {
        return MesowestPattern;
    }

    /**
     * @param mesowestPattern
     *            the mesowestPattern to set
     */
    public void setMesowestPattern() {
        // /ldadmesonet/2011-06-29_22:10:00.0/mesonet/NWSRAWS/RINN4/41.1181/-74.2403
        MesowestPattern = Pattern.compile("/ldadmesonet/" + wildCard
                + uriSeperator + wildCard + uriSeperator + wildCard
                + uriSeperator + getStn());
    }

    @Override
    public URIGenerateMessage createGenerateMessage() {
        return new FSSObsURIGenrtMessage(this);
    }

    /**
     * @return the stn
     */
    public String getStn() {
        return stn;
    }

    /**
     * @param stn
     *            the stn to set
     */
    public void setStn(String stn) {
        this.stn = stn;
    }

    /**
     * @param the
     *            dataType to set
     */
    public void setDataType(String marine) {
        this.dataType = marine;
    }

    /**
     * @return the dataType
     */
    public String getDataType() {
        return dataType;
    }

    /**
     * @return the monitorName
     */
    public String getMonitorName() {
        return monitorName;
    }

    /**
     * @param monitorName
     *            the monitorName to set
     */
    public void setMonitorName(String monitorName) {
        this.monitorName = monitorName;
    }

}
