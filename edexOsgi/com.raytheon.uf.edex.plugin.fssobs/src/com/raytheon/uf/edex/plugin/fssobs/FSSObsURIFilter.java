package com.raytheon.uf.edex.plugin.fssobs;

import java.text.SimpleDateFormat;
import java.util.Date;
import java.util.HashMap;
import java.util.Set;
import java.util.TimeZone;
import java.util.regex.Pattern;

import com.raytheon.edex.urifilter.URIFilter;
import com.raytheon.uf.common.dataplugin.message.DataURINotificationMessage;
import com.raytheon.uf.common.monitor.data.ObConst;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import org.locationtech.jts.geom.Coordinate;

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
 * Feb 15, 2013 1638       mschenke    Moved DataURINotificationMessage to uf.common.dataplugin
 * Sep 04, 2014 3220       skorolev    Removed CWA from filter.
 * Sep 17, 2015 3873       skorolev    Added pattern for moving platforms.
 * Dec 02, 2015 3873       dhladky     Revamped for better performance.
 * 
 * </pre>
 * 
 * @author skorolev
 * @version 1.0
 */
public class FSSObsURIFilter extends URIFilter {
    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(URIFilter.class);

    /** Station coordinates **/
    private Coordinate stationCoor = null;

    /** Patterns used for matching URI's **/
    private HashMap<String, Pattern> patternKeys = null;

    /** All filtered stations */
    private Set<String> stations = null;

    /** Date format **/
    private static String datePattern = "yyyy-MM-dd_HH:mm:ss.S";

    /**
     * Constructor
     * 
     * @param name
     *            of filter
     * 
     * @param stations
     *            for FSSObs filter
     */
    public FSSObsURIFilter(String name, Set<String> stations) {
        super(name);
        logger.info("FSSObsFilter " + name + " Filter construction...");
        setDataTypes(new String[] { "obs", "sfcobs", "ldadmesonet" });
        setExclude(false);
        SimpleDateFormat datef = new SimpleDateFormat(datePattern);
        datef.setTimeZone(TimeZone.getTimeZone("Zulu"));
        setDateFormatter(datef);
        this.stations = stations;
        setMatchURIs();
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.edex.urifilter.URIFilter#setMatchURIs()
     */
    @Override
    public void setMatchURIs() {
        
        Pattern pat = Pattern.compile("#");
        /**
         * For non moving platforms we setup specific patterns as they are fixed locations.
         */
        for (String st : stations) {
            String[] tokens = pat.split(st);
            String station = tokens[0];
            String dataType = tokens[1];
            switch (dataType) {
            case ObConst.METAR:
                Pattern metarPattern = getMetarPattern(station);
                statusHandler.info("Adding Pattern for type: "+ObConst.METAR+" Pattern: "+metarPattern.toString());
                getMatchURIs().put(metarPattern, 0l);
                break;
            case ObConst.MARITIME:
                Pattern maritimePattern = getMaritimePattern(station);
                statusHandler.info("Adding Pattern for type: "+ObConst.MARITIME+" Pattern: "+maritimePattern.toString());
                getMatchURIs().put(maritimePattern, 0l);
                break;
            case ObConst.MESONET:
                Pattern mesowestPattern = getMesowestPattern(station);
                statusHandler.info("Adding Pattern for type: "+ObConst.MESONET+" Pattern: "+mesowestPattern.toString());
                getMatchURIs().put(mesowestPattern, 0l);
                break;
            default:
                statusHandler.error("Get unknown data type " + dataType);
                break;
            }
        }
        
        /**
         * Moving types aren't fixed, we use a general pattern and then filter upon discovery.
         */
        
        for (String rt : FSSObsGenerator.movingTypes) {
            Pattern movingPattern = getMovingPattern(rt);
            statusHandler.info("Adding Pattern for type: "+rt+" Pattern: "+movingPattern.toString());
            getMatchURIs().put(movingPattern, 0l);
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
     * Gets station coordinates
     * 
     * @return stationCoor
     */
    public Coordinate getStationCoor() {
        return stationCoor;
    }

    /**
     * Gets Metar Pattern.
     */
    private Pattern getMetarPattern(String station) {
        // "/obs/2010-11-01_14:15:00.0/METAR<SPECI???>/null/K0A9/36.371/-82.173"
        Pattern metarPattern = Pattern.compile("/obs/" + wildCard + uriSeperator
                + wildCard + uriSeperator + "null" + uriSeperator + station
                + uriSeperator);
        return metarPattern;
    }

    /**
     * Gets Maritime Pattern
     */
    private Pattern getMaritimePattern(String station) {
        // /sfcobs/2010-10-28_10:36:00.0/1004[5]/null/BEPB6/32.373/-64.703
        Pattern maritimePattern = Pattern.compile("/sfcobs/" + wildCard + uriSeperator
                + wildCard + uriSeperator + "null" + uriSeperator + station
                + uriSeperator);
        return maritimePattern;
    }

    /**
     * Gets Mesowest Pattern.
     * 
     * @param mesowestPattern
     *            the mesowestPattern to set
     */
    private Pattern getMesowestPattern(String station) {
        // There is no dataURI for ldadmesonet data
        // /ldadmesonet/2011-06-29_22:10:00.0/mesonet/NWSRAWS/RINN4/41.1181/-74.2403
        Pattern mesowestPattern = Pattern.compile("/ldadmesonet/" + wildCard
                + uriSeperator + wildCard + uriSeperator + wildCard
                + uriSeperator + station);
        return mesowestPattern;
    }


    /**
     * Sets pattern for ships "1003", drifting buoys "1006" and MAROBs "1007".
     * 
     * @param reportType
     */
    private Pattern getMovingPattern(String reportType) {
        // /sfcobs/2010-10-28_10:36:00.0/1003<6,7>/null/BEPB6/32.373/-64.703
       Pattern movingPattern = Pattern.compile("/sfcobs/" + wildCard
                + uriSeperator + reportType + uriSeperator + wildCard
                + uriSeperator);
       return movingPattern;
    }

}
