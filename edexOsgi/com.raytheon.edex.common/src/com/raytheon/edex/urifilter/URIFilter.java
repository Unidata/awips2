/**
 * This software was developed and / or modified by Raytheon Company,
 * pursuant to Contract DG133W-05-CQ-1067 with the US Government.
 * 
 * U.S. EXPORT CONTROLLED TECHNICAL DATA
 * This software product contains export-restricted data whose
 * export/transfer/disclosure is restricted by U.S. law. Dissemination
 * to non-U.S. persons whether in the United States or abroad requires
 * an export license or other authorization.
 * 
 * Contractor Name:        Raytheon Company
 * Contractor Address:     6825 Pine Street, Suite 340
 *                         Mail Stop B8
 *                         Omaha, NE 68106
 *                         402.291.0100
 * 
 * See the AWIPS II Master Rights File ("Master Rights File.pdf") for
 * further licensing information.
 **/
package com.raytheon.edex.urifilter;

import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Calendar;
import java.util.Date;
import java.util.HashMap;
import java.util.Iterator;
import java.util.TimeZone;
import java.util.regex.Pattern;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;

import com.raytheon.edex.msg.DataURINotificationMessage;
import com.raytheon.uf.common.serialization.ISerializableObject;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.edex.core.EdexException;

/**
 * 
 * Abstract Filter class used for filtering URI's.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 02/07/2009   1981       dhladky    Initial Creation.
 * 05/27/2009   2037       dhladky    Fixed concurrent mod 
 *                                    exception with URI maps on removal
 * 
 * </pre>
 * 
 * @author dhladky
 * @version 1.0
 */
public abstract class URIFilter implements ISerializableObject {
    private static final transient IUFStatusHandler statusHandler = UFStatus.getHandler(URIFilter.class);

    /**
     * 
     */
    private static final long serialVersionUID = 1L;

    /** whether or not you wish this to be an exclusionary filter */
    protected boolean exclude = false;

    /**
     * list of URI's you are trying to match, notice MatchURI's uses a Long,
     * this is a duration or validity measure not a time in the truest sense.
     */
    protected HashMap<Pattern, Long> matchURIs = null;

    /** list of URI's you have matched so far */
    protected HashMap<String, Date> matchedURIs = null;

    /** keeps track of the old matches that need updates */
    protected HashMap<String, Date> matchTimes = null;

    /** Types of data plugins in this CPG. */
    protected String[] dataTypes = null;

    /** whether or not you found a match */
    protected boolean match = false;

    /** whether or not you found a match */
    protected boolean repeat = true;

    /** whether or not you have already made a product for this */
    protected boolean generate = false;

    /** set the name of this URI filter */
    protected String name = null;

    /** regex wild card filler */
    protected static String wildCard = "[\\w\\(\\)\\[\\]\\-:_.]+";

    /** time used for matching comparisons */
    protected Date validTime = null;

    /** time used for matching comparisons */
    protected Date currentTime = null;

    /** formatter used in this matching */
    protected SimpleDateFormat sdf = null;

    /** previous URIs for matched */
    protected String[] previousURIs = null;

    /** Separator for URI's */
    public static final String uriSeperator = "/";

    /** sets the start for the forecast hour */
    public static final String forecastTimeSepStart = "(";

    /** sets the end for the forecast hour */
    public static final String forecastTimeSepEnd = ")";

    /** The logger */
    protected transient final Log logger = LogFactory.getLog(getClass());

    /** Pattern for dates in radar */
    public static String radarDatePattern = "yyyy-MM-dd_HH:mm:ss.S";

    /**
     * Public URI Abstract constructor
     */
    public URIFilter(String name) {
        // setup matched URIs
        init(name);
    }

    /**
     * rest after a successful match
     */
    public void reset() {
        match = false;
        setPreviousURIs(getURIs());
        getMatchedURIs().clear();
    }

    /**
     * Get matching date formatter
     * 
     * @return
     */
    public SimpleDateFormat getDateFormatter() {
        return sdf;
    }

    /**
     * Set the SimpleDataFormat
     * 
     * @param sdf
     */
    public void setDateFormatter(SimpleDateFormat sdf) {
        this.sdf = sdf;
    }

    /**
     * Get the exclusionary bool
     * 
     * @return
     */
    public boolean isExclude() {
        return exclude;
    }

    /**
     * Set is exclusionary or not
     * 
     * @param exclude
     */
    public void setExclude(boolean exclude) {
        this.exclude = exclude;
    }

    /**
     * Get the matched URI's and TLD's
     * 
     * @return
     */
    public HashMap<String, Date> getMatchedURIs() {
        return matchedURIs;
    }

    /**
     * Add the matchable URI's, with TLD's
     * 
     * @param matchURIs
     */
    public abstract void setMatchURIs();

    public URIGenerateMessage createGenerateMessage() {
        return new URIGenerateMessage(this);
    }

    /**
     * Initialize the parameters for your Filter.
     */
    public void init(String name) {
        this.name = name;
        matchedURIs = new HashMap<String, Date>();
        matchTimes = new HashMap<String, Date>();
        matchURIs = new HashMap<Pattern, Long>();
        SimpleDateFormat datef = new SimpleDateFormat(radarDatePattern);
        datef.setTimeZone(TimeZone.getTimeZone("Zulu"));
        setDateFormatter(datef);
    }

    /**
     * Gets the name of this filter
     * 
     * @return
     */
    public String getName() {
        return name;
    }

    /**
     * Get your list of URI's you will search for. You may construct this list
     * to use wildcards and such.
     * 
     * @return
     */
    public HashMap<Pattern, Long> getMatchURIs() {
        return matchURIs;
    }

    /**
     * Get the DataTypes that you need for this CPG.
     * 
     * @return
     */
    public String[] getDataTypes() {
        return dataTypes;
    }

    /**
     * Set the DataTypes that you need for this CPG.
     * 
     * @return
     */
    protected void setDataTypes(String[] dataTypes) {
        this.dataTypes = dataTypes;
    }

    /**
     * Get the list of true time dilation
     * 
     * @return
     */
    protected HashMap<String, Date> getMatchTimes() {
        return matchTimes;
    }

    /** set wild carded sections of URI to wild card char */
    protected abstract String applyWildCards(String key);

    /** remove wild carded sections of URI */
    protected abstract String removeWildCards(String key);

    /**
     * extract the time from a particular URI.
     * 
     * @param key
     * @return
     * @throws EdexException
     */
    public static Date getTime(String key, SimpleDateFormat sdf) {

        Calendar cal = Calendar.getInstance();
        String[] comps = key.split(uriSeperator);
        Date date = null;
        String dateString = null;
        // peel off the forecast time
        if (comps[2].endsWith(forecastTimeSepStart)) {
            int index = comps[2].indexOf(forecastTimeSepEnd);
            // ignore it in this, -1 index
            dateString = comps[2].substring(0, index - 1);
        } else {
            dateString = comps[2];
        }
        try {
            date = sdf.parse(dateString);
            cal.setTimeInMillis(date.getTime());
        } catch (ParseException pe) {
            statusHandler.handle(Priority.ERROR, key
                    + " can't parse time: " + pe);
        }

        return date;
    }

    /**
     * extract the forecast time from a particular URI.
     * 
     * @param key
     * @return
     * @throws EdexException
     */
    public Calendar getForecastTime(String key) throws ParseException {

        Calendar cal = Calendar.getInstance();
        String[] comps = key.split(uriSeperator);
        Date date = null;

        String dateString = null;
        String forecastString = null;
        // peel off the forecast time
        if (comps[2].endsWith(forecastTimeSepStart)) {
            int sindex = comps[2].indexOf(forecastTimeSepStart);
            int eindex = comps[2].indexOf(forecastTimeSepEnd);
            // ignore it in this, -1 index
            dateString = comps[2].substring(0, sindex - 1);
            forecastString = comps[2].substring(sindex, eindex);
        } else {
            // no forecast time associated with this URI
            dateString = null;
        }

        date = getDateFormatter().parse(dateString);
        cal.setTimeInMillis(date.getTime());

        int hours = Integer.parseInt(forecastString);
        cal.roll(Calendar.HOUR, hours);

        return cal;
    }

    /**
     * Get the type of plugin data from URI key.
     * 
     * @param key
     * @return
     */
    public String getType(String key) {

        String type = null;
        for (int i = 0; i < getDataTypes().length; i++) {
            if (key.startsWith(uriSeperator + getDataTypes()[i])) {
                type = getDataTypes()[i];
                break;
            }
        }
        return type;
    }

    /**
     * Set the time base used for matching.
     * 
     * @param validTime
     */
    public void setValidTime(Date validTime) {
        this.validTime = validTime;
    }

    /**
     * Get the time base used for matching.
     * 
     * @param validTime
     */
    public Date getValidTime() {
        return validTime;
    }

    /**
     * Set the current time base used for matching.
     * 
     * @param cuurentTime
     */
    public void setCurrentTime(Date currentTime) {
        this.currentTime = currentTime;
    }

    /**
     * Get the current time base used for matching.
     * 
     * @param currentTime
     */
    public Date getCurrentTime() {
        return currentTime;
    }

    /**
     * This is the real deal where you look for matches based on your
     * suppositions. It keeps a list called matchedURIs, when all matchURIs have
     * been satisfied with a matchedURI, this will return true.
     * 
     * @param message
     * @return boolean
     */
    public boolean isMatched(DataURINotificationMessage message) {

        boolean debug = logger.isDebugEnabled();
        setCurrentTime(new Date(System.currentTimeMillis()));
        // process all the product messages
        if ((message != null) && (message.getDataURIs().length > 0)) {
            if (debug) {
                logger.debug(name + ": Filtering Messages..."
                        + message.getDataURIs().length);
            }
            for (String key : message.getDataURIs()) {
                // add your pattern checks to the key
                for (Pattern pattern : getMatchURIs().keySet()) {
                    if (debug) {
                        logger.debug("Pattern: " + pattern.toString()
                                + " Key: " + key);
                    }
                    if (pattern.matcher(key).matches()) {
                        // extract times, used later
                        setValidTime(getTime(key, getDateFormatter()));
                        long duration = 0l;
                        if (debug) {
                            logger.debug(name + ": match Found: " + key);
                        }

                        if (!getMatchedURIs().containsKey(pattern.toString())) {
                            // brand spanking new
                            getMatchedURIs().put(pattern.toString(),
                                    getValidTime());
                            getMatchTimes().put(pattern.toString(),
                                    getCurrentTime());
                            if (debug) {
                                logger.debug(name + ": new product. " + key);
                            }
                        } else {
                            duration = getValidTime().getTime()
                                    - getMatchedURIs().get(pattern.toString())
                                            .getTime();
                            if (debug) {
                                logger.debug(name + ": not new. " + key
                                        + " Age of MatchedURI: " + duration
                                        / (1000 * 60) + " minutes");

                                // got a replacement
                                logger.debug(name
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
                                    logger.debug(name
                                            + ": in range: replaced in matchedURIs. "
                                            + key);
                                }
                            } else if (debug) {
                                logger.debug(name
                                        + ": not in range: discarded. " + key);
                            }
                        }
                    }
                }
            }
            if (debug) {
                logger.debug(name + ": matchedURIs size: "
                        + getMatchedURIs().size());
            }
        }

        // check for repeats and equality of size
        if ((getMatchedURIs().size() == getMatchURIs().size()) && !isRepeat()) {
            // this is your green light condition
            if (debug) {
                logger.debug(name + ": Matched all products! ");
            }
            match = true;

        } else {
            if (getMatchedURIs().size() > 0) {
                ageTimes();
            }
        }

        return match;
    }

    /**
     * Checks to see if you already created a product for this set of URI's.
     * 
     * @return
     */
    protected boolean isRepeat() {
        // we are looking for inequality here
        if (getPreviousURIs() != null) {

            for (int i = 0; i < getPreviousURIs().length; i++) {
                if (getPreviousURIs()[i].equals(getURIs()[i])) {
                    repeat = true;
                    if (logger.isDebugEnabled()) {
                        logger.debug(name + ": Repeat: " + repeat);
                    }
                } else {
                    repeat = false;
                    break;
                }
            }
        }
        // no previous, definitely false
        else {
            repeat = false;
        }

        return repeat;
    }

    /**
     * Ages the matchedURIs times. This covers for message URI's that arrived
     * earlier with other notifications.
     */
    protected void ageTimes() {
        Iterator<String> iter = getMatchedURIs().keySet().iterator();
        ArrayList<String> removeKeys = new ArrayList<String>();
        boolean debug = logger.isDebugEnabled();
        while (iter.hasNext()) {
            String patternKey = iter.next();
            long duration = getCurrentTime().getTime()
                    - getMatchTimes().get(patternKey).getTime();
            if (debug) {
                logger.debug(name + " Age Times: Key: " + patternKey
                        + " duration: " + duration / (1000 * 60) + " minutes");
            }
            // too old, remove from matchedURIs
            for (Pattern pattern : getMatchURIs().keySet()) {
                if (pattern.toString().equals(patternKey)) {
                    if (duration > getMatchURIs().get(pattern).longValue()) {
                        if (getMatchedURIs().containsKey(patternKey)
                                && getMatchTimes().containsKey(patternKey)) {
                            if (debug) {
                                logger.debug(name + ": key too old, "
                                        + duration / (1000 * 60)
                                        + " minutes, removed match: "
                                        + patternKey);
                            }
                            removeKeys.add(patternKey);
                        }
                    }
                }
            }
        }
        // dump old keys
        for (String key : removeKeys) {
            getMatchedURIs().remove(key);
            getMatchTimes().remove(key);
        }
    }

    /**
     * You call this when you want to return the list of URI's you've matched
     * 
     */
    public String[] getURIs() {

        String[] uris = new String[getMatchedURIs().size()];
        Iterator<String> iter = getMatchedURIs().keySet().iterator();
        for (int i = 0; i < getMatchedURIs().size(); i++) {
            uris[i] = removeWildCards(iter.next());
        }

        return uris;
    }

    /**
     * Find a URI that contains this match string
     * 
     * @param match
     * @return
     */
    public String getURI(String match) {

        String uri = null;

        for (int i = 0; i < getURIs().length; i++) {
            String[] comps = getURIs()[i].split(uriSeperator);
            for (int j = 0; j < comps.length; j++) {
                if (comps[j].equals(match)) {
                    uri = getURIs()[i];
                    break;
                }
            }
            if (uri != null) {
                break;
            }
        }
        return uri;
    }

    /**
     * Sets the previous URIs
     * 
     * @param previousURIs
     */
    private void setPreviousURIs(String[] previousURIs) {
        this.previousURIs = previousURIs;
    }

    /**
     * Returns the previous URI string array.
     * 
     * @return
     */
    private String[] getPreviousURIs() {
        return previousURIs;
    }

}
