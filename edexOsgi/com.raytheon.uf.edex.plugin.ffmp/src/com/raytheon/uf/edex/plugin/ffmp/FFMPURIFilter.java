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
package com.raytheon.uf.edex.plugin.ffmp;

import java.io.File;
import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.util.Date;
import java.util.HashMap;
import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;
import java.util.regex.Pattern;

import com.raytheon.edex.site.SiteUtil;
import com.raytheon.edex.urifilter.URIFilter;
import com.raytheon.edex.urifilter.URIGenerateMessage;
import com.raytheon.uf.common.dataplugin.message.DataURINotificationMessage;
import com.raytheon.uf.common.monitor.config.FFMPRunConfigurationManager;
import com.raytheon.uf.common.monitor.config.FFMPSourceConfigurationManager;
import com.raytheon.uf.common.monitor.xml.FFMPRunXML;
import com.raytheon.uf.common.monitor.xml.SourceIngestConfigXML;
import com.raytheon.uf.common.monitor.xml.SourceXML;
import com.raytheon.uf.common.ohd.AppsDefaults;

/**
 * 
 * FFMP Filter class doesn't do much for FFMP
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 06/21/2009   2521       dhladky    Initial Creation.
 * Feb 15, 2013 1638       mschenke    Moved DataURINotificationMessage to uf.common.dataplugin
 * Jul 10, 2014 2914       garmendariz Remove EnvProperties
 * 
 * </pre>
 * 
 * @author dhladky
 * @version 1.0
 */

public class FFMPURIFilter extends URIFilter {

    /**
     * 
     */
    private static final long serialVersionUID = 3389517248482079383L;

    /** an ICAO you may wish to use in matching */
    protected String[] icao = null;

    /** an RFC you may wish to use in matching */
    protected String[] rfc = null;

    /** an CWA you may wish to use in matching */
    protected String cwa = null;

    /** File Dates for XMRG (HPE) sources */
    public ConcurrentHashMap<SourceXML, Date> sourceFileTimes = null;

    /** patterns used for matching URI's **/
    private ConcurrentHashMap<String, Pattern> patternKeys = null;

    /** sources to process */
    public Map<String, String> sources = null;

    /** date differential, basicly 30 seconds */
    public static long differential = 1000 * 60 * 30;

    /** create a date formatter for XMRG **/
    public static SimpleDateFormat xmrgDateFmt = new SimpleDateFormat(
            "yyyyMMddHHmm");

    /** Application defaults **/
    private AppsDefaults appsDefaults = null;

    public FFMPURIFilter(String name) {
        super(name);

        logger.debug("FFMPFilter " + name + " Filter construction...");

        setDataTypes(new String[] { "XMRG, RADAR, GRIB, PDO" });

        sourceFileTimes = new ConcurrentHashMap<SourceXML, Date>();
        appsDefaults = AppsDefaults.getInstance();

        // this will come from the localization bundle
        String[] tokens = name.split(":");

        if (tokens[0].split(",").length > 1) {
            icao = tokens[0].split(",");
        } else {
            icao = new String[] { tokens[0] };
        }

        // extra work needed for rfc manipulation
        if (tokens[1].split(",").length > 1) {
            rfc = tokens[1].split(",");
        } else {
            rfc = new String[] { tokens[1] };
        }
        setRFC(rfc);
        setCWA(tokens[2]);
        setExclude(false);

        // Create the sources map used in the processor
        sources = new HashMap<String, String>();
        patternKeys = new ConcurrentHashMap<String, Pattern>();

        // set the matchers
        setMatchURIs();
    }

    /**
     * Filter By Radar Site
     * 
     * @return
     */
    public String[] getIcao() {
        return icao;
    }

    /**
     * Set the filtering RFCs
     * 
     * @param rfc
     */
    public void setRFC(String[] rfc) {
        this.rfc = rfc;
    }

    /**
     * Filter By RFC
     * 
     * @return
     */
    public String[] getRFC() {
        return rfc;
    }

    /**
     * Set the filtering ICAOs
     * 
     * @param icaos
     */
    public void setIcao(String[] icao) {
        this.icao = icao;
    }

    /**
     * Set the filtering CWA
     * 
     * @param rfc
     */
    public void setCWA(String cwa) {
        this.cwa = cwa;
    }

    /**
     * Filter By CWA
     * 
     * @return
     */
    public String getCWA() {
        return cwa;
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

    /**
     * Replace the wildcard for the siteID, with actual site
     * 
     * @param site
     * @param dataPath
     * @return
     */
    private String replaceWildCard(String site, String dataPath, int pos) {
        String[] comps = dataPath.split(uriSeperator);
        StringBuffer newKey = new StringBuffer();

        for (int i = 1; i < comps.length; i++) {
            if (i == pos) {
                // third comp is site, wildcarded for this
                newKey.append(uriSeperator + site);
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
        FFMPSourceConfigurationManager sourceConfig = FFMPSourceConfigurationManager
                .getInstance();
        FFMPRunConfigurationManager runConfig = FFMPRunConfigurationManager
                .getInstance();
        FFMPRunXML runner = runConfig.getRunner(SiteUtil.getSite());

        for (SourceXML source : sourceConfig.getSources()) {

            SourceIngestConfigXML sicx = runner.getSourceIngest(source
                    .getSourceName());
            // just use an average time, not the expiration
            long duration = 60 * 1000l * 10;
            // setup URI filtering for PDO/RADAR types, multiple RADAR sites
            // possible, don't process gages

            if (!source.getSourceType().equals(
                    FFMPSourceConfigurationManager.SOURCE_TYPE.GAGE
                            .getSourceType())) {

                if (sicx != null && sicx.getDataKey().size() > 0) {

                    for (String dataKey : sicx.getDataKey()) {

                        String matcher = null;
                        // RFC FFG, special matching criteria and override
                        // potentials
                        if (source.isRfc()) {
                            String pathReplace = source.getDataPath(dataKey);
                            if (pathReplace.equals(source.getDataPath())) {
                                matcher = replaceWildCard(
                                        "FFG-" + dataKey.substring(1),
                                        source.getDataPath(),
                                        sicx.getUriSubLocation());
                            } else {
                                matcher = pathReplace;
                            }
                        }
                        // All others use this pattern
                        else {
                            matcher = replaceWildCard(dataKey,
                                    source.getDataPath(dataKey),
                                    sicx.getUriSubLocation());
                        }
                        // take care of time match
                        matcher = replaceWildCard(URIFilter.wildCard, matcher,
                                2);
                        Pattern pattern = Pattern.compile(matcher);
                        patternKeys.put(source.getSourceName() + ":" + dataKey,
                                pattern);
                        getMatchURIs().put(pattern, duration);
                    }
                } else {
                    // XMRG dosen't use the URI Filtering
                    if (source.getDataType().equals(
                            FFMPSourceConfigurationManager.DATA_TYPE.XMRG
                                    .getDataType())) {
                        sourceFileTimes.put(source,
                                getMostRecentXMRGTime(source));
                    } else {
                        // only the time has a match replace
                        String matcher = replaceWildCard(URIFilter.wildCard,
                                source.getDataPath(), 2);
                        Pattern pattern = Pattern.compile(matcher);
                        getMatchURIs().put(pattern, duration);
                    }
                }
            }
        }
    }

    /**
     * Grab the most recent XMRG file time by HPE file type
     * 
     * @param sourceName
     * @return
     */
    private Date getMostRecentXMRGTime(SourceXML xml) {

        String dirPath = getHPEDirectoryPath(xml);
        // System.out.println("Checking path: " + dirPath);
        File dir = new File(dirPath);
        Date rdate = new Date(0l);
        Date currdate = null;
        if (dir.isDirectory()) {
            File[] files = dir.listFiles();
            for (int i = 0; i < files.length; i++) {
                if (files[i].getName().startsWith(xml.getSourceName())) {

                    try {
                        SimpleDateFormat formatter = xmrgDateFmt;
                        int length = 12;

                        if (xml.getDateFormat() != null) {
                            formatter = new SimpleDateFormat(
                                    xml.getDateFormat());
                            length = xml.getDateFormat().length();
                        }

                        String dateString = files[i].getName().substring(
                                (files[i].getName().length() - 1) - length,
                                (files[i].getName().length() - 1));

                        currdate = formatter.parse(dateString);
                        // checks for HPE runs in last 15 mins
                        if (rdate != null) {
                            if (currdate.after(rdate)) {
                                rdate = currdate;
                            }
                        } else {
                            rdate = currdate;
                        }
                    } catch (ParseException e) {
                        e.printStackTrace();
                    }
                }
            }
        }

        return rdate;
    }

    /**
     * Grab the most recent XMRG file time by HPE file type
     * 
     * @param sourceName
     * @return
     */
    private File getMostRecentXMRGFile(SourceXML xml) {

        String dirPath = getHPEDirectoryPath(xml);
        File dir = new File(dirPath);
        Date rdate = null;
        Date currdate = null;
        File file = null;
        if (dir.isDirectory()) {
            File[] files = dir.listFiles();
            for (int i = 0; i < files.length; i++) {
                if (files[i].getName().startsWith(xml.getSourceName())) {

                    try {

                        SimpleDateFormat formatter = xmrgDateFmt;
                        int length = 12;

                        if (xml.getDateFormat() != null) {
                            formatter = new SimpleDateFormat(
                                    xml.getDateFormat());
                            length = xml.getDateFormat().length();
                        }

                        String dateString = files[i].getName().substring(
                                (files[i].getName().length() - 1) - length,
                                (files[i].getName().length() - 1));

                        currdate = formatter.parse(dateString);

                        if (rdate != null) {
                            if (currdate.after(rdate)) {
                                rdate = currdate;
                                file = files[i];
                            }
                        } else {
                            rdate = currdate;
                            file = files[i];
                        }
                    } catch (ParseException e) {
                        e.printStackTrace();
                    }
                }
            }
        }

        return file;
    }

    /**
     * Grab the most recent mod time on the directory
     * 
     * @param sourceName
     * @return
     */
    private Date getLastModified(SourceXML xml) {

        String dirPath = getHPEDirectoryPath(xml);
        File dir = new File(dirPath);

        return new Date(dir.lastModified());
    }

    /**
     * looks into defaults and gets the path for the dir to these files
     * 
     * @param sourceName
     * @return
     */
    private String getHPEDirectoryPath(SourceXML xml) {
        // can only have one entry for XMRG type
        return appsDefaults.getToken(xml.getDataPath());
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

    /**
     * Gets the source hash
     * 
     * @return
     */
    public Map<String, String> getSources() {
        return sources;
    }

    /**
     * overrider
     */
    @Override
    public boolean isMatched(DataURINotificationMessage message) {

        boolean debug = logger.isDebugEnabled();
        setCurrentTime(new Date(System.currentTimeMillis()));

        // process all the product messages
        if ((message != null) && (message.getDataURIs().length > 0)) {
            if (debug) {
                logger.debug(name + ": Filtering Messages..."
                        + message.getDataURIs().length);
            }
            for (String dataUri : message.getDataURIs()) {
                // add your pattern checks to the key
                for (Pattern pattern : getMatchURIs().keySet()) {
                    if (debug) {
                        logger.info("Pattern: " + pattern.toString() + " Key: "
                                + dataUri);
                    }

                    if (pattern.matcher(dataUri).find()) {
                        // find a segmented match of them, which one?
                        String matchKey = getPatternName(pattern);
                        // put the sourceName:dataPath key into the sources
                        // array list
                        sources.put(matchKey, dataUri);
                        match = true;
                        break;
                    }
                }
            }
        }

        // Go over my HPE sources, check differential time for data directories
        if (sourceFileTimes.keySet().size() > 0) {
            for (SourceXML source : sourceFileTimes.keySet()) {
                // cursory check
                if (getLastModified(source).getTime() > (getCurrentTime()
                        .getTime() - (differential))) {
                    // more intensive check
                    Date date = getMostRecentXMRGTime(source);

                    if (date.after(sourceFileTimes.get(source))) {
                        // put the sourceName:dataPath key into the sources
                        // array list
                        sources.put(
                                source.getSourceName()
                                        + ":"
                                        + FFMPSourceConfigurationManager.DATA_TYPE.XMRG
                                                .getDataType(),
                                getMostRecentXMRGFile(source).getAbsolutePath());
                        match = true;
                        // recycle the current source
                        sourceFileTimes.replace(source, date);
                    }
                }
            }
        }

        return match;
    }

    /**
     * overrider
     */
    @Override
    public void reset() {
        match = false;
        sources.clear();
    }

    /**
     * Used by the FFG data pull strategy
     * 
     * @param match
     */
    public void setMatch(boolean match) {
        this.match = match;
    }

    @Override
    public URIGenerateMessage createGenerateMessage() {
        return new FFMPURIGenerateMessage(this);
    }

}
