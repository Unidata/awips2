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
package com.raytheon.uf.edex.plugin.preciprate;

import java.util.regex.Pattern;

import com.raytheon.edex.urifilter.URIFilter;
import com.raytheon.edex.urifilter.URIGenerateMessage;

public class PrecipRateURIFilter extends URIFilter {

    /**
     * 
     */
    private static final long serialVersionUID = 1346354654L;

    public PrecipRateURIFilter(String name) {
        super(name);
        logger.debug("PrecipRateFilter " + name + " Filter construction...");
        setDataTypes(new String[] { "radar" });
        setIcao(name);
        setExclude(false);
        setMatchURIs();
    }

    /** an ICAO you may wish to use in matching */
    protected String icao = null;

    public static double tiltAngle = 0.0;

    public static double layer = 0.0;

    /** Digital Hybrid Reflectivity prod ID */
    public static String dhr = "32";

    /** URI pattern for DHR radar */
    public Pattern dhrURIpattern = null;

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
        // Duration dosen't really matter as we are just keying on DHR
        setDHRPattern();
        long duration = 60 * 1000l * 0; // 5 mins
        getMatchURIs().put(getDHRPattern(), duration);
    }

    /**
     * Sets the DHR URI Pattern
     * 
     * @return
     */
    public void setDHRPattern() {
        dhrURIpattern = Pattern.compile("/radar/" + wildCard + uriSeperator
                + getIcao() + uriSeperator + dhr + uriSeperator + tiltAngle
                + uriSeperator + layer);
    }

    @Override
    public URIGenerateMessage createGenerateMessage() {
        return new PrecipRateURIGenerateMessage(this);
    }

    /**
     * Gets the DHR URI pattern
     * 
     * @return
     */
    public Pattern getDHRPattern() {
        return dhrURIpattern;
    }
}
