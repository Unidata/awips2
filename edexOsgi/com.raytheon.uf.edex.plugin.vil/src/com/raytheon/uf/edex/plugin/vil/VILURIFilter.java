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
package com.raytheon.uf.edex.plugin.vil;

import java.util.regex.Pattern;

import com.raytheon.edex.urifilter.URIFilter;

/**
 * 
 * Filter class used for filtering URI's to make vil density.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 11/11/2009   2037       dhladky    Initial Creation.
 * Aug 26, 2014 3503       bclement   removed serial version id
 * 
 * </pre>
 * 
 * @author dhladky
 * @version 1.0
 */

public class VILURIFilter extends URIFilter {

    /** an ICAO you may wish to use in matching */
    protected String icao = null;

    public static double tiltAngle = 0.0;

    public static double layer = 0.0;

    /** Echo Top prod ID */
    public static String et = "41";

    /** Enhanced Echo Top */
    public static String eet = "135";

    /** VIL */
    public static String vil = "57";

    /** Digital VIL */
    public static String dvil = "134";

    /** URI pattern for ET */
    public Pattern etURIpattern = null;

    /** URI pattern for EET */
    public Pattern eetURIpattern = null;

    /** URI pattern for VIL */
    public Pattern vilURIpattern = null;

    /** URI pattern for DVIL */
    public Pattern dvilURIpattern = null;

    /**
     * Public vilURI constructor We must pull this from localization eventually
     */
    public VILURIFilter(String name) {

        super(name);
        logger.debug("vilFilter " + name + " Filter construction...");
        setDataTypes(new String[] { "radar" });
        // this will come from the localization bundle
        setIcao(name);
        setExclude(false);
        setMatchURIs();
    }

    @Override
    public void setMatchURIs() {

        setETPattern();
        setEETPattern();
        setVILPattern();
        setDVILPattern();

        long duration = 60 * 1000l * 10; // 10 mins
        // look for / how long before new
        getMatchURIs().put(getETPattern(), duration);
        getMatchURIs().put(getEETPattern(), duration);
        getMatchURIs().put(getVILPattern(), duration);
        getMatchURIs().put(getDVILPattern(), duration);
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

    /**
     * Sets the ET URI Pattern
     * 
     * @return
     */
    public void setETPattern() {
        etURIpattern = Pattern.compile("/radar/" + wildCard + uriSeperator
                + getIcao() + uriSeperator + et + uriSeperator + tiltAngle
                + uriSeperator + layer);
    }

    /**
     * Gets the ET pattern
     * 
     * @return
     */
    public Pattern getETPattern() {
        return etURIpattern;
    }

    /**
     * Sets the ET URI Pattern
     * 
     * @return
     */
    public void setEETPattern() {
        eetURIpattern = Pattern.compile("/radar/" + wildCard + uriSeperator
                + getIcao() + uriSeperator + eet + uriSeperator + tiltAngle
                + uriSeperator + layer);
    }

    /**
     * Gets the EET pattern
     * 
     * @return
     */
    public Pattern getEETPattern() {
        return eetURIpattern;
    }

    /**
     * Sets the VIL URI Pattern
     * 
     * @return
     */
    public void setVILPattern() {
        vilURIpattern = Pattern.compile("/radar/" + wildCard + uriSeperator
                + getIcao() + uriSeperator + vil + uriSeperator + tiltAngle
                + uriSeperator + layer);
    }

    /**
     * Gets the VIL pattern
     * 
     * @return
     */
    public Pattern getVILPattern() {
        return vilURIpattern;
    }

    /**
     * DVIL URI Pattern
     * 
     * @return
     */
    public void setDVILPattern() {
        dvilURIpattern = Pattern.compile("/radar/" + wildCard + uriSeperator
                + getIcao() + uriSeperator + dvil + uriSeperator + tiltAngle
                + uriSeperator + layer);
    }

    /**
     * Gets the DVIL Pattern
     * 
     * @return
     */
    public Pattern getDVILPattern() {
        return dvilURIpattern;
    }

}
