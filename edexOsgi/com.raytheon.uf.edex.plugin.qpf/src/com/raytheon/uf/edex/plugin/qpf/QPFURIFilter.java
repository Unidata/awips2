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
package com.raytheon.uf.edex.plugin.qpf;

import java.util.regex.Pattern;

import com.raytheon.edex.urifilter.URIFilter;
import com.raytheon.edex.urifilter.URIGenerateMessage;

/**
 * 
 * QPF Filter class used for filtering URI's used to make QPF.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 02/07/2009   1981       dhladky    Initial Creation.
 * 
 * </pre>
 * 
 * @author dhladky
 * @version 1.0
 */

public class QPFURIFilter extends URIFilter {

    /**
     * 
     */
    private static final long serialVersionUID = -6454499264120605587L;

    /** an ICAO you may wish to use in matching */
    protected String icao = null;

    public static double tiltAngle = 0.0;

    public static double layer = 0.0;

    /** composite Reflectivity prod ID */
    public static String cz = "37";

    /** Vertically Integrated Liquid prod ID */
    public static String vil = "57";

    /** STI prod ID */
    public static String sti = "58";

    /** uWinds */
    public static String uwind = "uW";

    /** vWinds */
    public static String vwind = "vW";

    /** URI pattern for VIL */
    public Pattern vilURIpattern = null;

    /** URI pattern for Composite Reflectivity (CZ) */
    public Pattern czURIpattern = null;

    /** URI pattern for STI */
    public Pattern stiURIpattern = null;

    /** 12 minutes between CZ and STI data */
    public static final long stiInterval = 1000 * 60 * 12;

    /**
     * Public QPFURI constructor We must pull this from localization eventually
     */
    public QPFURIFilter(String name) {

        super(name);
        logger.debug("QPFFilter " + name + " Filter construction...");
        setDataTypes(new String[] { "radar" });
        // this will come from the localization bundle
        setIcao(name);
        setExclude(false);
        setMatchURIs();
    }

    @Override
    public void setMatchURIs() {

        setVILPattern();
        setCZPattern();
        setSTIPattern();

        long duration = 60 * 1000l * 5; // 5 mins
        // look for / how long before new
        getMatchURIs().put(getVILPattern(), duration);
        getMatchURIs().put(getCZPattern(), duration);
        getMatchURIs().put(getSTIPattern(), duration);
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
     * Sets the Vil URI Pattern
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
     * Sets the Composite Reflectivity URI Pattern
     * 
     * @return
     */
    public void setCZPattern() {
        czURIpattern = Pattern.compile("/radar/" + wildCard + uriSeperator
                + getIcao() + uriSeperator + cz + uriSeperator + tiltAngle
                + uriSeperator + layer);
    }

    /**
     * Gets the CZ pattern
     * 
     * @return
     */
    public Pattern getCZPattern() {
        return czURIpattern;
    }

    /**
     * STI URI Pattern
     * 
     * @return
     */
    public void setSTIPattern() {
        stiURIpattern = Pattern.compile("/radar/" + wildCard + uriSeperator
                + getIcao() + uriSeperator + sti + uriSeperator + tiltAngle
                + uriSeperator + layer);
    }

    /**
     * Gets the STI Pattern
     * 
     * @return
     */
    public Pattern getSTIPattern() {
        return stiURIpattern;
    }

    @Override
    public URIGenerateMessage createGenerateMessage() {
        return new QPFURIGenerateMessage(this);
    }
}
