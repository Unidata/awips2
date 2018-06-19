/**
 * This software was developed and / or modified by NOAA/NWS/OCP/ASDT
 **/
package com.raytheon.uf.common.mpe.util;

import java.util.HashMap;
import java.util.Map;

/**
 * Class for RFC Site name to RFC lookup and vice versa.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * APR 28 2017  17911      wkwock      Initial Creation.
 * 
 * </pre>
 * 
 * @author wkwock
 * @version 1.0
 */

public class RFCSiteLookup {
    /** RFC Site name to RFC lookup map */
    public static Map<String, String> RFCMAP = new HashMap<String, String>();

    /** RFC to RFC Site name lookup map */
    public static Map<String, String> RFC_SITEMAP = new HashMap<String, String>();

    static {
        RFCMAP.put("TUA", "ABRFC");
        RFCMAP.put("ACR", "AKRFC");
        RFCMAP.put("STR", "CBRFC");
        RFCMAP.put("RSA", "CNRFC");
        RFCMAP.put("ORN", "LMRFC");
        RFCMAP.put("RHA", "MARFC");
        RFCMAP.put("KRF", "MBRFC");
        RFCMAP.put("MSR", "NCRFC");
        RFCMAP.put("TAR", "NERFC");
        RFCMAP.put("PTR", "NWRFC");
        RFCMAP.put("TIR", "OHRFC");
        RFCMAP.put("ALR", "SERFC");
        RFCMAP.put("FWR", "WGRFC");

        RFC_SITEMAP.put("ABRFC", "TUA");
        RFC_SITEMAP.put("AKRFC", "ACR");
        RFC_SITEMAP.put("CBRFC", "STR");
        RFC_SITEMAP.put("CNRFC", "RSA");
        RFC_SITEMAP.put("LMRFC", "ORN");
        RFC_SITEMAP.put("MARFC", "RHA");
        RFC_SITEMAP.put("MBRFC", "KRF");
        RFC_SITEMAP.put("NCRFC", "MSR");
        RFC_SITEMAP.put("NERFC", "TAR");
        RFC_SITEMAP.put("NWRFC", "PTR");
        RFC_SITEMAP.put("OHRFC", "TIR");
        RFC_SITEMAP.put("SERFC", "ALR");
        RFC_SITEMAP.put("WGRFC", "FWR");

    }

    /**
     * RFC to RFC Site name lookup.
     * 
     * @param site
     *            The RFC or Site Identifier
     * @return The RFC Name or the Site Identifier, or null if nothing found
     */
    public static String rfcSiteLookup(String site) {
        if (RFCSiteLookup.RFCMAP.containsKey(site)) {
            return RFCSiteLookup.RFCMAP.get(site);
        }

        if (RFCSiteLookup.RFC_SITEMAP.containsKey(site)) {
            return RFCSiteLookup.RFC_SITEMAP.get(site);
        }

        return null;
    }

}
