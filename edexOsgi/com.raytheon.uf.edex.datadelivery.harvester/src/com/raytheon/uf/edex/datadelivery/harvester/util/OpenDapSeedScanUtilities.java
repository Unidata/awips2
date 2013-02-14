package com.raytheon.uf.edex.datadelivery.harvester.util;

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

import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.util.Date;
import java.util.TimeZone;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import com.raytheon.uf.common.datadelivery.registry.Provider.ServiceType;
import com.raytheon.uf.common.datadelivery.retrieval.util.HarvesterServiceManager;
import com.raytheon.uf.common.datadelivery.retrieval.xml.DateConfig;
import com.raytheon.uf.common.datadelivery.retrieval.xml.ServiceConfig;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;

/**
 * OpenDapSeedScanUtilities Sets of methods that are of particular use to
 * OPEnDAP scans
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Oct 27, 2012   1163     dhladky     Initial creation
 * Nov 19, 2012 1166       djohnson    Clean up JAXB representation of registry objects.
 * 
 * </pre>
 * 
 * @author dhladky
 * @version 1.0
 */

public class OpenDapSeedScanUtilities {

    protected static final IUFStatusHandler statusHandler = UFStatus
            .getHandler(OpenDapSeedScanUtilities.class);

    private final static String charcaterStripExpression1 = "[a-zA-Z_-]*(\\d*)\\/(\\d*)";

    private final static String charcaterStripExpression2 = "[a-zA-Z_-]*(\\d*)";

    static final String urlSeparator = "/";

    /** Singleton instance of this class */
    private static final OpenDapSeedScanUtilities instance = new OpenDapSeedScanUtilities();

    /**
     * Breaks the url into chunks so we can analyze
     * 
     * @param url
     * @return
     */
    public static String[] breakupUrl(String url) {
        return url.split(urlSeparator);
    }

    /**
     * Gets the date from the pattern you have extracted
     * 
     * @param sdf
     * @param urlFrag
     * @return
     */
    public static Date getDate(SimpleDateFormat sdf, String urlFrag) {

        String regex = null;
        if (urlFrag.contains("/")) {
            regex = charcaterStripExpression1;
        } else {
            regex = charcaterStripExpression2;
        }

        Pattern pat = Pattern.compile(regex);
        Matcher matcher = pat.matcher(urlFrag);
        boolean matchFound = matcher.find();

        if (matchFound) {
            // Get all groups for this match
            for (int i = 0; i <= matcher.groupCount(); i++) {
                String frag = matcher.group(i);
                Date date;
                try {
                    date = sdf.parse(frag);
                    return date;
                } catch (ParseException e) {
                    if (statusHandler.isPriorityEnabled(Priority.DEBUG)) {
                        statusHandler.debug("Unparseable Date: " + frag);
                    }
                }
            }
        }

        if (statusHandler.isPriorityEnabled(Priority.DEBUG)) {
            statusHandler.warn("No Date Found: " + urlFrag);
        }
        return null;
    }

    /**
     * Check URL frags against possible date formats
     * 
     * @param urlFrag
     * @return
     */
    public static SimpleDateFormat getDateFormat(String urlFrag) {
        // find any alphas followed by any number of chars
        String regex = null;
        if (urlFrag.contains("/")) {
            regex = charcaterStripExpression1;
        } else {
            regex = charcaterStripExpression2;
        }
        SimpleDateFormat sdf = null;
        Pattern pat = Pattern.compile(regex);
        Matcher matcher = pat.matcher(urlFrag);
        boolean matchFound = matcher.find();

        if (matchFound) {
            // Get all groups for this match
            for (int i = 0; i <= matcher.groupCount(); i++) {
                String frag = matcher.group(i);
                sdf = getDatePattern(frag);
                if (sdf != null) {
                    return sdf;
                }
            }
        }
        if (statusHandler.isPriorityEnabled(Priority.DEBUG)) {
            statusHandler.warn("No Date Format Found: " + urlFrag);
        }
        return null;
    }

    /**
     * Try a few formats on for size
     * 
     * @param frag
     * @return
     */
    private static SimpleDateFormat getDatePattern(String frag) {

        SimpleDateFormat sdf = null;

        if (instance.serviceConfig.getDateConfig() != null) {
            DateConfig dc = instance.serviceConfig.getDateConfig();
            if (dc.getFormats() != null) {

                for (String pattern : dc.getFormats()) {
                    try {
                        sdf = new SimpleDateFormat();
                        sdf.setTimeZone(TimeZone.getTimeZone("UTC"));
                        sdf.applyLocalizedPattern(pattern);
                        sdf.parse(frag);
                        return sdf;

                    } catch (ParseException pe) {
                        if (statusHandler.isPriorityEnabled(Priority.DEBUG)) {
                            statusHandler.warn("Unparseable DateFormat: "
                                    + pattern + " url:" + frag);
                        }
                    }
                }
            }
        }

        return null;
    }

    /**
     * reassemble resultant url at depth
     * 
     * @param depth
     * @param url
     * @return
     */
    public static String getUrlAtDepth(int depth, String url) {
        String[] chunks = breakupUrl(url);
        StringBuilder sb = new StringBuilder();
        for (int i = 0; i <= depth; i++) {
            sb.append(chunks[i]);
            if (i <= depth - 1) {
                sb.append(urlSeparator);
            }
        }

        return sb.toString();
    }

    /**
     * Give me a complete url up to the depth I want
     * 
     * @param chunks
     * @param depth
     * @return
     */
    public static String reconstructUrl(String[] chunks, int depth) {
        StringBuilder sb = new StringBuilder();
        for (int i = 0; i < depth; i++) {
            sb.append(chunks[i]);
            if (i < depth - 1) {
                sb.append(urlSeparator);
            }
        }

        return sb.toString();
    }

    /*
     * Service configuration for OPENDAP
     */
    private final ServiceConfig serviceConfig;

    /* Private Constructor */
    private OpenDapSeedScanUtilities() {
        serviceConfig = HarvesterServiceManager.getInstance().getServiceConfig(
                ServiceType.OPENDAP);
    }

}
