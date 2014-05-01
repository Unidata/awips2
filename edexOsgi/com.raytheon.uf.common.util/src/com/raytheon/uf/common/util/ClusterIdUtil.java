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
package com.raytheon.uf.common.util;

import java.net.InetAddress;
import java.net.UnknownHostException;


/**
 * Utility class that returns the Cluster Id name from the environment or parsed
 * from the host name.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Feb 13, 2014 2771       bgonzale     Initial creation
 * 
 * </pre>
 * 
 * @author bgonzale
 * @version 1.0
 */

public class ClusterIdUtil {

    private static final String ENV_VAR_NAME = "CLUSTER_ID";

    /**
     * Return the data delivery for this instance of Data Delivery.
     * 
     * @return Data Delivery identification.
     */
    public static String getId() {
        String id = System.getenv(ENV_VAR_NAME);

        if (id == null || id.trim().length() == 0) {
            try {
                id = parseClusterNameFromHostname(InetAddress.getLocalHost()
                        .getHostName());
            } catch (UnknownHostException e) {
                // no data delivery id set, and unable to parse from host.
                throw new RuntimeException("No " + ENV_VAR_NAME
                        + " set in the environment and unable "
                        + "to determine the name from hostname.", e);
            }
        }
        return id;
    }

    static String parseClusterNameFromHostname(String hostname) {
        /*
         * Host names in the format of blah.company.com or dx1-blah.company.com
         * or dx1-blah where the blah is the name desired.
         */
        int firstDotIndex = hostname.indexOf('.');
        int endIndex = firstDotIndex == -1 ? hostname.length() : firstDotIndex;
        int firstHyphenIndex = hostname.indexOf('-');
        int startIndex = firstHyphenIndex == -1 ? 0 : firstHyphenIndex + 1;
        // catch cases where a hyphen may be in the company.com part of the
        // address and not in the blah part
        startIndex = startIndex > endIndex ? 0 : startIndex;
        return hostname.substring(startIndex, endIndex);
    }

}
