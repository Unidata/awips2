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
package com.raytheon.uf.viz.core;

import java.util.Collections;
import java.util.Map;

/**
 * Provides the ability to retrieve server locations.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Nov 5, 2012  1302      djohnson     Initial creation
 * 
 * </pre>
 * 
 * @author djohnson
 * @version 1.0
 */

public class VizServers {

    private static final VizServers INSTANCE = new VizServers();

    /**
     * @return the instance
     */
    public static VizServers getInstance() {
        return INSTANCE;
    }

    private Map<String, String> serverLocations;

    private VizServers() {
    }

    /**
     * Retrieves a server location.
     * 
     * @param key
     *            the key used as the server name
     * @return the location of the server
     * @throws NullPointerException
     *             if the server locations have not been initialized yet
     */
    public String getServerLocation(final String key) {
        if (serverLocations == null) {
            throw new NullPointerException("serverLocations must not be null!");
        }
        return serverLocations.get(key);
    }

    /**
     * @param serverLocations2
     */
    public void setServerLocations(Map<String, String> serverLocations2) {
        this.serverLocations = Collections.unmodifiableMap(serverLocations2);
    }
}
