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

package com.raytheon.edex.plugin.gfe.isc;

import java.lang.Thread.State;
import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;

import com.raytheon.uf.common.dataplugin.gfe.exception.GfeException;

/**
 * Manages interactions for the IRT server used with the GFE ISC capability
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#     Engineer    Description
 * ------------ ----------  ----------- --------------------------
 * 08/10/09     1995       bphillip    Initial creation
 * 
 * </pre>
 * 
 * @author bphillip
 * @version 1
 */
public class IRTManager {

    /** The logger */
    protected transient Log logger = LogFactory.getLog(getClass());

    /** The singleton instance */
    private static IRTManager instance;

    /** Map of active IRT connections keyed by site */
    private Map<String, GfeIRT> irtMap;

    /**
     * Gets the singleton instance of the IRTManager
     * 
     * @return The singleton instance of the IRTManager
     */
    public static synchronized IRTManager getInstance() {
        if (instance == null) {
            instance = new IRTManager();
        }

        return instance;
    }

    /**
     * Constructs the singleton instance of the IRT Manager
     */
    private IRTManager() {
        irtMap = new ConcurrentHashMap<String, GfeIRT>();
    }

    /**
     * Enables ISC functionality for a site
     * 
     * @param siteID
     *            The site to activate ISC functionality for
     * @throws GfeException
     *             If the ISC functionality cannot be activated
     */
    public void enableISC(String siteID, String mhsID) throws GfeException {

        if (!irtMap.containsKey(mhsID + "--" + siteID)) {
            irtMap.put(mhsID + "--" + siteID, new GfeIRT(mhsID, siteID));
        }

        logger.info("Starting IRT registration thread for site [" + siteID
                + "]");
        irtMap.get(mhsID + "--" + siteID).start();
    }

    /**
     * Disables ISC functionality for a site
     * 
     * @param siteID
     *            The site to disable ISC functionality for
     */
    public void disableISC(String mhsID, String siteID) {
        GfeIRT gfeIrt = null;
        String irtKey = mhsID + "--" + siteID;
        gfeIrt = irtMap.remove(irtKey);
        if (gfeIrt != null) {
            if (gfeIrt.getState() != null) {
                while (!gfeIrt.getState().equals(State.TERMINATED)) {
                }
            }
            // Remove the shutdown hook so an unregister is not attempted upon
            // shutdown
            gfeIrt.removeShutdownHook(mhsID, siteID);
        }
    }

    public boolean isRegistered(String mhsID, String siteID) {
        boolean registered = irtMap.containsKey(mhsID + "--" + siteID);
        return registered;
    }
}
