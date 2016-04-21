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
package com.raytheon.uf.viz.collaboration.ui.prefs;

import java.util.HashMap;
import java.util.Map;

import org.eclipse.jface.preference.IPersistentPreferenceStore;
import org.jivesoftware.smack.XMPPException;
import org.jivesoftware.smack.packet.Presence;

import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.viz.collaboration.comm.identity.info.SiteConfigInformation;
import com.raytheon.uf.viz.collaboration.comm.provider.connection.CollaborationConnection;
import com.raytheon.uf.viz.collaboration.comm.provider.user.UserId;
import com.raytheon.uf.viz.collaboration.comm.provider.user.UserSearch;
import com.raytheon.uf.viz.collaboration.ui.Activator;
import com.raytheon.uf.viz.collaboration.ui.prefs.CollabPrefConstants.HandleOption;

/**
 * Utility for default session handles
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Feb  3, 2014 2699       bclement     Initial creation
 * Feb  3, 2014 2699       bclement     fixed assumption that username search was exact
 * 
 * </pre>
 * 
 * @author bclement
 * @version 1.0
 */
public class HandleUtil {

    private static final IUFStatusHandler log = UFStatus
            .getHandler(HandleUtil.class);

    private static final Map<String, String> fullNameMap = new HashMap<String, String>();

    /**
     * @return default session handle from preferences
     */
    public static String getDefaultHandle() {
        IPersistentPreferenceStore prefs = Activator.getDefault()
                .getPreferenceStore();
        String handlePrefStr = prefs
                .getString(CollabPrefConstants.DEFAULT_HANDLE);
        HandleOption handleOp = HandleOption.valueOf(handlePrefStr);

        CollaborationConnection conn = CollaborationConnection.getConnection();

        String rval = null;
        switch (handleOp) {
        case BLANK:
            rval = "";
            break;
        case USERNAME:
            rval = conn.getUser().getName();
            break;
        case FULLNAME:
            rval = getFullName(conn);
            break;
        case ROLE:
            Presence p = conn.getPresence();
            Object site = p.getProperty(SiteConfigInformation.SITE_NAME);
            Object role = p.getProperty(SiteConfigInformation.ROLE_NAME);
            if (site != null && role != null) {
                rval = site + " - " + role;
            } else {
                log.warn("Site and/or role not set in collaboration presence");
            }
            break;
        case CUSTOM:
            rval = prefs.getString(CollabPrefConstants.CUSTOM_HANDLE);
            break;
        }

        if (rval == null) {
            rval = "";
        }

        return rval;
    }

    /**
     * Get full name for account from server. Caches result. Returns null if
     * unable to search server and does not cache null.
     * 
     * @param conn
     * @return
     */
    public static String getFullName(CollaborationConnection conn) {
        String rval;
        UserId account = conn.getUser();
        synchronized (fullNameMap) {
            String fullName = fullNameMap.get(account.getNormalizedId());
            if (fullName == null) {
                String username = account.getName();
                UserSearch search = conn.createSearch();
                UserId result = null;
                try {
                    result = search.byExactUsername(username);
                } catch (XMPPException e) {
                    log.error("Unable to search by username: " + username, e);
                }
                if (result != null) {
                    rval = result.getAlias();
                    fullNameMap.put(account.getNormalizedId(), rval);
                } else {
                    log.warn("Unable to find collaboration account via server search: "
                            + username);
                    rval = null;
                }
            } else {
                rval = fullName;
            }
        }
        return rval;
    }

}
