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

import java.util.Collections;
import java.util.Set;
import java.util.concurrent.ConcurrentHashMap;

import org.eclipse.jface.preference.IPersistentPreferenceStore;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Shell;
import org.jivesoftware.smack.XMPPException;

import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.viz.collaboration.comm.identity.roster.ISubscriptionResponder;
import com.raytheon.uf.viz.collaboration.comm.identity.roster.SubscriptionResponse;
import com.raytheon.uf.viz.collaboration.comm.provider.account.ISubscriptionRequestCompleteAction;
import com.raytheon.uf.viz.collaboration.comm.provider.connection.CollaborationConnection;
import com.raytheon.uf.viz.collaboration.comm.provider.user.ContactsManager;
import com.raytheon.uf.viz.collaboration.comm.provider.user.UserId;
import com.raytheon.uf.viz.collaboration.comm.provider.user.UserSearch;
import com.raytheon.uf.viz.collaboration.ui.Activator;
import com.raytheon.uf.viz.collaboration.ui.SubRequestDialog;
import com.raytheon.uf.viz.core.VizApp;

/**
 * The subscription responder impelementation.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Apr 03, 2014     2785   mpduff      Initial creation
 * Apr 24, 2014     3070   bclement    added default group for auto accept
 *                                      fixed auto accept known contacts
 * 
 * </pre>
 * 
 * @author mpduff
 * @version 1.0
 */

public class SubscriptionResponderImpl implements ISubscriptionResponder {
    /**
     * Log instance
     */
    private final IUFStatusHandler log = UFStatus.getHandler(this.getClass());

    /**
     * Map tracking user contact requests
     */
    private final Set<String> subRequest = Collections
            .newSetFromMap(new ConcurrentHashMap<String, Boolean>());

    /**
     * Search instance
     */
    private final UserSearch search;

    public SubscriptionResponderImpl(CollaborationConnection connection) {
        this.search = connection.createSearch();
    }

    @Override
    public void handleSubscribeRequest(final UserId fromID,
            final ISubscriptionRequestCompleteAction action) {
        final SubscriptionResponse rval = new SubscriptionResponse();
        IPersistentPreferenceStore prefs = Activator.getDefault()
                .getPreferenceStore();
        if (prefs.getBoolean(CollabPrefConstants.AUTO_ACCEPT_SUBSCRIBE)) {
            rval.setAccepted(true);
            rval.setGroup(prefs
                    .getString(CollabPrefConstants.DEFAULT_GROUPNAME_PREF));
            action.executeSubscriptionRequestComplete(fromID, rval);
            return;
        }

        CollaborationConnection conn = CollaborationConnection.getConnection();
        ContactsManager cm = conn.getContactsManager();
        if (cm.isContact(fromID)) {
            /* we already have a subscription to this user */
            rval.setAccepted(true);
            action.executeSubscriptionRequestComplete(fromID, rval);
        } else {
            Boolean dlgExists = subRequest.contains(fromID.getFQName());

            if (!dlgExists) {
                subRequest.add(fromID.getFQName());
                VizApp.runAsync(new Runnable() {
                    @Override
                    public void run() {
                        String displayName = getDisplayName(fromID);
                        Shell shell = new Shell(Display.getCurrent());

                        SubRequestDialog dlg = new SubRequestDialog(shell,
                                displayName);
                        Object returnVal = dlg.open();
                        if (returnVal == null) {
                            // Denied
                            rval.setAccepted(false);
                            rval.setGroup(null);
                        } else {
                            if (returnVal instanceof String) {
                                rval.setAccepted(true);
                                rval.setGroup((String) returnVal);
                            }
                        }
                        subRequest.remove(fromID.getFQName());
                        action.executeSubscriptionRequestComplete(fromID, rval);
                    }
                });
            }
        }
    }

    /**
     * Get display name for user from server
     * 
     * @param fromID
     * @return null if none found
     */
    private String getDisplayName(UserId fromID) {
        String rval = null;
        try {
            UserId user = search.byExactUsername(fromID.getName());
            return user != null ? user.getAlias() : null;
        } catch (XMPPException e) {
            log.error("Unable to get display name for user: " + fromID, e);
        }
        return rval;
    }
}
