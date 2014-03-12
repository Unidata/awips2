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

import org.eclipse.jface.preference.IPersistentPreferenceStore;
import org.eclipse.jface.util.IPropertyChangeListener;
import org.eclipse.jface.util.PropertyChangeEvent;
import org.eclipse.jface.window.Window;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Shell;
import org.jivesoftware.smack.XMPPException;

import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.viz.collaboration.comm.identity.IAccountManager;
import com.raytheon.uf.viz.collaboration.comm.identity.roster.ISubscriptionResponder;
import com.raytheon.uf.viz.collaboration.comm.identity.roster.SubscriptionResponse;
import com.raytheon.uf.viz.collaboration.comm.provider.session.CollaborationConnection;
import com.raytheon.uf.viz.collaboration.comm.provider.user.UserId;
import com.raytheon.uf.viz.collaboration.comm.provider.user.UserSearch;
import com.raytheon.uf.viz.collaboration.ui.Activator;
import com.raytheon.uf.viz.collaboration.ui.SubRequestDialog;
import com.raytheon.uf.viz.core.VizApp;

/**
 * Listens to collaboration preferences and alters the subscription request
 * handler as appropriate.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jan 24, 2014 2700       bclement     Initial creation
 * Feb  3, 2014 2699       bclement     fixed assumption that username search was exact
 * Feb 13, 2014 2755       bclement     added user input for which group to add contact to
 * Mar 12, 2014 2632       mpduff       Property change to handle String and Boolean
 * 
 * </pre>
 * 
 * @author bclement
 * @version 1.0
 */
public class AutoSubscribePropertyListener implements IPropertyChangeListener {

    private static final IUFStatusHandler log = UFStatus
            .getHandler(AutoSubscribePropertyListener.class);

    private static AutoSubscribePropertyListener instance;

    private static final Object INSTANCE_MUTEX = new Object();

    public static AutoSubscribePropertyListener getInstance() {
        synchronized (INSTANCE_MUTEX) {
            if (instance == null) {
                instance = new AutoSubscribePropertyListener();
            }
        }
        return instance;
    }

    private CollaborationConnection connection;

    private IAccountManager accountManager;

    private AutoSubscribePropertyListener() {
    }

    /**
     * Initialize auto subscribe state after login
     * 
     * @param connection
     */
    public synchronized void initialize(CollaborationConnection connection) {
        this.connection = connection;
        this.accountManager = connection.getAccountManager();
        updateManager(isAutoInPrefs());
    }

    /**
     * Clean up auto subscribe state after logout
     */
    public synchronized void close() {
        this.accountManager = null;
        this.connection = null;
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * org.eclipse.jface.util.IPropertyChangeListener#propertyChange(org.eclipse
     * .jface.util.PropertyChangeEvent)
     */
    @Override
    public void propertyChange(PropertyChangeEvent event) {
        if (event.getProperty().equals(
                CollabPrefConstants.AUTO_ACCEPT_SUBSCRIBE)
                && accountManager != null) {

            // The HierarchicalPreferenceStore store sometimes returns a string
            Object valueObject = event.getNewValue();

            if (valueObject instanceof Boolean) {
                updateManager((Boolean) valueObject);
            } else {
                updateManager(Boolean.valueOf(valueObject.toString()));
            }
        }
    }

    /**
     * @return true if preferences have auto accept enabled
     */
    private boolean isAutoInPrefs() {
        IPersistentPreferenceStore prefs = Activator.getDefault()
                .getPreferenceStore();
        return prefs.getBoolean(CollabPrefConstants.AUTO_ACCEPT_SUBSCRIBE);
    }

    /**
     * Update auto subscribe state in account manager
     * 
     * @param auto
     */
    private void updateManager(boolean auto) {
        // manager auto accepts by default if responder is not set
        boolean accountManagerAutoAccepts = !accountManager
                .isSubscriptionRequestResponderSet();
        // update if the settings don't match
        if (auto != accountManagerAutoAccepts) {
            if (auto) {
                accountManager.removeSubscriptionRequestResponder();
            } else {
                accountManager.setSubscriptionRequestResponder(newResponder());
            }
        }
    }

    /**
     * Create responder to handle subscription requests
     * 
     * @return
     */
    private ISubscriptionResponder newResponder() {
        return new ISubscriptionResponder() {

            private final UserSearch search = connection.createSearch();

            @Override
            public void handleUnsubscribed(UserId fromID) {
            }

            @Override
            public void handleSubscribed(UserId fromID) {
            }

            @Override
            public SubscriptionResponse handleSubscribeRequest(
                    final UserId fromID) {
                String displayName = getDisplayName(fromID);
                StringBuilder builder = new StringBuilder();
                builder.append(fromID.getFQName());
                if (displayName != null) {
                    builder.append(" (").append(displayName).append(")");
                }
                builder.append(" wants to add you to his or her contacts list.");
                final String msg = builder.toString();
                final SubscriptionResponse rval = new SubscriptionResponse();
                VizApp.runSync(new Runnable() {

                    @Override
                    public void run() {
                        Shell shell = new Shell(Display.getCurrent());
                        SubRequestDialog dlg = new SubRequestDialog(shell,
                                "Authorize Collaboration Contact", msg, fromID);
                        int index = dlg.open();

                        rval.setAccepted(index == Window.OK);
                        rval.setGroup(dlg.getGroup());
                    }
                });

                return rval;
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
                    log.error("Unable to get display name for user: " + fromID,
                            e);
                }
                return rval;
            }
        };
    }

}
