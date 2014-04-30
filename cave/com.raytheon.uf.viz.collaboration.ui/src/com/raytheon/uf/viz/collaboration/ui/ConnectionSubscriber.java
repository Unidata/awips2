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
package com.raytheon.uf.viz.collaboration.ui;

import java.util.HashSet;
import java.util.Set;

import org.eclipse.core.runtime.jobs.Job;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.ui.IWorkbench;
import org.eclipse.ui.IWorkbenchListener;
import org.eclipse.ui.IWorkbenchPage;
import org.eclipse.ui.PartInitException;
import org.eclipse.ui.PlatformUI;

import com.google.common.eventbus.Subscribe;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.viz.collaboration.comm.identity.CollaborationException;
import com.raytheon.uf.viz.collaboration.comm.identity.ISession;
import com.raytheon.uf.viz.collaboration.comm.identity.IVenueSession;
import com.raytheon.uf.viz.collaboration.comm.identity.event.IHttpdCollaborationConfigurationEvent;
import com.raytheon.uf.viz.collaboration.comm.identity.event.ITextMessageEvent;
import com.raytheon.uf.viz.collaboration.comm.identity.event.IVenueInvitationEvent;
import com.raytheon.uf.viz.collaboration.comm.identity.user.IUser;
import com.raytheon.uf.viz.collaboration.comm.provider.TextMessage;
import com.raytheon.uf.viz.collaboration.comm.provider.connection.CollaborationConnection;
import com.raytheon.uf.viz.collaboration.comm.provider.user.UserId;
import com.raytheon.uf.viz.collaboration.ui.actions.PeerToPeerChatAction;
import com.raytheon.uf.viz.collaboration.ui.jobs.AwayTimeOut;
import com.raytheon.uf.viz.collaboration.ui.prefs.CollabPrefConstants;
import com.raytheon.uf.viz.collaboration.ui.prefs.SubscriptionResponderImpl;
import com.raytheon.uf.viz.collaboration.ui.session.CollaborationSessionView;
import com.raytheon.uf.viz.collaboration.ui.session.PeerToPeerView;
import com.raytheon.uf.viz.collaboration.ui.session.SessionView;
import com.raytheon.uf.viz.core.VizApp;
import com.raytheon.viz.ui.views.CaveWorkbenchPageManager;

/**
 * Subscribes to events that occur on the collaboration connection, ie the
 * logged in user. This is for events not tied to a particular session.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jun 8, 2012            njensen     Initial creation
 * Dec 18, 2013 2562      bclement    fixed venue invite
 * Jan 14, 2014 2630      bclement    added away timeout
 * Jan 27, 2014 2700      bclement    added auto subscribe property listener
 * Jan 30, 2014 2698      bclement    moved xmpp join logic to dialog so we can reprompt user on failure
 * Feb 13, 2014 2751      bclement    messages return IUser instead of IQualifiedID
 * Mar 06, 2014 2848      bclement    moved SharedDisplaySessionMgr.joinSession call to InviteDialog
 * Apr 08, 2014 2785      mpduff      removed preference listener
 * Apr 11, 2014 2903      bclement    added disconnect handler
 * Apr 24, 2014 2955      bclement    ignore duplicate session invites
 * 
 * </pre>
 * 
 * @author njensen
 * @version 1.0
 */

public class ConnectionSubscriber {

    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(ConnectionSubscriber.class);

    private static ConnectionSubscriber instance;

    private IWorkbenchListener wbListener;

    private final AwayTimeOut awayTimeOut = new AwayTimeOut();

    private final DisconnectHandler disconnect = new DisconnectHandler();

    private final Set<String> pendingInviteDialogs = new HashSet<String>();

    private ConnectionSubscriber() {
    }

    /**
     * Subscribes to events on the given connection. Should only be called on
     * login.
     * 
     * @param connection
     */
    public static void subscribe(CollaborationConnection connection) {
        if (instance == null) {
            instance = new ConnectionSubscriber();
            instance.setup(connection);
        }
    }

    /**
     * Unsubscribes from events on the given connection. Should only be called
     * on logout.
     * 
     * @param connection
     */
    public static void unsubscribe(CollaborationConnection connection) {
        if (instance != null) {
            instance.dispose(connection);
        }

        instance = null;
    }

    private void setup(final CollaborationConnection connection) {
        if (connection != null) {
            connection.getAccountManager().setSubscriptionRequestResponder(
                    new SubscriptionResponderImpl(connection));
            // Register handlers and events for the new sessionManager.
            connection.registerEventHandler(this);
            connection.registerEventHandler(disconnect);
            try {
                ISession p2pSession = connection.getPeerToPeerSession();
                p2pSession.registerEventHandler(this);
            } catch (CollaborationException e) {
                statusHandler.handle(Priority.PROBLEM,
                        "Error registering peer to peer handler", e);
            }
            // TODO the wblistener should perhaps be elsewhere
            wbListener = new IWorkbenchListener() {

                @Override
                public boolean preShutdown(IWorkbench workbench, boolean forced) {
                    return true;
                }

                @Override
                public void postShutdown(IWorkbench workbench) {
                    dispose(connection);
                    if (connection != null) {
                        connection.close();
                    }
                }
            };
            PlatformUI.getWorkbench().addWorkbenchListener(wbListener);
            awayTimeOut.setSystem(true);
            awayTimeOut.setPriority(Job.LONG);
            awayTimeOut.schedule();
        }
    }

    private void dispose(CollaborationConnection connection) {
        if (connection != null) {
            awayTimeOut.cancel();
            try {
                ISession p2pSession = connection.getPeerToPeerSession();
                if (p2pSession != null) {
                    p2pSession.unregisterEventHandler(this);
                }
            } catch (CollaborationException e) {
                statusHandler.handle(Priority.PROBLEM,
                        "Error unregistering peer to peer handler", e);
            }
            connection.unregisterEventHandler(disconnect);
            connection.unregisterEventHandler(this);
        }
        PlatformUI.getWorkbench().removeWorkbenchListener(wbListener);
    }

    @Subscribe
    public void handleInvitationEvent(final IVenueInvitationEvent event) {
        final String roomId = event.getRoomId().getFQName();
        
        VizApp.runAsync(new Runnable() {

            @Override
            public void run() {
                if (!invitePending(roomId)) {
                    try {
                        Shell shell = new Shell(Display.getCurrent());
                        InviteDialog inviteBox = new InviteDialog(shell, event);
                        if ((Boolean) inviteBox.open()) {
                            /* user accepted invite */
                            openSession(inviteBox);
                        }
                    } finally {
                        synchronized (pendingInviteDialogs) {
                            pendingInviteDialogs.remove(roomId);
                        }
                    }
                } else {
                    statusHandler.debug("Ignoring duplicate session invite: "
                            + roomId);
                }
            }

            /**
             * @param roomId
             * @return true if there is already an invitation pending for this
             *         room
             */
            private boolean invitePending(String roomId) {
                synchronized (pendingInviteDialogs) {
                    boolean pending = pendingInviteDialogs.contains(roomId);
                    if (!pending) {
                        /* immediately set to pending to ignore dup invites */
                        pendingInviteDialogs.add(roomId);
                    }
                    return pending;
                }
            }

            /**
             * Open session view after invite has been accepted
             * 
             * @param inviteBox
             */
            private void openSession(InviteDialog inviteBox) {
                try {
                    IVenueSession session = inviteBox.getSession();
                    if (inviteBox.isSharedDisplay()) {
                        CaveWorkbenchPageManager.getActiveInstance().showView(
                                CollaborationSessionView.ID,
                                session.getSessionId(),
                                IWorkbenchPage.VIEW_ACTIVATE);
                    } else {
                        CaveWorkbenchPageManager.getActiveInstance().showView(
                                SessionView.ID, session.getSessionId(),
                                IWorkbenchPage.VIEW_ACTIVATE);
                    }
                } catch (PartInitException e) {
                    statusHandler.error("Unable to display session view", e);
                }
            }
        });
    }

    /**
     * This takes a peer to peer message and displays it in the proper view.
     * 
     * @param messageEvent
     */
    @Subscribe
    public void peer2peerMessage(ITextMessageEvent messageEvent) {
        final TextMessage message = messageEvent.getMessage();
        VizApp.runAsync(new Runnable() {

            @Override
            public void run() {
                IUser peer = message.getFrom();

                UserId user = null;
                if (peer instanceof UserId) {
                    user = (UserId) peer;
                } else {
                    user = CollaborationConnection.getConnection()
                            .getContactsManager().getUser(peer.getFQName());
                }
                PeerToPeerView view = new PeerToPeerChatAction(user)
                        .createP2PChat(IWorkbenchPage.VIEW_CREATE);
                message.setFrom(view.getPeer());
                if (view != null) {
                    view.appendMessage(message);
                }
            }
        });
    }

    @Subscribe
    public void handleHttpdConfigurationEvent(
            IHttpdCollaborationConfigurationEvent configurationEvent) {

        boolean wasPreviouslyConfigured = Activator
                .getDefault()
                .getPreferenceStore()
                .getBoolean(
                        CollabPrefConstants.HttpCollaborationConfiguration.P_SESSION_CONFIGURED);
        boolean configured = false;
        if (configurationEvent.getHttpdCollaborationURL() != null) {
            // Add the httpd collaboration url to the CAVE configuration.
            Activator
                    .getDefault()
                    .getPreferenceStore()
                    .setValue(
                            CollabPrefConstants.HttpCollaborationConfiguration.P_HTTP_SESSION_URL,
                            configurationEvent.getHttpdCollaborationURL());
            configured = true;
            if (wasPreviouslyConfigured == false) {
                statusHandler
                        .handle(Priority.INFO,
                                "Collaboration: Shared Display Sessions have been enabled.");
            }
        }
        Activator
                .getDefault()
                .getPreferenceStore()
                .setValue(
                        CollabPrefConstants.HttpCollaborationConfiguration.P_SESSION_CONFIGURED,
                        configured);
    }

}
