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

import org.eclipse.swt.SWT;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.MessageBox;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.ui.IViewReference;
import org.eclipse.ui.IWorkbench;
import org.eclipse.ui.IWorkbenchListener;
import org.eclipse.ui.IWorkbenchPage;
import org.eclipse.ui.IWorkbenchPart;
import org.eclipse.ui.PartInitException;
import org.eclipse.ui.PlatformUI;

import com.google.common.eventbus.Subscribe;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.viz.collaboration.comm.identity.CollaborationException;
import com.raytheon.uf.viz.collaboration.comm.identity.ISession;
import com.raytheon.uf.viz.collaboration.comm.identity.ISharedDisplaySession;
import com.raytheon.uf.viz.collaboration.comm.identity.IVenueSession;
import com.raytheon.uf.viz.collaboration.comm.identity.event.IHttpdCollaborationConfigurationEvent;
import com.raytheon.uf.viz.collaboration.comm.identity.event.ITextMessageEvent;
import com.raytheon.uf.viz.collaboration.comm.identity.event.IVenueInvitationEvent;
import com.raytheon.uf.viz.collaboration.comm.identity.invite.SharedDisplayVenueInvite;
import com.raytheon.uf.viz.collaboration.comm.identity.user.IQualifiedID;
import com.raytheon.uf.viz.collaboration.comm.identity.user.SharedDisplayRole;
import com.raytheon.uf.viz.collaboration.comm.provider.TextMessage;
import com.raytheon.uf.viz.collaboration.comm.provider.session.CollaborationConnection;
import com.raytheon.uf.viz.collaboration.comm.provider.user.UserId;
import com.raytheon.uf.viz.collaboration.ui.data.CollaborationDataManager;
import com.raytheon.uf.viz.collaboration.ui.data.SharedDisplaySessionMgr;
import com.raytheon.uf.viz.collaboration.ui.prefs.CollabPrefConstants;
import com.raytheon.uf.viz.collaboration.ui.session.CollaborationSessionView;
import com.raytheon.uf.viz.collaboration.ui.session.PeerToPeerView;
import com.raytheon.uf.viz.collaboration.ui.session.SessionView;
import com.raytheon.uf.viz.core.VizApp;

/**
 * TODO Add Description
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jun 8, 2012            njensen     Initial creation
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

    private ConnectionSubscriber() {

    }

    public static void subscribe(CollaborationConnection connection) {
        if (instance == null) {
            instance = new ConnectionSubscriber();
            instance.setup(connection);
        }
    }

    public static void unsubscribe(CollaborationConnection connection) {
        if (instance != null) {
            instance.dispose(connection);
        }

        instance = null;
    }

    private void setup(final CollaborationConnection connection) {
        if (connection != null) {
            // Register handlers and events for the new sessionManager.
            connection.registerEventHandler(this);
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
                        connection.closeManager();
                    }
                }
            };
            PlatformUI.getWorkbench().addWorkbenchListener(wbListener);
        }

    }

    private void dispose(CollaborationConnection connection) {
        if (connection != null) {
            try {
                ISession p2pSession = connection.getPeerToPeerSession();
                p2pSession.unRegisterEventHandler(this);
            } catch (CollaborationException e) {
                statusHandler.handle(Priority.PROBLEM,
                        "Error unregistering peer to peer handler", e);
            }
            connection.unRegisterEventHandler(this);
        }
        PlatformUI.getWorkbench().removeWorkbenchListener(wbListener);
    }

    @Subscribe
    public void handleInvitationEvent(IVenueInvitationEvent event) {
        final IVenueInvitationEvent invitation = event;
        VizApp.runSync(new Runnable() {

            @Override
            public void run() {
                IQualifiedID inviter = invitation.getInviter();
                IQualifiedID room = invitation.getRoomId();
                Shell shell = new Shell(Display.getCurrent());
                MessageBox box = new MessageBox(shell, SWT.ICON_QUESTION
                        | SWT.OK | SWT.CANCEL);
                box.setText("Invitation");
                StringBuilder sb = new StringBuilder();
                boolean sharedDisplay = invitation.getInvite() instanceof SharedDisplayVenueInvite;
                sb.append("You are invited to a ");
                if (sharedDisplay) {
                    sb.append("collaboration session.\n");
                } else {
                    sb.append("chat room.\n");
                }
                sb.append("Inviter: ").append(inviter.getName()).append("\n");
                sb.append("Room: ").append(room.getName()).append("\n");
                sb.append("Subject: ").append(invitation.getSubject());
                if (invitation.getInvite() != null
                        && invitation.getInvite().getMessage() != null) {
                    sb.append("\n").append("Message: ")
                            .append(invitation.getInvite().getMessage());
                }
                box.setMessage(sb.toString());
                if (SWT.OK != box.open()) {
                    return;
                }

                CollaborationConnection connection = CollaborationConnection
                        .getConnection();
                try {
                    IVenueSession session = connection
                            .joinCollaborationVenue(invitation);
                    String sessionId = session.getSessionId();
                    CollaborationDataManager.getInstance().addSession(
                            sessionId, session);
                    if (sharedDisplay) {
                        ISharedDisplaySession displaySession = (ISharedDisplaySession) session;
                        SessionColorManager man = new SessionColorManager();
                        man.setColors(((SharedDisplayVenueInvite) invitation
                                .getInvite()).getRGBColors());
                        SharedDisplaySessionMgr.joinSession(displaySession,
                                SharedDisplayRole.PARTICIPANT, man);

                        PlatformUI
                                .getWorkbench()
                                .getActiveWorkbenchWindow()
                                .getActivePage()
                                .showView(CollaborationSessionView.ID,
                                        sessionId, IWorkbenchPage.VIEW_ACTIVATE);
                    } else {
                        PlatformUI
                                .getWorkbench()
                                .getActiveWorkbenchWindow()
                                .getActivePage()
                                .showView(SessionView.ID, sessionId,
                                        IWorkbenchPage.VIEW_ACTIVATE);
                    }
                } catch (CollaborationException e) {
                    // TODO Auto-generated catch block. Please revise as
                    // appropriate.
                    statusHandler.handle(Priority.PROBLEM,
                            e.getLocalizedMessage(), e);
                } catch (PartInitException e) {
                    // TODO Auto-generated catch block. Please revise as
                    // appropriate.
                    statusHandler.handle(Priority.PROBLEM,
                            e.getLocalizedMessage(), e);
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
                IQualifiedID peer = message.getFrom();
                for (IViewReference ref : PlatformUI.getWorkbench()
                        .getActiveWorkbenchWindow().getActivePage()
                        .getViewReferences()) {
                    IWorkbenchPart part = ref.getPart(false);
                    if (part != null && part instanceof PeerToPeerView) {
                        PeerToPeerView p2pView = (PeerToPeerView) part;
                        if (p2pView.getPeer().equals(peer)) {
                            p2pView.appendMessage(message);
                            return;
                        }
                    }
                }
                try {
                    // use the aliased name if there is one
                    String sId = peer.getName();
                    for (UserId id : CollaborationUtils.getIds()) {
                        if (id.equals(peer)) {
                            sId = id.getAlias();
                        }
                    }
                    PeerToPeerView p2pView = (PeerToPeerView) PlatformUI
                            .getWorkbench()
                            .getActiveWorkbenchWindow()
                            .getActivePage()
                            .showView(PeerToPeerView.ID, sId,
                                    IWorkbenchPage.VIEW_ACTIVATE);
                    p2pView.setPeer(peer);
                    p2pView.appendMessage(message);
                } catch (PartInitException e) {
                    statusHandler.handle(Priority.PROBLEM,
                            "Error opening peer to peer view", e);
                }
            }
        });
    }

    @Subscribe
    public void handleHttpdConfigurationEvent(
            IHttpdCollaborationConfigurationEvent configurationEvent) {

        // Add the httpd collaboration url to the CAVE configuration.
        Activator
                .getDefault()
                .getPreferenceStore()
                .setValue(
                        CollabPrefConstants.HttpCollaborationConfiguration.P_HTTP_SESSION_URL,
                        configurationEvent.getHttpdCollaborationURL());
        Activator
                .getDefault()
                .getPreferenceStore()
                .setValue(
                        CollabPrefConstants.HttpCollaborationConfiguration.P_SESSION_CONFIGURED,
                        true);
    }

}
