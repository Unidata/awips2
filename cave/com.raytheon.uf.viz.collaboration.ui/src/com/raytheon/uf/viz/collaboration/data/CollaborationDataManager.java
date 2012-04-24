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
package com.raytheon.uf.viz.collaboration.data;

import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

import org.eclipse.core.runtime.Assert;
import org.eclipse.swt.SWT;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.MessageBox;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.ui.IEditorReference;
import org.eclipse.ui.IViewReference;
import org.eclipse.ui.IWorkbench;
import org.eclipse.ui.IWorkbenchListener;
import org.eclipse.ui.IWorkbenchPage;
import org.eclipse.ui.IWorkbenchPart;
import org.eclipse.ui.PartInitException;
import org.eclipse.ui.PlatformUI;

import com.google.common.eventbus.EventBus;
import com.google.common.eventbus.Subscribe;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.viz.collaboration.comm.identity.CollaborationException;
import com.raytheon.uf.viz.collaboration.comm.identity.IPresence;
import com.raytheon.uf.viz.collaboration.comm.identity.IPresence.Mode;
import com.raytheon.uf.viz.collaboration.comm.identity.IPresence.Type;
import com.raytheon.uf.viz.collaboration.comm.identity.ISession;
import com.raytheon.uf.viz.collaboration.comm.identity.ISharedDisplaySession;
import com.raytheon.uf.viz.collaboration.comm.identity.IVenueSession;
import com.raytheon.uf.viz.collaboration.comm.identity.event.IRosterChangeEvent;
import com.raytheon.uf.viz.collaboration.comm.identity.event.IRosterEventSubscriber;
import com.raytheon.uf.viz.collaboration.comm.identity.event.ITextMessageEvent;
import com.raytheon.uf.viz.collaboration.comm.identity.event.IVenueInvitationEvent;
import com.raytheon.uf.viz.collaboration.comm.identity.invite.SharedDisplayVenueInvite;
import com.raytheon.uf.viz.collaboration.comm.identity.roster.IRoster;
import com.raytheon.uf.viz.collaboration.comm.identity.roster.IRosterEntry;
import com.raytheon.uf.viz.collaboration.comm.identity.roster.IRosterGroup;
import com.raytheon.uf.viz.collaboration.comm.identity.roster.IRosterItem;
import com.raytheon.uf.viz.collaboration.comm.identity.user.IQualifiedID;
import com.raytheon.uf.viz.collaboration.comm.identity.user.SharedDisplayRole;
import com.raytheon.uf.viz.collaboration.comm.provider.TextMessage;
import com.raytheon.uf.viz.collaboration.comm.provider.roster.RosterEntry;
import com.raytheon.uf.viz.collaboration.comm.provider.session.CollaborationConnection;
import com.raytheon.uf.viz.collaboration.comm.provider.user.UserId;
import com.raytheon.uf.viz.collaboration.ui.SessionColorManager;
import com.raytheon.uf.viz.collaboration.ui.editor.CollaborationEditor;
import com.raytheon.uf.viz.collaboration.ui.login.LoginDialog;
import com.raytheon.uf.viz.collaboration.ui.session.CollaborationSessionView;
import com.raytheon.uf.viz.collaboration.ui.session.PeerToPeerView;
import com.raytheon.uf.viz.collaboration.ui.session.SessionView;
import com.raytheon.uf.viz.core.VizApp;

/**
 * A single class that contains all data information.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Mar 1, 2012            rferrel     Initial creation
 * 
 * </pre>
 * 
 * @author rferrel
 * @version 1.0
 */
public class CollaborationDataManager implements IRosterEventSubscriber {
    private static CollaborationDataManager instance;

    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(CollaborationDataManager.class);

    /**
     * The connection to the server.
     */
    private CollaborationConnection connection;

    Shell shell;

    /**
     * Created when connection made. Used to clean up connection when CAVE shuts
     * down.
     */
    private IWorkbenchListener wbListener;

    /**
     * User information such as sessions and groups user is in.
     */
    Map<UserId, IRosterEntry> usersMap;

    Set<IRosterGroup> groups;

    private boolean linkCollaboration;

    /**
     * Mapping for all active chat sessions.
     */
    Map<String, IVenueSession> sessionsMap;

    private EventBus eventBus;

    public static CollaborationDataManager getInstance() {
        if (instance == null) {
            instance = new CollaborationDataManager();
        }
        return instance;
    }

    /**
     * Private constructor to for singleton class.
     */
    private CollaborationDataManager() {
        linkCollaboration = false;
        groups = new HashSet<IRosterGroup>();
        usersMap = new HashMap<UserId, IRosterEntry>();
        sessionsMap = new HashMap<String, IVenueSession>();
        eventBus = new EventBus();
    }

    private void populateGroups() {
        IRoster roster = connection.getRosterManager().getRoster();

        for (IRosterGroup rosterGroup : roster.getGroups()) {
            groups.add(rosterGroup);
            for (IRosterEntry rosterEntry : rosterGroup.getEntries()) {
                usersMap.put(rosterEntry.getUser(), rosterEntry);
            }
        }

        // Orphan users not in any group.
        for (IRosterEntry rosterEntry : roster.getEntries()) {
            usersMap.put(rosterEntry.getUser(), rosterEntry);
        }

        usersMap.put(connection.getUser(),
                new RosterEntry(connection.getUser()));
    }

    /**
     * Get a sorted list of groups
     * 
     * @param allGroups
     *            - When true all groups otherwise the groups selected for
     *            display.
     * @return groups
     */
    public List<IRosterItem> getGroups(boolean allGroups) {
        List<IRosterItem> result = new ArrayList<IRosterItem>();
        if (allGroups) {
            for (IRosterGroup dataGroup : groups) {
                result.add(dataGroup);
            }
        }
        return result;
    }

    public List<IRosterEntry> getUsersInGroup(IRosterItem groupId) {
        List<IRosterEntry> userList = new ArrayList<IRosterEntry>();
        for (UserId userId : usersMap.keySet()) {
            IRosterEntry user = usersMap.get(userId);
            for (IRosterGroup group : user.getGroups()) {
                if (groupId.equals(group)) {
                    userList.add(user);
                    break;
                }
            }
        }
        return userList;
    }

    public boolean displayGroup(String groupId) {
        boolean display = true;
        // TODO maybe need to make this displayGroup function do something
        return display;
    }

    public void setLinkCollaboration(boolean state) {
        this.linkCollaboration = state;
    }

    public boolean getLinkCollaboration() {
        return linkCollaboration;
    }

    public void editorCreated(ISharedDisplaySession session,
            CollaborationEditor editor) {
        SessionContainer container = SharedDisplaySessionMgr
                .getSessionContainer(session.getSessionId());
        container.setCollaborationEditor(editor);
        editor.setTabTitle(((IVenueSession) session).getVenue().getInfo()
                .getVenueDescription());
    }

    /**
     * Get the session sessionManager and if needed the user/password.
     * 
     * @return sessionManager or null if unable to get connection.
     */
    synchronized public CollaborationConnection getCollaborationConnection() {
        // Get user's server account information and make connection.
        if (isConnected() == false) {
            VizApp.runSync(new Runnable() {

                @Override
                public void run() {
                    shell = Display.getDefault().getActiveShell();
                    if (shell == null) {
                        return;
                    }
                    LoginDialog dlg = new LoginDialog(shell,
                            CollaborationDataManager.this);
                    CollaborationConnection newConn = null;
                    newConn = (CollaborationConnection) dlg.open();
                    dlg.close();
                    if (newConn != null) {
                        connection = newConn;
                    }
                }
            });

            if (isConnected()) {
                // Register handlers and events for the new sessionManager.
                connection.registerEventHandler(this);
                try {
                    ISession p2pSession = connection.getPeerToPeerSession();
                    p2pSession.registerEventHandler(this);
                } catch (CollaborationException e) {
                    // TODO Auto-generated catch block. Please revise as
                    // appropriate.
                    statusHandler.handle(Priority.PROBLEM,
                            e.getLocalizedMessage(), e);
                }
                wbListener = new IWorkbenchListener() {

                    @Override
                    public boolean preShutdown(IWorkbench workbench,
                            boolean forced) {
                        return true;
                    }

                    @Override
                    public void postShutdown(IWorkbench workbench) {
                        if (connection != null) {
                            try {
                                ISession p2pSession = connection
                                        .getPeerToPeerSession();
                                p2pSession.unRegisterEventHandler(this);
                            } catch (CollaborationException e) {
                                // TODO Auto-generated catch block. Please
                                // revise as appropriate.
                                statusHandler.handle(Priority.PROBLEM,
                                        e.getLocalizedMessage(), e);
                            }
                            connection.unRegisterEventHandler(this);
                            connection.closeManager();
                            connection = null;
                        }
                    }
                };
                PlatformUI.getWorkbench().addWorkbenchListener(wbListener);
                populateGroups();
            }
        }

        return connection;
    }

    synchronized public void closeManager() {
        if (connection != null) {
            // The close unRegisters the event handler
            connection.closeManager();
            connection = null;
        }
        if (wbListener != null) {
            PlatformUI.getWorkbench().removeWorkbenchListener(wbListener);
            wbListener = null;
        }
    }

    /**
     * Get the Venue session associated with the key or any session when key is
     * null.
     * 
     * @param sessionId
     *            - key to fetch session
     * @return session - The venue session or null if none found
     */
    public IVenueSession getSession(String sessionId) {
        Assert.isNotNull(sessionId,
                "getSession should never be passed a null sessionId");
        // IVenueSession session = null;
        // if (sessionId == null) {
        // if (sessionsMap.size() > 0) {
        // session = sessionsMap.get(sessionsMap.keySet().toArray()[0]);
        // }
        // } else {
        // session = sessionsMap.get(sessionId);
        // }
        // return session;
        return sessionsMap.get(sessionId);
    }

    public Map<String, IVenueSession> getSessions() {
        return sessionsMap;
    }

    /**
     * Closes connection to the session.
     * 
     * @param sessionId
     */
    public void closeSession(String sessionId) {
        IVenueSession session = sessionsMap.get(sessionId);
        if (session != null) {
            sessionsMap.remove(sessionId);
            session.close();
        }
    }

    public void closeEditor(String sessionId) {
        SessionContainer container = SharedDisplaySessionMgr
                .getSessionContainer(sessionId);
        container.getRoleEventController().shutdown();
        CollaborationEditor editor = container.getCollaborationEditor();
        if (editor != null) {
            IWorkbenchPage page = PlatformUI.getWorkbench()
                    .getActiveWorkbenchWindow().getActivePage();
            if (page != null) {
                for (IEditorReference ref : page.getEditorReferences()) {
                    if (editor == ref.getEditor(false)) {
                        PlatformUI.getWorkbench().getActiveWorkbenchWindow()
                                .getActivePage().hideEditor(ref);
                    }
                }
            }
        }

    }

    public void editorBringToTop(String sessionId) {
        if (linkCollaboration) {
            CollaborationEditor editor = SharedDisplaySessionMgr
                    .getSessionContainer(sessionId).getCollaborationEditor();
            if (editor != null) {
                PlatformUI.getWorkbench().getActiveWorkbenchWindow()
                        .getActivePage().bringToTop(editor);
            }
        }
    }

    public String getSessionId(CollaborationEditor editor) {
        String sessionId = null;
        for (String key : SharedDisplaySessionMgr.getActiveSessionIds()) {
            SessionContainer container = SharedDisplaySessionMgr
                    .getSessionContainer(key);
            if (editor == container.getCollaborationEditor()) {
                sessionId = key;
                break;
            }
        }
        return sessionId;
    }

    /**
     * Bring the view associated with the sessionId to the top.
     * 
     * @param sessionId
     */
    public void viewBringToTop(String sessionId) {
        if (linkCollaboration) {
            for (IViewReference ref : PlatformUI.getWorkbench()
                    .getActiveWorkbenchWindow().getActivePage()
                    .getViewReferences()) {
                if (sessionId.equals(ref.getSecondaryId())) {
                    try {
                        PlatformUI.getWorkbench().getActiveWorkbenchWindow()
                                .getActivePage().bringToTop(ref.getView(false));
                    } catch (NullPointerException ex) {
                        // Ignore happens during creation of view/editor.
                    }
                    break;
                }
            }
        }
    }

    /**
     * Generate a new session.
     * 
     * @param venue
     *            - Session name
     * @param subject
     *            - Session topic
     * @return sessionId - the key to use to retrieve the sesson or null if
     *         unable to create the session
     */
    public String createCollaborationSession(String venue, String subject)
            throws CollaborationException {
        CollaborationConnection sessionManager = getCollaborationConnection();
        IVenueSession session = null;
        String sessionId = null;
        // try {
        session = sessionManager.createCollaborationVenue(venue, subject);
        // sessionId = venueIdToSessionId(session.getVenue().getInfo()
        // .getVenueID());
        sessionId = session.getSessionId();
        // TODO throw an exception if unable to make connection?
        if (session.isConnected()) {
            ISharedDisplaySession displaySession = (ISharedDisplaySession) session;
            sessionsMap.put(sessionId, session);
            SharedDisplaySessionMgr.joinSession(displaySession,
                    SharedDisplayRole.DATA_PROVIDER, null);

        }

        return sessionId;
    }

    public String createTextOnlySession(String venueName, String subject)
            throws CollaborationException {
        CollaborationConnection sessionManager = getCollaborationConnection();
        IVenueSession session = null;
        String sessionId = null;
        session = sessionManager.createTextOnlyVenue(venueName, subject);
        if (session.isConnected()) {
            sessionId = session.getSessionId();
            sessionsMap.put(sessionId, session);
        }
        return sessionId;
    }

    public boolean isConnected() {
        return connection != null && connection.isConnected();
    }

    @Subscribe
    public void handleInvitationEvent(IVenueInvitationEvent event) {
        final IVenueInvitationEvent invitation = event;
        System.out.println("==== handleInvitationEvent sessionId: "
                + invitation.getInvite().getSessionId());
        System.out.println("==== handleInvitationEvent inviter: "
                + invitation.getInviter());
        VizApp.runSync(new Runnable() {

            @Override
            public void run() {
                IQualifiedID inviter = invitation.getInviter();
                IQualifiedID room = invitation.getRoomId();
                if (shell.isDisposed()) {
                    shell = new Shell(Display.getCurrent());
                }
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
                try {
                    IVenueSession session = connection
                            .joinCollaborationVenue(invitation);
                    String sessionId = session.getSessionId();
                    sessionsMap.put(sessionId, session);
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
        // System.out.println("p2pMsg from: " + message.getFrom().getFQName());
        // System.out.println("p2pMsgt body: " + message.getBody());
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
                    PeerToPeerView p2pView = (PeerToPeerView) PlatformUI
                            .getWorkbench()
                            .getActiveWorkbenchWindow()
                            .getActivePage()
                            .showView(PeerToPeerView.ID, peer.getFQName(),
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

    public void fireModifiedPresence(Mode mode, String msg) {
        IPresence presence = connection.getPresence();
        if (mode != null) {
            presence.setMode(mode);
        }
        presence.setType(Type.AVAILABLE);
        if (msg != null) {
            presence.setStatusMessage(msg);
        }
        try {
            connection.getAccountManager().sendPresence(presence);
            UserId id = connection.getUser();
            RosterEntry rosterEntry = new RosterEntry(id);
            rosterEntry.setPresence(presence);
            handleModifiedPresence(rosterEntry);
        } catch (CollaborationException e) {
            // TODO Auto-generated catch block. Please revise as
            // appropriate.
            statusHandler.handle(Priority.PROBLEM, e.getLocalizedMessage(), e);
        } catch (Exception ex) {
            ex.printStackTrace();
        }
    }

    /**
     * @param entry
     */
    @Subscribe
    public void handleModifiedPresence(IRosterEntry entry) {
        final IRosterEntry rosterEntry = entry;
        IRosterEntry user = usersMap.get(entry.getUser());
        if (user != null) {
            // Assumes only UI updates will be registered.
            VizApp.runAsync(new Runnable() {

                @Override
                public void run() {
                    eventBus.post(rosterEntry);
                }
            });
        }
    }

    /**
     * This updates the Data Manager's information then informs and UI of the
     * change.
     * 
     * @param event
     */
    @Subscribe
    public void handleRosterChangeEvent(IRosterChangeEvent event) {
        final IRosterChangeEvent rosterChangeEvent = event;
        // TODO update the event's user groups here for the desired type
        IRosterEntry rosterEntry = rosterChangeEvent.getEntry();
        IPresence presence = rosterChangeEvent.getEntry().getPresence();
        if (presence != null) {
            System.out.println("\t" + presence.getMode() + "/"
                    + presence.getType() + ": \"" + presence.getStatusMessage()
                    + "\"");
        }
        Collection<IRosterGroup> userGroups = rosterEntry.getGroups();
        switch (rosterChangeEvent.getType()) {
        case ADD:
            for (IRosterGroup group : userGroups) {
                boolean matched = false;
                for (IRosterGroup dGroup : groups) {
                    if (dGroup.getName().equals(group.getName())) {
                        dGroup = group;
                        matched = true;
                        break;
                    }
                }
                if (!matched) {
                    groups.add(group);
                }
            }
            break;
        case DELETE:
            // Assume user no longer exists and remove.
            usersMap.remove(rosterEntry);
            break;
        case MODIFY:
            // Assume only the presence needs to be updated.
            if (presence == null) {
                // Nothing to do don't bother doing eventBus post.
                return;
            }
            for (UserId id : usersMap.keySet()) {
                if (rosterEntry.getUser().equals(id)) {
                    usersMap.put(id, rosterEntry);
                    break;
                }
            }
            break;
        case PRESENCE:
            break;
        default:
            statusHandler.handle(Priority.PROBLEM, "Unhandled type: "
                    + rosterChangeEvent.getType());
            return;

        }

        // Assume only UI updates are registered.
        VizApp.runAsync(new Runnable() {
            @Override
            public void run() {
                eventBus.post(rosterChangeEvent);
            }
        });
    }

    /**
     * @return the usersMap
     */
    public Map<UserId, IRosterEntry> getUsersMap() {
        return usersMap;
    }

    public void registerEventHandler(Object handler) {
        eventBus.register(handler);
    }

    public void unRegisterEventHandler(Object handler) {
        eventBus.unregister(handler);
    }

}
