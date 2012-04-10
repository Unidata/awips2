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
import java.util.List;
import java.util.Map;

import org.eclipse.swt.SWT;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.MessageBox;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.ui.IEditorReference;
import org.eclipse.ui.IViewReference;
import org.eclipse.ui.IWorkbench;
import org.eclipse.ui.IWorkbenchListener;
import org.eclipse.ui.IWorkbenchPage;
import org.eclipse.ui.PartInitException;
import org.eclipse.ui.PlatformUI;

import com.google.common.collect.HashMultimap;
import com.google.common.collect.Multimap;
import com.google.common.eventbus.EventBus;
import com.google.common.eventbus.Subscribe;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.viz.collaboration.comm.identity.CollaborationException;
import com.raytheon.uf.viz.collaboration.comm.identity.IPresence;
import com.raytheon.uf.viz.collaboration.comm.identity.ISession;
import com.raytheon.uf.viz.collaboration.comm.identity.ISharedDisplaySession;
import com.raytheon.uf.viz.collaboration.comm.identity.IVenueSession;
import com.raytheon.uf.viz.collaboration.comm.identity.event.ITextMessageEvent;
import com.raytheon.uf.viz.collaboration.comm.identity.event.IVenueInvitationEvent;
import com.raytheon.uf.viz.collaboration.comm.identity.roster.IRoster;
import com.raytheon.uf.viz.collaboration.comm.identity.roster.IRosterEntry;
import com.raytheon.uf.viz.collaboration.comm.identity.user.IChatID;
import com.raytheon.uf.viz.collaboration.comm.identity.user.IQualifiedID;
import com.raytheon.uf.viz.collaboration.comm.provider.Presence;
import com.raytheon.uf.viz.collaboration.comm.provider.TextMessage;
import com.raytheon.uf.viz.collaboration.comm.provider.roster.RosterEntry;
import com.raytheon.uf.viz.collaboration.comm.provider.session.SessionManager;
import com.raytheon.uf.viz.collaboration.ui.editor.CollaborationEditor;
import com.raytheon.uf.viz.collaboration.ui.login.LoginData;
import com.raytheon.uf.viz.collaboration.ui.login.LoginDialog;
import com.raytheon.uf.viz.collaboration.ui.role.DataProviderEventController;
import com.raytheon.uf.viz.collaboration.ui.role.IRoleEventController;
import com.raytheon.uf.viz.collaboration.ui.role.ParticipantEventController;
import com.raytheon.uf.viz.collaboration.ui.session.CollaborationSessionView;
import com.raytheon.uf.viz.collaboration.ui.session.PeerToPeerView;
import com.raytheon.uf.viz.core.VizApp;
import com.raytheon.viz.ui.VizWorkbenchManager;
import com.raytheon.viz.ui.editor.AbstractEditor;

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
public class CollaborationDataManager {
    private static CollaborationDataManager instance;

    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(CollaborationDataManager.class);

    /**
     * The connection to the server.
     */
    private SessionManager sessionManager;

    String loginId;

    private LoginData loginData;

    Shell shell;

    /**
     * Created when connection made. Used to clean up connection when CAVE shuts
     * down.
     */
    private IWorkbenchListener wbListener;

    /**
     * User information such as sessions and groups user is in.
     */
    Map<String, DataUser> usersMap;

    private boolean linkCollaboration;

    /**
     * Mapping for all active chat sessions.
     */
    Map<String, IVenueSession> sessionsMap;

    private Multimap<String, IRoleEventController> roleEventControllersMap;

    Map<String, CollaborationEditor> editorsMap;

    private EventBus eventBus;

    public static CollaborationDataManager getInstance() {
        if (instance == null) {
            instance = new CollaborationDataManager();
        }
        return instance;
    }

    public LoginData getLoginData() {
        return loginData;
    }

    /**
     * Converts the venu's Id into a string that usable for a view's secondary
     * ID. This is the used as the key in the session Map.
     * 
     * @param venueId
     * @return sessionId
     */
    public String venueIdToSessionId(String venueId) {
        return venueId.replace(':', ';');
    }

    /**
     * Private constructor to for singleton class.
     */
    private CollaborationDataManager() {
        linkCollaboration = false;
        usersMap = new HashMap<String, DataUser>();
        sessionsMap = new HashMap<String, IVenueSession>();
        roleEventControllersMap = HashMultimap.create();
        editorsMap = new HashMap<String, CollaborationEditor>();
        eventBus = new EventBus();
    }

    public String getLoginId() {
        return loginId;
    }

    public DataUser getUser(String id) {
        DataUser user = usersMap.get(id);
        if (user == null) {
            user = new DataUser(id);
            usersMap.put(id, user);
        }
        return usersMap.get(id);
    }

    public void setLinkCollaboration(boolean state) {
        this.linkCollaboration = state;
    }

    public boolean getLinkCollaboration() {
        return linkCollaboration;
    }

    public void editorCreated(String sessionId, CollaborationEditor editor) {
        editorsMap.put(sessionId, editor);
    }

    public CollaborationEditor getEditor(String sessionId) {
        return editorsMap.get(sessionId);
    }

    /**
     * Get the session sessionManager and if needed the user/password.
     * 
     * @return sessionManager or null if unable to get connection.
     */
    synchronized public SessionManager getSessionManager() {
        // Get user's server account information and make connection.
        if (isConnected() == false) {
            VizApp.runSync(new Runnable() {

                @Override
                public void run() {
                    shell = Display.getDefault().getActiveShell();
                    if (shell == null) {
                        return;
                    }
                    LoginDialog dlg = new LoginDialog(shell);
                    loginData = null;
                    while (isConnected() == false) {
                        loginData = (LoginData) dlg.open();
                        dlg.close();
                        if (loginData == null) {
                            break;
                        }
                        try {
                            sessionManager = new SessionManager(loginData
                                    .getAccount(), loginData.getPassword());
                            loginId = loginData.getAccount();
                            DataUser user = CollaborationDataManager
                                    .getInstance().getUser(loginId);
                            // try {
                            // System.out.println("enter sleep...");
                            // Thread.sleep(5000L);
                            // System.out.println("Wake from sleep...");
                            // } catch (InterruptedException e) {
                            // }
                            // // TODO set mode and message here.
                            // user.setMode(loginData.getMode());
                            // user.type = Type.AVAILABLE;
                            // user.statusMessage = loginData.getModeMessage();
                        } catch (Exception e) {
                            MessageBox box = new MessageBox(shell, SWT.ERROR);
                            box.setText("Login Failed");
                            if (e.getMessage() != null) {
                                box.setMessage(e.getMessage());
                            } else {
                                box.setMessage("Login Failed.");
                            }
                            box.open();
                            // statusHandler.handle(Priority.WARN,
                            // e.getLocalizedMessage(), e);
                        }
                    }
                }
            });

            if (isConnected()) {
                // Register handlers and events for the new sessionManager.
                sessionManager.registerEventHandler(this);
                try {
                    ISession p2pSession = sessionManager.getPeerToPeerSession();
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
                        if (sessionManager != null) {
                            try {
                                ISession p2pSession = sessionManager
                                        .getPeerToPeerSession();
                                p2pSession.unRegisterEventHandler(this);
                            } catch (CollaborationException e) {
                                // TODO Auto-generated catch block. Please
                                // revise as appropriate.
                                statusHandler.handle(Priority.PROBLEM,
                                        e.getLocalizedMessage(), e);
                            }
                            sessionManager.unRegisterEventHandler(this);
                            sessionManager.closeManager();
                            sessionManager = null;
                        }
                    }
                };
                PlatformUI.getWorkbench().addWorkbenchListener(wbListener);
                // TODO this sleep needs to go away. It is a temporary fix to
                // allow the roster manager to get all its entries before we
                // use
                // it. Once we have needed eventhandlers for roster updating
                // this can go away.
                try {
                    System.out.println("enter sleep...");
                    Thread.sleep(5000L);
                    System.out.println("Wake from sleep...");
                } catch (InterruptedException e) {
                }
                IPresence presence = sessionManager.getPresence();
                if (sessionManager.getPresence() == null) {
                    presence = new Presence();
                    presence.setProperty("dummy", "dummy");
                    sessionManager.setPresence(presence);
                }
                fireModifiedPresence();
            }
        }

        return sessionManager;
    }

    synchronized public void closeManager() {
        if (sessionManager != null) {
            sessionManager.unRegisterEventHandler(this);
            sessionManager.closeManager();
            sessionManager = null;
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
        IVenueSession session = null;
        if (sessionId == null) {
            if (sessionsMap.size() > 0) {
                session = sessionsMap.get(sessionsMap.keySet().toArray()[0]);
            }
        } else {
            session = sessionsMap.get(sessionId);
        }
        return session;
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
        CollaborationEditor editor = editorsMap.remove(sessionId);
        if (editor != null) {
            for (IEditorReference ref : PlatformUI.getWorkbench()
                    .getActiveWorkbenchWindow().getActivePage()
                    .getEditorReferences()) {
                if (editor == ref.getEditor(false)) {
                    PlatformUI.getWorkbench().getActiveWorkbenchWindow()
                            .getActivePage().hideEditor(ref);
                }
            }
        }

        Collection<IRoleEventController> controller = roleEventControllersMap
                .removeAll(sessionId);
        if (controller != null) {
            for (IRoleEventController cont : controller) {
                cont.shutdown();
            }
        }
    }

    public void editorBringToTop(String sessionId) {
        if (linkCollaboration) {
            CollaborationEditor editor = CollaborationDataManager.getInstance()
                    .getEditor(sessionId);
            if (editor != null) {
                PlatformUI.getWorkbench().getActiveWorkbenchWindow()
                        .getActivePage().bringToTop(editor);
            }
        }
    }

    public String getSessionId(CollaborationEditor editor) {
        String sessionId = null;
        for (String key : editorsMap.keySet()) {
            if (editor == editorsMap.get(key)) {
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
        SessionManager sessionManager = getSessionManager();
        IVenueSession session = null;
        String sessionId = null;
        // try {
        session = sessionManager.createCollaborationVenue(venue, subject);
        // sessionId = venueIdToSessionId(session.getVenue().getInfo()
        // .getVenueID());
        sessionId = session.getSessionId();
        // TODO throw an exception if unable to make connection?
        if (session.isConnected()) {
            ISharedDisplaySession displaySession = session
                    .spawnSharedDisplaySession();
            sessionsMap.put(sessionId, session);
            DataProviderEventController dpec = new DataProviderEventController(
                    displaySession);
            dpec.startup();
            roleEventControllersMap.put(sessionId, dpec);
            // TODO set displaySession's data provider and session leader.
        }
        // TODO Start CAVE editor associated with this session and make sure the
        // user is data provider and session leader.
        return sessionId;
    }

    public String createTextOnlySession(String venueName, String subject)
            throws CollaborationException {
        SessionManager sessionManager = getSessionManager();
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
        return sessionManager != null && sessionManager.isConnected();
    }

    @Subscribe
    public void handleInvitationEvent(IVenueInvitationEvent event) {
        final IVenueInvitationEvent invitation = event;
        System.out.println("==== handleInvitationEvent sessionId: "
                + invitation.getSessionId());
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
                sb.append("You are invited to a collaboration.\n");
                sb.append("Inviter: ").append(inviter.getName()).append("\n");
                sb.append("Room: ").append(room.getName()).append("\n");
                sb.append("Subject: ").append(invitation.getSubject());
                if (invitation.getBody() != null) {
                    sb.append("\n").append("Message: ")
                            .append(invitation.getBody());
                }
                box.setMessage(sb.toString());
                if (SWT.OK != box.open()) {
                    return;
                }
                try {
                    IVenueSession session = sessionManager
                            .joinCollaborationVenue(invitation);
                    String sessionId = session.getSessionId();
                    sessionsMap.put(sessionId, session);
                    ISharedDisplaySession displaySession = session
                            .spawnSharedDisplaySession();
                    ParticipantEventController pec = new ParticipantEventController(
                            displaySession);
                    pec.startup();
                    roleEventControllersMap.put(sessionId, pec);
                    PlatformUI
                            .getWorkbench()
                            .getActiveWorkbenchWindow()
                            .getActivePage()
                            .showView(CollaborationSessionView.ID, sessionId,
                                    IWorkbenchPage.VIEW_ACTIVATE);
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
                String id = message.getFrom().getFQName();
                for (IViewReference ref : PlatformUI.getWorkbench()
                        .getActiveWorkbenchWindow().getActivePage()
                        .getViewReferences()) {
                    if (id.equals(ref.getSecondaryId())) {
                        PeerToPeerView p2pView = (PeerToPeerView) ref
                                .getView(false);
                        p2pView.appendMessage(message);
                        return;
                    }
                }
                try {
                    PeerToPeerView p2pView = (PeerToPeerView) PlatformUI
                            .getWorkbench()
                            .getActiveWorkbenchWindow()
                            .getActivePage()
                            .showView(PeerToPeerView.ID, id,
                                    IWorkbenchPage.VIEW_ACTIVATE);
                    p2pView.appendMessage(message);
                } catch (PartInitException e) {
                    // TODO Auto-generated catch block. Please revise as
                    // appropriate.
                    statusHandler.handle(Priority.PROBLEM,
                            e.getLocalizedMessage(), e);
                }
            }
        });
    }

    public void fireModifiedPresence() {
        IPresence presence = sessionManager.getPresence();
        presence.setMode(loginData.getMode());
        presence.setStatusMessage(loginData.getModeMessage());
        try {
            sessionManager.getAccountManager().sendPresence(presence);
            IRoster roster = sessionManager.getRosterManager().getRoster();
            // Generate a fake entry here to update the login user presence in
            // all registered views.
            IChatID id = roster.getUser();
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
        System.out.println("CollaborationDataManager.handleModifiedPresence");
        System.out.println("    user " + rosterEntry.getUser().getFQName());
        System.out.println("    mode " + rosterEntry.getPresence().getMode());
        System.out.println("    type " + rosterEntry.getPresence().getType());
        System.out.println("    message"
                + rosterEntry.getPresence().getStatusMessage());
        System.out.println("    groups " + rosterEntry.getGroups());
        String userId = rosterEntry.getUser().getFQName();
        DataUser user = usersMap.get(userId);
        if (user != null) {
            user.mode = rosterEntry.getPresence().getMode();
            user.type = rosterEntry.getPresence().getType();
            user.statusMessage = rosterEntry.getPresence().getStatusMessage();
            // Assumes only UI updates will be registered.
            VizApp.runAsync(new Runnable() {

                @Override
                public void run() {
                    eventBus.post(rosterEntry);
                }
            });
        }
    }

    @Deprecated
    public String joinCollaborationSession(String venueName, String sessionId) {
        String result = sessionId;
        if (sessionsMap.get(sessionId) == null) {

            IVenueSession session = null;
            try {
                session = getSessionManager().joinCollaborationVenue(venueName);
                result = session.getSessionId();
                ISharedDisplaySession displaySession = session
                        .spawnSharedDisplaySession();
                sessionsMap.put(result, session);
                ParticipantEventController pec = new ParticipantEventController(
                        displaySession);
                pec.startup();
                roleEventControllersMap.put(sessionId, pec);
                // TODO test only delete
                // SharedEditor editor = EditorSetup.testLoadEditorData();
                // pec.initDataArrived(editor);
            } catch (CollaborationException e) {
                // TODO Auto-generated catch block. Please revise as
                // appropriate.
                statusHandler.handle(Priority.PROBLEM, e.getLocalizedMessage(),
                        e);
            }
        }
        return result;
    }

    public Collection<IRoleEventController> getEventControllers(String sessionId) {
        return roleEventControllersMap.get(sessionId);
    }

    public List<AbstractEditor> getActivelySharedEditors(String sessionId) {
        List<AbstractEditor> list = new ArrayList<AbstractEditor>();

        // TODO actually keep track of a list and return that
        // list should be empty if you're not the data provider
        list.add((AbstractEditor) VizWorkbenchManager.getInstance()
                .getActiveEditor());
        return list;
    }

    public void registerEventHandler(Object handler) {
        eventBus.register(handler);
    }

    public void unRegisterEventHandler(Object handler) {
        eventBus.unregister(handler);
    }
}
