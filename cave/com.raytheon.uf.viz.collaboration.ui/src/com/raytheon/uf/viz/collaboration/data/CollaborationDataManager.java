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

import java.util.Collection;
import java.util.HashMap;
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
import com.google.common.eventbus.Subscribe;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.viz.collaboration.comm.identity.CollaborationException;
import com.raytheon.uf.viz.collaboration.comm.identity.IPresence.Type;
import com.raytheon.uf.viz.collaboration.comm.identity.ISharedDisplaySession;
import com.raytheon.uf.viz.collaboration.comm.identity.IVenueSession;
import com.raytheon.uf.viz.collaboration.comm.identity.event.IVenueInvitationEvent;
import com.raytheon.uf.viz.collaboration.comm.identity.user.IQualifiedID;
import com.raytheon.uf.viz.collaboration.comm.provider.session.SessionManager;
import com.raytheon.uf.viz.collaboration.ui.editor.CollaborationEditor;
import com.raytheon.uf.viz.collaboration.ui.login.LoginData;
import com.raytheon.uf.viz.collaboration.ui.login.LoginDialog;
import com.raytheon.uf.viz.collaboration.ui.role.AbstractRoleEventController;
import com.raytheon.uf.viz.collaboration.ui.role.DataProviderEventController;
import com.raytheon.uf.viz.collaboration.ui.role.ParticipantEventController;
import com.raytheon.uf.viz.collaboration.ui.role.SessionLeaderEventController;
import com.raytheon.uf.viz.collaboration.ui.session.CollaborationSessionView;
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
public class CollaborationDataManager {
    private static CollaborationDataManager instance;

    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(CollaborationDataManager.class);

    /**
     * The connection to the server.
     */
    private SessionManager manager;

    String loginId;

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

    private Multimap<String, AbstractRoleEventController> roleEventControllersMap;

    Map<String, CollaborationEditor> editorsMap;

    public static CollaborationDataManager getInstance() {
        if (instance == null) {
            instance = new CollaborationDataManager();
        }
        return instance;
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
     * Get the session manager and if needed the user/password.
     * 
     * @return manager or null if unable to get connection.
     */
    synchronized public SessionManager getSessionManager() {
        // Get user's server account information and make connection.
        if (manager == null) {
            VizApp.runSync(new Runnable() {

                @Override
                public void run() {
                    shell = Display.getDefault().getActiveShell();
                    if (shell == null) {
                        return;
                    }
                    LoginDialog dlg = new LoginDialog(shell);
                    LoginData loginData = null;
                    while (manager == null) {
                        loginData = (LoginData) dlg.open();
                        dlg.close();
                        if (loginData == null) {
                            break;
                        }
                        try {
                            manager = new SessionManager(
                                    loginData.getAccount(), loginData
                                            .getPassword());
                            loginId = loginData.getAccount();
                            DataUser user = CollaborationDataManager
                                    .getInstance().getUser(loginId);
                            // TODO set mode and message here.
                            user.setMode(loginData.getMode());
                            user.type = Type.AVAILABLE;
                            user.statusMessage = loginData.getModeMessage();
                        } catch (Exception e) {
                            statusHandler.handle(Priority.PROBLEM,
                                    e.getLocalizedMessage(), e);
                            MessageBox box = new MessageBox(shell, SWT.ERROR);
                            box.setText("Log On Failed");
                            box.setMessage(e.getMessage());
                            box.open();
                            e.printStackTrace();
                        }
                    }
                }
            });

            if (isConnected()) {
                // Register handlers and events for the new manager.
                manager.registerEventHandler(this);
                wbListener = new IWorkbenchListener() {

                    @Override
                    public boolean preShutdown(IWorkbench workbench,
                            boolean forced) {
                        return true;
                    }

                    @Override
                    public void postShutdown(IWorkbench workbench) {
                        if (manager != null) {
                            manager.unRegisterEventHandler(this);
                            manager.closeManager();
                            manager = null;
                        }
                    }
                };
                PlatformUI.getWorkbench().addWorkbenchListener(wbListener);
            }
        }

        return manager;
    }

    synchronized public void closeManager() {
        if (manager != null) {
            manager.unRegisterEventHandler(this);
            manager.closeManager();
            manager = null;
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

        Collection<AbstractRoleEventController> controller = roleEventControllersMap
                .removeAll(sessionId);
        if (controller != null) {
            for (AbstractRoleEventController cont : controller) {
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
    public String createCollaborationSession(String venue, String subject) {
        SessionManager manager = getSessionManager();
        IVenueSession session = null;
        String sessionId = null;
        try {
            session = manager.createCollaborationVenue(venue, subject);
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
                SessionLeaderEventController slec = new SessionLeaderEventController(
                        displaySession);
                slec.startup();
                roleEventControllersMap.put(sessionId, dpec);
                roleEventControllersMap.put(sessionId, slec);
                // TODO set displaySession's data provider and session leader.
            }
        } catch (CollaborationException e) {
            statusHandler.handle(Priority.PROBLEM, e.getLocalizedMessage(), e);
            sessionId = null;
        }

        // TODO Start CAVE editor associated with this session and make sure the
        // user is data provider and session leader.
        return sessionId;
    }

    public boolean isConnected() {
        return manager != null;
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
                box.setMessage(sb.toString());
                if (SWT.OK != box.open()) {
                    return;
                }
                try {
                    IVenueSession session = manager
                            .joinCollaborationVenue(invitation);
                    String sessionId = session.getSessionId();
                    sessionsMap.put(sessionId, session);
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
}
