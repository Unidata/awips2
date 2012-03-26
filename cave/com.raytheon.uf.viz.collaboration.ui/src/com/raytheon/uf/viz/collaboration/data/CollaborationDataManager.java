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

import java.util.HashMap;
import java.util.Map;

import org.eclipse.swt.SWT;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.MessageBox;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.ui.IWorkbench;
import org.eclipse.ui.IWorkbenchListener;
import org.eclipse.ui.PlatformUI;

import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.viz.collaboration.comm.identity.CollaborationException;
import com.raytheon.uf.viz.collaboration.comm.identity.IVenueSession;
import com.raytheon.uf.viz.collaboration.comm.provider.session.SessionManager;
import com.raytheon.uf.viz.collaboration.ui.login.LoginData;
import com.raytheon.uf.viz.collaboration.ui.login.LoginDialog;
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

    /**
     * Created when connection made. Used to clean up connection when CAVE shuts
     * down.
     */
    private IWorkbenchListener wbListener;

    /**
     * User information such as sessions and groups user is in.
     */
    Map<String, DataUser> usersMap;

    /**
     * Mapping for all active chat sessions.
     */
    Map<String, IVenueSession> sessionsMap;

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
     * @param venuId
     * @return sessionId
     */
    public String venuIdToSessionId(String venuId) {
        return venuId.replace(':', ';');
    }

    /**
     * Private constructor to for singleton class.
     */
    private CollaborationDataManager() {
        usersMap = new HashMap<String, DataUser>();
        sessionsMap = new HashMap<String, IVenueSession>();
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
                    Shell shell = Display.getDefault().getActiveShell();
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
                            user.statusMessage = loginData.getModeMessage();
                            wbListener = new IWorkbenchListener() {

                                @Override
                                public boolean preShutdown(
                                        IWorkbench workbench, boolean forced) {
                                    return true;
                                }

                                @Override
                                public void postShutdown(IWorkbench workbench) {
                                    if (manager != null) {
                                        manager.closeManager();
                                        manager = null;
                                    }
                                }
                            };
                            PlatformUI.getWorkbench().addWorkbenchListener(
                                    wbListener);
                        } catch (Exception e) {
                            if (manager != null) {
                                manager.closeManager();
                                manager = null;
                            }
                            if (wbListener != null) {
                                PlatformUI.getWorkbench()
                                        .removeWorkbenchListener(wbListener);
                                wbListener = null;
                            }
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
        }
        return manager;
    }

    synchronized public void closeManager() {
        if (manager != null) {
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
            sessionId = venuIdToSessionId(session.getVenue().getInfo()
                    .getVenueID());
            // TODO throw an exception if unable to make connection?
            if (session.isConnected()) {
                sessionsMap.put(sessionId, session);
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

    public String joinCollaborationSession(String venueName, String sessionId) {
        if (sessionsMap.get(sessionId) == null) {

            IVenueSession session = null;
            try {
                session = getSessionManager().joinCollaborationVenue(venueName);
                sessionsMap.put(sessionId, session);
            } catch (CollaborationException e) {
                // TODO Auto-generated catch block. Please revise as
                // appropriate.
                statusHandler.handle(Priority.PROBLEM, e.getLocalizedMessage(),
                        e);
            }
        }
        return sessionId;
    }
}
