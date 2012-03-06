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

import org.eclipse.swt.widgets.Display;

import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.viz.collaboration.comm.SessionManager;
import com.raytheon.uf.viz.collaboration.comm.identity.ISession;
import com.raytheon.uf.viz.collaboration.comm.identity.info.IVenueInfo;
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

/**
 * TODO Add Description
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Mar 5, 2012            rferrel     Initial creation
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
    SessionManager manager;

    String loginId;

    /**
     * User information such as sessions and groups user is in.
     */
    Map<String, DataUser> usersMap;

    /**
     * Mapping for all active chat sessions.
     */
    Map<String, ISession> sessionsMap;

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
        usersMap = new HashMap<String, DataUser>();
        sessionsMap = new HashMap<String, ISession>();
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
    public SessionManager getSessionManager() {
        if (manager == null) {
            VizApp.runSync(new Runnable() {

                @Override
                public void run() {
                    LoginDialog dlg = new LoginDialog(Display.getDefault()
                            .getActiveShell());
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
                        } catch (Exception e) {
                            // TODO Auto-generated catch block. Please
                            // revise as appropriate.
                            statusHandler.handle(Priority.PROBLEM,
                                    e.getLocalizedMessage(), e);
                        }
                    }
                    if (manager != null) {
                        loginId = loginData.getAccount();
                        DataUser user = CollaborationDataManager.getInstance()
                                .getUser(loginId);
                        user.status = loginData.getStatus();
                        user.statusMessage = loginData.getMessage();
                    }
                }
            });

            Collection<IVenueInfo> info = manager.getVenueInfo();
            for (IVenueInfo i : info) {
                System.out.println(i);
            }
        }
        return manager;
    }

    /**
     * @param sessionId
     *            - key to fetch session
     * @return session
     */
    public ISession getSession(String sessionId) {
        return sessionsMap.get(sessionId);
    }

    private int colRm = 0;

    /**
     * @return sessionId
     */
    public String createCollaborationSession() {
        SessionManager manager = getSessionManager();
        ISession session = manager
                .createCollaborationSession(SessionManager.SESSION_COLLABORATION);
        // TODO get unique venue name from server
        session.createVenue("collaboration-" + ++colRm);
        String sessionId = session.getVenue().getInfo().getVenueID()
                .replace(':', ';');
        // TODO throw an exception if unable to make connection?
        if (session == null || session.isConnected() == false) {
            return null;
        }
        sessionsMap.put(sessionId, session);
        return sessionId;
    }
}
