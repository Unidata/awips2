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
package com.raytheon.uf.viz.collaboration.ui.role;

import java.util.Collection;
import java.util.Iterator;

import com.google.common.eventbus.Subscribe;
import com.raytheon.uf.viz.collaboration.comm.identity.ISharedDisplaySession;
import com.raytheon.uf.viz.collaboration.comm.identity.user.ParticipantRole;
import com.raytheon.uf.viz.collaboration.comm.provider.TransferRoleCommand;
import com.raytheon.uf.viz.collaboration.data.CollaborationDataManager;
import com.raytheon.uf.viz.collaboration.data.DataUser;

/**
 * Handles the events of a session that are common to all collaborators
 * regardless of role.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Apr 5, 2012            njensen     Initial creation
 * 
 * </pre>
 * 
 * @author njensen
 * @version 1.0
 */

public class RoleAgnosticEventController extends AbstractRoleEventController {

    protected RoleAgnosticEventController(ISharedDisplaySession session) {
        super(session);
    }

    @Subscribe
    public void roleTransferred(TransferRoleCommand command) {
        // update the user data for this change
        String oldUserName = null;
        ParticipantRole changedRole = command.getRole();
        if (changedRole.equals(ParticipantRole.DATA_PROVIDER)) {
            oldUserName = session.getCurrentDataProvider().getFQName();
        } else if (changedRole.equals(ParticipantRole.SESSION_LEADER)) {
            oldUserName = session.getCurrentDataProvider().getFQName();
        }
        DataUser oldUser = CollaborationDataManager.getInstance().getUser(
                oldUserName);
        oldUser.removeSessionRole(session.getSessionId(), changedRole);
        DataUser newUser = CollaborationDataManager.getInstance().getUser(
                command.getUser());
        newUser.addSessionRole(session.getSessionId(), changedRole);

        // shut down the role specific events if applicable
        if (session.getUserID().getFQName().equals(oldUserName)) {
            Collection<IRoleEventController> list = CollaborationDataManager
                    .getInstance().getEventControllers(session.getSessionId());
            Iterator<IRoleEventController> itr = list.iterator();
            while (itr.hasNext()) {
                IRoleEventController rc = itr.next();
                if (changedRole == ParticipantRole.SESSION_LEADER
                        && rc instanceof SessionLeaderEventController) {
                    rc.shutdown();
                    itr.remove();
                } else if (changedRole == ParticipantRole.DATA_PROVIDER
                        && rc instanceof DataProviderEventController) {
                    rc.shutdown();
                    itr.remove();
                }
            }
        }

        // start up new role if applicable
        if (session.getUserID().getFQName().equals(command.getUser())) {
            Collection<IRoleEventController> list = CollaborationDataManager
                    .getInstance().getEventControllers(session.getSessionId());
            if (changedRole.equals(ParticipantRole.SESSION_LEADER)) {
                SessionLeaderEventController slec = new SessionLeaderEventController(
                        session);
                slec.startup();
                list.add(slec);
            } else if (changedRole.equals(ParticipantRole.DATA_PROVIDER)) {
                DataProviderEventController dpec = new DataProviderEventController(
                        session);
                dpec.startup();
                list.add(dpec);
            }
        }
    }

}
