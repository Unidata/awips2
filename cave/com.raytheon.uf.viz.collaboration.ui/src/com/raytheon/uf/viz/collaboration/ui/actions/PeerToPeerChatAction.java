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
package com.raytheon.uf.viz.collaboration.ui.actions;

import org.eclipse.jface.action.Action;
import org.eclipse.ui.IWorkbenchPage;
import org.eclipse.ui.PartInitException;
import org.jivesoftware.smack.packet.Presence;
import org.jivesoftware.smack.packet.Presence.Type;

import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.viz.collaboration.comm.provider.session.CollaborationConnection;
import com.raytheon.uf.viz.collaboration.comm.provider.user.UserId;
import com.raytheon.uf.viz.collaboration.ui.Activator;
import com.raytheon.uf.viz.collaboration.ui.session.PeerToPeerView;
import com.raytheon.uf.viz.core.icon.IconUtil;
import com.raytheon.viz.ui.views.CaveWorkbenchPageManager;

/**
 * Create a new chat session with a user
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jul 3, 2012            bsteffen     Initial creation
 * 
 * </pre>
 * 
 * @author bsteffen
 * @version 1.0
 */

public class PeerToPeerChatAction extends Action {

    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(PeerToPeerChatAction.class);

    private final UserId user;

    public PeerToPeerChatAction(UserId user) {
        super("Chat", IconUtil.getImageDescriptor(Activator.getDefault()
                .getBundle(), "chats.gif"));
        this.user = user;
        updateEnabled();
    }

    @Override
    public void run() {
        Presence presence = CollaborationConnection.getConnection()
                .getContactsManager().getPresence(user);
        if (presence.getType() != Type.unavailable) {
            UserId loginUserId = CollaborationConnection.getConnection()
                    .getUser();
            if (!loginUserId.equals(user)) {
                createP2PChat(IWorkbenchPage.VIEW_ACTIVATE);
            }
        }
    }

    /**
     * Set the enabled status of this action to be determined based off what
     * users are available.
     */
    public void updateEnabled() {
        boolean enabled = false;
        Presence presence = CollaborationConnection.getConnection()
                .getContactsManager().getPresence(user);
        if (presence.getType() != Type.unavailable) {
            UserId loginUserId = CollaborationConnection.getConnection()
                    .getUser();
            if (!loginUserId.getName().equals(user.getName())) {
                enabled = true;
            }
        }
        setEnabled(enabled);
    }

    /**
     * For creating a peer to peer chat, or adding to it.
     * 
     * @param viewMode
     *            IWorkbenchPage.VIEW_CREATE or IWorkbenchPage.VIEW_ACTIVATE
     *            where create will not pop up the view in front of others and
     *            activate will.
     * @return
     */
    public PeerToPeerView createP2PChat(Integer viewMode) {
        try {
            String name = user.getName();
            PeerToPeerView p2pView = (PeerToPeerView) CaveWorkbenchPageManager
                    .getActiveInstance().showView(PeerToPeerView.ID, name,
                            viewMode);
            if (p2pView.getPeer() == null) {
                p2pView.setPeer(user);
            }
            return p2pView;
        } catch (PartInitException e) {
            statusHandler.handle(Priority.PROBLEM, "Unable to open chat", e);
        }
        return null;
    }

}
