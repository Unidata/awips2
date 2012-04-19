package com.raytheon.uf.viz.collaboration.ui.session;

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

import org.eclipse.jface.action.IMenuManager;
import org.eclipse.swt.custom.SashForm;
import org.eclipse.swt.widgets.Composite;

import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.viz.collaboration.comm.identity.CollaborationException;
import com.raytheon.uf.viz.collaboration.comm.identity.IPeerToPeer;
import com.raytheon.uf.viz.collaboration.comm.identity.listener.IMessageListener;
import com.raytheon.uf.viz.collaboration.comm.identity.user.IQualifiedID;
import com.raytheon.uf.viz.collaboration.data.CollaborationDataManager;

/**
 * TODO Add Description
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
public class PeerToPeerView extends AbstractSessionView {
    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(PeerToPeerView.class);

    private static final String PEER_TO_PEER_IMAGE_NAME = "chats.gif";

    public static final String ID = "com.raytheon.uf.viz.collaboration.PeerToPeerView";

    protected IMessageListener messageListener;

    private IQualifiedID peer;

    public PeerToPeerView() {
        super();
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.uf.viz.collaboration.ui.session.AbstractSessionView#
     * populateSashForm(org.eclipse.swt.custom.SashForm)
     */
    protected void populateSashForm(SashForm sashForm) {
        super.populateSashForm(sashForm);
        sashForm.setWeights(new int[] { 20, 5 });
    }

    protected void createActions() {
        // TODO create peer-to-peer chat action here
        // chatAction = new Action("Chat") {
        // @Override
        // public void run() {
        // try {
        // CollaborationDataManager dataManager = CollaborationDataManager
        // .getInstance();
        // CollaborationUser user = (CollaborationUser) ((IStructuredSelection)
        // usersTable
        // .getSelection()).getFirstElement();
        // String session = dataManager.createCollaborationSession(
        // user.getId(), "Chatting...");
        // PlatformUI
        // .getWorkbench()
        // .getActiveWorkbenchWindow()
        // .getActivePage()
        // .showView(CollaborationSessionView.ID, session,
        // IWorkbenchPage.VIEW_ACTIVATE);
        // // }
        // } catch (PartInitException e) {
        // statusHandler.handle(Priority.PROBLEM,
        // "Unable to open chat", e);
        // }
        // }
        // };
    }

    // /**
    // *
    // */
    // private void createContextMenu() {
    // MenuManager menuManager = new MenuManager();
    // menuManager.setRemoveAllWhenShown(true);
    // menuManager.addMenuListener(new IMenuListener() {
    // /*
    // * (non-Javadoc)
    // *
    // * @see
    // * org.eclipse.jface.action.IMenuListener#menuAboutToShow(org.eclipse
    // * .jface.action.IMenuManager)
    // */
    // @Override
    // public void menuAboutToShow(IMenuManager manager) {
    // fillContextMenu(manager);
    // }
    // });
    // // Menu menu = menuManager.createContextMenu(usersTable.getControl());
    // // usersTable.getControl().setMenu(menu);
    // // PlatformUI.getWorkbench().getActiveWorkbenchWindow().getActivePage()
    // // .getActivePart().getSite()
    // // .registerContextMenu(menuManager, usersTable);
    // // usersTable.getTable().setMenu(menu);
    // }

    protected void fillContextMenu(IMenuManager manager) {
        // IStructuredSelection selection = (IStructuredSelection) usersTable
        // .getSelection();
        // do something here!
        // Object ob = selection.getFirstElement();
        // System.out.println(ob.toString());
        // manager.add(chatAction);
        // manager.add(new Separator());
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.uf.viz.collaboration.ui.session.AbstractSessionView#
     * setMessageLabel(org.eclipse.swt.widgets.Label)
     */
    protected void setMessageLabel(Composite comp) {
        // no message needed as there is no subject and we know that it is
        // private based on the fact that there are no participants
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.viz.collaboration.ui.session.AbstractSessionView#sendMessage
     * ()
     */
    public void sendMessage() {
        String message = getComposedMessage();
        if (message.length() > 0) {
            try {
                CollaborationDataManager manager = CollaborationDataManager
                        .getInstance();
                IPeerToPeer p2p = (IPeerToPeer) manager.getSessionManager()
                        .getPeerToPeerSession();
                p2p.sendPeerToPeer(peer, message);
                appendMessage(manager.getLoginId(), null,
                        System.currentTimeMillis(), message);
            } catch (CollaborationException e) {
                // TODO Auto-generated catch block. Please revise as
                // appropriate.
                statusHandler.handle(Priority.PROBLEM, e.getLocalizedMessage(),
                        e);
            }
        }
    }

    protected String getSessionImageName() {
        return PEER_TO_PEER_IMAGE_NAME;
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.uf.viz.collaboration.ui.session.AbstractSessionView#
     * getSessionName()
     */
    @Override
    protected String getSessionName() {
        return getViewSite().getSecondaryId();
    }

    public void setPeer(IQualifiedID peer) {
        this.peer = peer;
    }

    public IQualifiedID getPeer() {
        return peer;
    }
}
