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

import java.util.Collection;

import org.eclipse.jface.action.Action;
import org.eclipse.jface.action.ActionContributionItem;
import org.eclipse.jface.action.IMenuCreator;
import org.eclipse.jface.action.IMenuManager;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.swt.SWT;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Menu;
import org.eclipse.ui.IWorkbenchPart;

import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.viz.collaboration.comm.identity.CollaborationException;
import com.raytheon.uf.viz.collaboration.comm.identity.ISharedDisplaySession;
import com.raytheon.uf.viz.collaboration.comm.identity.IVenueSession;
import com.raytheon.uf.viz.collaboration.comm.identity.info.IVenueInfo;
import com.raytheon.uf.viz.collaboration.comm.identity.user.ParticipantRole;
import com.raytheon.uf.viz.collaboration.comm.provider.Tools;
import com.raytheon.uf.viz.collaboration.comm.provider.TransferRoleCommand;
import com.raytheon.uf.viz.collaboration.comm.provider.user.VenueParticipant;
import com.raytheon.uf.viz.collaboration.data.CollaborationDataManager;
import com.raytheon.uf.viz.collaboration.data.CollaborationUser;

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
public class CollaborationSessionView extends SessionView {
    public static final String ID = "com.raytheon.uf.viz.collaboration.CollaborationSession";

    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(CollaborationSessionView.class);

    private static final String COLLABORATION_SESSION_IMAGE_NAME = "messages.gif";

    private Action switchToAction;

    private ISharedDisplaySession session;

    protected void createActions() {
        super.createActions();
        switchToAction = new Action("Transfer Role...",
                Action.AS_DROP_DOWN_MENU) {
            public void run() {
                // do nothing
            };
        };

        IMenuCreator creator = new IMenuCreator() {
            Menu menu;

            @Override
            public Menu getMenu(Menu parent) {
                if (menu == null || menu.isDisposed()) {
                    menu = new Menu(parent);
                }
                if (session.hasRole(ParticipantRole.SESSION_LEADER)) {
                    Action leaderAction = new Action("Session Leader") {
                        public void run() {
                            IStructuredSelection selection = (IStructuredSelection) usersTable
                                    .getSelection();
                            CollaborationUser selectedUser = (CollaborationUser) selection
                                    .getFirstElement();
                            switchLeader(selectedUser.getId());
                        };
                    };
                    ActionContributionItem leaderItem = new ActionContributionItem(
                            leaderAction);
                    leaderItem.fill(menu, -1);
                }

                if (session.hasRole(ParticipantRole.DATA_PROVIDER)) {
                    Action dataProviderAction = new Action("Data Provider") {
                        public void run() {
                            IStructuredSelection selection = (IStructuredSelection) usersTable
                                    .getSelection();
                            CollaborationUser selectedUser = (CollaborationUser) selection
                                    .getFirstElement();
                            switchDataProvider(selectedUser.getId());
                        };
                    };
                    ActionContributionItem dataProviderItem = new ActionContributionItem(
                            dataProviderAction);
                    dataProviderItem.fill(menu, -1);
                }
                return menu;
            }

            @Override
            public void dispose() {
                menu.dispose();
            }

            @Override
            public Menu getMenu(Control parent) {
                return getMenu(parent.getMenu());
            }
        };
        switchToAction.setMenuCreator(creator);
    }

    private void switchDataProvider(String fqname) {
        System.out.println("Send switchDataProvider request. " + fqname);
        // TODO need to send invite/request for transfer, and then if successful
        // deactivate the local ones since we won't receive the message
    }

    private void switchLeader(String fqname) {
        System.out.println("Send switchLeader request. " + fqname);
        // TODO need to send invite/request for transfer, and then if successful
        // deactivate the local ones since we won't receive the message
        TransferRoleCommand trc = new TransferRoleCommand();
        VenueParticipant vp = new VenueParticipant(Tools.parseName(fqname),
                Tools.parseHost(fqname));
        trc.setUser(vp);
        session.setCurrentSessionLeader(vp);
        trc.setRole(ParticipantRole.SESSION_LEADER);
        try {
            session.sendObjectToVenue(trc);
        } catch (CollaborationException e) {
            statusHandler.handle(Priority.PROBLEM,
                    "Unable to send message to transfer role", e);
        }
    }

    @Override
    protected String getSessionImageName() {
        return COLLABORATION_SESSION_IMAGE_NAME;
    }

    @Override
    protected String buildParticipantTooltip(CollaborationUser user) {
        StringBuilder builder = new StringBuilder(
                super.buildParticipantTooltip(user));
        // TODO these should be smarter ifs
        boolean isSessionLeader = Tools.parseName(user.getId()).equals(
                session.getCurrentSessionLeader().getName());
        boolean isDataProvider = Tools.parseName(user.getId()).equals(
                session.getCurrentDataProvider().getName());
        if (isSessionLeader || isDataProvider) {
            builder.append("\n-- Roles --");
            if (isSessionLeader) {
                builder.append("\nSession Leader");
            }
            if (isDataProvider) {
                builder.append("\nData Provider");
            }
        }

        return builder.toString();
    }

    @Override
    protected Collection<String> findAlertWords(StringBuilder builder,
            int offset) {
        // TODO
        // 1) if needed read in localized list of key words/sounds
        // 2) search builder starting at offset for key works. besides the key
        // words found may also want to return the location(s) of where they are
        // found along with the sound to used.
        return super.findAlertWords(builder, offset);
    }

    @Override
    protected void executeSightsSounds() {
        // TODO From the alert words found determine what sound to play and for
        // how long?
        super.executeSightsSounds();
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.viz.collaboration.ui.session.SessionView#sendMessage()
     */
    @Override
    public void sendMessage() {
        String message = getComposedMessage();
        if (message.length() > 0) {
            try {
                ((IVenueSession) session).sendTextMessage(message);
            } catch (CollaborationException e) {
                // TODO Auto-generated catch block. Please revise as
                // appropriate.
                e.printStackTrace();
            }
        }
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.viz.collaboration.ui.session.SessionView#fillContextMenu
     * (org.eclipse.jface.action.IMenuManager)
     */
    @Override
    protected void fillContextMenu(IMenuManager manager) {
        super.fillContextMenu(manager);
        if (session.hasRole(ParticipantRole.DATA_PROVIDER)
                || session.hasRole(ParticipantRole.SESSION_LEADER)) {
            IStructuredSelection selection = (IStructuredSelection) usersTable
                    .getSelection();
            CollaborationUser selectedUser = (CollaborationUser) selection
                    .getFirstElement();
            String selectedUserName = Tools.parseName(selectedUser.getId());
            if (!selectedUserName.equals(session.getUserID().getName())) {
                manager.add(switchToAction);
            }
        }
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.uf.viz.collaboration.ui.session.AbstractSessionView#
     * setMessageLabel(org.eclipse.swt.widgets.Label)
     */
    @Override
    protected void setMessageLabel(Composite comp) {
        Label label = new Label(comp, SWT.NONE);
        StringBuilder labelInfo = new StringBuilder();
        if (session != null) {
            IVenueInfo info = ((IVenueSession) session).getVenue().getInfo();
            labelInfo.append(info.getVenueSubject());
            label.setToolTipText(info.getVenueSubject());
        }
        label.setText(labelInfo.toString());
    }

    @Override
    protected void setSession(String sessionId) {
        super.setSession(sessionId);
        this.session = (ISharedDisplaySession) CollaborationDataManager
                .getInstance().getSession(this.sessionId);
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.uf.viz.collaboration.ui.session.AbstractSessionView#
     * partActivated(org.eclipse.ui.IWorkbenchPart)
     */
    @Override
    public void partActivated(IWorkbenchPart part) {
        super.partActivated(part);
        if (this == part) {
            CollaborationDataManager.getInstance().editorBringToTop(sessionId);
        }
    }

    @Override
    public void partBroughtToTop(IWorkbenchPart part) {
        super.partBroughtToTop(part);
    }

    @Override
    public void partDeactivated(IWorkbenchPart part) {
        super.partDeactivated(part);
    }

    @Override
    public void partOpened(IWorkbenchPart part) {
        super.partOpened(part);
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.viz.collaboration.ui.session.SessionView#partClosed(org
     * .eclipse.ui.IWorkbenchPart)
     */
    @Override
    public void partClosed(IWorkbenchPart part) {
        super.partClosed(part);
        if (part == this) {
            CollaborationDataManager.getInstance().closeEditor(sessionId);
        }
    }
}
