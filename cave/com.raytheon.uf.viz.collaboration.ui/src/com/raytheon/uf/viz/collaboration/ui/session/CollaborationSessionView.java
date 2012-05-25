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

import org.eclipse.ecf.presence.roster.IRosterEntry;
import org.eclipse.jface.action.Action;
import org.eclipse.jface.action.IMenuManager;
import org.eclipse.jface.action.Separator;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.swt.SWT;
import org.eclipse.swt.graphics.RGB;
import org.eclipse.swt.widgets.ColorDialog;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Label;

import com.google.common.eventbus.Subscribe;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.viz.collaboration.comm.identity.CollaborationException;
import com.raytheon.uf.viz.collaboration.comm.identity.ISharedDisplaySession;
import com.raytheon.uf.viz.collaboration.comm.identity.IVenueSession;
import com.raytheon.uf.viz.collaboration.comm.identity.info.IVenueInfo;
import com.raytheon.uf.viz.collaboration.comm.identity.user.SharedDisplayRole;
import com.raytheon.uf.viz.collaboration.comm.provider.TransferRoleCommand;
import com.raytheon.uf.viz.collaboration.comm.provider.user.IDConverter;
import com.raytheon.uf.viz.collaboration.comm.provider.user.UserId;
import com.raytheon.uf.viz.collaboration.data.CollaborationDataManager;
import com.raytheon.uf.viz.collaboration.data.SharedDisplaySessionMgr;
import com.raytheon.uf.viz.collaboration.ui.ColorChangeEvent;
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

    private Action colorChangeAction;

    private ISharedDisplaySession session;

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.viz.collaboration.ui.session.SessionView#createPartControl
     * (org.eclipse.swt.widgets.Composite)
     */
    @Override
    public void createPartControl(Composite parent) {
        super.createPartControl(parent);
        SharedDisplaySessionMgr.getSessionContainer(sessionId).getSession()
                .getEventPublisher().register(this);
    }

    protected void createActions() {
        super.createActions();

        colorChangeAction = new Action("Change Color...") {
            @Override
            public void run() {
                ColorDialog dlg = new ColorDialog(Display.getCurrent()
                        .getActiveShell());
                RGB rgb = dlg.open();
                IStructuredSelection selection = (IStructuredSelection) usersTable
                        .getSelection();
                IRosterEntry entry = (IRosterEntry) selection.getFirstElement();
                ColorChangeEvent event = new ColorChangeEvent(
                        IDConverter.convertFrom(entry.getUser()), rgb);
                try {
                    session.sendObjectToVenue(event);
                } catch (CollaborationException e) {
                    statusHandler.handle(Priority.PROBLEM,
                            "Unable to send color change to venue", e);
                }
            }
        };
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.viz.collaboration.ui.session.SessionView#initColorManager
     * ()
     */
    @Override
    protected void initColorManager() {
        manager = SharedDisplaySessionMgr.getSessionContainer(sessionId)
                .getColorManager();
    }

    @Subscribe
    public void refreshAfterTransfer(TransferRoleCommand command) {
        VizApp.runAsync(new Runnable() {
            @Override
            public void run() {
                usersTable.refresh();
            }
        });
    }

    @Override
    protected String getSessionImageName() {
        return COLLABORATION_SESSION_IMAGE_NAME;
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
                UserId id = CollaborationDataManager.getInstance()
                        .getCollaborationConnection(true).getUser();
                appendMessage(id, System.currentTimeMillis(), message);
                ((IVenueSession) session).sendChatMessage(message);
            } catch (CollaborationException e) {
                statusHandler.handle(Priority.ERROR,
                        "Unable to send chat message", e);
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
        if (session.hasRole(SharedDisplayRole.DATA_PROVIDER)
                || session.hasRole(SharedDisplayRole.SESSION_LEADER)) {
            if (session.hasRole(SharedDisplayRole.SESSION_LEADER)) {
                manager.add(new Separator());
                manager.add(colorChangeAction);
            }
        }
    }

    @Subscribe
    public void modifyColors(ColorChangeEvent event) {
        SharedDisplaySessionMgr.getSessionContainer(sessionId)
                .getColorManager()
                .setColorForUser(event.getUserName(), event.getColor());
        VizApp.runAsync(new Runnable() {
            @Override
            public void run() {
                usersTable.refresh();
            }
        });
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

    public String getSessionId() {
        return session.getSessionId();
    }

    @Override
    public void dispose() {
        SharedDisplaySessionMgr.exitSession(session.getSessionId());
        super.dispose();
    }
}
