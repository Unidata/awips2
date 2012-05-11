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
package com.raytheon.uf.viz.collaboration.ui.telestrator;

import org.eclipse.swt.SWT;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.swt.widgets.ToolItem;

import com.raytheon.uf.viz.collaboration.comm.identity.ISharedDisplaySession;
import com.raytheon.uf.viz.collaboration.comm.identity.user.SharedDisplayRole;
import com.raytheon.uf.viz.collaboration.data.CollaborationDataManager;
import com.raytheon.uf.viz.collaboration.data.SharedDisplaySessionMgr;
import com.raytheon.uf.viz.collaboration.ui.Activator;
import com.raytheon.uf.viz.collaboration.ui.telestrator.event.CollaborationDrawingEvent;
import com.raytheon.uf.viz.collaboration.ui.telestrator.event.CollaborationDrawingEvent.CollaborationEventType;
import com.raytheon.uf.viz.core.VizApp;
import com.raytheon.uf.viz.core.icon.IconUtil;
import com.raytheon.uf.viz.drawing.DrawingLayer;
import com.raytheon.uf.viz.drawing.PathToolbar;
import com.raytheon.uf.viz.drawing.events.DrawingEvent;
import com.raytheon.uf.viz.drawing.events.DrawingEventBus;

/**
 * Extends the toolbar and adds the "Leader Only" button
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Apr 3, 2012            mnash     Initial creation
 * 
 * </pre>
 * 
 * @author mnash
 * @version 1.0
 */

public class CollaborationPathToolbar extends PathToolbar {
    private ToolItem leaderOnly;

    /**
     * @param parentShell
     */
    protected CollaborationPathToolbar(Shell parentShell) {
        super(parentShell);
        setText("Collaboration Drawing");
    }

    public static PathToolbar getToolbar() {
        if (pathToolbar == null) {
            pathToolbar = new CollaborationPathToolbar(new Shell(
                    Display.getCurrent()));
            DrawingEventBus.register(PathToolbar.getToolbar());
        }
        return pathToolbar;
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.viz.drawing.PathToolbar#initializeComponents(org.eclipse
     * .swt.widgets.Shell)
     */
    @Override
    protected void initializeComponents(Shell shell) {
        // allows for subclasses to add more items to the toolbar, in this case
        // allowing the user to turn off other collaborator drawings
        super.initializeComponents(shell);
        createLeaderItem();
    }

    @Override
    public void handleMessage(final DrawingEvent event) {
        VizApp.runAsync(new Runnable() {
            @Override
            public void run() {
                if (event instanceof CollaborationDrawingEvent) {
                    CollaborationDrawingEvent cde = (CollaborationDrawingEvent) event;
                    if (!cde.getUserName()
                            .equals(CollaborationDataManager.getInstance()
                                    .getCollaborationConnection(true).getUser())) {
                        if (cde.getType() == CollaborationEventType.DISABLE) {
                            disableAll();
                        }
                    }
                }
                CollaborationPathToolbar.super.handleMessage(event);
            }
        });
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.uf.viz.drawing.PathToolbar#updateToolbar()
     */
    @Override
    public void updateToolbar() {
        super.updateToolbar();
        // for non-leaders, need to disable/remove the leaderOnly button
        DrawingLayer resource = getDrawingResource();
        if (resource.getResourceData() instanceof CollaborationPathDrawingResourceData) {
            String sessionId = ((CollaborationPathDrawingResourceData) resource
                    .getResourceData()).getSessionId();
            ISharedDisplaySession session = SharedDisplaySessionMgr
                    .getSessionContainer(sessionId).getSession();
            if (session != null
                    && !session.hasRole(SharedDisplayRole.SESSION_LEADER)
                    && leaderOnly != null && !leaderOnly.isDisposed()) {
                leaderOnly.setEnabled(false);
            }
        }
    }

    private void createLeaderItem() {
        leaderOnly = new ToolItem(toolbar, SWT.CHECK);
        leaderOnly.setText("Lock Collaborators");
        leaderOnly.setImage(IconUtil.getImageDescriptor(
                Activator.getDefault().getBundle(), "multiple_draw.gif")
                .createImage());
        leaderOnly.setSelection(false);
        leaderOnly.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent e) {
                DrawingLayer layer = getDrawingResource();
                if (layer instanceof CollaborationDrawingLayer) {
                    CollaborationDrawingLayer dLayer = (CollaborationDrawingLayer) layer;
                    dLayer.sendDisableOthers();
                }
                if (leaderOnly.getSelection()) {
                    lastTool.activate();
                } else {
                    lastTool.deactivate();
                }
            }
        });
    }

    public void disableAll() {
        if (toolbar != null && !toolbar.isDisposed()) {
            toolbar.setEnabled(!toolbar.getEnabled());
        }
    }

}
