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

import org.eclipse.swt.SWT;
import org.eclipse.swt.widgets.Event;

import com.google.common.eventbus.Subscribe;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.viz.collaboration.comm.identity.CollaborationException;
import com.raytheon.uf.viz.collaboration.comm.identity.ISharedDisplaySession;
import com.raytheon.uf.viz.collaboration.comm.identity.event.ParticipantEventType;
import com.raytheon.uf.viz.collaboration.comm.identity.user.ParticipantRole;
import com.raytheon.uf.viz.collaboration.comm.provider.TransferRoleCommand;
import com.raytheon.uf.viz.collaboration.comm.provider.event.VenueParticipantEvent;
import com.raytheon.uf.viz.collaboration.data.SharedDisplaySessionMgr;
import com.raytheon.uf.viz.collaboration.ui.editor.EditorSetup;
import com.raytheon.uf.viz.collaboration.ui.editor.SharedEditorData;
import com.raytheon.uf.viz.collaboration.ui.editor.SharedResource;
import com.raytheon.uf.viz.collaboration.ui.editor.event.InputEvent;
import com.raytheon.uf.viz.collaboration.ui.rsc.CollaborationWrapperResource;
import com.raytheon.uf.viz.collaboration.ui.rsc.CollaborationWrapperResourceData;
import com.raytheon.uf.viz.core.IDisplayPane;
import com.raytheon.uf.viz.core.IDisplayPaneContainer;
import com.raytheon.uf.viz.core.drawables.ResourcePair;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.rsc.ResourceList;
import com.raytheon.uf.viz.core.rsc.ResourceList.AddListener;
import com.raytheon.uf.viz.core.rsc.ResourceList.RemoveListener;
import com.raytheon.uf.viz.remote.graphics.AbstractRemoteGraphicsEvent;
import com.raytheon.uf.viz.remote.graphics.Dispatcher;
import com.raytheon.uf.viz.remote.graphics.DispatcherFactory;
import com.raytheon.uf.viz.remote.graphics.DispatchingGraphicsFactory;
import com.raytheon.viz.ui.EditorUtil;
import com.raytheon.viz.ui.editor.AbstractEditor;

/**
 * Handles the events of a session that are specific to the Data Provider role.
 * 
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Mar 26, 2012            njensen     Initial creation
 * 
 * </pre>
 * 
 * @author njensen
 * @version 1.0
 */

public class DataProviderEventController extends AbstractRoleEventController {

    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(DataProviderEventController.class);

    public DataProviderEventController(ISharedDisplaySession session) {
        super(session);
    }

    @Subscribe
    public void participantChanged(VenueParticipantEvent event) {
        if (event.getEventType().equals(ParticipantEventType.ARRIVED)) {
            // TODO this seems to trigger when you create the room, in which
            // case you don't need to send it for yourself
            // TODO instead of going to active editor, should get ones
            // specifically shared with this session
            AbstractEditor editor = EditorUtil
                    .getActiveEditorAs(AbstractEditor.class);
            SharedEditorData se = EditorSetup.extractSharedEditorData(editor);
            try {
                session.sendObjectToPeer(event.getParticipant()
                        .getQualifiedId(), se);
            } catch (CollaborationException e) {
                statusHandler.handle(Priority.PROBLEM,
                        "Error sending initialization data to new participant "
                                + event.getParticipant().getName(), e);
            }
        }
    }

    @Subscribe
    public void roleTransferred(TransferRoleCommand cmd) {
        if (cmd.getRole() == ParticipantRole.SESSION_LEADER) {
            System.out.println("Current session's username: "
                    + session.getUserID().getFQName());
            System.out.println("Command received username: "
                    + cmd.getUser().getFQName());
            if (cmd.getUser().getFQName()
                    .equals(session.getUserID().getFQName())) {
                // this cave should assume session leader control
                InputUtil.enableDataProviderInput(session.getSessionId());
            } else if (session.getCurrentSessionLeader().getFQName()
                    .equals(session.getUserID().getFQName())
                    && !session.getCurrentSessionLeader().getFQName()
                            .equals(cmd.getUser().getFQName())) {
                // this cave should release session leader control
                InputUtil.disableDataProviderInput(session.getSessionId());
            }
        }
    }

    @Subscribe
    public void sessionLeaderInput(InputEvent event) {
        // TODO needs to be based on the editor that is both shared and active
        AbstractEditor editor = SharedDisplaySessionMgr
                .getSessionContainer(session.getSessionId()).getSharedEditors()
                .get(0);
        IDisplayPane pane = editor.getDisplayPanes()[0];
        Event swtEvent = new Event();

        // translate event type
        switch (event.getType()) {
        case MOUSE_DOWN:
            swtEvent.type = SWT.MouseDown;
            break;
        case MOUSE_UP:
            swtEvent.type = SWT.MouseUp;
            break;
        case MOUSE_DOWN_MOVE:
        case MOUSE_MOVE:
            swtEvent.type = SWT.MouseMove;
            break;
        case DOUBLE_CLICK:
            swtEvent.type = SWT.MouseDoubleClick;
            break;
        case MOUSE_HOVER:
            swtEvent.type = SWT.MouseHover;
            break;
        case MOUSE_WHEEL:
            swtEvent.type = SWT.MouseWheel;
            break;
        case KEY_DOWN:
            swtEvent.type = SWT.KeyDown;
            break;
        case KEY_UP:
            swtEvent.type = SWT.KeyUp;
            break;
        }

        // translate coordinates of event
        switch (event.getType()) {
        case MOUSE_DOWN:
        case MOUSE_DOWN_MOVE:
        case MOUSE_UP:
        case MOUSE_HOVER:
        case MOUSE_MOVE:
        case MOUSE_WHEEL:
        case DOUBLE_CLICK:
            double[] screen = pane.gridToScreen(new double[] { event.getX(),
                    event.getY() });
            swtEvent.x = (int) Math.round(screen[0]);
            swtEvent.y = (int) Math.round(screen[1]);
            break;
        }

        // translate specific metadata
        switch (event.getType()) {
        case MOUSE_DOWN:
        case MOUSE_DOWN_MOVE:
        case MOUSE_UP:
        case DOUBLE_CLICK:
            swtEvent.button = event.getEventData();
            break;
        case MOUSE_WHEEL:
            swtEvent.count = event.getEventData();
            break;
        case KEY_DOWN:
        case KEY_UP:
            swtEvent.keyCode = event.getEventData();
            break;
        }

        editor.getMouseManager().handleEvent(swtEvent);
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.viz.collaboration.ui.role.AbstractRoleEventController
     * #startup()
     */
    @Override
    public void startup() {
        super.startup();
        super.activateTelestrator();
        for (IDisplayPaneContainer container : SharedDisplaySessionMgr
                .getSessionContainer(session.getSessionId()).getSharedEditors()) {
            // Replace pane resources that will be shared with
            // CollaborationWrapperResource objects
            for (IDisplayPane pane : container.getDisplayPanes()) {
                ResourceList list = pane.getDescriptor().getResourceList();
                for (ResourcePair rp : pane.getDescriptor().getResourceList()) {
                    wrapResourcePair(rp);
                }
                list.addPreAddListener(new AddListener() {
                    @Override
                    public void notifyAdd(ResourcePair rp) throws VizException {
                        if (wrapResourcePair(rp)) {
                            // Send event to venue to load
                            sendSharedResource(rp, false);
                        }
                    }
                });
                list.addPostRemoveListener(new RemoveListener() {
                    @Override
                    public void notifyRemove(ResourcePair rp)
                            throws VizException {
                        if (rp.getResource() instanceof CollaborationWrapperResource) {
                            // Send event to venue to unload
                            sendSharedResource(rp, true);
                        }
                    }
                });
            }

            // Inject remote graphics functionality in container
            DispatchingGraphicsFactory.injectRemoteFunctionality(container,
                    new DispatcherFactory() {
                        @Override
                        public Dispatcher createNewDispatcher() {
                            Dispatcher dispatcher = new Dispatcher() {
                                @Override
                                public void dispatch(
                                        AbstractRemoteGraphicsEvent eventObject) {
                                    try {
                                        session.sendObjectToVenue(eventObject);
                                    } catch (CollaborationException e) {
                                        statusHandler.handle(Priority.PROBLEM,
                                                e.getLocalizedMessage(), e);
                                    }
                                }
                            };
                            return dispatcher;
                        }
                    });
            try {
                EditorSetup.shareEditor(this.session.getSessionId(),
                        (AbstractEditor) container);
            } catch (CollaborationException e) {
                // TODO Auto-generated catch block. Please revise as
                // appropriate.
                statusHandler.handle(Priority.PROBLEM, e.getLocalizedMessage(),
                        e);
            }
        }
    }

    private void sendSharedResource(ResourcePair rp, boolean remove) {
        // Send event to venue to load resource
        SharedResource sr = new SharedResource();
        ResourcePair copy = new ResourcePair();
        copy.setLoadProperties(rp.getLoadProperties());
        copy.setProperties(rp.getProperties());
        if (rp.getResourceData() instanceof CollaborationWrapperResourceData) {
            copy.setResourceData(((CollaborationWrapperResourceData) rp
                    .getResourceData()).getWrappedResourceData());
        } else if (rp.getResource() instanceof CollaborationWrapperResource) {
            copy.setResourceData(rp.getResource().getResourceData());
        }
        sr.setResource(copy);
        sr.setRemoveResource(remove);
        try {
            session.sendObjectToVenue(sr);
        } catch (CollaborationException e) {
            statusHandler.handle(Priority.PROBLEM, e.getLocalizedMessage(), e);
        }
    }

    /**
     * Wraps ResourcePair in collaboration wrapper resource if resource should
     * be loaded on locally for every user in venue
     * 
     * @param rp
     * @return true if ResourcePair was wrapped, false otherwise
     */
    private boolean wrapResourcePair(ResourcePair rp) {
        if (rp.getProperties() != null && rp.getProperties().isMapLayer()
                && (rp.getResource() != null || rp.getResourceData() != null)) {
            CollaborationWrapperResourceData wrapperRscData = new CollaborationWrapperResourceData();
            if (rp.getResource() != null) {
                wrapperRscData.setWrappedResourceData(rp.getResource()
                        .getResourceData());
                rp.setResource(new CollaborationWrapperResource(wrapperRscData,
                        rp.getLoadProperties(), rp.getResource()));
            } else {
                wrapperRscData.setWrappedResourceData(rp.getResourceData());
            }

            if (rp.getResourceData() != null) {
                rp.setResourceData(wrapperRscData);
            }
            return true;
        }
        return false;
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.viz.collaboration.ui.role.AbstractRoleEventController
     * #shutdown()
     */
    @Override
    public void shutdown() {
        super.shutdown();
        super.deactivateTelestrator();
        // TODO should remove the SharedEditorIndiciatorRsc
    }

}
