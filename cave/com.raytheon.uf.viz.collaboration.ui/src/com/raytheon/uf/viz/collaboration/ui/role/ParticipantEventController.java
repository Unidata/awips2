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

import com.google.common.eventbus.Subscribe;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.viz.collaboration.comm.identity.ISharedDisplaySession;
import com.raytheon.uf.viz.collaboration.comm.identity.user.SharedDisplayRole;
import com.raytheon.uf.viz.collaboration.comm.provider.TransferRoleCommand;
import com.raytheon.uf.viz.collaboration.data.SessionContainer;
import com.raytheon.uf.viz.collaboration.data.SharedDisplaySessionMgr;
import com.raytheon.uf.viz.collaboration.display.editor.CollaborationEditor;
import com.raytheon.uf.viz.collaboration.display.editor.ReprojectEditor;
import com.raytheon.uf.viz.collaboration.display.editor.SharedEditorData;
import com.raytheon.uf.viz.collaboration.ui.editor.EditorSetup;
import com.raytheon.uf.viz.collaboration.ui.editor.SharedResource;
import com.raytheon.uf.viz.collaboration.ui.rsc.CollaborationResource;
import com.raytheon.uf.viz.collaboration.ui.rsc.CollaborationResourceData;
import com.raytheon.uf.viz.core.IDisplayPane;
import com.raytheon.uf.viz.core.VizApp;
import com.raytheon.uf.viz.core.drawables.AbstractDescriptor;
import com.raytheon.uf.viz.core.drawables.IDescriptor;
import com.raytheon.uf.viz.core.drawables.ResourcePair;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.rsc.ResourceProperties;

/**
 * Handles the events of a session that are specific to the Participant role.
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

public class ParticipantEventController extends AbstractRoleEventController {

    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(ParticipantEventController.class);

    private CollaborationResource collabRsc;

    public ParticipantEventController(ISharedDisplaySession session) {
        super(session);
    }

    @Subscribe
    public void editorDataArrived(final SharedEditorData se) {
        // TODO need to detect if we already have a CollaborationEditor for
        // this session. If so, that implies DataProvider changed and we
        // should reuse the editor, reinitializing the descriptor and
        // renderable display but keeping the drawing and telestrator
        VizApp.runSync(new Runnable() {

            @Override
            public void run() {
                // initialize and open editor
                CollaborationEditor editor = EditorSetup.createEditor(se);
                String sessionId = ParticipantEventController.this.session
                        .getSessionId();
                editor.setSessionId(sessionId);
                SessionContainer container = SharedDisplaySessionMgr
                        .getSessionContainer(sessionId);
                container.setCollaborationEditor(editor);
                String title = session.getVenue().getInfo()
                        .getVenueDescription();
                editor.setTabTitle(title);
                editor.disableClose("Please close the \"" + title
                        + "\" chat to exit the session.");

                initializeResources(editor.getActiveDisplayPane()
                        .getDescriptor());

                activateResources(editor);
            }
        });
    }

    @Subscribe
    public void reprojectEditor(final ReprojectEditor event) {
        VizApp.runAsync(new Runnable() {
            @Override
            public void run() {
                CollaborationEditor editor = SharedDisplaySessionMgr
                        .getSessionContainer(session.getSessionId())
                        .getCollaborationEditor();
                for (IDisplayPane pane : editor.getDisplayPanes()) {
                    IDescriptor desc = pane.getDescriptor();
                    if (desc instanceof AbstractDescriptor) {
                        try {
                            ((AbstractDescriptor) desc).setGridGeometry(event
                                    .getTargetGeometry());
                        } catch (VizException e) {
                            statusHandler.handle(Priority.PROBLEM,
                                    "Error reprojecting collaboration display: "
                                            + e.getLocalizedMessage(), e);
                        }
                    }
                    pane.setZoomLevel(1.0);
                    pane.scaleToClientArea();
                    pane.refresh();
                }
            }
        });
    }

    @Subscribe
    public void resourceDataArrived(SharedResource sr) {
        ResourcePair rp = sr.getResource();
        // TODO: Need to tie shared resource adding to a displayId so we add it
        // to the correct editor/pane
        CollaborationEditor editor = SharedDisplaySessionMgr
                .getSessionContainer(session.getSessionId())
                .getCollaborationEditor();
        IDescriptor affectedDescriptor = editor.getActiveDisplayPane()
                .getDescriptor();
        if (sr.isRemoveResource()) {
            affectedDescriptor.getResourceList().remove(
                    convertToLocalResourcePair(rp));
        } else {
            affectedDescriptor.getResourceList().add(
                    convertToLocalResourcePair(rp));
            affectedDescriptor.getResourceList().instantiateResources(
                    affectedDescriptor, true);
        }
    }

    private ResourcePair convertToLocalResourcePair(ResourcePair rp) {
        if (rp.getProperties() != null) {
            rp.getProperties().setSystemResource(true);
        } else {
            ResourceProperties props = new ResourceProperties();
            props.setSystemResource(true);
            rp.setProperties(props);
        }
        return rp;
    }

    private void initializeResources(IDescriptor desc) {
        CollaborationResourceData crd = new CollaborationResourceData();
        crd.setSession(session);
        ResourcePair rp = ResourcePair.constructSystemResourcePair(crd);
        desc.getResourceList().add(rp);
        desc.getResourceList().instantiateResources(desc, true);
        collabRsc = (CollaborationResource) rp.getResource();
        this.session.registerEventHandler(collabRsc);
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
        SessionContainer container = SharedDisplaySessionMgr
                .getSessionContainer(session.getSessionId());
        if (container != null) {
            super.deactivateResources(container.getCollaborationEditor());
        }
        if (this.collabRsc != null) {
            this.session.unRegisterEventHandler(collabRsc);
        }
    }

    @Subscribe
    public void roleTransferred(TransferRoleCommand cmd) {
        if (cmd.getRole() == SharedDisplayRole.SESSION_LEADER) {
            session.setCurrentSessionLeader(cmd.getUser());
            if (cmd.getUser().getFQName()
                    .equals(session.getUserID().getFQName())) {
                // this cave should assume session leader control
                InputUtil.enableSessionLeaderInput(SharedDisplaySessionMgr
                        .getSessionContainer(session.getSessionId())
                        .getCollaborationEditor());
            } else if (session.getCurrentSessionLeader().getFQName()
                    .equals(session.getUserID().getFQName())
                    && !session.getCurrentSessionLeader().getFQName()
                            .equals(cmd.getUser().getFQName())) {
                // this cave should release session leader control
                InputUtil.disableSessionLeaderInput(SharedDisplaySessionMgr
                        .getSessionContainer(session.getSessionId())
                        .getCollaborationEditor());
            }
        }
    }
}
