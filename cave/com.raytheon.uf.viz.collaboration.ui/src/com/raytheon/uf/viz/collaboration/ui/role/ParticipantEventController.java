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
import com.raytheon.uf.viz.collaboration.comm.identity.event.IInitData;
import com.raytheon.uf.viz.collaboration.data.CollaborationDataManager;
import com.raytheon.uf.viz.collaboration.ui.editor.CollaborationEditor;
import com.raytheon.uf.viz.collaboration.ui.editor.CollaborationEditorInputHandler;
import com.raytheon.uf.viz.collaboration.ui.editor.EditorSetup;
import com.raytheon.uf.viz.collaboration.ui.editor.SharedEditor;
import com.raytheon.uf.viz.collaboration.ui.rsc.CollaborationResource;
import com.raytheon.uf.viz.collaboration.ui.rsc.CollaborationResourceData;
import com.raytheon.uf.viz.collaboration.ui.telestrator.CollaborationPathDrawingResourceData;
import com.raytheon.uf.viz.collaboration.ui.telestrator.CollaborationPathToolbar;
import com.raytheon.uf.viz.core.VizApp;
import com.raytheon.uf.viz.core.drawables.IDescriptor;
import com.raytheon.uf.viz.core.drawables.ResourcePair;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.rsc.IInputHandler.InputPriority;
import com.raytheon.uf.viz.core.rsc.LoadProperties;
import com.raytheon.uf.viz.drawing.PathDrawingResourceData;

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
    public void initDataArrived(IInitData initData) {
        if (initData instanceof SharedEditor) {
            final SharedEditor se = (SharedEditor) initData;
            VizApp.runAsync(new Runnable() {

                @Override
                public void run() {
                    CollaborationEditor editor = EditorSetup.createEditor(se);
                    initializeResources(editor.getActiveDisplayPane()
                            .getDescriptor());
                    editor.registerMouseHandler(
                            new CollaborationEditorInputHandler(session, editor
                                    .getDisplayPanes()[0]),
                            InputPriority.SYSTEM_RESOURCE);
                    CollaborationDataManager.getInstance().editorCreated(
                            session.getSessionId(), editor);

                    // activate the drawing tool by default for participants
                    PathDrawingResourceData data = new CollaborationPathDrawingResourceData();
                    try {
                        editor.getActiveDisplayPane()
                                .getDescriptor()
                                .getResourceList()
                                .add(data.construct(new LoadProperties(),
                                        editor.getActiveDisplayPane()
                                                .getDescriptor()));
                    } catch (VizException e) {
                        statusHandler.handle(Priority.PROBLEM,
                                e.getLocalizedMessage(), e);
                    }
                    // PathDrawingTool tool = new
                    // CollaborationPathDrawingTool();
                    // tool.activate();
                }

            });
        }
    }

    private void initializeResources(IDescriptor desc) {
        CollaborationResourceData crd = new CollaborationResourceData();
        ResourcePair rp = ResourcePair.constructSystemResourcePair(crd);
        desc.getResourceList().add(rp);
        desc.getResourceList().instantiateResources(desc, false);
        collabRsc = (CollaborationResource) rp.getResource();
        this.session.registerEventHandler(collabRsc);
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
        // VizApp.runAsync(new Runnable() {
        // @Override
        // public void run() {
        // PathToolbar toolbar = CollaborationPathToolbar.getToolbar();
        // toolbar.open();
        // }
        // });
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
        CollaborationPathToolbar.getToolbar().close();

        if (this.collabRsc != null) {
            this.session.unRegisterEventHandler(collabRsc);
        }
    }
}
