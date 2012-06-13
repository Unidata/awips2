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
package com.raytheon.uf.viz.collaboration.display.roles;

import org.eclipse.ui.PartInitException;

import com.raytheon.uf.viz.collaboration.comm.identity.ISharedDisplaySession;
import com.raytheon.uf.viz.collaboration.display.data.SessionContainer;
import com.raytheon.uf.viz.collaboration.display.data.SharedDisplaySessionMgr;
import com.raytheon.uf.viz.collaboration.display.editor.CollaborationEditorInput;
import com.raytheon.uf.viz.collaboration.display.editor.ICollaborationEditor;
import com.raytheon.viz.ui.VizWorkbenchManager;

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

    public ParticipantEventController(ISharedDisplaySession session) {
        super(session);
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
        CollaborationEditorInput input = new CollaborationEditorInput(
                session.getSessionId(), session.getVenue().getInfo()
                        .getVenueDescription());
        try {
            ICollaborationEditor editor = (ICollaborationEditor) VizWorkbenchManager
                    .getInstance().getCurrentWindow().getActivePage()
                    .openEditor(input, ICollaborationEditor.EDITOR_ID);
            SessionContainer sc = SharedDisplaySessionMgr
                    .getSessionContainer(session.getSessionId());
            sc.setCollaborationEditor(editor);
        } catch (PartInitException e) {
            throw new RuntimeException("Unable to open collaboration editor", e);
        }

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
        SessionContainer sc = SharedDisplaySessionMgr
                .getSessionContainer(session.getSessionId());
        if (sc != null) {
            sc.getCollaborationEditor().getSite().getPage()
                    .closeEditor(sc.getCollaborationEditor(), false);
        }
    }

}
