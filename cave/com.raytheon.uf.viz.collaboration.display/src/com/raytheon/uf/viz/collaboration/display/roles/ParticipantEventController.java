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
 * Jan 28, 2014 2698       bclement    removed venue info
 * Mar 06, 2014 2848       bclement    get venueName direct from session
 * 
 * </pre>
 * 
 * @author njensen
 * @version 1.0
 */

public class ParticipantEventController extends
        AbstractRoleEventController<ICollaborationEditor> {

    public ParticipantEventController(ISharedDisplaySession session) {
        super(session);
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.viz.collaboration.display.roles.AbstractRoleEventController
     * #createDisplayContainer()
     */
    @Override
    protected ICollaborationEditor createDisplayContainer() {
        String name = session.getVenueName();
        CollaborationEditorInput input = new CollaborationEditorInput(
                session.getSessionId(), name);
        try {
            return (ICollaborationEditor) VizWorkbenchManager.getInstance()
                    .getCurrentWindow().getActivePage()
                    .openEditor(input, ICollaborationEditor.EDITOR_ID);
        } catch (PartInitException e) {
            throw new RuntimeException("Error opening collaboration editor", e);
        }
    }
}
