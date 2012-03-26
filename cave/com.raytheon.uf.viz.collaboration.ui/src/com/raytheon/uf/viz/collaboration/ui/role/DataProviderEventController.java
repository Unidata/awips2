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
import com.raytheon.uf.viz.collaboration.comm.identity.CollaborationException;
import com.raytheon.uf.viz.collaboration.comm.identity.event.ParticipantEventType;
import com.raytheon.uf.viz.collaboration.comm.provider.event.VenueParticipantEvent;
import com.raytheon.uf.viz.collaboration.comm.provider.session.VenueSession;
import com.raytheon.uf.viz.collaboration.ui.editor.EditorSetup;
import com.raytheon.uf.viz.collaboration.ui.editor.SharedEditor;
import com.raytheon.viz.ui.VizWorkbenchManager;
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

    private VenueSession session;

    public DataProviderEventController(VenueSession session) {
        super(session);
    }

    @Subscribe
    public void participantChanged(VenueParticipantEvent event) {
        if (event.getEventType().equals(ParticipantEventType.ARRIVED)) {
            // TODO instead of going to active editor, should get ones
            // specifically shared with this session
            AbstractEditor editor = (AbstractEditor) VizWorkbenchManager
                    .getInstance().getActiveEditor();
            SharedEditor se = EditorSetup.extractSharedEditor(editor);
            try {
                session.sendInitData(event.getParticipant(), se);
            } catch (CollaborationException e) {
                statusHandler.handle(Priority.PROBLEM,
                        "Error sending initialization data to new participant "
                                + event.getParticipant().getName(), e);
            }
        }
    }

}
