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
import com.raytheon.uf.viz.collaboration.comm.provider.event.VenueParticipantEvent;
import com.raytheon.uf.viz.collaboration.ui.editor.EditorSetup;
import com.raytheon.uf.viz.collaboration.ui.editor.SharedEditor;
import com.raytheon.uf.viz.collaboration.ui.editor.event.InputEvent;
import com.raytheon.uf.viz.core.IDisplayPane;
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

    @Subscribe
    public void sessionLeaderInput(InputEvent event) {
        // TODO TBD will this pick up events sent by the data provider (ie this
        // cave) too? if so, need to rework it cause this code should only
        // execute on the data provider when someone else's cave is the session
        // leader. ideally no InputEvents are sent when data provider and
        // session leader are one and the same

        // TODO get the actively shared editor, not the active editor
        AbstractEditor editor = (AbstractEditor) VizWorkbenchManager
                .getInstance().getActiveEditor();
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

}
