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

import java.util.List;

import com.raytheon.uf.viz.collaboration.comm.identity.ISharedDisplaySession;
import com.raytheon.uf.viz.collaboration.data.CollaborationDataManager;
import com.raytheon.uf.viz.collaboration.data.SharedDisplaySessionMgr;
import com.raytheon.uf.viz.collaboration.ui.editor.CollaborationEditor;
import com.raytheon.uf.viz.collaboration.ui.editor.event.CollaborationInputHandler;
import com.raytheon.uf.viz.collaboration.ui.editor.event.EventForwardingInputHandler;
import com.raytheon.uf.viz.core.IDisplayPane;
import com.raytheon.uf.viz.core.rsc.IInputHandler;
import com.raytheon.uf.viz.core.rsc.IInputHandler.InputPriority;
import com.raytheon.viz.ui.editor.AbstractEditor;

/**
 * TODO Add Description
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Apr 9, 2012            njensen     Initial creation
 * 
 * </pre>
 * 
 * @author njensen
 * @version 1.0
 */

public class InputUtil {

    /**
     * Gets the CollaborationInputHandler associated with an editor.
     * 
     * @param editor
     * @return
     */
    public static CollaborationInputHandler getCollaborationInputHandler(
            AbstractEditor editor) {
        CollaborationInputHandler handler = null;
        IInputHandler[] array = editor.getMouseManager()
                .getHandlersForPriority(InputPriority.SYSTEM_RESOURCE);
        for (IInputHandler h : array) {
            if (h instanceof CollaborationInputHandler) {
                handler = (CollaborationInputHandler) h;
                break;
            }
        }

        return handler;
    }

    /**
     * Disables the data provider's input on the actively shared editors by
     * adding a CollaborationInputHandler that disables other inputs.
     * 
     * @param sessionId
     *            the session to disable input for
     */
    public static void disableDataProviderInput(String sessionId) {
        List<AbstractEditor> list = SharedDisplaySessionMgr
                .getSessionContainer(sessionId).getSharedEditors();
        for (AbstractEditor editor : list) {
            CollaborationInputHandler handler = getCollaborationInputHandler(editor);
            if (handler == null) {
                handler = new CollaborationInputHandler();
                editor.registerMouseHandler(handler,
                        InputPriority.SYSTEM_RESOURCE);
            }
            List<IInputHandler> handlers = handler.getRegisteredHandlers();
            for (IInputHandler input : handlers) {
                handler.unregisterInputHandler(input);
            }
            // TODO need to leave telestrator power in the editor
        }
    }

    /**
     * Enables the standard input on editors for a data provider by removing a
     * CollaborationInputHandler on the shared editors.
     * 
     * @param sessionId
     *            the session to enable the input on editors
     */
    public static void enableDataProviderInput(String sessionId) {
        List<AbstractEditor> list = SharedDisplaySessionMgr
                .getSessionContainer(sessionId).getSharedEditors();
        for (AbstractEditor editor : list) {
            CollaborationInputHandler handler = getCollaborationInputHandler(editor);
            if (handler != null) {
                editor.unregisterMouseHandler(handler);
            }
        }
    }

    /**
     * Enables a session leader's input on a CollaborationEditor by adding an
     * EventForwardingInputHandler that sends events back to the Data Provider.
     * 
     * @param editor
     *            the editor to enable input for
     */
    public static void enableSessionLeaderInput(CollaborationEditor editor) {
        IDisplayPane pane = editor.getActiveDisplayPane();
        ISharedDisplaySession session = (ISharedDisplaySession) CollaborationDataManager
                .getInstance().getSession(
                        CollaborationDataManager.getInstance().getSessionId(
                                editor));
        CollaborationInputHandler handler = getCollaborationInputHandler(editor);
        EventForwardingInputHandler mouseHandler = new EventForwardingInputHandler(
                session, pane);
        handler.registerInputHandler(mouseHandler);
    }

    /**
     * Disables a session leader's input on the CollaborationEditor by removing
     * the EventForwardingInputHandler.
     * 
     * @param editor
     *            the editor to disable input for
     */
    public static void disableSessionLeaderInput(CollaborationEditor editor) {
        CollaborationInputHandler handler = getCollaborationInputHandler(editor);
        for (IInputHandler h : handler.getRegisteredHandlers()) {
            handler.unregisterInputHandler(h);
        }
        // TODO need to leave telestrator power in the editor
    }

}
