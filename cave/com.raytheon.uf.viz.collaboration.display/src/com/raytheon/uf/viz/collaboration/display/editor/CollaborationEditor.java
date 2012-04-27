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
package com.raytheon.uf.viz.collaboration.display.editor;

import com.raytheon.uf.viz.collaboration.display.editor.input.CollaborationInputHandler;
import com.raytheon.uf.viz.core.rsc.IInputHandler;
import com.raytheon.uf.viz.core.rsc.IInputHandler.InputPriority;
import com.raytheon.viz.ui.editor.AbstractEditor;
import com.raytheon.viz.ui.input.InputManager;
import com.raytheon.viz.ui.panes.PaneManager;

/**
 * A collaboration editor that displays the display of an editor shared by the
 * Data Provider.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Mar 16, 2012            njensen     Initial creation
 * 
 * </pre>
 * 
 * @author njensen
 * @version 1.0
 */

public class CollaborationEditor extends AbstractEditor {

    public static final String EDITOR_ID = "com.raytheon.uf.viz.collaboration.display.editor.CollaborationEditor";

    private String sessionId;

    private CollaborationInputHandler inputHandler = new CollaborationInputHandler();

    @Override
    protected PaneManager getNewPaneManager() {
        return new PaneManager();
    }

    @Override
    protected void addCustomHandlers(InputManager manager) {
        super.registerMouseHandler(inputHandler, InputPriority.SYSTEM_RESOURCE);
    }

    @Override
    public void registerMouseHandler(IInputHandler handler) {
        inputHandler.registerInputHandler(handler);
    }

    @Override
    public void unregisterMouseHandler(IInputHandler handler) {
        inputHandler.unregisterInputHandler(handler);
    }

    public String getSessionId() {
        return sessionId;
    }

    public void setSessionId(String sessionId) {
        this.sessionId = sessionId;
    }

}
