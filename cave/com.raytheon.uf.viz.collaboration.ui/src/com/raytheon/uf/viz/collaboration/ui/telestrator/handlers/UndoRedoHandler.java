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
package com.raytheon.uf.viz.collaboration.ui.telestrator.handlers;

import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.eclipse.core.commands.AbstractHandler;
import org.eclipse.core.commands.ExecutionEvent;
import org.eclipse.core.commands.ExecutionException;
import org.eclipse.ui.IViewReference;
import org.eclipse.ui.IWorkbenchWindow;
import org.eclipse.ui.handlers.HandlerUtil;

import com.raytheon.uf.viz.collaboration.ui.session.CollaborationSessionView;
import com.raytheon.uf.viz.collaboration.ui.telestrator.CollaborationDrawingResource;
import com.raytheon.uf.viz.core.IDisplayPane;
import com.raytheon.uf.viz.core.IDisplayPaneContainer;
import com.raytheon.uf.viz.drawing.DrawingToolLayer;
import com.raytheon.viz.ui.EditorUtil;

/**
 * Action for invoking undo/redo on the CollaborationDrawingToolbar
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * May 24, 2012            mschenke     Initial creation
 * 
 * </pre>
 * 
 * @author mschenke
 * @version 1.0
 */

public class UndoRedoHandler extends AbstractHandler {

    private static final String ACTION_ID = "com.raytheon.uf.viz.collaboration.tellestrator.action";

    private static final String UNDO_ID = "UNDO";

    private static final String REDO_ID = "REDO";

    /*
     * (non-Javadoc)
     * 
     * @see
     * org.eclipse.core.commands.IHandler#execute(org.eclipse.core.commands.
     * ExecutionEvent)
     */
    @Override
    public Object execute(ExecutionEvent event) throws ExecutionException {
        IWorkbenchWindow window = HandlerUtil.getActiveWorkbenchWindow(event);
        if (window != null) {
            IDisplayPaneContainer container = EditorUtil.getActiveEditorAs(
                    window, IDisplayPaneContainer.class);
            if (container != null) {
                Map<String, CollaborationSessionView> viewMap = new HashMap<String, CollaborationSessionView>();
                for (IViewReference view : window.getActivePage()
                        .getViewReferences()) {
                    if (CollaborationSessionView.ID.equals(view.getId())) {
                        CollaborationSessionView csv = (CollaborationSessionView) view
                                .getPart(false);
                        viewMap.put(csv.getSessionId(), csv);
                    }
                }

                for (IDisplayPane pane : container.getDisplayPanes()) {
                    List<CollaborationDrawingResource> resources = pane
                            .getDescriptor()
                            .getResourceList()
                            .getResourcesByTypeAsType(
                                    CollaborationDrawingResource.class);
                    for (CollaborationDrawingResource resource : resources) {
                        DrawingToolLayer layer = resource
                                .getDrawingLayerFor(resource.getMyUser());
                        String action = event.getParameter(ACTION_ID);
                        if (UNDO_ID.equals(action)) {
                            layer.undo();
                        } else if (REDO_ID.equals(action)) {
                            layer.redo();
                        }
                        CollaborationSessionView view = viewMap.get(resource
                                .getResourceData().getSessionId());
                        if (view != null) {
                            view.updateToolItems();
                        }
                    }
                }
            }

        }
        return null;
    }
}
