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

package com.raytheon.uf.viz.d2d.ui.map.actions;

import org.eclipse.core.commands.AbstractHandler;
import org.eclipse.core.commands.ExecutionEvent;
import org.eclipse.core.commands.ExecutionException;
import org.eclipse.ui.IEditorPart;

import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.viz.core.IDisplayPane;
import com.raytheon.uf.viz.core.IDisplayPaneContainer;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.globals.VizGlobalsManager;
import com.raytheon.uf.viz.core.status.StatusConstants;
import com.raytheon.uf.viz.d2d.ui.Activator;
import com.raytheon.viz.ui.EditorUtil;
import com.raytheon.viz.ui.HistoryList;
import com.raytheon.viz.ui.editor.IMultiPaneEditor;

/**
 * Action to reset a map to its initial state
 * 
 * <pre>
 * 
 *  SOFTWARE HISTORY
 * 
 *  Date         Ticket#     Engineer    Description
 *  ------------ ----------  ----------- --------------------------
 *  Mar 10, 2007             chammack    Initial Creation.
 *  23Oct2007                ebabin      Added pan as default action, so clear works
 *                                       properly.
 *  Jul 10, 2008             chammack    Properly clear the resource list on the old 
 *                                       descriptor to facilitate better cleanup
 * Oct 21, 2008   #1450      randerso    Fixed to support multipane editors
 * 
 * </pre>
 * 
 * @author chammack
 * @version 1
 */
public class ClearAction extends AbstractHandler {
    private static final transient IUFStatusHandler statusHandler = UFStatus.getHandler(ClearAction.class);

    @Override
    public Object execute(ExecutionEvent arg0) throws ExecutionException {
        IEditorPart part = EditorUtil.getActiveEditor();
        if (part == null) {
            new NewMapEditor().execute(null);
            return null;
        }

        if (!(part instanceof IDisplayPaneContainer)) {
            return null;
        }

        try {
            HistoryList.getInstance().refreshLatestBundle();
            clear((IDisplayPaneContainer) part);
            HistoryList.getInstance().addBundle();
        } catch (VizException e) {
            throw new ExecutionException("Error during clear", e);
        }

        return null;
    }

    /**
     * Clears the map
     * 
     * The map is cleared with either the provided default-procedure.xml or a
     * default world map, if no default-bundle.xml is available.
     * 
     * @param target
     *            the graphics target
     * @return the map descriptor prepared
     * @throws VizException
     */
    public static void clear(IDisplayPaneContainer container)
            throws VizException {
        if (container instanceof IMultiPaneEditor) {
            ((IMultiPaneEditor) container).clear();
        } else {
            for (IDisplayPane displayPane : container.getDisplayPanes()) {
                displayPane.clear();
            }
        }
        if (EditorUtil.getActiveEditor() == null) {
            try {
                new NewMapEditor().execute(null);
            } catch (ExecutionException e) {
                statusHandler.handle(Priority.SIGNIFICANT,
                        "Error loading default map", e);
            }
        }
        VizGlobalsManager.getCurrentInstance().updateUI(
                EditorUtil.getActiveVizContainer());
    }
}
