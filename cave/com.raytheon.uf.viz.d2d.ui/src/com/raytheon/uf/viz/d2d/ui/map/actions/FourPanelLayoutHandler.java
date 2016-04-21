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

import org.eclipse.core.commands.ExecutionEvent;
import org.eclipse.core.commands.ExecutionException;
import org.eclipse.ui.IEditorPart;

import com.raytheon.uf.viz.core.IDisplayPaneContainer;
import com.raytheon.viz.ui.EditorUtil;
import com.raytheon.viz.ui.actions.NewFourPanelEditor;
import com.raytheon.viz.ui.tools.AbstractTool;

/**
 * 
 * Handles entering 4-pane mode
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Nov 5, 2009            bsteffen     Initial creation
 * 
 * </pre>
 * 
 * @author bsteffen
 * @version 1.0
 */
public class FourPanelLayoutHandler extends AbstractTool {
    public Object execute(ExecutionEvent arg0) throws ExecutionException {
        // There might be a better way to merge these two actions.
        IEditorPart curEditor = EditorUtil.getActiveEditor();
        if (curEditor == null) {
            new NewFourPanelEditor().execute(null);
        } else if (curEditor instanceof IDisplayPaneContainer) {
            FourPanelLayoutMenuAction menuAction = new FourPanelLayoutMenuAction();
            menuAction.setContainer((IDisplayPaneContainer) curEditor);
            menuAction.run();
        }
        return null;
    }
}
