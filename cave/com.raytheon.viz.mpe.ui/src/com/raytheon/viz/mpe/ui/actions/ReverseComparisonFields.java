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
package com.raytheon.viz.mpe.ui.actions;


import org.eclipse.core.commands.AbstractHandler;
import org.eclipse.core.commands.ExecutionEvent;
import org.eclipse.core.commands.ExecutionException;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.ui.IEditorPart;
import org.eclipse.ui.handlers.HandlerUtil;

import com.raytheon.uf.viz.core.IDisplayPane;
import com.raytheon.uf.viz.core.IDisplayPaneContainer;
import com.raytheon.viz.mpe.ui.ComparisonFields;
import com.raytheon.viz.mpe.ui.DisplayFieldData;
import com.raytheon.viz.mpe.ui.MPEDisplayManager;
import com.raytheon.viz.mpe.ui.rsc.MPEFieldResource;
import com.raytheon.viz.ui.editor.IMultiPaneEditor;

/**
 * TODO Add Description
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Sep 23, 2008            randerso     Initial creation
 * </pre>
 * 
 * @author randerso
 * @version 1.0
 */

public class ReverseComparisonFields extends AbstractHandler {

    /*
     * (non-Javadoc)
     * 
     * @see
     * org.eclipse.core.commands.AbstractHandler#execute(org.eclipse.core.commands
     * .ExecutionEvent)
     */
    @Override
    public Object execute(ExecutionEvent event) throws ExecutionException {
    
    	IEditorPart editor = HandlerUtil.getActiveEditor(event);
        IDisplayPane pane = null;
        if (editor instanceof IMultiPaneEditor) {
            IMultiPaneEditor multiPane = (IMultiPaneEditor) editor;
            if (multiPane.getNumberofPanes() > 1
                    && multiPane.displayedPaneCount() > 1) {
                pane = multiPane.getSelectedPane(IMultiPaneEditor.LOAD_ACTION);
            } else {
                pane = ((IDisplayPaneContainer) editor).getDisplayPanes()[0];
            }
        }
      
        MPEDisplayManager displayMgr = MPEDisplayManager.getInstance(pane);
        DisplayFieldData field = displayMgr.getDisplayFieldType();
  
        ComparisonFields comparisonFields = displayMgr.getComparisonFields();    
        comparisonFields.reverse();
          
        return setDisplayField((IDisplayPaneContainer) editor, field);

    }
    
    public static MPEFieldResource setDisplayField(
            IDisplayPaneContainer editor, DisplayFieldData field) {
        if (editor == null) {
            return null;
        }
        IDisplayPane[] panes = editor.getDisplayPanes();
        IDisplayPane pane = panes[0];
        if (panes.length > 1) {
            if (editor instanceof IMultiPaneEditor) {
                IMultiPaneEditor multiPane = (IMultiPaneEditor) editor;
                IDisplayPane loadTo = multiPane
                        .getSelectedPane(IMultiPaneEditor.LOAD_ACTION);
                if (loadTo != null) {
                    pane = loadTo;
                }
            }
        }

        MPEDisplayManager displayMgr = MPEDisplayManager.getInstance(pane);
        
        displayMgr.displayFieldData(DisplayFieldData.satPre);
        displayMgr.displayFieldData(field);

        // Update the screen
        return displayMgr.getDisplayedFieldResource();
    }
    
}
