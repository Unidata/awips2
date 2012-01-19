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
package com.raytheon.viz.ui.actions;

import org.eclipse.core.commands.ExecutionException;

import com.raytheon.uf.viz.core.IDisplayPane;
import com.raytheon.viz.ui.cmenu.AbstractRightClickAction;
import com.raytheon.viz.ui.editor.IMultiPaneEditor;

/**
 * Handles selection of a specific pane of a multipane map editor. This action
 * is triggered by right clicking in the pane and selecting one of the "load to"
 * options.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 19Dec2007    560        MW Fegan    Initial creation.
 * 
 * </pre>
 * 
 * @author mfegan
 * @version 1.0
 */

public class SelectPaneAction extends AbstractRightClickAction {
    /**
     * Identifies by number the pane this action is applied to
     */
    private IDisplayPane pane;

    private String action;

    /**
     * Constructor. Creates a SelectPaneAction wired to the specified pane and
     * displaying the specified text.
     * 
     * @param pane
     *            the pane this action works on
     * @param actionText
     *            the text to display in the menu button
     */
    public SelectPaneAction(IDisplayPane pane, String action) {
        this.pane = pane;
        this.action = action;
    }

    @Override
    public void run() {
        if (!(container instanceof IMultiPaneEditor)) {
            return;
        }
        IMultiPaneEditor editor = (IMultiPaneEditor) container;
        editor.setSelectedPane(action, pane);
        if (action.equals(IMultiPaneEditor.IMAGE_ACTION)) {
            try {
                (new ImagePropertiesAction()).execute(null);
            } catch (ExecutionException e) {
                e.printStackTrace();
            }
        }
    }

    @Override
    public String getText() {
        String format = action + " to %s panel";
        if (action.equals(IMultiPaneEditor.IMAGE_ACTION)) {
            format = "Control Color of %s Image";
        } else if (action.equals(IMultiPaneEditor.LOAD_ACTION)) {
            format = "Load to %s Panel";
        }
        if (pane == null) {
            return String.format(format, "All") + "s";
        } else {
            return String.format(format, "This");
        }
    }

}
