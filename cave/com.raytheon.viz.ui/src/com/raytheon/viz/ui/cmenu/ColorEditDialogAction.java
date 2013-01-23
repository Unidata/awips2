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

package com.raytheon.viz.ui.cmenu;

import com.raytheon.uf.viz.core.IDisplayPane;
import com.raytheon.uf.viz.core.IDisplayPaneContainer;
import com.raytheon.uf.viz.core.rsc.AbstractVizResource;
import com.raytheon.viz.ui.VizWorkbenchManager;
import com.raytheon.viz.ui.dialogs.colordialog.ColorEditDialog;
import com.raytheon.viz.ui.editor.IMultiPaneEditor;

/**
 *
 * Allows a resource's color to be changed
 *
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#     Engineer    Description
 * ------------ ----------  ----------- --------------------------
 * Nov 13, 2006             chammack    Initial Creation.
 * Jan 9, 2013  15648       ryu         Fix NPE on closing color editor.
 *
 * </pre>
 *
 * @author chammack
 * @version 1
 */
public class ColorEditDialogAction extends AbstractRightClickAction {

    public ColorEditDialogAction() {
        this("Edit Colors...");
    }

    public ColorEditDialogAction(String name) {
        super(name);
    }

    /*
     * (non-Javadoc)
     *
     * @see org.eclipse.jface.action.Action#run()
     */
    @Override
    public void run() {
        AbstractVizResource<?, ?> selectedRsc = getSelectedRsc();
        if (selectedRsc != null) {
            IDisplayPaneContainer container = getContainer();
            if (container instanceof IMultiPaneEditor) {
                for (IDisplayPane pane : container.getDisplayPanes()) {
                    if (pane.getDescriptor() == selectedRsc.getDescriptor()) {
                        ((IMultiPaneEditor) container).setSelectedPane(
                                IMultiPaneEditor.IMAGE_ACTION, pane);
                        break;
                    }
                }
            }

            ColorEditDialog.openDialog(VizWorkbenchManager.getInstance()
                    .getCurrentWindow().getShell(), container,
                    getSelectedRsc(), false, false);

            if (container != null)
                container.refresh();
        }
    }

}
