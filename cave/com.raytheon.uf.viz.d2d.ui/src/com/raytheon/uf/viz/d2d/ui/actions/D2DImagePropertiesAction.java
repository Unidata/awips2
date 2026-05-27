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

package com.raytheon.uf.viz.d2d.ui.actions;

import org.eclipse.core.commands.ExecutionEvent;
import org.eclipse.core.commands.ExecutionException;

import com.raytheon.uf.viz.core.IDisplayPaneContainer;
import com.raytheon.uf.viz.core.rsc.AbstractVizResource;
import com.raytheon.uf.viz.d2d.ui.dialogs.D2DImagingDialog;
import com.raytheon.viz.ui.EditorUtil;
import com.raytheon.viz.ui.VizWorkbenchManager;
import com.raytheon.viz.ui.actions.ImagePropertiesAction;

/**
 * Open a D2D specific Image properties dialog that includes an image
 * combination action.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * 
 * Date          Ticket#  Engineer  Description
 * ------------- -------- --------- ----------------------------------------
 * Sep 13, 2016  3241     bsteffen  Initial Creation.
 * 
 * </pre>
 * 
 * @author bsteffen
 */
public class D2DImagePropertiesAction extends ImagePropertiesAction {

    private static D2DImagingDialog dialog = null;

    @Override
    public Object execute(ExecutionEvent arg0) throws ExecutionException {
        IDisplayPaneContainer container = EditorUtil.getActiveVizContainer();

        AbstractVizResource<?, ?> selected = getSelectedResource(arg0);

        if (container == null && selected == null) {
            return null;
        }

        if (dialog == null || dialog.getShell() == null
                || dialog.isDisposed()) {
            if (selected != null) {
                dialog = new D2DImagingDialog(VizWorkbenchManager.getInstance()
                        .getCurrentWindow().getShell(), selected);
            } else {
                dialog = new D2DImagingDialog(VizWorkbenchManager.getInstance()
                        .getCurrentWindow().getShell(), container);
            }
            // initalize
            dialog.open();
        } else {
            if (selected != null) {
                dialog.setResource(selected);
            } else {
                dialog.setContainer(container);
            }
            dialog.refreshComponents();
            dialog.bringToTop();
        }

        return null;
    }

}
