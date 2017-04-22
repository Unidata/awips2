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

import com.raytheon.uf.viz.core.IDisplayPane;
import com.raytheon.uf.viz.core.IDisplayPaneContainer;
import com.raytheon.uf.viz.core.drawables.ResourcePair;
import com.raytheon.uf.viz.core.rsc.AbstractVizResource;
import com.raytheon.uf.viz.core.rsc.ResourceProperties;
import com.raytheon.uf.viz.core.rsc.capabilities.ImagingCapability;
import com.raytheon.viz.ui.EditorUtil;
import com.raytheon.viz.ui.tools.AbstractTool;

/**
 * 
 * Handles toggling of a resource at a specific index
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
public class ToggleResourceHandler extends AbstractTool {

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.viz.ui.tools.AbstractTool#execute(org.eclipse.core.commands
     * .ExecutionEvent)
     */
    @Override
    public Object execute(ExecutionEvent arg0) throws ExecutionException {
        IDisplayPaneContainer editor = EditorUtil.getActiveVizContainer();
        if (editor != null) {
            this.editor = editor;
            String indexStr = arg0.getParameter("index");
            int index = Integer.parseInt(indexStr);

            IDisplayPane[] panes = editor.getDisplayPanes();

            if (index == 0) {
                for (IDisplayPane pane : panes) {
                    for (ResourcePair rp : pane.getDescriptor()
                            .getResourceList()) {
                        AbstractVizResource<?, ?> rsc = rp.getResource();
                        if (rsc != null
                                && rsc.hasCapability(ImagingCapability.class)) {
                            rp.getProperties().setVisible(
                                    !rp.getProperties().isVisible());
                        }
                    }
                }
            } else {

                for (IDisplayPane pane : panes) {
                    int i = 1;
                    for (ResourcePair rscPair : pane.getDescriptor()
                            .getResourceList()) {
                        ResourceProperties props = rscPair.getProperties();
                        if (!props.isMapLayer() && !props.isSystemResource()) {
                            if (i == index) {
                                props.setVisible(!props.isVisible());
                                break;
                            }
                            i += 1;
                        }
                    }
                }
            }
            editor.refresh();
        }
        return null;
    }
}
