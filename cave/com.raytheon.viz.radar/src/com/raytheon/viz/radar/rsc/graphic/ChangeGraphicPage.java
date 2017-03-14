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
package com.raytheon.viz.radar.rsc.graphic;

import org.eclipse.core.commands.AbstractHandler;
import org.eclipse.core.commands.ExecutionEvent;
import org.eclipse.core.commands.ExecutionException;

import com.raytheon.uf.viz.core.IDisplayPane;
import com.raytheon.uf.viz.core.rsc.ResourceList;
import com.raytheon.viz.ui.EditorUtil;

/**
 * Add key bindings to radar graphics to move pages with alt+leftarrow or
 * alt+rightarrow
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Nov 19, 2010            mnash     Initial creation
 * 
 * </pre>
 * 
 * @author mnash
 * @version 1.0
 */

public class ChangeGraphicPage extends AbstractHandler {

    /*
     * (non-Javadoc)
     * 
     * @see
     * org.eclipse.core.commands.IHandler#execute(org.eclipse.core.commands.
     * ExecutionEvent)
     */
    @Override
    public Object execute(ExecutionEvent event) throws ExecutionException {
        IDisplayPane[] panes = EditorUtil.getActiveVizContainer()
                .getDisplayPanes();
        for (IDisplayPane pane : panes) {
            ResourceList list = pane.getDescriptor().getResourceList();
            for (int i = 0; i < list.size(); i++) {
                if (list.get(i).getResource() instanceof RadarGraphicsResource) {
                    RadarGraphicsResource resource = (RadarGraphicsResource) list
                            .get(i).getResource();
                    if (list.get(i).getProperties().isVisible()) {
                        if ("right".equals(event.getParameter("pageDir"))) {
                            NextGraphicPage.goToNext(resource);
                        } else {
                            PreviousGraphicPage.goToPrevious(resource);
                        }
                        resource.issueRefresh();
                    }
                }
            }
        }
        return null;
    }

    @Override
    public boolean isEnabled() {
        if (EditorUtil.getActiveVizContainer() == null) {
            return false;
        }
        EditorUtil.getActiveVizContainer().getDisplayPanes();
        return super.isEnabled();
    }

}
