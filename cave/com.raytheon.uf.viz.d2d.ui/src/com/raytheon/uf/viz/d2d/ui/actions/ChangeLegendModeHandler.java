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

import java.util.ArrayList;
import java.util.List;

import org.eclipse.core.commands.AbstractHandler;
import org.eclipse.core.commands.ExecutionEvent;
import org.eclipse.core.commands.ExecutionException;
import org.eclipse.ui.IEditorPart;
import org.eclipse.ui.handlers.HandlerUtil;

import com.raytheon.uf.viz.core.IDisplayPane;
import com.raytheon.uf.viz.core.IDisplayPaneContainer;
import com.raytheon.uf.viz.d2d.core.legend.D2DLegendResource;
import com.raytheon.uf.viz.d2d.core.legend.D2DLegendResource.LegendMode;

/**
 * TODO Add Description
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Dec 20, 2010            mschenke     Initial creation
 * 
 * </pre>
 * 
 * @author mschenke
 * @version 1.0
 */

public class ChangeLegendModeHandler extends AbstractHandler {

    private static final List<LegendMode> modes = new ArrayList<LegendMode>();
    static {
        modes.add(LegendMode.NONE);
        modes.add(LegendMode.PRODUCT);
        modes.add(LegendMode.MAP);
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * org.eclipse.core.commands.IHandler#execute(org.eclipse.core.commands.
     * ExecutionEvent)
     */
    @Override
    public Object execute(ExecutionEvent event) throws ExecutionException {
        IEditorPart part = HandlerUtil.getActiveEditor(event);
        if (part instanceof IDisplayPaneContainer) {
            IDisplayPane[] panes = ((IDisplayPaneContainer) part)
                    .getDisplayPanes();
            for (IDisplayPane pane : panes) {
                List<D2DLegendResource> rscs = pane.getDescriptor()
                        .getResourceList()
                        .getResourcesByTypeAsType(D2DLegendResource.class);
                for (D2DLegendResource rsc : rscs) {
                    int idx = modes.indexOf(rsc.getLegendMode());
                    if (idx > -1) {
                        // cycle through
                        idx = (idx + 1) % modes.size();
                        rsc.setLegendMode(modes.get(idx));
                    }
                }
                pane.refresh();
            }
        }
        return null;
    }

}
