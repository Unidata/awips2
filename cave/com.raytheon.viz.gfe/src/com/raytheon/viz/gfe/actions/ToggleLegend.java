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
package com.raytheon.viz.gfe.actions;

import java.util.List;

import org.eclipse.core.commands.AbstractHandler;
import org.eclipse.core.commands.ExecutionEvent;
import org.eclipse.core.commands.ExecutionException;

import com.raytheon.uf.viz.core.IDisplayPaneContainer;
import com.raytheon.viz.gfe.rsc.GFELegendResource;
import com.raytheon.viz.gfe.rsc.GFELegendResource.LegendMode;
import com.raytheon.viz.ui.EditorUtil;

/**
 * Toggles the mode of the legend
 * 
 * This functionality is also available in a right click context sensitive menu
 * on the editor.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date			Ticket#		Engineer	Description
 * ------------	----------	-----------	--------------------------
 * Jun 25, 2008				chammack	Initial creation
 * 
 * </pre>
 * 
 * @author chammack
 * @version 1.0
 */

public class ToggleLegend extends AbstractHandler {

    @Override
    public Object execute(ExecutionEvent arg0) throws ExecutionException {

        IDisplayPaneContainer container = EditorUtil.getActiveVizContainer();
        if (container != null) {
            List<GFELegendResource> decors = container.getActiveDisplayPane()
                    .getDescriptor().getResourceList()
                    .getResourcesByTypeAsType(GFELegendResource.class);
            for (GFELegendResource gfelegend : decors) {
                LegendMode mode = gfelegend.getLegendMode();

                int position = mode.ordinal();
                position++;
                if (position >= LegendMode.values().length) {
                    position = 0;
                }

                gfelegend.setLegendMode(LegendMode.values()[position]);

            }
            container.refresh();
        }
        return null;
    }

}
