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
/**
 * 
 */
package com.raytheon.uf.viz.d2d.ui.actions;

import java.util.Map;

import org.eclipse.core.commands.AbstractHandler;
import org.eclipse.core.commands.ExecutionEvent;
import org.eclipse.core.commands.ExecutionException;
import org.eclipse.ui.IWorkbenchWindow;
import org.eclipse.ui.commands.IElementUpdater;
import org.eclipse.ui.handlers.HandlerUtil;
import org.eclipse.ui.menus.UIElement;

import com.raytheon.uf.viz.core.IDisplayPane;
import com.raytheon.uf.viz.core.IDisplayPaneContainer;
import com.raytheon.uf.viz.core.VizConstants;
import com.raytheon.uf.viz.core.drawables.IRenderableDisplay;
import com.raytheon.uf.viz.core.globals.VizGlobalsManager;
import com.raytheon.uf.viz.core.rsc.capabilities.DensityCapability;
import com.raytheon.uf.viz.d2d.core.ID2DRenderableDisplay;
import com.raytheon.viz.ui.EditorUtil;
import com.raytheon.viz.ui.VizWorkbenchManager;

/**
 * Handles a set density command
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jul 19, 2007            randerso    Initial Creation.
 * 
 * </pre>
 * 
 * @author randerso
 * 
 */
public class DensityHandler extends AbstractHandler implements IElementUpdater {

    /*
     * (non-Javadoc)
     * 
     * @see
     * org.eclipse.core.commands.AbstractHandler#execute(org.eclipse.core.commands
     * .ExecutionEvent)
     */
    @Override
    public Object execute(ExecutionEvent event) throws ExecutionException {
        IWorkbenchWindow window = HandlerUtil.getActiveWorkbenchWindow(event);
        if (window == null) {
            window = VizWorkbenchManager.getInstance().getCurrentWindow();
        }
        IDisplayPaneContainer container = EditorUtil
                .getActiveVizContainer(window);
        VizGlobalsManager mgr = VizGlobalsManager.getInstance(window);
        try {
            double density = Double
                    .parseDouble((event.getParameter("density")));
            if (container != null) {
                IDisplayPane[] panes = container.getDisplayPanes();
                for (IDisplayPane pane : panes) {
                    IRenderableDisplay disp = pane.getRenderableDisplay();
                    if (disp != null) {
                        ((ID2DRenderableDisplay) disp).setDensity(density);
                    }
                }
            }
            mgr.updateChange(VizConstants.DENSITY_ID, density);
        } catch (NumberFormatException e) {
            throw (new ExecutionException("Invalid density", e));
        }
        return null;
    }

    @SuppressWarnings("unchecked")
    public void updateElement(UIElement element, Map parameters) {
        String text = parameters.get("density").toString();
        try {
            double density = Double.parseDouble(text);
            if (density < DensityCapability.MAX_THRESHOLD)
                text = Double.toString(density);
            else
                text = "Max";
        } catch (NumberFormatException e) {
            text += " <Invalid>";
        }
        element.setText(text);
    }

}
