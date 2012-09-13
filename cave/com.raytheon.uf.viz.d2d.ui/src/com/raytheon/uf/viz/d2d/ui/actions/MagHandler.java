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
import org.eclipse.ui.commands.IElementUpdater;
import org.eclipse.ui.handlers.HandlerUtil;
import org.eclipse.ui.menus.UIElement;

import com.raytheon.uf.viz.core.IDisplayPane;
import com.raytheon.uf.viz.core.IDisplayPaneContainer;
import com.raytheon.uf.viz.core.VizConstants;
import com.raytheon.uf.viz.core.drawables.IRenderableDisplay;
import com.raytheon.uf.viz.core.globals.VizGlobalsManager;
import com.raytheon.uf.viz.d2d.core.ID2DRenderableDisplay;
import com.raytheon.viz.ui.EditorUtil;

/**
 * Handles a set magnification command
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jul 19, 2007            randerso    Initial Creation.
 * Sep 4, 2012  15335      kshresth    Will now display lightning/wind 
 *                                     fields when magnification set to 0
 * 
 * </pre>
 * 
 * @author randerso
 * 
 */
public class MagHandler extends AbstractHandler implements IElementUpdater {

    /*
     * (non-Javadoc)
     * 
     * @see
     * org.eclipse.core.commands.AbstractHandler#execute(org.eclipse.core.commands
     * .ExecutionEvent)
     */
    @Override
    public Object execute(ExecutionEvent arg0) throws ExecutionException {
        IDisplayPaneContainer editor = EditorUtil.getActiveVizContainer();
        VizGlobalsManager mgr = VizGlobalsManager.getInstance(HandlerUtil
                .getActiveWorkbenchWindow(arg0));
        try {
            double magnification = Double.parseDouble((arg0
                    .getParameter("magnification")));
            if (editor != null) {
            	if (magnification == 0.0f) magnification=0.1;
                IDisplayPane[] panes = editor.getDisplayPanes();
                for (IDisplayPane pane : panes) {
                    IRenderableDisplay disp = pane.getRenderableDisplay();
                    if (disp != null) {
                        ((ID2DRenderableDisplay) disp)
                                .setMagnification(magnification);
                    }
                }
            }
            mgr.updateChange(VizConstants.MAGNIFICATION_ID, magnification);
        } catch (NumberFormatException e) {
            throw (new ExecutionException("Invalid magnification", e));
        }
        return null;
    }

    @Override
    @SuppressWarnings("unchecked")
    public void updateElement(UIElement element, Map parameters) {
        String text = parameters.get("magnification").toString();
        try {
            double magnification = Double.parseDouble(text);
            text = Double.toString(magnification);
        } catch (NumberFormatException e) {
            text += " <Invalid>";
        }
        element.setText(text);
    }

}
