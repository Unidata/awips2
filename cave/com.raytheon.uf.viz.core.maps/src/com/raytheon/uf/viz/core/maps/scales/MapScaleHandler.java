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
package com.raytheon.uf.viz.core.maps.scales;

import org.eclipse.core.commands.AbstractHandler;
import org.eclipse.core.commands.ExecutionEvent;
import org.eclipse.core.commands.ExecutionException;
import org.eclipse.ui.IEditorPart;
import org.eclipse.ui.handlers.HandlerUtil;

import com.raytheon.uf.viz.core.IDisplayPane;
import com.raytheon.uf.viz.core.IDisplayPaneContainer;
import com.raytheon.uf.viz.core.drawables.IRenderableDisplay;
import com.raytheon.uf.viz.core.globals.VizGlobalsManager;
import com.raytheon.uf.viz.core.maps.scales.MapScales.MapScale;

/**
 * Handler of setScale command that looks for a {@link MapScale} by name of
 * attribute scale and sets on any {@link IMapScaleDisplay}s on the current
 * editor
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Mar 22, 2013            mschenke     Initial creation
 * 
 * </pre>
 * 
 * @author mschenke
 * @version 1.0
 */

public class MapScaleHandler extends AbstractHandler {

    public static final String SET_SCALE_COMMAND_ID = "com.raytheon.viz.ui.setScale";

    public static final String SCALE_NAME_ID = "scale";

    /*
     * (non-Javadoc)
     * 
     * @see
     * org.eclipse.core.commands.IHandler#execute(org.eclipse.core.commands.
     * ExecutionEvent)
     */
    @Override
    public Object execute(ExecutionEvent event) throws ExecutionException {
        MapScale scale = MapScales.getInstance().getScaleByName(
                event.getParameter(SCALE_NAME_ID));
        if (scale != null) {
            IEditorPart part = HandlerUtil.getActiveEditor(event);
            if (part instanceof IDisplayPaneContainer) {
                IDisplayPaneContainer container = (IDisplayPaneContainer) part;
                for (IDisplayPane pane : container.getDisplayPanes()) {
                    IRenderableDisplay display = pane.getRenderableDisplay();
                    if (display instanceof IMapScaleDisplay) {
                        ((IMapScaleDisplay) display).changeScale(scale);
                    }
                }
                container.refresh();
                VizGlobalsManager.getInstance(
                        HandlerUtil.getActiveWorkbenchWindow(event)).updateUI(
                        container);
            }
        }
        return null;
    }

}
