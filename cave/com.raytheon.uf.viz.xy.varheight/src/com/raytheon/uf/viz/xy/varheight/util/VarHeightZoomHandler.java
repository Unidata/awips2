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
package com.raytheon.uf.viz.xy.varheight.util;

import org.eclipse.swt.widgets.Event;

import com.raytheon.uf.viz.core.IDisplayPaneContainer;
import com.raytheon.uf.viz.core.drawables.IRenderableDisplay;
import com.raytheon.uf.viz.core.drawables.ResourcePair;
import com.raytheon.uf.viz.xy.AbstractGraphInputHandler;
import com.raytheon.uf.viz.xy.graph.IGraph;
import com.raytheon.uf.viz.xy.graph.XyGraphDescriptor;
import com.raytheon.uf.viz.xy.map.rsc.IGraphableResource;
import com.raytheon.viz.ui.cmenu.ZoomMenuAction;
import com.raytheon.viz.ui.input.PanHandler;
import com.raytheon.viz.ui.input.preferences.MouseEvent;
import com.raytheon.viz.ui.input.preferences.MousePreferenceManager;
import com.vividsolutions.jts.geom.Coordinate;

/**
 * 
 * TODO Add Description
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jul 3, 2010            bsteffen     Initial creation
 * 
 * </pre>
 * 
 * @author bsteffen
 * @version 1.0
 */
public class VarHeightZoomHandler extends AbstractGraphInputHandler {

    private MousePreferenceManager prefManager = MousePreferenceManager
            .getInstance();

    protected int zoomIndex = 0;

    public VarHeightZoomHandler(IRenderableDisplay display) {
        super(display);
    }

    @Override
    public boolean handleMouseWheel(Event event, int x, int y) {
        IDisplayPaneContainer editor = display.getContainer();
        if (editor.getActiveDisplayPane().getDescriptor() instanceof XyGraphDescriptor == false) {
            return super.handleMouseWheel(event, x, y);
        } else {
            MouseEvent mouseEvent = MouseEvent.SCROLL_FORWARD;
            if (event.count == 0) {
                return false;
            } else if (event.count < 0) {
                mouseEvent = MouseEvent.SCROLL_BACK;
            }
            if (prefManager.handleEvent(PanHandler.ZOOMOUT_PREF, mouseEvent)) {
                return zoom(PanHandler.ZOOMOUT_PREF, x, y);
            } else if (prefManager.handleEvent(PanHandler.ZOOMIN_PREF,
                    mouseEvent)) {
                return zoom(PanHandler.ZOOMIN_PREF, x, y);
            }
        }
        return false;
    }

    @Override
    public boolean handleMouseUp(int x, int y, int mouseButton) {
        IDisplayPaneContainer editor = display.getContainer();
        if (editor.getActiveDisplayPane().getDescriptor() instanceof XyGraphDescriptor == false) {
            return super.handleMouseUp(x, y, mouseButton);
        } else {
            if (prefManager.handleClick(PanHandler.ZOOMIN_PREF, mouseButton)) {
                return zoom(PanHandler.ZOOMIN_PREF, x, y);
            } else if (prefManager.handleClick(PanHandler.ZOOMOUT_PREF,
                    mouseButton)) {
                return zoom(PanHandler.ZOOMOUT_PREF, x, y);
            }
        }
        return false;
    }

    private boolean zoom(String pref, int x, int y) {
        IDisplayPaneContainer editor = display.getContainer();
        Coordinate grid = editor.translateClick(x, y);
        if (grid == null) {
            return false;
        }
        XyGraphDescriptor desc = (XyGraphDescriptor) editor
                .getActiveDisplayPane().getDescriptor();
        if (pref.equals(PanHandler.ZOOMOUT_PREF) && zoomIndex > 0) {
            zoomIndex -= 1;
        } else if (pref.equals(PanHandler.ZOOMIN_PREF)
                && zoomIndex < ZoomMenuAction.ZOOM_LEVELS.length - 1) {
            zoomIndex += 1;
        }
        for (ResourcePair rsc : desc.getResourceList()) {
            if (rsc.getResource() instanceof IGraphableResource<?, ?>) {
                IGraph graph = desc.getGraph((IGraphableResource<?, ?>) rsc
                        .getResource());
                if (graph.getExtent().contains(new double[] { grid.x, grid.y })) {
                    graph.zoom((int) Math.pow(2, zoomIndex), grid);
                }

            }
        }
        return true;
    }
}
