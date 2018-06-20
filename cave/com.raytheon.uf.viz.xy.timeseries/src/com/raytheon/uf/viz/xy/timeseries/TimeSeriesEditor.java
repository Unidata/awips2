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
package com.raytheon.uf.viz.xy.timeseries;

import com.raytheon.uf.viz.core.IDisplayPane;
import com.raytheon.uf.viz.core.drawables.IRenderableDisplay;
import com.raytheon.uf.viz.core.rsc.IInputHandler;
import com.raytheon.uf.viz.core.rsc.IInputHandler.InputPriority;
import com.raytheon.uf.viz.xy.VizXyEditor;
import com.raytheon.uf.viz.xy.timeseries.util.TimeSeriesPanHandler;
import com.raytheon.uf.viz.xy.timeseries.util.TimeSeriesZoomHandler;
import com.raytheon.viz.ui.input.InputManager;

/**
 * Editor for time series graphs
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#     Engineer    Description
 * ------------ ----------	----------- --------------------------
 * Oct 11, 2006              chammack    Initial Creation.
 * 
 * </pre>
 * 
 * @author chammack
 * @version 1
 */

public class TimeSeriesEditor extends VizXyEditor {

    private TimeSeriesZoomHandler zoomHandler = null;

    private TimeSeriesPanHandler panHandler = null;

    @Override
    protected void addCustomHandlers(InputManager manager) {
        // zoomHandler = new TimeSeriesZoomHandler(null);
        // manager.registerMouseHandler(zoomHandler);
        // panHandler = new TimeSeriesPanHandler(null);
        // manager.registerMouseHandler(panHandler);
    }

    @Override
    public void registerMouseHandler(IInputHandler handler) {
        this.registerMouseHandler(handler, InputPriority.RESOURCE);
    }

    @Override
    public void registerMouseHandler(IInputHandler handler,
            InputPriority priority) {
        if (handler instanceof TimeSeriesZoomHandler) {
            if (zoomHandler == null) {
                zoomHandler = (TimeSeriesZoomHandler) handler;
                this.updateHandlers();
            } else {
                // remove the old one
                this.getMouseManager().unregisterMouseHandler(zoomHandler);
                // register the new one
                zoomHandler = (TimeSeriesZoomHandler) handler;
                this.getMouseManager().registerMouseHandler(zoomHandler);
                this.updateHandlers();
            }
        } else if (handler instanceof TimeSeriesPanHandler) {
            if (panHandler == null) {
                panHandler = (TimeSeriesPanHandler) handler;
                this.updateHandlers();
            } else {
                // remove the old one
                this.getMouseManager().unregisterMouseHandler(panHandler);
                // register the new one
                panHandler = (TimeSeriesPanHandler) handler;
                this.getMouseManager().registerMouseHandler(panHandler);
                this.updateHandlers();
            }
        } else {
            super.registerMouseHandler(handler, priority);
        }
    }

    private void updateHandlers() {
        if (this.getActiveDisplayPane() != null) {
            if (this.getActiveDisplayPane().getRenderableDisplay() != null) {
                this.updateHandlers(this.getActiveDisplayPane()
                        .getRenderableDisplay());
            }
        }
    }

    private void updateHandlers(IRenderableDisplay display) {
        if (zoomHandler == null) {
            // create
            zoomHandler = new TimeSeriesZoomHandler(display);
            this.getMouseManager().registerMouseHandler(zoomHandler);
        } else {
            zoomHandler.setRenderableDisplay(display);
        }
        if (panHandler == null) {
            // create
            panHandler = new TimeSeriesPanHandler(display);
            this.getMouseManager().registerMouseHandler(panHandler);
        } else {
            zoomHandler.setRenderableDisplay(display);
        }
    }

    @Override
    public void renderableDisplayChanged(IDisplayPane pane,
            IRenderableDisplay newRenderableDisplay, DisplayChangeType type) {
        // mouse listener?
        // System.err.println("renderable display changed!");
        if (type.equals(DisplayChangeType.ADD)) {
            this.updateHandlers(newRenderableDisplay);
        }
        super.renderableDisplayChanged(pane, newRenderableDisplay, type);
    }

    @Override
    public String getDefaultTool() {
        // We have our own custom tools
        return null;
    }
}
