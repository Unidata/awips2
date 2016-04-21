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
package com.raytheon.uf.viz.xy.varheight;

import com.raytheon.uf.viz.core.IDisplayPane;
import com.raytheon.uf.viz.core.drawables.IRenderableDisplay;
import com.raytheon.uf.viz.core.rsc.IInputHandler;
import com.raytheon.uf.viz.core.rsc.IInputHandler.InputPriority;
import com.raytheon.uf.viz.xy.VizXyEditor;
import com.raytheon.uf.viz.xy.varheight.util.VarHeightPanHandler;
import com.raytheon.uf.viz.xy.varheight.util.VarHeightZoomHandler;
import com.raytheon.viz.ui.input.InputManager;

/**
 * TODO Add Description
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jun 28, 2010            bsteffen     Initial creation
 * 
 * </pre>
 * 
 * @author bsteffen
 * @version 1.0
 */

public class VarHeightEditor extends VizXyEditor {

    private VarHeightPanHandler panHandler = null;

    private VarHeightZoomHandler zoomHandler = null;

    @Override
    protected void addCustomHandlers(InputManager manager) {
        // IRenderableDisplay display = this.getActiveDisplayPane()
        // .getRenderableDisplay();
        // // Register the pan and zoom handlers
        // manager.registerMouseHandler(new VarHeightZoomHandler(display));
        // manager.registerMouseHandler(new VarHeightPanHandler(display));
    }

    @Override
    public void registerMouseHandler(IInputHandler handler) {
        this.registerMouseHandler(handler, InputPriority.RESOURCE);
    }

    @Override
    public void registerMouseHandler(IInputHandler handler,
            InputPriority priority) {
        if (handler instanceof VarHeightZoomHandler) {
            if (zoomHandler == null) {
                zoomHandler = (VarHeightZoomHandler) handler;
                this.updateHandlers();
            } else {
                // remove the old one
                this.getMouseManager().unregisterMouseHandler(zoomHandler);
                // register the new one
                zoomHandler = (VarHeightZoomHandler) handler;
                this.getMouseManager().registerMouseHandler(zoomHandler);
                this.updateHandlers();
            }
        } else if (handler instanceof VarHeightPanHandler) {
            if (panHandler == null) {
                panHandler = (VarHeightPanHandler) handler;
                this.updateHandlers();
            } else {
                // remove the old one
                this.getMouseManager().unregisterMouseHandler(panHandler);
                // register the new one
                panHandler = (VarHeightPanHandler) handler;
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
            zoomHandler = new VarHeightZoomHandler(display);
            this.getMouseManager().registerMouseHandler(zoomHandler);
        } else {
            zoomHandler.setRenderableDisplay(display);
        }
        if (panHandler == null) {
            // create
            panHandler = new VarHeightPanHandler(display);
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
