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
package com.raytheon.uf.viz.drawing;

import org.eclipse.swt.graphics.Cursor;
import org.eclipse.swt.graphics.ImageData;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Event;
import org.eclipse.swt.widgets.Shell;

import com.raytheon.uf.viz.core.IDisplayPaneContainer;
import com.raytheon.uf.viz.core.VizApp;
import com.raytheon.uf.viz.core.icon.IconUtil;
import com.raytheon.uf.viz.drawing.DrawingToolLayer.DrawMode;
import com.raytheon.viz.ui.input.InputAdapter;
import com.vividsolutions.jts.geom.Coordinate;

/**
 * UI manager for a DrawingToolLayer, handles mouse actions for drawing
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * May 23, 2012            mschenke     Initial creation
 * 
 * </pre>
 * 
 * @author mschenke
 * @version 1.0
 */

public class DrawingToolUIManager extends InputAdapter {

    private DrawingToolLayer drawingLayer;

    private Cursor erasor;

    private Cursor pencil;

    /** Normal will never be initialized as null tells SWT to use the default */
    private Cursor normal = null;

    private boolean handlingInput = false;

    protected IDisplayPaneContainer container;

    private Shell currentShell = null;

    public DrawingToolUIManager(DrawingToolLayer drawingLayer,
            IDisplayPaneContainer container) {
        this.drawingLayer = drawingLayer;
        this.container = container;

        VizApp.runAsync(new Runnable() {
            @Override
            public void run() {
                // Create erasor cursor
                ImageData data = IconUtil.getImageDescriptor(
                        Activator.getDefault().getBundle(), "eraser_box.gif")
                        .getImageData();
                data.alpha = 255;
                erasor = new Cursor(Display.getCurrent(), data, 8, 8);

                // Create pencil cursor
                data = IconUtil.getImageDescriptor(
                        Activator.getDefault().getBundle(), "draw.gif")
                        .getImageData();
                pencil = new Cursor(Display.getCurrent(), data, 1, 15);

                DrawingToolUIManager.this.container
                        .registerMouseHandler(DrawingToolUIManager.this);
            }
        });
    }

    public void dispose() {
        erasor.dispose();
        pencil.dispose();

        container.unregisterMouseHandler(this);
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.viz.ui.input.InputAdapter#handleMouseExit(org.eclipse.swt
     * .widgets.Event)
     */
    @Override
    public boolean handleMouseExit(Event event) {
        if (canTellestrate(event.button)) {
            if (currentShell != null) {
                currentShell.setCursor(normal);
            }
        }
        return false;
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.viz.ui.input.InputAdapter#handleMouseEnter(org.eclipse.swt
     * .widgets.Event)
     */
    @Override
    public boolean handleMouseEnter(Event event) {
        if (canTellestrate(event.button)) {
            currentShell = ((Control) event.widget).getShell();
            switch (drawingLayer.getDrawMode()) {
            case DRAW:
                currentShell.setCursor(pencil);
                break;
            case ERASE:
                currentShell.setCursor(erasor);
                break;
            default:
                currentShell.setCursor(normal);
            }
        }
        return false;
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.viz.ui.input.InputAdapter#handleMouseDown(int, int,
     * int)
     */
    @Override
    public boolean handleMouseDown(int x, int y, int mouseButton) {
        if (!canTellestrate(mouseButton)) {
            return false;
        }
        handlingInput = true;
        return handleMouseDownMove(x, y, mouseButton);
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.viz.ui.input.InputAdapter#handleMouseDownMove(int, int,
     * int)
     */
    @Override
    public boolean handleMouseDownMove(int x, int y, int mouseButton) {
        if (handlingInput == false) {
            return false;
        }

        Coordinate c = container.translateClick(x, y);
        if (c != null) {
            drawingLayer.addCoordinate(c);
        }
        container.refresh();
        return true;
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.viz.ui.input.InputAdapter#handleMouseUp(int, int, int)
     */
    @Override
    public boolean handleMouseUp(int x, int y, int mouseButton) {
        if (handlingInput == false) {
            return false;
        }

        if (drawingLayer.getDrawMode() == DrawMode.DRAW) {
            drawingLayer.doneDrawing();
        } else {
            drawingLayer.doneErasing();
        }
        container.refresh();
        handlingInput = false;
        return true;
    }

    protected boolean canTellestrate(int mouseButton) {
        return handlingInput == false && mouseButton == 1
                && drawingLayer.getDrawMode() != DrawMode.NONE;
    }
}
