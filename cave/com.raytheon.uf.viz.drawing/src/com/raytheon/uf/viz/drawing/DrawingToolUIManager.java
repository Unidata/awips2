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
 * Jun 30, 2014 1798       bclement     updated clearCursor() to only clear its own cursor
 *                                      updateCursor() no longer clears cursor
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
        if (currentShell != null) {
            currentShell.setCursor(null);
        }
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
                clearCursor(currentShell);
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
            updateCursor(currentShell);
        }
        return false;
    }

    /**
     * @see #getCurrentShell()
     * @see #clearCursor(Shell)
     */
    public void clearCursor() {
        clearCursor(currentShell);
    }

    /**
     * Clear cursor from shell if this manager set its current cursor
     * 
     * @param shell
     */
    protected void clearCursor(Shell shell) {
        if (shell != null) {
            Cursor cursor = shell.getCursor();
            /*
             * Only alternative to checking references is to check if the mouse
             * is outside of this layer's map editor. This is to prevent this
             * manager from clearing another session's cursor.
             */
            if (cursor == erasor || cursor == pencil) {
                shell.setCursor(normal);
            }
        }
    }

    /**
     * Sets cursor icon according to draw mode
     * 
     * @param shell
     */
    protected void updateCursor(Shell shell) {
        if (shell == null) {
            return;
        }
        switch (drawingLayer.getDrawMode()) {
        case DRAW:
            if (pencil != null && pencil.isDisposed() == false) {
                shell.setCursor(pencil);
            }
            break;
        case ERASE:
            if (erasor != null && erasor.isDisposed() == false) {
                shell.setCursor(erasor);
            }
            break;
        default:
            /*
             * only active layers (draw mode not equal to NONE) need to update
             * the cursor. cursor will have already been cleared by
             * handleMouseExit()
             */
        }
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

        switch (drawingLayer.getDrawMode()) {
        case DRAW:
            drawingLayer.doneDrawing();
            break;
        case ERASE:
            drawingLayer.doneErasing();
            break;
        default:
            /* no action */
        }
        container.refresh();
        handlingInput = false;
        return true;
    }

    protected boolean canTellestrate(int mouseButton) {
        return handlingInput == false && mouseButton <= 1
                && drawingLayer.getDrawMode() != DrawMode.NONE;
    }

    /**
     * handlingInput is the private variable that tells this class whether it
     * should be adding points as mouse actions are happening or not.
     * 
     * @param handlingInput
     */
    protected void setHandlingInput(boolean handlingInput) {
        this.handlingInput = handlingInput;
    }

    /**
     * @return the currentShell
     */
    public Shell getCurrentShell() {
        return currentShell;
    }
}
