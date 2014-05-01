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
package com.raytheon.viz.gfe.gridmanager;

import org.eclipse.swt.events.MouseAdapter;
import org.eclipse.swt.events.MouseEvent;
import org.eclipse.swt.events.MouseMoveListener;
import org.eclipse.swt.graphics.Point;
import org.eclipse.swt.widgets.Control;

import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.viz.gfe.GFEServerException;

/**
 * TODO Add Description
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Apr 21, 2009            randerso     Initial creation
 * 
 * </pre>
 * 
 * @author randerso
 * @version 1.0
 */

public class MouseHandler extends MouseAdapter implements MouseMoveListener {
    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(MouseHandler.class);

    public static final int DEFAULT_DRAG_TOLERANCE = 4;

    private int dragTolerance = DEFAULT_DRAG_TOLERANCE;

    private int buttonState = 0;

    private int dragMask = 0x6; // buttons 1 or 2

    private int contextMenuButton = 3;

    private boolean dragging = false;

    private Point dragAnchor = new Point(0, 0);

    /**
     * @return the dragTolerance
     */
    public int getDragTolerance() {
        return dragTolerance;
    }

    /**
     * @param dragTolerance
     *            the dragTolerance to set
     */
    public void setDragTolerance(int dragTolerance) {
        this.dragTolerance = dragTolerance;
    }

    public void setDragButtons(int... buttons) {
        int newDragMask = 0;
        for (int button : buttons) {
            newDragMask |= 1 << button;
        }

        dragMask = newDragMask;
    }

    public void setContextMenuButton(int contextMenuButton) {
        this.contextMenuButton = contextMenuButton;
    }

    /**
     * @return the dragAnchor
     */
    public Point getDragAnchor() {
        return dragAnchor;
    }

    /**
     * @param dragAnchor
     *            the dragAnchor to set
     */
    public void setDragAnchor(Point dragAnchor) {
        this.dragAnchor = dragAnchor;
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * org.eclipse.swt.events.MouseAdapter#mouseDown(org.eclipse.swt.events.
     * MouseEvent)
     */
    @Override
    public void mouseDown(MouseEvent e) {
        if (e.button == contextMenuButton) {
            try {
                displayContextMenu(e);
            } catch (GFEServerException e1) {
                statusHandler.handle(Priority.PROBLEM,
                        "Unable to display context menu", e1);
            }
        } else {
            buttonState |= 1 << e.button;

            if (!dragging && isDragButtonDown()) {
                dragAnchor.x = e.x;
                dragAnchor.y = e.y;
                ((Control) e.widget).addMouseMoveListener(this);
            }
        }
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * org.eclipse.swt.events.MouseAdapter#mouseUp(org.eclipse.swt.events.MouseEvent
     * )
     */
    @Override
    public void mouseUp(MouseEvent e) {
        buttonState &= ~(1 << e.button);

        if (!dragging) {
            mouseClick(e);
            return;
        }

        if (!isDragButtonDown()) {
            dragging = false;

            ((Control) e.widget).removeMouseMoveListener(this);
            dragEnd(e);
        }
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * org.eclipse.swt.events.MouseMoveListener#mouseMove(org.eclipse.swt.events
     * .MouseEvent)
     */
    @Override
    public void mouseMove(MouseEvent e) {
        if (!dragging
                && isDragButtonDown()
                && (Math.abs(e.x - dragAnchor.x) > dragTolerance || Math
                        .abs(e.y - dragAnchor.y) > dragTolerance)) {
            dragging = true;
            dragStart(e);
        }

        if (dragging) {
            dragMove(e);
        }
    }

    private boolean isDragButtonDown() {
        return (buttonState & dragMask) != 0;
    }

    public boolean isButtonDown(int button) {
        return (buttonState & 1 << button) != 0;
    }

    public boolean isAnyButtonDown() {
        return buttonState != 0;
    }

    public void dragStart(MouseEvent e) {

    }

    public void dragMove(MouseEvent e) {

    }

    public void dragEnd(MouseEvent e) {

    }

    public void mouseClick(MouseEvent e) {

    }

    public void displayContextMenu(MouseEvent e) throws GFEServerException {

    }

}
