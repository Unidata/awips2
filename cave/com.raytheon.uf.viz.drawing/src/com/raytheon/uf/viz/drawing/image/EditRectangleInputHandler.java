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
package com.raytheon.uf.viz.drawing.image;

import java.util.EnumSet;

import org.eclipse.swt.SWT;
import org.eclipse.swt.graphics.Point;
import org.eclipse.swt.graphics.Rectangle;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Event;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.ui.IWorkbenchWindow;
import org.eclipse.ui.PlatformUI;

import com.raytheon.uf.viz.core.IDisplayPaneContainer;
import com.raytheon.viz.ui.input.InputAdapter;

/**
 * Input handler for editing a rectangle. Provides the ability to move and
 * resize, moving edges individually or from a corner.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date          Ticket#  Engineer    Description
 * ------------- -------- ----------- --------------------------
 * Jan 15, 2014  2313     bsteffen    Initial creation
 * 
 * </pre>
 * 
 * @author bsteffen
 * @version 1.0
 */
public class EditRectangleInputHandler extends InputAdapter {

    private static final int CLICK_WIDTH = 15;

    private static final int MOUSE_BUTTON = 1;

    private final EditRectangleTarget target;

    private EnumSet<ResizeDirection> activeModes = EnumSet
            .noneOf(ResizeDirection.class);

    private Point movePoint = null;

    public EditRectangleInputHandler(EditRectangleTarget target) {
        this.target = target;
    }

    @Override
    public boolean handleMouseMove(int x, int y) {
        Rectangle rect = target.getRectangle();
        if (rect == null) {
            return false;
        }

        if (ResizeDirection.NORTH.intersects(rect, x, y)) {
            if (ResizeDirection.EAST.intersects(rect, x, y)) {
                setCursor(SWT.CURSOR_SIZENE);
            } else if (ResizeDirection.WEST.intersects(rect, x, y)) {
                setCursor(SWT.CURSOR_SIZENW);
            } else {
                setCursor(SWT.CURSOR_SIZEN);
            }
        } else if (ResizeDirection.SOUTH.intersects(rect, x, y)) {
            if (ResizeDirection.EAST.intersects(rect, x, y)) {
                setCursor(SWT.CURSOR_SIZESE);
            } else if (ResizeDirection.WEST.intersects(rect, x, y)) {
                setCursor(SWT.CURSOR_SIZESW);
            } else {
                setCursor(SWT.CURSOR_SIZES);
            }
        } else if (ResizeDirection.EAST.intersects(rect, x, y)) {
            setCursor(SWT.CURSOR_SIZEE);
        } else if (ResizeDirection.WEST.intersects(rect, x, y)) {
            setCursor(SWT.CURSOR_SIZEW);
        } else if (rect.contains(x, y)) {
            setCursor(SWT.CURSOR_HAND);
        } else {
            unsetCursor();
        }
        return true;
    }

    @Override
    public boolean handleMouseDown(int x, int y, int mouseButton) {
        if (mouseButton != MOUSE_BUTTON) {
            return false;
        }
        Rectangle rect = target.getRectangle();
        if (rect == null) {
            return false;
        }
        for (ResizeDirection mode : ResizeDirection.values()) {
            if (mode.intersects(rect, x, y)) {
                activeModes.add(mode);
            }
        }
        if (activeModes.isEmpty()) {
            if (rect.contains(x, y)) {
                movePoint = new Point(x, y);
                return true;
            }
            return false;
        } else {
            return true;
        }
    }

    @Override
    public boolean handleMouseDownMove(int x, int y, int mouseButton) {
        if (mouseButton != MOUSE_BUTTON) {
            return false;
        }
        Rectangle rect = target.getRectangle();
        if (rect == null) {
            return false;
        }
        if (movePoint != null) {
            int xDist = x - movePoint.x;
            int yDist = y - movePoint.y;
            target.resize(new Rectangle(rect.x + xDist, rect.y + yDist,
                    rect.width, rect.height));
            movePoint = new Point(x, y);
            setCursor(SWT.CURSOR_SIZEALL);
            return true;
        } else if (!activeModes.isEmpty()) {
            for (ResizeDirection mode : activeModes) {
                rect = mode.resize(rect, x, y);
            }
            target.resize(rect);
            return true;
        }
        return false;
    }

    @Override
    public boolean handleMouseUp(int x, int y, int mouseButton) {
        if (handleMouseDownMove(x, y, mouseButton)) {
            activeModes.clear();
            movePoint = null;
            handleMouseMove(x, y);
            return true;
        } else {
            return false;
        }
    }

    @Override
    public boolean handleMouseExit(Event event) {
        unsetCursor();
        return false;
    }

    @Override
    public boolean handleMouseEnter(Event event) {
        return handleMouseMove(event.x, event.y);
    }

    public void enable(IDisplayPaneContainer container) {
        if (container == null) {
            return;
        }
        container.registerMouseHandler(this);
    }

    public void disable(IDisplayPaneContainer container) {
        if (container == null) {
            return;
        }
        container.unregisterMouseHandler(this);
        Display.getDefault().asyncExec(new Runnable() {

            @Override
            public void run() {
                unsetCursor();
            }
        });
    }

    protected void unsetCursor() {
        getShell().setCursor(null);
    }

    protected void setCursor(int cursor) {
        Shell shell = getShell();
        Display display = shell.getDisplay();
        shell.setCursor(display.getSystemCursor(cursor));
    }

    protected Shell getShell() {
        IWorkbenchWindow window = PlatformUI.getWorkbench()
                .getActiveWorkbenchWindow();
        return window.getShell();
    }

    public static interface EditRectangleTarget {

        public Rectangle getRectangle();

        public void resize(Rectangle rect);

    }

    private static enum ResizeDirection {
        NORTH {
            public boolean intersects(Rectangle rect, int cx, int cy) {
                int x = rect.x - CLICK_WIDTH;
                int y = rect.y - CLICK_WIDTH;
                int width = rect.width + CLICK_WIDTH * 2;
                int height = CLICK_WIDTH * 2;
                Rectangle area = new Rectangle(x, y, width, height);
                return area.contains(cx, cy);
            }

            public Rectangle resize(Rectangle rect, int cx, int cy) {
                int x = rect.x;
                int y = cy;
                int width = rect.width;
                int height = rect.height - y + rect.y;
                return new Rectangle(x, y, width, height);
            }
        },
        SOUTH {
            public boolean intersects(Rectangle rect, int cx, int cy) {
                int x = rect.x - CLICK_WIDTH;
                int y = rect.y + rect.height - CLICK_WIDTH;
                int width = rect.width + CLICK_WIDTH * 2;
                int height = CLICK_WIDTH * 2;
                Rectangle area = new Rectangle(x, y, width, height);
                return area.contains(cx, cy);
            }

            public Rectangle resize(Rectangle rect, int cx, int cy) {
                int x = rect.x;
                int y = rect.y;
                int width = rect.width;
                int height = rect.height + cy - rect.y - rect.height;
                return new Rectangle(x, y, width, height);
            }
        },
        WEST {
            public boolean intersects(Rectangle rect, int cx, int cy) {
                int x = rect.x - CLICK_WIDTH;
                int y = rect.y - CLICK_WIDTH;
                int width = CLICK_WIDTH * 2;
                int height = rect.height + CLICK_WIDTH * 2;
                Rectangle area = new Rectangle(x, y, width, height);
                return area.contains(cx, cy);
            }

            public Rectangle resize(Rectangle rect, int cx, int cy) {
                int x = cx;
                int y = rect.y;
                int width = rect.width - cx + rect.x;
                int height = rect.height;
                return new Rectangle(x, y, width, height);
            }

        },
        EAST {
            public boolean intersects(Rectangle rect, int cx, int cy) {
                int x = rect.x + rect.width - CLICK_WIDTH;
                int y = rect.y - CLICK_WIDTH;
                int width = CLICK_WIDTH * 2;
                int height = rect.height + CLICK_WIDTH * 2;
                Rectangle area = new Rectangle(x, y, width, height);
                return area.contains(cx, cy);
            }

            public Rectangle resize(Rectangle rect, int cx, int cy) {
                int x = rect.x;
                int y = rect.y;
                int width = rect.width + cx - rect.x - rect.width;
                int height = rect.height;
                return new Rectangle(x, y, width, height);
            }

        };

        public abstract boolean intersects(Rectangle rect, int cx, int cy);

        public abstract Rectangle resize(Rectangle rect, int cx, int cy);

    };

}
