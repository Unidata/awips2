/*****************************************************************************************
 * COPYRIGHT (c), 2007, RAYTHEON COMPANY
 * ALL RIGHTS RESERVED, An Unpublished Work 
 *
 * RAYTHEON PROPRIETARY
 * If the end user is not the U.S. Government or any agency thereof, use
 * or disclosure of data contained in this source code file is subject to
 * the proprietary restrictions set forth in the Master Rights File.
 *
 * U.S. GOVERNMENT PURPOSE RIGHTS NOTICE
 * If the end user is the U.S. Government or any agency thereof, this source
 * code is provided to the U.S. Government with Government Purpose Rights.
 * Use or disclosure of data contained in this source code file is subject to
 * the "Government Purpose Rights" restriction in the Master Rights File.
 *
 * U.S. EXPORT CONTROLLED TECHNICAL DATA
 * Use or disclosure of data contained in this source code file is subject to
 * the export restrictions set forth in the Master Rights File.
 ******************************************************************************************/
package com.raytheon.viz.awipstools.ui.layer;

import java.util.ArrayDeque;
import java.util.Arrays;
import java.util.Queue;

import org.eclipse.jface.action.Action;
import org.eclipse.jface.action.IMenuManager;
import org.eclipse.swt.SWT;
import org.eclipse.ui.IWorkbenchWindow;
import org.eclipse.ui.PlatformUI;

import com.raytheon.uf.viz.core.IDisplayPaneContainer;
import com.raytheon.uf.viz.core.drawables.IDescriptor;
import com.raytheon.uf.viz.core.drawables.IRenderableDisplay;
import com.raytheon.viz.awipstools.ToolsDataManager;
import com.raytheon.viz.awipstools.common.ToolsUiUtil;
import com.raytheon.viz.awipstools.ui.layer.InteractiveBaselinesLayer.Baseline;
import com.raytheon.viz.ui.cmenu.IContextMenuContributor;
import com.raytheon.viz.ui.cmenu.IContextMenuProvider;
import com.raytheon.viz.ui.input.InputAdapter;
import com.vividsolutions.jts.algorithm.Angle;
import com.vividsolutions.jts.geom.Coordinate;
import com.vividsolutions.jts.geom.GeometryFactory;
import com.vividsolutions.jts.geom.LineSegment;
import com.vividsolutions.jts.geom.LineString;

/**
 * Interface manager for Interactive Baselines
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Aug 29, 2013 2281       bsteffen    Rename ToolsUiUitil.
 * Sep 18, 2013 2360       njensen     Don't handle mouse actions when layer is invisible
 * 
 * 
 * </pre>
 * 
 * @author mschenke
 * @version 1.0
 */

public class InteractiveBaselineUIManager extends InputAdapter implements
        IContextMenuContributor, IContextMenuProvider {

    private static final int CLICK_RADIUS_PIX = InteractiveBaselinesLayer.CIRCLE_RADIUS_PIX * 3;

    private static final ToolsDataManager dataManager = ToolsDataManager
            .getInstance();

    private static final GeometryFactory gf = new GeometryFactory();

    /** Baseline layer we are interacting with */
    private InteractiveBaselinesLayer baselinesLayer;

    /** baseline currently hovered over */
    private Baseline hoverLine = null;

    /** The coordinates we are hovered over (moving or not) */
    private Coordinate[] hoverCoords = null;

    /** Last mouse x/y location */
    private int lastX, lastY;

    /** Selection coordinates in screen space */
    private int selectX = -1, selectY = -1;

    /** Flag if we are deleting instead of moving */
    private boolean deleting = false;

    /** Current cursor type */
    private int currentCursor = SWT.CURSOR_ARROW;

    /** Our Queue which we limit to size 5 of recently LRU lines */
    private Queue<String> selectionQueue = new ArrayDeque<String>();

    /** The baseline we are currently moving through "selection" */
    private String currentlySelecting = null;

    /**
     * For selection logic, if true it is the first time we selected a point
     * after moving the entire line
     */
    private boolean firstSelection = true;

    public InteractiveBaselineUIManager(InteractiveBaselinesLayer baselinesLayer) {
        this.baselinesLayer = baselinesLayer;

        // Build selection queue
        Baseline[] baselines = baselinesLayer.getCurrentBaselines();
        for (int i = 0, j = 0; i < baselines.length && j < 5; ++i, ++j) {
            selectionQueue.add(baselines[i].name);
        }
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.viz.ui.input.InputAdapter#handleMouseMove(int, int)
     */
    @Override
    public boolean handleMouseMove(int x, int y) {
        // In this method, we want to figure out what Baseline we are
        // hovered over and whether it is over the line or over a specific
        // point. In the case of the line, we will set hoverCoords to be all
        // coords in hoverLine, otherwise it will be just the coordinate we
        // are over. hoverLine will set inMotion when click drag occurs
        // until mouse up in which case it will take the inMotion LineString
        // and update the dataManager
        int prevX = lastX;
        int prevY = lastY;

        // Only update lastX/Y if within bounds, otherwise invalid
        if (withinBounds(x, y)) {
            lastX = x;
            lastY = y;
        }

        Baseline inMotion = baselinesLayer.getLineInMotion();

        if (inMotion == null) {
            // Reset if not in motion
            hoverLine = null;
            hoverCoords = null;
        }

        if (baselinesLayer.isEditable()
                && baselinesLayer.getProperties().isVisible()) {
            // Only operate if editable
            if (inMotion != null) {
                // Already have something in motion, process the move
                selectX = selectY = -1;
                move(hoverCoords, prevX, prevY, x, y);
                baselinesLayer.issueRefresh();
            } else {
                // Figure out if we are hovered near a line or point in a
                // line and setup hover variables
                IDisplayPaneContainer container = baselinesLayer
                        .getResourceContainer();
                Baseline[] lines = baselinesLayer.getCurrentBaselines();
                int mouse = SWT.CURSOR_ARROW;
                for (Baseline line : lines) {
                    int idx = ToolsUiUtil.closeToCoordinate(container,
                            line.line.getCoordinates(), x, y, CLICK_RADIUS_PIX);
                    if (idx >= 0) {
                        // We are close to a point in this line
                        mouse = SWT.CURSOR_HAND;
                        hoverLine = new Baseline(
                                (LineString) line.line.clone(), line.name);
                        hoverCoords = new Coordinate[] { hoverLine.line
                                .getCoordinateN(idx) };
                        break;
                    } else if (ToolsUiUtil.closeToLine(container,
                            line.line.getCoordinates(), x, y, CLICK_RADIUS_PIX)) {
                        // We are close this this line
                        mouse = SWT.CURSOR_SIZEALL;
                        hoverLine = new Baseline(
                                (LineString) line.line.clone(), line.name);
                        hoverCoords = hoverLine.line.getCoordinates();
                        break;
                    }
                }

                if (mouse != currentCursor) {
                    // Update the mouse
                    updateCursorStandard(mouse);
                    currentCursor = mouse;
                }
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
        if (baselinesLayer.isEditable()
                && baselinesLayer.getProperties().isVisible()) {
            Baseline inMotion = baselinesLayer.getLineInMotion();
            // Only operate if editable
            if (inMotion != null) {
                // Do nothing, we are already in motion
                return true;
            } else if (mouseButton == 3) {
                // Mark our selection location
                selectX = x;
                selectY = y;
                return true;
            } else if (hoverLine != null) {
                selectX = selectY = -1;
                if (mouseButton == 1) {
                    // Set inMotion so we move our hoverCoords
                    baselinesLayer.setLineInMotion(hoverLine);
                } else if (mouseButton == 2) {
                    if (hoverCoords.length == 1
                            && hoverLine.line.getNumPoints() > 2) {
                        // Near a single vertex, delete it. Don't replace
                        // until mouse up though. mark flag
                        deleting = true;
                        hoverLine = new Baseline(
                                gf.createLineString(deleteVertex()),
                                hoverLine.name);
                        baselinesLayer.setLineInMotion(hoverLine);
                    } else if (hoverCoords.length > 1) {
                        // Middle clicked over a line, add a vertex there
                        addVertex(x, y);
                    } else {
                        return false;
                    }
                }
                return true;
            }
        }
        return false;
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.viz.ui.input.InputAdapter#handleMouseUp(int, int, int)
     */
    @Override
    public boolean handleMouseUp(int x, int y, int mouseButton) {
        if (withinBounds(x, y)) {
            // Only set lastx/y if within bounds
            lastX = x;
            lastY = y;
        }
        if (baselinesLayer.isEditable()
                && baselinesLayer.getProperties().isVisible()) {
            Baseline inMotion = baselinesLayer.getLineInMotion();
            // Only process if editable
            if (selectX >= 0 && selectY >= 0) {
                // We selected a location before, select the original
                // location using select location algorithm
                selectLocation(selectX, selectY);
                selectX = selectY = -1;
            } else if (deleting) {
                // We are deleting a vertex, update the hoverLine
                saveBaseline(hoverLine.line, hoverLine.name);
                baselinesLayer.setLineInMotion(null);
                deleting = false;
            } else if (inMotion != null) {
                // We were moving a line, update the line
                saveBaseline(inMotion.line, inMotion.name);
                baselinesLayer.setLineInMotion(null);
            } else {
                return false;
            }
            handleMouseMove(x, y);
            baselinesLayer.issueRefresh();
            return true;
        }
        return false;
    }

    private void saveBaseline(LineString line, String name) {
        dataManager.setBaseline(name, line);

        // Maintain selectionQueue
        if (currentlySelecting != null) {
            selectionQueue.add(currentlySelecting);
            if (selectionQueue.size() > 5) {
                selectionQueue.poll();
            }
        }
        if (selectionQueue.contains(name)) {
            selectionQueue.remove(name);
        }
        selectionQueue.add(name);
        if (selectionQueue.size() > 5) {
            selectionQueue.poll();
        }
        currentlySelecting = null;
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.viz.ui.input.InputAdapter#handleMouseDownMove(int, int,
     * int)
     */
    @Override
    public boolean handleMouseDownMove(int x, int y, int mouseButton) {
        int prevX = lastX;
        int prevY = lastY;
        if (withinBounds(x, y)) {
            // Only set lastX/Y if we are within bounds
            lastX = x;
            lastY = y;
        }
        if (baselinesLayer.isEditable()
                && baselinesLayer.getProperties().isVisible()) {
            if (deleting) {
                baselinesLayer.issueRefresh();
                return true;
            } else if (baselinesLayer.getLineInMotion() != null) {
                // Only move if within bounds (still need to return true
                // though)
                if (withinBounds(x, y)) {
                    move(hoverCoords, prevX, prevY, x, y);
                }
                baselinesLayer.issueRefresh();
                return true;
            }
        }
        return false;
    }

    private boolean withinBounds(int x, int y) {
        // Checks if the x/y location is within the display bounds
        IRenderableDisplay display = baselinesLayer.getDescriptor()
                .getRenderableDisplay();
        return display.getBounds().contains(x, y);
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.viz.ui.cmenu.IContextMenuContributor#addContextMenuItems
     * (org.eclipse.jface.action.IMenuManager, int, int)
     */
    @Override
    public void addContextMenuItems(IMenuManager menuManager, final int x,
            final int y) {
        if (baselinesLayer.isEditable() == false) {
            return;
        }
        menuManager.add(new Action("Select Location") {
            @Override
            public void run() {
                selectLocation(x, y);
                baselinesLayer.issueRefresh();
            }
        });
        if (hoverLine != null) {
            if (hoverCoords.length == 1) {
                // Single vertex
                if (hoverLine.line.getNumPoints() > 2) {
                    // If more than 2 points, we can delete it
                    menuManager.add(new Action("Delete Vertex") {
                        @Override
                        public void run() {
                            Coordinate[] updated = deleteVertex();
                            dataManager.setBaseline(hoverLine.name,
                                    gf.createLineString(updated));
                            baselinesLayer.issueRefresh();
                        }
                    });
                }

                menuManager.add(new Action("Move Vertex") {
                    @Override
                    public void run() {
                        baselinesLayer.setLineInMotion(hoverLine);
                        baselinesLayer.issueRefresh();
                    }
                });
            } else {
                menuManager.add(new Action("Move Entire Element") {
                    @Override
                    public void run() {
                        moveEntireElement();
                        baselinesLayer.issueRefresh();
                    }
                });

                menuManager.add(new Action("Add Vertex") {
                    @Override
                    public void run() {
                        addVertex(x, y);
                        baselinesLayer.issueRefresh();
                    }
                });
            }

            menuManager.add(new Action("Delete Entire Element") {
                @Override
                public void run() {
                    deleteEntireElement();
                    baselinesLayer.issueRefresh();
                }
            });
        }
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.viz.ui.cmenu.IContextMenuProvider#provideContextMenuItems
     * (org.eclipse.jface.action.IMenuManager, int, int)
     */
    @Override
    public void provideContextMenuItems(IMenuManager menuManager, int x, int y) {
        if (baselinesLayer.isEditable()) {
            if (hoverLine != null) {
                // Only "provide" the menu if we are hovered over a line
                addContextMenuItems(menuManager, x, y);
            }
        }
    }

    private void move(Coordinate[] coords, double prevX, double prevY,
            double newX, double newY) {
        IDisplayPaneContainer container = baselinesLayer.getResourceContainer();
        // Move hoverCoords!
        double changeX = newX - prevX;
        double changeY = newY - prevY;
        for (Coordinate c : coords) {
            double[] screenLoc = container.translateInverseClick(c);
            screenLoc[0] += changeX;
            screenLoc[1] += changeY;
            Coordinate translated = ToolsUiUtil.translateClick(container,
                    baselinesLayer.getDescriptor(), screenLoc[0], screenLoc[1]);
            c.x = translated.x;
            c.y = translated.y;
            c.z = translated.z;
        }
    }

    /**
     * Update the window's shell cursor with the specified cursor val
     * 
     * @param cursorEnum
     */
    private void updateCursorStandard(int cursorEnum) {
        IWorkbenchWindow window = PlatformUI.getWorkbench()
                .getActiveWorkbenchWindow();

        window.getShell().setCursor(
                window.getShell().getDisplay().getSystemCursor(cursorEnum));
    }

    /**
     * TODO: Handle small possibility of line not existing anymore when
     * selecting
     * 
     * Selects location for the baseline.
     * 
     * @param x
     * @param y
     */
    private void selectLocation(int x, int y) {
        IDisplayPaneContainer container = baselinesLayer.getResourceContainer();
        IDescriptor descriptor = baselinesLayer.getDescriptor();

        boolean newLine = false;
        if (currentlySelecting == null) {
            newLine = true;
            firstSelection = true;
            currentlySelecting = selectionQueue.poll();
        }

        LineString lineString = dataManager.getBaseline(currentlySelecting);
        Coordinate[] coords = lineString.getCoordinates();

        if (newLine) {
            // New line selected, move entire line to point
            double[] firstLoc = container.translateInverseClick(coords[0]);
            move(coords, firstLoc[0], firstLoc[1], x, y);
        } else if (firstSelection) {
            // First time we've moved our new line, change to two points
            firstSelection = false;
            coords = new Coordinate[] { coords[0],
                    ToolsUiUtil.translateClick(container, descriptor, x, y) };
        } else {
            double[] d1 = container
                    .translateInverseClick(coords[coords.length - 1]);
            double[] d2 = container
                    .translateInverseClick(coords[coords.length - 2]);
            Coordinate c1 = new Coordinate(d1[0], d1[1]);
            Coordinate c2 = new Coordinate(d2[0], d2[1]);
            Coordinate c = new Coordinate(x, y);
            if (Angle.angleBetween(c2, c1, c) > (Math.PI / 2)) {
                coords = Arrays.copyOf(coords, coords.length + 1);
                coords[coords.length - 1] = ToolsUiUtil.translateClick(
                        container, descriptor, c.x, c.y);
            } else {
                selectionQueue.add(currentlySelecting);
                currentlySelecting = null;
                selectLocation(x, y);
                return;
            }
        }

        dataManager
                .setBaseline(currentlySelecting, gf.createLineString(coords));
        baselinesLayer.issueRefresh();
    }

    /**
     * Add's a vertex at the specified screen x/y in the hoverLine object at the
     * closest point in the line
     * 
     * @param x
     * @param y
     */
    private void addVertex(int x, int y) {
        Coordinate screenRefPoint = new Coordinate(x, y);
        IDisplayPaneContainer container = baselinesLayer.getResourceContainer();
        IDescriptor descriptor = baselinesLayer.getDescriptor();

        double bestDistance = Double.MAX_VALUE;
        int insertionPoint = 0;

        // Convert hoverCoords to screen coords
        Coordinate[] screenLocs = new Coordinate[hoverCoords.length];
        for (int i = 0; i < hoverCoords.length; ++i) {
            double[] screenLoc = container
                    .translateInverseClick(hoverCoords[i]);
            screenLocs[i] = new Coordinate(screenLoc[0], screenLoc[1]);
        }

        // Find best insertion point
        for (int i = 1; i < screenLocs.length; i++) {
            LineSegment segment = new LineSegment(screenLocs[i - 1],
                    screenLocs[i]);
            double distance = segment.distance(screenRefPoint);
            if (distance < bestDistance) {
                insertionPoint = i;
                bestDistance = distance;
            }
        }

        // Create new line based on insertion point
        Coordinate[] newLine = new Coordinate[hoverCoords.length + 1];
        for (int i = 0; i < newLine.length; i++) {
            if (i < insertionPoint) {
                newLine[i] = ToolsUiUtil.translateClick(container, descriptor,
                        screenLocs[i].x, screenLocs[i].y);
            } else if (i > insertionPoint) {
                newLine[i] = ToolsUiUtil.translateClick(container, descriptor,
                        screenLocs[i - 1].x, screenLocs[i - 1].y);
            } else {
                newLine[i] = ToolsUiUtil.translateClick(container, descriptor,
                        screenRefPoint.x, screenRefPoint.y);
            }
        }

        // Set inMotion/hoverLine/hoverCoords
        hoverLine = new Baseline(gf.createLineString(newLine), hoverLine.name);
        baselinesLayer.setLineInMotion(hoverLine);
        hoverCoords = new Coordinate[] { newLine[insertionPoint] };
        baselinesLayer.issueRefresh();
    }

    /**
     * Deletes hoverCoords[0] from hoverLine and returns resulting Coordinate[]
     * 
     * @return Coordinate[] without hoverCoords[0]
     */
    private Coordinate[] deleteVertex() {
        Coordinate toSkip = hoverCoords[0];
        Coordinate[] allCoords = hoverLine.line.getCoordinates();
        Coordinate[] updatedCoords = new Coordinate[allCoords.length - 1];
        for (int i = 0, j = 0; i < allCoords.length; ++i) {
            Coordinate use = allCoords[i];
            if (use != toSkip) {
                updatedCoords[j] = use;
                ++j;
            }
        }
        return updatedCoords;
    }

    /**
     * "Deletes" the entire selected element, actually adds to set to not draw
     */
    private void deleteEntireElement() {
        baselinesLayer.doNotDraw(hoverLine.name);
        if (hoverLine.name.equals(currentlySelecting)) {
            currentlySelecting = null;
        } else if (selectionQueue.contains(hoverLine.name)) {
            selectionQueue.remove(hoverLine.name);
            for (Baseline baseline : baselinesLayer.getCurrentBaselines()) {
                if (selectionQueue.contains(baseline.name) == false) {
                    selectionQueue.add(baseline.name);
                    break;
                }
            }
        }
        hoverLine = null;
        hoverCoords = null;
    }

    /**
     * Move the entire selected element, just set inMotion
     */
    private void moveEntireElement() {
        // Mark we are in motion!
        baselinesLayer.setLineInMotion(hoverLine);
    }
}
