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
package com.raytheon.viz.awipstools.ui.action;

import org.eclipse.swt.SWT;
import org.eclipse.ui.IWorkbenchWindow;
import org.eclipse.ui.PlatformUI;

import com.raytheon.uf.viz.core.drawables.IDescriptor;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.rsc.LoadProperties;
import com.raytheon.uf.viz.core.rsc.tools.action.AbstractGenericToolAction;
import com.raytheon.viz.awipstools.ui.layer.ShearLayer;
import com.raytheon.viz.ui.input.InputAdapter;
import com.vividsolutions.jts.geom.Coordinate;

/**
 * Handles the shear action.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 01/11/2010               mnash
 * 
 * </pre>
 * 
 * @author mnash
 * @version 1.0
 */

public abstract class ShearAction extends AbstractGenericToolAction<ShearLayer> {

    protected ShearLayer getResource(LoadProperties loadProperties,
            IDescriptor descriptor) throws VizException {
        ShearLayer sLayer = super.getResource(loadProperties, descriptor);
        sLayer.setMouseHandler(new MouseHandler(sLayer));
        return sLayer;
    }

    /**
     * Associated navigation modes:
     * <UL>
     * <LI>CREATE - Create the initial baseline
     * <LI>MOVE_LINE - Move an existing line within the baseline.
     * <LI>MOVE_POINT - Move one endpoint of an existing baseline linestring.
     * <LI>PAN - Allow other tools, such as pan, to have control
     */
    private enum Mode {
        CREATE, MOVE_LINE, MOVE_POINT, PAN
    };

    public class MouseHandler extends InputAdapter {

        private int indexOfMovedEndpoint;

        private ShearLayer shearLayer;

        /** The mode of the mouse. By default, create */
        private Mode mode = Mode.CREATE;

        /** The last mouse position - x */
        private int lastMouseX = -1;

        /** The last mouse position - y */
        private int lastMouseY = -1;

        /** The index of the line to be moved */
        private int lineToMove;

        /** The millisecond time of the right mouse button down event */
        private long rightMouseButtonDownTime;

        private Coordinate coordinateMoved;

        private Coordinate coordinateFound = null;

        public MouseHandler(ShearLayer shearLayer) {
            this.shearLayer = shearLayer;
        }

        public boolean handleMouseDown(int x, int y, int mouseButton) {
            lastMouseX = x;
            lastMouseY = y;

            if (shearLayer.isEditable()) {
                Coordinate c = editor.translateClick(x, y);

                if (mouseButton == 1) {
                    lineToMove = -1;
                    coordinateFound = shearLayer.isInsideEndpoint(c);

                    if (coordinateFound != null) {
                        this.mode = Mode.MOVE_POINT;
                        coordinateMoved = coordinateFound;
                        return true;
                    }

                    if ((lineToMove = shearLayer.isInsideLine(c)) != -1) {
                        this.mode = Mode.MOVE_LINE;
                        return true;
                    }
                } else if (mouseButton == 3) {
                    // move prior unmoved end point
                    this.rightMouseButtonDownTime = System.currentTimeMillis();
                }
            } else {
                this.mode = Mode.PAN;
            }
            editor.refresh();
            return false;
        }

        /*
         * (non-Javadoc)
         * 
         * @see com.raytheon.viz.ui.map.IMouseHandler#handleMouseDownMove(int,
         * int)
         */
        public boolean handleMouseDownMove(int x, int y, int button) {

            if (button != 1)
                return false;

            if (this.mode == Mode.PAN)
                return false;

            if (this.mode == Mode.MOVE_LINE || this.mode == Mode.MOVE_POINT) {
                Coordinate c = editor.translateClick(lastMouseX, lastMouseY);
                Coordinate c2 = editor.translateClick(x, y);

                Coordinate delta = new Coordinate(c2.x - c.x, c2.y - c.y);

                if (this.mode == Mode.MOVE_LINE) {
                    shearLayer.moveBaseline(delta, this.lineToMove);
                } else {
                    shearLayer.movePoint(delta, coordinateMoved);
                }

                lastMouseX = x;
                lastMouseY = y;
                editor.refresh();
                return true;
            }

            return true;
        }

        /*
         * (non-Javadoc)
         * 
         * @see com.raytheon.viz.ui.map.IMouseHandler#handleMouseUp(int, int)
         */
        public boolean handleMouseUp(int x, int y, int mouseButton) {
            if (mouseButton == 3) {
                if (System.currentTimeMillis() - this.rightMouseButtonDownTime < 275) {
                    Coordinate c = editor.translateClick(x, y);

                    // move prior unmoved end point
                    Coordinate[] coords = shearLayer.getBaseline()
                            .getCoordinates();
                    indexOfMovedEndpoint = (indexOfMovedEndpoint >= coords.length - 1) ? 0
                            : ++indexOfMovedEndpoint;
                    Coordinate coord = coords[indexOfMovedEndpoint];
                    Coordinate delta = new Coordinate(c.x - coord.x, c.y
                            - coord.y, c.z - coord.z);

                    shearLayer.movePoint(delta, coord);
                }
            } else if (this.mode == Mode.PAN)
                return false;

            // Default back to pan operation
            mode = Mode.PAN;
            return true;
        }

        /*
         * (non-Javadoc)
         * 
         * @see com.raytheon.viz.ui.mouse.IMouseHandler#handleMouseMove(int,
         * int)
         */
        public boolean handleMouseMove(int x, int y) {
            if (shearLayer.isEditable()) {
                Coordinate c = editor.translateClick(x, y);

                if (shearLayer.isInsideEndpoint(c) != null) {
                    // Change the cursor to a hand.
                    this.setCursorHand();
                    return true;
                }

                if (shearLayer.isInsideLine(c) != -1) {
                    // Change the cursor to crosshairs.
                    this.setCursorCross();
                    return true;
                }
            }

            this.changeCursorNormal();
            return false;
        }

        protected void changeCursorNormal() {
            this.updateCursorStandard(SWT.CURSOR_ARROW);
        }

        private void setCursorHand() {
            this.updateCursorStandard(SWT.CURSOR_HAND);
        }

        private void setCursorCross() {
            this.updateCursorStandard(SWT.CURSOR_SIZEALL);
        }

        private void updateCursorStandard(int cursorEnum) {
            IWorkbenchWindow window = PlatformUI.getWorkbench()
                    .getActiveWorkbenchWindow();

            window.getShell().setCursor(
                    window.getShell().getDisplay().getSystemCursor(cursorEnum));
        }
    }

}
