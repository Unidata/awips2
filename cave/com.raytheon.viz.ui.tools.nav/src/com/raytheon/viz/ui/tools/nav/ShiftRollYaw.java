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

package com.raytheon.viz.ui.tools.nav;

import com.raytheon.uf.viz.core.IView;
import com.raytheon.uf.viz.core.IView.POVShiftType;
import com.raytheon.uf.viz.core.drawables.IDescriptor;
import com.raytheon.uf.viz.core.map.IMapDescriptor;
import com.raytheon.viz.ui.input.InputAdapter;
import com.raytheon.viz.ui.tools.AbstractModalTool;

/**
 * Activate panning support in the editor
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date       	Ticket#		Engineer	Description
 * ------------	----------	-----------	--------------------------
 * 7/1/06                   chammack    Initial Creation.
 * 
 * </pre>
 * 
 * @author chammack
 * @version 1
 */
public class ShiftRollYaw extends AbstractModalTool {

    /** The mouse handler */
    private final PanHandler thePanHandler;

    /**
     * Constructor
     * 
     */
    public ShiftRollYaw() {
        super();
        thePanHandler = new PanHandler();
    }

    @Override
    protected void activateTool() {
        editor.registerMouseHandler(thePanHandler);
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.viz.ui.tools.AbstractModalTool#deactivateTool()
     */
    @Override
    public void deactivateTool() {
        if (editor != null)
            editor.unregisterMouseHandler(thePanHandler);
    }

    public class PanHandler extends InputAdapter {

        float theLastMouseX = 0;

        float theLastMouseY = 0;

        double[] lookatCoord = new double[] { 0.0, 0.0, 0.0 };

        private boolean fDown = false;

        /*
         * (non-Javadoc)
         * 
         * @see com.raytheon.viz.ui.input.IInputHandler#handleMouseDown(int,
         * int, int)
         */
        @Override
        public boolean handleMouseDown(int aX, int aY, int button) {
            if (button != 1 && button != 2)
                return false;

            IDescriptor id = editor.getActiveDisplayPane().getDescriptor();
            if (!(id instanceof IMapDescriptor)) {
                return false;
            }

            // if (button == 2) {
            // float deltaY = theLastMouseY - aY;
            //
            // double[] lookatCoord = ((IMapDescriptor) id)
            // .getMapCoords(editor.getActiveDisplayPane().getTarget()
            // .createRayFromMouse(new double[] { aX, aY }));
            //
            // // editor.getActiveDisplayPane().setLookAt(lookatCoord);
            // }

            theLastMouseX = aX;
            theLastMouseY = aY;

            return true;
        }

        /*
         * (non-Javadoc)
         * 
         * @see com.raytheon.viz.ui.input.IInputHandler#handleMouseDownMove(int,
         * int, int)
         */
        @Override
        public boolean handleMouseDownMove(int aX, int aY, int button) {

            IDescriptor id = editor.getActiveDisplayPane().getDescriptor();
            if (!(id instanceof IMapDescriptor)) {
                return false;
            }

            POVShiftType type = IView.POVShiftType.BOTH;
            if (button == 2) {
                type = IView.POVShiftType.EYE;
            }

            if (fDown) {
                type = IView.POVShiftType.FOCUS;

            }
            editor.getActiveDisplayPane().shiftPOV(
                    new double[] { theLastMouseX, theLastMouseY },
                    new double[] { aX, aY }, type);

            theLastMouseX = aX;
            theLastMouseY = aY;
            return true;
        }

        /*
         * (non-Javadoc)
         * 
         * @see com.raytheon.viz.ui.mouse.IMouseHandler#handleDoubleClick(int,
         * int)
         */
        public boolean handleDoubleClick(int x, int y) {
            editor.getActiveDisplayPane().setLookAt(new double[] { x, y });
            return true;

        }

        /*
         * (non-Javadoc)
         * 
         * @see com.raytheon.viz.ui.input.IInputHandler#handleKeyDown(int)
         */
        public boolean handleKeyDown(int keyCode) {
            if ((char) keyCode == 'f') {
                this.fDown = true;
            }
            return false;
        }

        /*
         * (non-Javadoc)
         * 
         * @see com.raytheon.viz.ui.input.IInputHandler#handleKeyUp(int)
         */
        public boolean handleKeyUp(int keyCode) {

            if ((char) keyCode == 'f') {
                this.fDown = false;
            }
            return false;
        }

    }

}
