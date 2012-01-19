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

package com.raytheon.viz.drawing;

import org.eclipse.swt.SWT;

import com.raytheon.uf.viz.core.rsc.IInputHandler;
import com.raytheon.viz.ui.input.InputAdapter;
import com.vividsolutions.jts.geom.Coordinate;

/**
 * Test Drawing Tool
 * 
 * Allows annotation of text on the map editor
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * May 8, 2007             chammack    Initial Creation.
 * 
 * </pre>
 * 
 * @author chammack
 * @version 1.0
 */
public class TextDrawingTool extends AbstractDrawingTool {

    /** The input handler */
    protected IInputHandler handler;

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.viz.drawing.AbstractDrawingTool#getMouseHandler()
     */
    @Override
    public IInputHandler getMouseHandler() {
        if (handler == null) {
            handler = new TextHandler();
        }
        return handler;
    }

    public class TextHandler extends InputAdapter {

        private int firstMouseX;

        private int firstMouseY;

        private boolean acceptInput;

        private boolean shiftDown;

        private StringBuffer text;

        /*
         * (non-Javadoc)
         * 
         * @see com.raytheon.viz.ui.input.IInputHandler#handleDoubleClick(int,
         * int)
         */
        public boolean handleDoubleClick(int x, int y) {
            firstMouseX = x;
            firstMouseY = y;
            acceptInput = true;

            this.text.setLength(0);

            return true;
        }

        /*
         * (non-Javadoc)
         * 
         * @see com.raytheon.viz.ui.input.IInputHandler#handleKeyDown(int)
         */
        public boolean handleKeyDown(int keyCode) {
            if (acceptInput == false)
                return true;

            Coordinate p = editor.translateClick(firstMouseX, firstMouseY);

            if (keyCode == SWT.CAPS_LOCK) {
                shiftDown = !shiftDown;
            } else if (keyCode == SWT.SHIFT) {
                shiftDown = true;
            } else if (keyCode == SWT.DEL || keyCode == SWT.BS) {
                text.deleteCharAt(text.length() - 1);
                theDrawingLayer.addText(text.toString(), p, null);
            } else if (keyCode == SWT.CR || keyCode == SWT.LF) {
                acceptInput = false;
            } else {
                if (text == null) {
                    text = new StringBuffer();
                }

                if (shiftDown) {
                    String s = "" + (char) keyCode;

                    text.append(s.toUpperCase());
                } else {
                    text.append((char) keyCode);
                }

                theDrawingLayer.addText(text.toString(), p, null);

            }
            editor.refresh();
            return true;
        }

        /*
         * (non-Javadoc)
         * 
         * @see com.raytheon.viz.ui.input.IInputHandler#handleKeyUp(int)
         */
        public boolean handleKeyUp(int keyCode) {
            if (keyCode == SWT.SHIFT) {
                shiftDown = false;
            }
            return true;
        }

    }

}
