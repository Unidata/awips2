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

package com.raytheon.viz.drawing.fronts;

import org.eclipse.swt.SWT;
import org.eclipse.swt.widgets.Event;

import com.raytheon.uf.viz.core.rsc.IInputHandler;
import com.raytheon.viz.adapter.CoordConverter;
import com.raytheon.viz.drawing.AbstractDrawingTool;
import com.raytheon.viz.ui.input.InputAdapter;
import com.vividsolutions.jts.geom.Coordinate;
import com.vividsolutions.jts.geom.GeometryFactory;
import com.vividsolutions.jts.geom.LineString;

import fsl.tools.scribble.Front;
import fsl.tools.scribble.WarmFront;

/**
 * Abstract Front drawing tool
 * 
 * <pre>
 * 
 *    SOFTWARE HISTORY
 *   
 *    Date         Ticket#     Engineer    Description
 *    ------------ ----------  ----------- --------------------------
 *    Oct 26, 2006 66          chammack    Initial Creation.
 * 
 * </pre>
 * 
 * @author chammack
 * @version 1
 */
public abstract class AbstractFrontTool extends AbstractDrawingTool {
    /** The mouse handler */
    protected IInputHandler theHandler;

    protected CoordConverter cc;

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.viz.drawing.AbstractDrawingTool#getMouseHandler()
     */
    @Override
    public IInputHandler getMouseHandler() {
        cc = new CoordConverter(editor);
        if (theHandler == null) {
            theHandler = new FrontDrawingHandler();
        }
        return theHandler;
    }

    /**
     * Instantiate a front
     * 
     * @return the front
     */
    public abstract Front createFront();

    public class FrontDrawingHandler extends InputAdapter {

        private int theLastMouseX;

        private int theLastMouseY;

        private Front currentFront;

        /*
         * (non-Javadoc)
         * 
         * @see com.raytheon.viz.ui.input.IInputHandler#handleMouseDown(int,
         * int, int)
         */
        public boolean handleMouseDown(int x, int y, int button) {
            if (button != 1)
                return false;
            Event e = new Event();
            e.type = SWT.MouseDown;
            e.x = x;
            e.y = y;

            currentFront = createFront();
            currentFront.startEdit(WarmFront.FINISH_CREATION, e, editor
                    .getActiveDisplayPane().getTarget(), cc);
            theLastMouseX = x;
            theLastMouseY = y;
            return true;
        }

        /*
         * (non-Javadoc)
         * 
         * @see com.raytheon.viz.ui.input.IInputHandler#handleMouseDownMove(int,
         * int, int)
         */
        public boolean handleMouseDownMove(int x, int y, int button) {

            Coordinate p1 = editor.translateClick(theLastMouseX, theLastMouseY);
            Coordinate p2 = editor.translateClick(x, y);

            if (p1 == null || p2 == null)
                return true;

            GeometryFactory gf = new GeometryFactory();
            LineString ls = gf.createLineString(new Coordinate[] { p1, p2 });

            theDrawingLayer.drawTempLine(ls, true);

            Event e = new Event();
            e.type = SWT.MouseMove;
            e.x = x;
            e.y = y;

            currentFront.edit(e, editor.getActiveDisplayPane().getTarget(), cc);

            theLastMouseX = x;
            theLastMouseY = y;
            editor.refresh();
            return true;
        }

        /*
         * (non-Javadoc)
         * 
         * @see com.raytheon.viz.ui.input.IInputHandler#handleMouseUp(int, int,
         * int)
         */
        public boolean handleMouseUp(int x, int y, int button) {
            if (button != 1)
                return false;

            Event e = new Event();
            e.type = SWT.MouseUp;
            e.x = x;
            e.y = y;

            currentFront.edit(e, editor.getActiveDisplayPane().getTarget(), cc);
            currentFront.finishEdit(editor.getActiveDisplayPane().getTarget(),
                    cc);
            currentFront.smooth(editor.getActiveDisplayPane().getTarget(), cc);
            theDrawingLayer.addFront(currentFront, true, null);
            theDrawingLayer.resetTemp();

            editor.refresh();
            return true;
        }

    }
}
