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

import com.raytheon.uf.viz.core.rsc.IInputHandler;
import com.raytheon.viz.ui.input.InputAdapter;
import com.vividsolutions.jts.geom.Coordinate;
import com.vividsolutions.jts.geom.GeometryFactory;
import com.vividsolutions.jts.geom.LineString;

/**
 * A regular line tool
 * 
 * @author chammack
 * 
 */
public class LineDrawingTool extends AbstractDrawingTool {

    /** The mouse handler */
    protected IInputHandler theHandler;

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.viz.drawing.AbstractDrawingTool#getMouseHandler()
     */
    @Override
    public IInputHandler getMouseHandler() {
        if (theHandler == null) {
            theHandler = new LineDrawingHandler();
        }
        return theHandler;
    }

    public class LineDrawingHandler extends InputAdapter {

        private int theFirstMouseX;

        private int theFirstMouseY;

        /*
         * (non-Javadoc)
         * 
         * @see com.raytheon.viz.ui.input.IInputHandler#handleMouseDown(int,
         * int, int)
         */
        public boolean handleMouseDown(int anX, int aY, int button) {
            if (button != 1)
                return false;
            theFirstMouseX = anX;
            theFirstMouseY = aY;
            return true;
        }

        /*
         * (non-Javadoc)
         * 
         * @see com.raytheon.viz.ui.input.IInputHandler#handleMouseDownMove(int,
         * int, int)
         */
        @Override
        public boolean handleMouseDownMove(int x, int y, int button) {
            if (button != 1)
                return false;

            Coordinate p1 = editor.translateClick(theFirstMouseX,
                    theFirstMouseY);
            Coordinate p2 = editor.translateClick(x, y);

            if (p1 == null || p2 == null)
                return true;

            GeometryFactory gf = new GeometryFactory();
            LineString ls = gf.createLineString(new Coordinate[] { p1, p2 });

            theDrawingLayer.drawLine(ls, false, null);
            editor.refresh();
            return true;
        }

        /*
         * (non-Javadoc)
         * 
         * @see com.raytheon.viz.ui.input.IInputHandler#handleMouseUp(int, int,
         * int)
         */
        @Override
        public boolean handleMouseUp(int x, int y, int button) {
            if (button != 1)
                return false;
            Coordinate p1 = editor.translateClick(theFirstMouseX,
                    theFirstMouseY);
            Coordinate p2 = editor.translateClick(x, y);

            if (p1 == null || p2 == null)
                return true;

            GeometryFactory gf = new GeometryFactory();
            LineString ls = gf.createLineString(new Coordinate[] { p1, p2 });

            theDrawingLayer.drawLine(ls, true, null);
            editor.refresh();
            return true;
        }

    }

}
