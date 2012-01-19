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

import java.util.ArrayList;
import java.util.List;

import com.raytheon.uf.viz.core.rsc.IInputHandler;
import com.raytheon.viz.ui.input.InputAdapter;
import com.vividsolutions.jts.geom.Coordinate;
import com.vividsolutions.jts.geom.GeometryFactory;
import com.vividsolutions.jts.geom.LineString;

/**
 * Draw unmodified path shapes (i.e. "pencil tool")
 * 
 * @author chammack
 * 
 */
public class PathDrawingTool extends AbstractDrawingTool {

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
            theHandler = new PathDrawingHandler();
        }
        return theHandler;
    }

    public class PathDrawingHandler extends InputAdapter {

        private List<Coordinate> pathList;

        private int theLastMouseX;

        private int theLastMouseY;

        /*
         * (non-Javadoc)
         * 
         * @see com.raytheon.viz.ui.input.IInputHandler#handleMouseDown(int,
         * int, int)
         */
        @Override
        public boolean handleMouseDown(int anX, int aY, int button) {
            if (button != 1)
                return false;

            if (pathList != null)
                pathList.clear();
            else
                pathList = new ArrayList<Coordinate>();

            theLastMouseX = anX;
            theLastMouseY = aY;
            Coordinate p1 = editor.translateClick(theLastMouseX, theLastMouseY);
            pathList.add(p1);
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
            Coordinate p1 = editor.translateClick(theLastMouseX, theLastMouseY);
            Coordinate p2 = editor.translateClick(x, y);

            if (p1 == null || p2 == null)
                return true;

            pathList.add(p2);

            GeometryFactory gf = new GeometryFactory();
            LineString ls = gf.createLineString(new Coordinate[] { p1, p2 });

            theDrawingLayer.drawTempLine(ls, true);

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
        @Override
        public boolean handleMouseUp(int anX, int aY, int button) {
            if (button != 1)
                return false;
            theDrawingLayer.resetTemp();
            Coordinate[] coords = (Coordinate[]) pathList
                    .toArray(new Coordinate[pathList.size()]);
            GeometryFactory gf = new GeometryFactory();
            LineString ls = gf.createLineString(coords);

            theDrawingLayer.drawLine(ls, true, null);
            return true;
        }

    }

}
