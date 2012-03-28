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

package com.raytheon.uf.viz.drawing.tools;

import java.util.ArrayList;
import java.util.List;

import org.eclipse.swt.SWT;
import org.eclipse.swt.graphics.Cursor;
import org.eclipse.swt.graphics.ImageData;
import org.eclipse.swt.widgets.Display;

import com.raytheon.uf.viz.core.rsc.IInputHandler;
import com.raytheon.uf.viz.drawing.AbstractDrawingTool;
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

            Cursor cursor = null;
            if (theDrawingLayer.isErase()) {
                ImageData data = ToolsUtils.getImageDescriptor("eraser.gif")
                        .getImageData();
                data.alpha = 255;
                cursor = new Cursor(Display.getCurrent(), data, 8, 8);
            } else {
                cursor = new Cursor(Display.getCurrent(), SWT.CURSOR_HAND);
            }

            Display.getCurrent().getActiveShell().setCursor(cursor);
            cursor.dispose();

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

            theDrawingLayer.addTempLine(ls);

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
            // change the cursor back on up
            Cursor cursor = new Cursor(Display.getCurrent(), SWT.CURSOR_ARROW);
            Display.getCurrent().getActiveShell().setCursor(cursor);
            cursor.dispose();

            if (button != 1) {
                return false;
            }

            theDrawingLayer.resetTemp();
            Coordinate[] coords = (Coordinate[]) pathList
                    .toArray(new Coordinate[pathList.size()]);
            GeometryFactory gf = new GeometryFactory();
            if (coords.length > 1) {
                LineString ls = gf.createLineString(coords);
                if (!theDrawingLayer.isErase()) {
                    theDrawingLayer.addLine(ls, null);
                }

                // TODO take this and send the coordinates
                // TransferLine object = new TransferLine();
                // object.setCoordinates(ls.getCoordinates());
                // object.setShape(theDrawingLayer.getTempWireframeShape());
                // object.setColor(theDrawingLayer.getCapability(
                // ColorableCapability.class).getColorAsString());
                // try {
                // // test writing out to a file for object size
                // UUID temp = UUID.randomUUID();
                // JAXBContext context = SerializationUtil.getJaxbContext();
                // Marshaller marshaller = context.createMarshaller();
                // marshaller.setProperty(Marshaller.JAXB_FORMATTED_OUTPUT,
                // false);
                // marshaller.marshal(object, new File(
                // "/home/mnash/Desktop/temp/jaxb" + temp));
                // // SerializationUtil.jaxbMarshalToXmlFile(object,
                // // "/home/mnash/Desktop/temp/jaxb" + temp);
                // } catch (JAXBException e) {
                // e.printStackTrace();
                // }
            }
            return true;
        }
    }
}
