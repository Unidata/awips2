/*
 * gov.noaa.nws.ncep.ui.pgen.rsc.PgenDualPointDrawingTool
 * 
 * 29 April 2009
 *
 * This code has been developed by the NCEP/SIB for use in the AWIPS2 system.
 */

package gov.noaa.nws.ncep.ui.pgen.tools;

import gov.noaa.nws.ncep.ui.pgen.PgenUtil;
import gov.noaa.nws.ncep.ui.pgen.attrdialog.AttrSettings;
import gov.noaa.nws.ncep.ui.pgen.display.IAttribute;
import gov.noaa.nws.ncep.ui.pgen.elements.AbstractDrawableComponent;
import gov.noaa.nws.ncep.ui.pgen.elements.DrawableElement;
import gov.noaa.nws.ncep.ui.pgen.elements.DrawableElementFactory;
import gov.noaa.nws.ncep.ui.pgen.elements.DrawableType;

import java.util.ArrayList;

import com.raytheon.uf.viz.core.rsc.IInputHandler;
import com.vividsolutions.jts.geom.Coordinate;

/**
 * Implements a modal map tool for PGEN kink line drawing.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#     Engineer    Description
 * ------------ ----------  ----------- --------------------------
 * 11/13        TTR 850     J. Wu       Initial Creation
 * 
 * </pre>
 * 
 * @author J. Wu
 */

public class PgenKinkLineDrawingTool extends AbstractPgenDrawingTool {

    public PgenKinkLineDrawingTool() {

        super();

    }

    /**
     * Returns the current mouse handler.
     * 
     * @return
     */
    public IInputHandler getMouseHandler() {

        if (this.mouseHandler == null) {

            this.mouseHandler = new PgenKinkLineDrawingHandler();

        }

        return this.mouseHandler;
    }

    /**
     * Implements input handler for mouse events.
     * 
     * @author jun
     * 
     */

    public class PgenKinkLineDrawingHandler extends InputHandlerDefaultImpl {

        /**
         * Points of the new element.
         */
        private ArrayList<Coordinate> points = new ArrayList<Coordinate>();

        /**
         * Current element.
         */
        private AbstractDrawableComponent elem;

        /**
         * An instance of DrawableElementFactory, which is used to create new
         * elements.
         */
        private DrawableElementFactory def = new DrawableElementFactory();

        /*
         * (non-Javadoc)
         * 
         * @see com.raytheon.viz.ui.input.IInputHandler#handleMouseDown(int,
         * int, int)
         */
        @Override
        public boolean handleMouseDown(int anX, int aY, int button) {
            if (!isResourceEditable())
                return false;

            // Check if mouse is in geographic extent
            Coordinate loc = mapEditor.translateClick(anX, aY);
            if (loc == null || shiftDown)
                return false;

            if (button == 1) {

                if (points.size() < 2) {

                    points.add(loc);

                } else {

                    // create a new DrawableElement.
                    elem = def.create(DrawableType.KINKLINE,
                            (IAttribute) attrDlg, pgenCategory, pgenType,
                            points, drawingLayer.getActiveLayer());

                    // add the product to PGEN resource
                    drawingLayer.addElement(elem);

                    drawingLayer.removeGhostLine();
                    points.clear();

                    mapEditor.refresh();
                    AttrSettings.getInstance().setSettings(
                            (DrawableElement) elem);
                }

                return true;

            } else if (button == 3) {

                drawingLayer.removeGhostLine();
                mapEditor.refresh();

                if (points.size() == 0) {
                    PgenUtil.setSelectingMode();
                } else {
                    points.clear();
                }

                return true;

            } else if (button == 2) {

                return true;

            } else {

                return false;

            }

        }

        /*
         * (non-Javadoc)
         * 
         * @see com.raytheon.viz.ui.input.IInputHandler#handleMouseMove(int,
         * int)
         */
        @Override
        public boolean handleMouseMove(int x, int y) {
            if (!isResourceEditable())
                return false;

            // Check if mouse is in geographic extent
            Coordinate loc = mapEditor.translateClick(x, y);
            if (loc == null)
                return false;

            // create the ghost element and put it in the drawing layer
            AbstractDrawableComponent ghost = null;

            if (points != null && points.size() > 0) {

                if (points.size() > 1)
                    points.set(1, loc);
                else {
                    points.add(1, loc);
                }

                ghost = def.create(DrawableType.KINKLINE, (IAttribute) attrDlg,
                        pgenCategory, pgenType, points,
                        drawingLayer.getActiveLayer());

                drawingLayer.setGhostLine(ghost);

                mapEditor.refresh();

            }

            return false;

        }

        @Override
        public boolean handleMouseDownMove(int x, int y, int mouseButton) {
            if (!isResourceEditable() || shiftDown)
                return false;
            else
                return true;
        }
    }

}
