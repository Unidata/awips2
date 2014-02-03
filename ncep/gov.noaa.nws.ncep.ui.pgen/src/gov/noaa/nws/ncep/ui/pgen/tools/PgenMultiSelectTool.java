/*
 * gov.noaa.nws.ncep.ui.pgen.tools.PgenMultiSelectTool
 * 
 * 22 August 2009
 *
 * This code has been developed by the NCEP/SIB for use in the AWIPS2 system.
 */

package gov.noaa.nws.ncep.ui.pgen.tools;

import gov.noaa.nws.ncep.ui.pgen.PgenSession;
import gov.noaa.nws.ncep.ui.pgen.attrdialog.AttrDlgFactory;
import gov.noaa.nws.ncep.ui.pgen.attrdialog.FrontAttrDlg;
import gov.noaa.nws.ncep.ui.pgen.attrdialog.SymbolAttrDlg;
import gov.noaa.nws.ncep.ui.pgen.attrdialog.WatchBoxAttrDlg;
import gov.noaa.nws.ncep.ui.pgen.attrdialog.vaadialog.VolcanoVaaAttrDlg;
import gov.noaa.nws.ncep.ui.pgen.contours.Contours;
import gov.noaa.nws.ncep.ui.pgen.elements.AbstractDrawableComponent;
import gov.noaa.nws.ncep.ui.pgen.elements.DrawableElement;
import gov.noaa.nws.ncep.ui.pgen.elements.DrawableElementFactory;
import gov.noaa.nws.ncep.ui.pgen.elements.DrawableType;
import gov.noaa.nws.ncep.ui.pgen.elements.Line;
import gov.noaa.nws.ncep.ui.pgen.filter.AcceptFilter;
import gov.noaa.nws.ncep.ui.pgen.rsc.PgenResource;

import java.awt.Color;
import java.awt.Polygon;
import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;

import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.swt.SWT;
import org.eclipse.ui.PlatformUI;

import com.raytheon.uf.viz.core.rsc.IInputHandler;
import com.vividsolutions.jts.geom.Coordinate;

//import gov.noaa.nws.ncep.ui.display.InputHandlerDefaultImpl;

/**
 * Implements PGEN palette MultiSelect functions.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date       	Ticket#		Engineer	Description
 * ------------	----------	-----------	--------------------------
 * 08/09		#149		B. Yin   	Initial Creation.
 * 04/10		#165		G. Zhang	add support for VAA.
 * 10/10        #289       Archana    Added logic to handle the delete key    
 * 07/12		#610		B. Yin		Make the multi-select work for GFA.
 * 12/12		#908		B. Yin		Do not change to selecting mode. 
 * 04/13		#874		B. Yin		Handle elements in contours.
 * 12/13        #1065       J. Wu       use LineAttrDlg for kink lines.
 * </pre>
 * 
 * @author B. Yin
 */
public class PgenMultiSelectTool extends AbstractPgenDrawingTool {

    @Override
    /**
     * Get the MultiSelect mouse handler.
     */
    public IInputHandler getMouseHandler() {
        if (this.mouseHandler == null) {

            this.mouseHandler = new PgenMultiSelectHandler();

        }

        return this.mouseHandler;
    }

    /**
     * Implements input handler for mouse events.
     * 
     * @author bingfan
     * 
     */
    public class PgenMultiSelectHandler extends InputHandlerDefaultImpl {

        // x of the first mouse click
        private int theFirstMouseX;

        // y of the first mouse click
        private int theFirstMouseY;

        // flag to indicate if Pgen category has been selected
        private boolean noCat;

        // flag to indicate if an area is selecting
        private boolean selectRect;

        // Current Pgen Category
        private String pgenCat;

        private String pgenObj;

        private List<Coordinate> polyPoints;

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

            theFirstMouseX = anX;
            theFirstMouseY = aY;

            if (button == 1) {

                pgenCat = PgenSession.getInstance().getPgenPalette()
                        .getCurrentCategory();
                pgenObj = PgenSession.getInstance().getPgenPalette()
                        .getCurrentObject();

                // if no pgen category, pop up a warning box.
                if (pgenCat == null) {

                    MessageDialog infoDlg = new MessageDialog(PlatformUI
                            .getWorkbench().getActiveWorkbenchWindow()
                            .getShell(), "Information", null,
                            "Please select a Pgen Class from the Palette.",
                            MessageDialog.INFORMATION, new String[] { "OK" }, 0);

                    infoDlg.open();
                    noCat = true;

                } else {
                    noCat = false;
                }

                return true;

            } else if (button == 3) {
                if (polyPoints == null || polyPoints.isEmpty()) {
                    // Close the attribute dialog and do the cleanup.
                    if (attrDlg != null) {
                        attrDlg.close();
                    }

                    attrDlg = null;
                    pgenCategory = null;
                    pgenType = null;

                    drawingLayer.removeGhostLine();
                    drawingLayer.removeSelected();
                    mapEditor.refresh();

                    // Keep in multi-selecting mode
                    // PgenUtil.setSelectingMode();
                }
                return false;

            } else {

                return false;

            }

        }

        /*
         * (non-Javadoc)
         * 
         * @see com.raytheon.viz.ui.input.IInputHandler#handleMouseDownMove(int,
         * int, int)
         */
        public boolean handleMouseDownMove(int anX, int aY, int button) {

            if (!isResourceEditable() || button != 1 || noCat) {
                return false;
            }

            selectRect = true;

            // draw the selected area
            ArrayList<Coordinate> points = new ArrayList<Coordinate>();

            points.add(mapEditor.translateClick(theFirstMouseX, theFirstMouseY));
            points.add(mapEditor.translateClick(theFirstMouseX, aY));
            points.add(mapEditor.translateClick(anX, aY));
            points.add(mapEditor.translateClick(anX, theFirstMouseY));

            DrawableElementFactory def = new DrawableElementFactory();
            Line ghost = (Line) def.create(DrawableType.LINE, null, "Lines",
                    "LINE_SOLID", points, drawingLayer.getActiveLayer());

            ghost.setLineWidth(1.0f);
            ghost.setColors(new Color[] { new java.awt.Color(255, 255, 255),
                    new java.awt.Color(255, 255, 255) });
            ghost.setClosed(true);
            ghost.setSmoothFactor(0);

            drawingLayer.setGhostLine(ghost);

            mapEditor.refresh();
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

            if (!isResourceEditable() || noCat) {
                return false;
            }

            if (button == 1) {

                if (shiftDown && !selectRect) {
                    if (polyPoints == null) {
                        polyPoints = new ArrayList<Coordinate>();
                    }
                    polyPoints.add(new Coordinate(anX, aY));

                } else if (selectRect) {

                    // select elements in the rectangle
                    int[] xpoints = { theFirstMouseX, theFirstMouseX, anX, anX };
                    int[] ypoints = { theFirstMouseY, aY, aY, theFirstMouseY };

                    Polygon rectangle = new Polygon(xpoints, ypoints, 4);

                    drawingLayer.addSelected(inPoly(rectangle));

                    drawingLayer.removeGhostLine();
                    selectRect = false;
                }

                else {

                    // Check if mouse is in geographic extent
                    Coordinate loc = mapEditor.translateClick(anX, aY);
                    if (loc == null)
                        return false;

                    if (!pgenCat.equalsIgnoreCase("met")) {

                        noCat = false;

                        // Get the nearest element and set it as the selected
                        // element.
                        // For contours, get the nearest DE inside of it.
                        AbstractDrawableComponent adc = null;
                        AbstractDrawableComponent contour = drawingLayer
                                .getNearestComponent(loc, new AcceptFilter(),
                                        false);
                        if (contour instanceof Contours) {
                            adc = drawingLayer.getNearestElement(loc,
                                    (Contours) contour);

                        } else {
                            adc = drawingLayer.getNearestComponent(loc,
                                    new AcceptFilter(), true);
                        }

                        if (adc != null
                                && adc.getPgenCategory().equalsIgnoreCase(
                                        pgenCat)) {

                            if (pgenType == null
                                    || pgenType.equalsIgnoreCase("MultiSelect")) {
                                pgenType = adc.getPgenType();
                            }

                            if (!pgenCat.equalsIgnoreCase("Text")
                                    || (pgenCat.equalsIgnoreCase("Text") && adc
                                            .getPgenType().equalsIgnoreCase(
                                                    pgenType))) {

                                if (!drawingLayer.getAllSelected()
                                        .contains(adc)) {
                                    drawingLayer.addSelected(adc);
                                } else {
                                    drawingLayer.removeSelected(adc);
                                }
                            }
                        }
                    } else if (pgenObj != null
                            && pgenObj.equalsIgnoreCase("GFA")) {
                        // Get the nearest element and set it as the selected
                        // element.
                        AbstractDrawableComponent adc = drawingLayer
                                .getNearestComponent(loc, new AcceptFilter(),
                                        true);

                        if (adc != null
                                && adc.getPgenType().equalsIgnoreCase("GFA")) {

                            if (pgenType == null
                                    || pgenType.equalsIgnoreCase("MultiSelect")) {
                                pgenType = adc.getPgenType();
                            }

                            // if ( adc.getPgenType().equalsIgnoreCase("GFA")){

                            if (!drawingLayer.getAllSelected().contains(adc)) {
                                drawingLayer.addSelected(adc);
                            } else {
                                drawingLayer.removeSelected(adc);
                            }
                            // }
                        }
                    }
                }
            }

            else if (button == 3) {

                if (polyPoints != null) {
                    if (polyPoints.size() > 2) {
                        int[] xpoints = new int[polyPoints.size()];
                        int[] ypoints = new int[polyPoints.size()];
                        for (int ii = 0; ii < polyPoints.size(); ii++) {
                            xpoints[ii] = (int) polyPoints.get(ii).x;
                            ypoints[ii] = (int) polyPoints.get(ii).y;
                        }

                        Polygon poly = new Polygon(xpoints, ypoints,
                                polyPoints.size());
                        drawingLayer.addSelected(inPoly(poly));

                        drawingLayer.removeGhostLine();

                    }

                    polyPoints.clear();
                    shiftDown = false;
                }
            }

            // pop up attribute window
            if (attrDlg != null && attrDlg.getShell() == null) {
                attrDlg = null;
            }

            if (attrDlg == null && drawingLayer.getAllSelected() != null
                    && !drawingLayer.getAllSelected().isEmpty()) {
                if (pgenCat.equalsIgnoreCase("MET")) {
                    pgenType = pgenObj;
                }

                // Use line attribute dialog for kink lines.
                if (pgenType != null
                        && (pgenType.equals("KINK_LINE_1") || pgenType
                                .equals("KINK_LINE_2"))) {
                    pgenType = null;
                }

                attrDlg = AttrDlgFactory.createAttrDlg(pgenCat, pgenType,
                        PlatformUI.getWorkbench().getActiveWorkbenchWindow()
                                .getShell());

                // NO Volcano attributes editing from multiple selecting
                if (attrDlg instanceof VolcanoVaaAttrDlg)
                    return false;

                // attrDlg.setBlockOnOpen(false);
                if (attrDlg != null) {
                    attrDlg.setBlockOnOpen(false);
                    attrDlg.open();
                    // attrDlg.setMultiSelectFlag(true);
                    attrDlg.enableButtons();
                    attrDlg.setPgenCategory(pgenCat);
                    attrDlg.setPgenType(null);
                    attrDlg.setDrawingLayer(drawingLayer);
                    attrDlg.setMapEditor(mapEditor);

                    if (attrDlg instanceof SymbolAttrDlg) {
                        ((SymbolAttrDlg) attrDlg).enableLatLon(false);
                    } else if (attrDlg instanceof WatchBoxAttrDlg) {
                        ((WatchBoxAttrDlg) attrDlg).enableShapeBtn(false);
                        ((WatchBoxAttrDlg) attrDlg).enableDspBtn(false);
                    } else if (attrDlg instanceof FrontAttrDlg) {
                        // for fronts with two color buttons
                        for (AbstractDrawableComponent adc : drawingLayer
                                .getAllSelected()) {
                            if (adc instanceof DrawableElement) {
                                if (((DrawableElement) adc).getColors().length > 1) {
                                    ((FrontAttrDlg) attrDlg)
                                            .setColor(new Color[] {
                                                    Color.green, Color.green });
                                }
                            }

                        }
                    }
                }

            }
            // System.out.println("From handleMouseUp()");
            mapEditor.setFocus();
            mapEditor.refresh();

            return true;
        }

        @Override
        public boolean handleMouseMove(int anX, int aY) {

            if (!isResourceEditable() || noCat) {
                return false;
            }

            // draw a ghost ploygon
            if (polyPoints != null && !polyPoints.isEmpty()) {

                polyPoints.add(new Coordinate(anX, aY));

                if (polyPoints.size() > 1) {

                    ArrayList<Coordinate> points = new ArrayList<Coordinate>();

                    for (Coordinate loc : polyPoints) {
                        points.add(mapEditor.translateClick(loc.x, loc.y));
                    }

                    DrawableElementFactory def = new DrawableElementFactory();
                    Line ghost = (Line) def.create(DrawableType.LINE, null,
                            "Lines", "LINE_SOLID", points,
                            drawingLayer.getActiveLayer());

                    ghost.setLineWidth(1.0f);
                    ghost.setColors(new Color[] {
                            new java.awt.Color(255, 255, 255),
                            new java.awt.Color(255, 255, 255) });
                    ghost.setClosed(true);
                    ghost.setSmoothFactor(0);

                    drawingLayer.setGhostLine(ghost);
                }

                mapEditor.refresh();

                polyPoints.remove(polyPoints.size() - 1);
            }
            return true;
        }

        @Override
        public boolean handleKeyDown(int keyCode) {
            if (!isResourceEditable())
                return false;

            if (keyCode == SWT.SHIFT) {
                shiftDown = true;
            } else if (keyCode == SWT.DEL) {
                PgenResource pResource = PgenSession.getInstance()
                        .getPgenResource();
                pResource.deleteSelectedElements();
                // System.out.println("Pgen elements deleted from PgenMultiSelect");
            }
            return true;
        }

        /**
         * return all elements of the current Pgen category in the input area.
         * 
         * @param poly
         * @return
         */
        private List<AbstractDrawableComponent> inPoly(Polygon poly) {

            String pgType = null;
            Iterator<AbstractDrawableComponent> it = drawingLayer
                    .getActiveLayer().getComponentIterator();
            List<AbstractDrawableComponent> adcList = new ArrayList<AbstractDrawableComponent>();

            while (it.hasNext()) {
                AbstractDrawableComponent adc = it.next();

                if (adc instanceof Contours) {
                    adcList.addAll(contourChildrenInPoly((Contours) adc, poly));
                }
                // if category is text, all elements need to be the same pgen
                // type
                else if ((pgType == null && adc.getPgenCategory()
                        .equalsIgnoreCase(pgenCat))
                        || (pgType != null && adc.getPgenType()
                                .equalsIgnoreCase(pgType))) {
                    List<Coordinate> pts = adc.getPoints();
                    for (Coordinate pt : pts) {
                        double pix[] = mapEditor.translateInverseClick(pt);
                        if (poly.contains(pix[0], pix[1])) {
                            adcList.add(adc);
                            if (pgenCat.equalsIgnoreCase("Text")) {
                                pgType = adc.getPgenType();
                                PgenMultiSelectTool.this.pgenType = pgType;
                            }
                            break;
                        }
                    }
                }
            }

            return adcList;
        }

        /**
         * Returns all elements of current Pgen category in a specified contour
         * in the input area.
         * 
         * @param con
         *            - a contour object
         * @param poly
         *            - polygon
         * @return - a list of drawable elements
         */
        private List<AbstractDrawableComponent> contourChildrenInPoly(
                Contours con, Polygon poly) {
            Iterator<DrawableElement> it = con.createDEIterator();
            List<AbstractDrawableComponent> adcList = new ArrayList<AbstractDrawableComponent>();

            while (it.hasNext()) {
                DrawableElement de = it.next();

                if (de.getPgenCategory().equalsIgnoreCase(pgenCat)) {
                    List<Coordinate> pts = de.getPoints();
                    for (Coordinate pt : pts) {
                        double pix[] = mapEditor.translateInverseClick(pt);
                        if (poly.contains(pix[0], pix[1])) {
                            adcList.add(de);
                            break;
                        }
                    }
                }
            }

            return adcList;
        }

    }
}
