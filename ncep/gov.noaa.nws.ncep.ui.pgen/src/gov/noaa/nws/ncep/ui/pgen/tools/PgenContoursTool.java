/*
 * gov.noaa.nws.ncep.ui.pgen.tools.PgenContoursTool
 * 
 * October 2009
 *
 * This code has been developed by the NCEP/SIB for use in the AWIPS2 system.
 */

package gov.noaa.nws.ncep.ui.pgen.tools;

import gov.noaa.nws.ncep.ui.pgen.PgenSession;
import gov.noaa.nws.ncep.ui.pgen.PgenUtil;
import gov.noaa.nws.ncep.ui.pgen.attrdialog.ContoursAttrDlg;
import gov.noaa.nws.ncep.ui.pgen.attrdialog.ContoursAttrDlg.ContourDrawingStatus;
import gov.noaa.nws.ncep.ui.pgen.contours.ContourCircle;
import gov.noaa.nws.ncep.ui.pgen.contours.ContourLine;
import gov.noaa.nws.ncep.ui.pgen.contours.ContourMinmax;
import gov.noaa.nws.ncep.ui.pgen.contours.Contours;
import gov.noaa.nws.ncep.ui.pgen.controls.CommandStackListener;
import gov.noaa.nws.ncep.ui.pgen.display.IAttribute;
import gov.noaa.nws.ncep.ui.pgen.display.ILine;
import gov.noaa.nws.ncep.ui.pgen.elements.AbstractDrawableComponent;
import gov.noaa.nws.ncep.ui.pgen.elements.Arc;
import gov.noaa.nws.ncep.ui.pgen.elements.DrawableElementFactory;
import gov.noaa.nws.ncep.ui.pgen.elements.DrawableType;
import gov.noaa.nws.ncep.ui.pgen.elements.Line;
import gov.noaa.nws.ncep.ui.pgen.elements.Outlook;
import gov.noaa.nws.ncep.ui.pgen.elements.Symbol;
import gov.noaa.nws.ncep.ui.pgen.elements.Text;
import gov.noaa.nws.ncep.viz.gempak.nativelib.LibraryLoader;

import java.util.ArrayList;
import java.util.Iterator;

import org.eclipse.core.commands.ExecutionEvent;
import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.ui.PlatformUI;

import com.raytheon.uf.viz.core.rsc.IInputHandler;
import com.vividsolutions.jts.geom.Coordinate;

/**
 * Implements a modal map tool for PGEN Contours drawing.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date       	Ticket#		Engineer	Description
 * ------------	----------	-----------	--------------------------
 * 10/09        #167		J. Wu  		Initial creation
 * 12/09		?			B. Yin		check if the attrDlg is 
 * 										the contours dialog
 * 12/09        #167		J. Wu  		Allow editing line and label attributes.
 * 06/10		#215		J. Wu		Added support for Contours Min/Max
 * 11/10		#345		J. Wu		Added support for Contours Circle
 * 02/11					J. Wu		Preserve auto/hide flags for text
 * 04/11		#?			B. Yin		Re-factor IAttribute
 * 11/11		#?			J. Wu		Add check for the existing Contours of
 * 										the same type.
 * 03/13		#927		B. Yin		Added right mouse click context menu
 * 
 * </pre>
 * 
 * @author J. Wu
 */

public class PgenContoursTool extends AbstractPgenDrawingTool implements
        CommandStackListener {

    /**
     * Points of the new element.
     */
    private ArrayList<Coordinate> points = new ArrayList<Coordinate>();

    /**
     * An instance of DrawableElementFactory, which is used to create new
     * elements.
     */
    private DrawableElementFactory def = new DrawableElementFactory();

    /**
     * Current Contours element.
     */
    private boolean addContourLine = false;

    private Contours elem = null;

    private Contours lastElem = null;
    
    private ExecutionEvent lastEvent = null;

    private int undo = -1;

    private int redo = -1;

    public PgenContoursTool() {

        super();

    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.viz.ui.tools.AbstractTool#runTool()
     */
    @Override
    protected void activateTool() {

        super.activateTool();
        LibraryLoader.load("g2g");

      
        /*
         * if the ExecutionEvent's trigger has been set, it should be something
         * from a Contours to start with. Load it's attributes to the Contours
         * attr Dialog. If not. we will start with a new Contours.
         */
        Object de = event.getTrigger();

        // The same tool could be activated again (for instance, click on PGEN palette and then click in the editor).
        // However the trigger of the event may not be the current contour if the contour is modified.
        if ( event != lastEvent ) {
            if (de instanceof Contours) {
                elem = (Contours) de;
                //addContourLine = true;
                this.setPgenSelectHandler();
            	PgenSession.getInstance().getPgenPalette().setActiveIcon("Select");
            } else {
                elem = null;
            }
        	lastEvent = event;
        }
        
        if (attrDlg instanceof ContoursAttrDlg) {
           // ((ContoursAttrDlg) attrDlg).disableActionButtons();
            ((ContoursAttrDlg)attrDlg).setDrawingTool( this );
            if ( de != null ){
            	((ContoursAttrDlg)attrDlg).setSelectMode();
            }
            else {
            	((ContoursAttrDlg)attrDlg).setDrawingStatus(ContoursAttrDlg.ContourDrawingStatus.DRAW_LINE);
            }
        }

        return;

    }

    /**
     * Returns the current mouse handler.
     * 
     * @return
     */
    public IInputHandler getMouseHandler() {

        if (this.mouseHandler == null) {

            this.mouseHandler = new PgenContoursHandler();

        }

        return this.mouseHandler;
    }

    /**
     * Implements input handler for mouse events.
     */
    public class PgenContoursHandler extends InputHandlerDefaultImpl {

        /*
         * (non-Javadoc)
         * 
         * @see com.raytheon.viz.ui.input.IInputHandler#handleMouseDown(int,
         * int, int)
         */
        @Override
        public boolean handleMouseUp(int anX, int aY, int button) {
            if (!isResourceEditable())
                return false;

            // Check if mouse is in geographic extent
            Coordinate loc = mapEditor.translateClick(anX, aY);
        	if ( loc == null || shiftDown ) return false;

            // Drawing Min/Max symbol
            if (attrDlg != null && ((ContoursAttrDlg) attrDlg).drawSymbol()) {

                if (button == 1) {
                    drawContourMinmax(loc);
                } else if (button == 3) {

                    points.clear();
                    if (attrDlg != null) {
                        ((ContoursAttrDlg) attrDlg).setDrawingStatus( ContourDrawingStatus.SELECT );
                    }
                    drawingLayer.removeGhostLine();

                    return true;
                } else {
                    return false;
                }
            }

            // Drawing Circle
            if (attrDlg != null && ((ContoursAttrDlg) attrDlg).drawCircle()) {

                if (button == 1) {

                    if (points.size() == 0) {

                        points.add(0, loc);

                    } else {

                        if (points.size() > 1)
                            points.remove(1);

                        points.add(1, loc);
                        drawContourCircle();
                    }

                    return true;

                } else if (button == 3) {

                    points.clear();
                    if (attrDlg != null) {
                        ((ContoursAttrDlg) attrDlg).setDrawingStatus(ContoursAttrDlg.ContourDrawingStatus.SELECT);
                    }
                    drawingLayer.removeGhostLine();

                    return true;
                } else {
                    return false;
                }

            }

            // Drawing line
            if (button == 1) {

                if (attrDlg != null
                        && !((ContoursAttrDlg) attrDlg).drawSymbol()
                        && !((ContoursAttrDlg) attrDlg).drawCircle()) {

                    points.add(loc);
                }

                return true;
            } else if (button == 3) {
                if (points.size() == 0) {
                	((ContoursAttrDlg) attrDlg).setDrawingStatus(ContoursAttrDlg.ContourDrawingStatus.SELECT);
                }
                else {
                	setDrawingMode();
                	drawContours();
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
        	if (  !isResourceEditable() || shiftDown ) return false;

            // Check if mouse is in geographic extent
            Coordinate loc = mapEditor.translateClick(x, y);
            if (loc == null)
                return false;

            // Draw a ghost contour min/max
            if (attrDlg != null && ((ContoursAttrDlg) attrDlg).drawSymbol()) {

                ContoursAttrDlg dlg = (ContoursAttrDlg) attrDlg;
                ContourMinmax ghost = null;
                ghost = new ContourMinmax(loc, dlg.getActiveSymbolClass(),
                        dlg.getActiveSymbolObjType(),
                        new String[] { dlg.getLabel() });

                IAttribute mmTemp = ((ContoursAttrDlg) attrDlg)
                        .getMinmaxTemplate();

                if (mmTemp != null) {
                    Symbol oneSymb = (Symbol) (ghost.getSymbol());
                    oneSymb.update(mmTemp);
                }

                IAttribute lblTemp = ((ContoursAttrDlg) attrDlg)
                        .getLabelTemplate();
                if (lblTemp != null) {
                    Text lbl = ghost.getLabel();
                    String[] oldText = lbl.getText();
                    boolean hide = lbl.getHide();
                    boolean auto = lbl.getAuto();
                    lbl.update(lblTemp);
                    lbl.setText(oldText);
                    lbl.setHide(hide);
                    lbl.setAuto(auto);
                }

                drawingLayer.setGhostLine(ghost);
                mapEditor.refresh();

                return false;

            }

            // Draw a ghost contour circle
            if (attrDlg != null && ((ContoursAttrDlg) attrDlg).drawCircle()) {

                if (points != null && points.size() >= 1) {

                    ContourCircle ghost = new ContourCircle(points.get(0), loc,
                            new String[] { ((ContoursAttrDlg) attrDlg)
                                    .getLabel() },
                            ((ContoursAttrDlg) attrDlg).hideCircleLabel());

                    IAttribute circleTemp = ((ContoursAttrDlg) attrDlg)
                            .getCircleTemplate();
                    if (circleTemp != null) {
                        ghost.getCircle().setColors(circleTemp.getColors());
                        ((Arc) ghost.getCircle()).setLineWidth(circleTemp
                                .getLineWidth());
                    }

                    IAttribute lblTemp = ((ContoursAttrDlg) attrDlg)
                            .getLabelTemplate();
                    if (lblTemp != null) {
                        Text lbl = ghost.getLabel();
                        String[] oldText = lbl.getText();
                        boolean hide = lbl.getHide();
                        boolean auto = lbl.getAuto();
                        lbl.update(lblTemp);
                        lbl.setText(oldText);
                        lbl.setHide(hide);
                        lbl.setAuto(auto);
                    }

                    drawingLayer.setGhostLine(ghost);
                    mapEditor.refresh();
                }

                return false;

            }

            // Draw a ghost ContourLine
            if (points != null && points.size() >= 1) {

                ArrayList<Coordinate> ghostPts = new ArrayList<Coordinate>(
                        points);
                ghostPts.add(loc);

                ContourLine cline = new ContourLine(
                        ghostPts,
                        ((ILine) attrDlg).isClosedLine(),
                        new String[] { ((ContoursAttrDlg) attrDlg).getLabel() },
                        ((ContoursAttrDlg) attrDlg).getNumOfLabels());

                IAttribute lineTemp = ((ContoursAttrDlg) attrDlg)
                        .getLineTemplate();

                if (lineTemp != null) {
                    Line oneLine = cline.getLine();
                    Boolean isClosed = oneLine.isClosedLine();
                    oneLine.update(lineTemp);
                    oneLine.setClosed(isClosed);
                }

                String lblstr = ((ContoursAttrDlg) attrDlg).getLabel();
                if (lblstr != null && lblstr.contains("9999")) {
                    cline.getLine().setSmoothFactor(0);
                }

                IAttribute lblTemp = ((ContoursAttrDlg) attrDlg)
                        .getLabelTemplate();
                if (lblTemp != null) {
                    for (Text lbl : cline.getLabels()) {
                        String[] oldText = lbl.getText();
                        boolean hide = lbl.getHide();
                        boolean auto = lbl.getAuto();
                        lbl.update(lblTemp);
                        lbl.setText(oldText);
                        lbl.setHide(hide);
                        lbl.setAuto(auto);
                    }
                }

                Contours el = (Contours) (def.create(DrawableType.CONTOURS,
                        null, "MET", "Contours", points,
                        drawingLayer.getActiveLayer()));

                cline.setParent(el);
                cline.getLine().setPgenType(
                        ((ContoursAttrDlg) attrDlg).getContourLineType());

                el.update((ContoursAttrDlg) attrDlg);
                el.add(cline);

                drawingLayer.setGhostLine(el);
                mapEditor.refresh();

            }

            return false;

        }

        @Override
        public boolean handleMouseDownMove(int x, int y, int mouseButton) {
        	if (  !isResourceEditable() || shiftDown ) return false;
        	else return true;
        }

        /*
         * create a Contours and add to the Pgen Resource.
         */
        private void drawContours() {

            if (points.size() > 1) {

                ContourLine cline = new ContourLine(
                        points,
                        ((ILine) attrDlg).isClosedLine(),
                        new String[] { ((ContoursAttrDlg) attrDlg).getLabel() },
                        ((ContoursAttrDlg) attrDlg).getNumOfLabels());

                cline.getLine().setPgenType(
                        ((ContoursAttrDlg) attrDlg).getContourLineType());

                IAttribute lineTemp = ((ContoursAttrDlg) attrDlg)
                        .getLineTemplate();
                if (lineTemp != null) {
                    Line oneLine = cline.getLine();
                    Boolean isClosed = oneLine.isClosedLine();
                    oneLine.update(lineTemp);
                    oneLine.setClosed(isClosed);
                }

                String lblstr = ((ContoursAttrDlg) attrDlg).getLabel();
                if (lblstr != null && lblstr.contains("9999")) {
                    cline.getLine().setSmoothFactor(0);
                }

                IAttribute lblTemp = ((ContoursAttrDlg) attrDlg)
                        .getLabelTemplate();
                if (lblTemp != null) {
                    for (Text lbl : cline.getLabels()) {
                        String[] oldText = lbl.getText();
                        boolean hide = lbl.getHide();
                        boolean auto = lbl.getAuto();
                        lbl.update(lblTemp);
                        lbl.setText(oldText);
                        lbl.setHide(hide);
                        lbl.setAuto(auto);
                    }
                }

                // Check if we need to add to existing contours or create a new
                // one
                elem = checkExistingContours();

                if (elem == null) {

                    /*
                     * create a new element with attributes from the Attr
                     * dialog, and add it to the PGEN Resource
                     */
                    elem = (Contours) (def.create(DrawableType.CONTOURS, null,
                            "MET", "Contours", points,
                            drawingLayer.getActiveLayer()));

                    cline.setParent(elem);
                    elem.update((ContoursAttrDlg) attrDlg);
                    elem.add(cline);
                    drawingLayer.addElement(elem);

                } else {

                    /*
                     * Make a copy of the existing element; update its
                     * attributes from those in the Attr Dialog; replace the
                     * existing element with the new one in the pgen resource -
                     * (This allows Undo/Redo)
                     */
                    Contours newElem = (Contours) elem.copy();

                    cline.setParent(newElem);

                    newElem.update((ContoursAttrDlg) attrDlg);

                    newElem.add(cline);

                    drawingLayer.replaceElement(elem, newElem);
                    elem = newElem;

                }

                ((ContoursAttrDlg) attrDlg).setCurrentContours(elem);

            }

            // Always clear the points for the next drawing.
            points.clear();

            // Update the display.
            drawingLayer.removeGhostLine();
            mapEditor.refresh();

        }

        /*
         * create a Contours and add to the Pgen Resource.
         */
        public void drawContourMinmax(Coordinate loc) {

            if (loc != null) {

                String cls = ((ContoursAttrDlg) attrDlg).getActiveSymbolClass();
                String type = ((ContoursAttrDlg) attrDlg)
                        .getActiveSymbolObjType();
                ContourMinmax cmm = new ContourMinmax(loc, cls, type,
                        new String[] { ((ContoursAttrDlg) attrDlg).getLabel() });

                IAttribute mmTemp = ((ContoursAttrDlg) attrDlg)
                        .getMinmaxTemplate();

                if (mmTemp != null) {
                    Symbol oneSymb = (Symbol) (cmm.getSymbol());
                    oneSymb.update(mmTemp);
                }

                IAttribute lblTemp = ((ContoursAttrDlg) attrDlg)
                        .getLabelTemplate();
                if (lblTemp != null) {
                    Text lbl = cmm.getLabel();
                    String[] oldText = lbl.getText();
                    boolean hide = lbl.getHide();
                    boolean auto = lbl.getAuto();
                    lbl.update(lblTemp);
                    lbl.setText(oldText);
                    lbl.setHide(hide);
                    lbl.setAuto(auto);
                }

                // Check if we need to add to existing contours or create a new
                // one
                elem = checkExistingContours();

                if (elem == null) {
                    /*
                     * create a new element with attributes from the Attr
                     * dialog, and add it to the PGEN Resource
                     */
                    elem = (Contours) (def.create(DrawableType.CONTOURS, null,
                            "MET", "Contours", points,
                            drawingLayer.getActiveLayer()));

                    cmm.setParent(elem);
                    elem.update((ContoursAttrDlg) attrDlg);
                    elem.add(cmm);

                    drawingLayer.addElement(elem);

                } else {

                    /*
                     * Make a copy of the existing element; update its
                     * attributes from those in the Attr Dialog; replace the
                     * existing element with the new one in the pgen resource -
                     * (This allows Undo/Redo)
                     */
                    Contours newElem = (Contours) elem.copy();

                    cmm.setParent(newElem);

                    newElem.update((ContoursAttrDlg) attrDlg);

                    newElem.add(cmm);

                    drawingLayer.replaceElement(elem, newElem);

                    lastElem = elem;
                    elem = newElem;

                }

                ((ContoursAttrDlg) attrDlg).setCurrentContours(elem);

            }

            // Always clear the points for the next drawing.
            points.clear();

            // Update the display.
            drawingLayer.removeGhostLine();
            mapEditor.refresh();

        }

        /*
         * Set drawing mode for adding a contour line or a new Contours.
         */
        private void setDrawingMode() {

            if (points.size() == 0) {
                if (elem == null) {

                    // quit Contours drawing
                    if (attrDlg != null) {
                        attrDlg.close();
                    }

                    attrDlg = null;

                    addContourLine = false;

                    PgenUtil.setSelectingMode();
                } else {

                    // start a new Contours element - new points will be drawn
                    // as new ContourLine in a new Contours element.
                    if (!addContourLine) {
                        elem = null;
                    } else { // back to selecting
                        PgenUtil.setSelectingMode();
                    }
                }

            }

        }

        /*
         * Add a circle to Contours.
         */
        private void drawContourCircle() {

            if (points != null && points.size() > 1) {

                ContourCircle cmm = new ContourCircle(
                        points.get(0),
                        points.get(1),
                        new String[] { ((ContoursAttrDlg) attrDlg).getLabel() },
                        ((ContoursAttrDlg) attrDlg).hideCircleLabel());

                IAttribute circleTemp = (((ContoursAttrDlg) attrDlg)
                        .getCircleTemplate());
                if (circleTemp != null) {
                    cmm.getCircle().setColors(circleTemp.getColors());
                    ((Arc) cmm.getCircle()).setLineWidth(circleTemp
                            .getLineWidth());
                }

                IAttribute lblTemp = ((ContoursAttrDlg) attrDlg)
                        .getLabelTemplate();
                if (lblTemp != null) {
                    Text lbl = cmm.getLabel();
                    String[] oldText = lbl.getText();
                    boolean hide = lbl.getHide();
                    boolean auto = lbl.getAuto();
                    lbl.update(lblTemp);
                    lbl.setText(oldText);
                    lbl.setHide(hide);
                    lbl.setAuto(auto);
                }

                // Check if we need to add to existing contours or create a new
                // one
                elem = checkExistingContours();

                if (elem == null) {
                    /*
                     * create a new element with attributes from the Attr
                     * dialog, and add it to the PGEN Resource
                     */
                    elem = (Contours) (def.create(DrawableType.CONTOURS, null,
                            "MET", "Contours", points,
                            drawingLayer.getActiveLayer()));

                    cmm.setParent(elem);
                    elem.update((ContoursAttrDlg) attrDlg);
                    elem.add(cmm);

                    drawingLayer.addElement(elem);

                } else {

                    /*
                     * Make a copy of the existing element; update its
                     * attributes from those in the Attr Dialog; replace the
                     * existing element with the new one in the pgen resource -
                     * (This allows Undo/Redo)
                     */
                    Contours newElem = (Contours) elem.copy();

                    cmm.setParent(newElem);

                    newElem.update((ContoursAttrDlg) attrDlg);

                    newElem.add(cmm);

                    drawingLayer.replaceElement(elem, newElem);

                    elem = newElem;

                }

                ((ContoursAttrDlg) attrDlg).setCurrentContours(elem);

            }

            // Always clear the points for the next drawing.
            points.clear();

            // Update the display.
            drawingLayer.removeGhostLine();
            mapEditor.refresh();

        }

        /*
         * Loop through current layer and see if there is an same type of
         * Contours.
         * 
         * If yes, show a warning message and ask for confirmation either add to
         * the existing contours or draw a new Cnntours.
         */
        private Contours checkExistingContours() {

            Contours existingContours = elem;

            // Loop through current layer and see if there is an same type of
            // Contours.
            // If yes, show a warning message and ask for confirmation either
            // add to the
            // existing contours or draw a new Contours.
            if (existingContours == null) {

                Iterator<AbstractDrawableComponent> it = drawingLayer
                        .getActiveLayer().getComponentIterator();
                while (it.hasNext()) {
                    AbstractDrawableComponent adc = it.next();
                    if (adc instanceof Contours && !(adc instanceof Outlook)) {
                        Contours thisContour = (Contours) adc;
                        ContoursAttrDlg thisDlg = (ContoursAttrDlg) attrDlg;
                        if (thisContour.getParm().equals(thisDlg.getParm())
                                && thisContour.getLevel().equals(
                                        thisDlg.getLevel())) {
                            existingContours = (Contours) adc;
                            break;
                        }
                    }
                }

                if (existingContours != null) {
                    MessageDialog msgDlg = new MessageDialog(PlatformUI
                            .getWorkbench().getActiveWorkbenchWindow()
                            .getShell(), "Warning!", null, "There is another ["
                            + existingContours.getParm() + ","
                            + existingContours.getLevel()
                            + "] Contours element in this layer.\n"
                            + "Do you want to add to it or create a new one?",
                            MessageDialog.INFORMATION,
                            new String[] { "Add to Existing One",
                                    "Create a New One" }, 0);
                    msgDlg.open();

                    // start a new Contours.
                    if (msgDlg.getReturnCode() != MessageDialog.OK) {
                        existingContours = null;
                    }
                }
            }

            return existingContours;
        }

    }

    public void resetUndoRedoCount() {
        undo = -1;
        redo = -1;
    }

    @Override
    public void stacksUpdated(int undoSize, int redoSize) {

        if (undoSize < undo || redoSize < redo) {
            // there is an undo or a redo
            Contours tmp = elem;
            elem = lastElem;
            lastElem = tmp;

        }

        undo = undoSize;
        redo = redoSize;

    }
    
    /**
     * Gets the current working contour.
     * @return
     */
    public Contours getCurrentContour(){
    	return elem;
    }

    /**
     * Sets the current working contour
     * @param con
     */
    public void setCurrentContour( Contours con){
    	attrDlg.setDrawableElement(con);
    	elem = con;
    }
    
    /**
     * Sets the selecting handler.
     */
	public void setPgenSelectHandler( ){

		setHandler(new PgenSelectHandler(this, mapEditor, drawingLayer,attrDlg ));
	}
	
	/**
	 * Sets the contour mouse handler.
	 */
	public void setPgenContoursHandler( ){

		setHandler(new PgenContoursHandler());
		
	}
	
	/**
	 * Clears selected elements.
	 */
	public void clearSelected(){
		drawingLayer.removeSelected();
		mapEditor.refresh();
	}
	
	/**
	 * Gets the default mouse handler.
	 */
	@Override
	protected IInputHandler getDefaultMouseHandler(){
		return new PgenSelectHandler(this, mapEditor, drawingLayer,attrDlg );
	}
	
	/**
	 * Sets current working component
	 */
	@Override
	protected void setWorkingComponent( AbstractDrawableComponent adc ){
		if ( adc instanceof Contours ){
			setCurrentContour( (Contours) adc );
			((ContoursAttrDlg) attrDlg).setCurrentContours(elem);
		}
	}
}
