/*
 * gov.noaa.nws.ncep.ui.pgen.tools.PgenWatchBoxModifyTool
 * 
 * 29 October 2009
 *
 * This code has been developed by the NCEP/SIB for use in the AWIPS2 system.
 */

package gov.noaa.nws.ncep.ui.pgen.tools;

import gov.noaa.nws.ncep.edex.common.stationTables.Station;
import gov.noaa.nws.ncep.ui.pgen.PgenUtil;
import gov.noaa.nws.ncep.ui.pgen.attrdialog.AttrDlg;
import gov.noaa.nws.ncep.ui.pgen.attrdialog.WatchBoxAttrDlg;
import gov.noaa.nws.ncep.ui.pgen.attrdialog.WatchInfoDlg;
import gov.noaa.nws.ncep.ui.pgen.elements.DECollection;
import gov.noaa.nws.ncep.ui.pgen.elements.DrawableElement;
import gov.noaa.nws.ncep.ui.pgen.elements.WatchBox;
import gov.noaa.nws.ncep.ui.pgen.rsc.PgenResource;
//import gov.noaa.nws.ncep.viz.ui.display.NCMapEditor;
import gov.noaa.nws.ncep.ui.pgen.display.IWatchBox;

import java.awt.Color;
import java.util.ArrayList;

import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.ui.PlatformUI;

import com.raytheon.uf.viz.core.rsc.IInputHandler;
import com.raytheon.viz.ui.editor.AbstractEditor;
import com.vividsolutions.jts.geom.Coordinate;


/**
 * Implements a modal map tool to modify PGEN watch boxes.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date       	Ticket#		Engineer	Description
 * ------------	----------	-----------	--------------------------
 * 10/09					B. Yin   	Initial Creation.
 * 04/11		?			B. Yin		Bring up the WatchBox spec dialog
 * 12/11		565			B. Yin		Modify watch box onlly when the curse is close enough
 * 02/12		TTR 525		B. Yin		Make sure points don't move when selecting.
 * 05/12		TTR 534		B. Yin		Re-set the watch dialog attributes
 * 03/13		#927		B. Yin		Added constructor for the handler class.
 * 12/13		TTR 800		B. Yin		Add a flag when opening the specification dialog.
 * 01/14		TTR 800		B. Yin		Fixed the county lock issue.
 * 
 * </pre>
 * 
 * @author	B. Yin
 */

public class PgenWatchBoxModifyTool extends PgenSelectingTool {

	/*
     * (non-Javadoc)
     * 
     * @see com.raytheon.viz.ui.tools.AbstractTool#runTool()
     */
    @Override
    protected void activateTool( ) {

    	super.activateTool();
    	
    	if ( event.getTrigger() instanceof WatchBox ) {
    		
    		((WatchBoxAttrDlg)attrDlg).setWbTool(this);
    		
    		//set wb as selected and open spec dialog
    		// this is for ending of the wb drawing.
    		if ( drawingLayer.getSelectedDE() == null ){
    			drawingLayer.setSelected( ((WatchBoxAttrDlg)attrDlg).getWatchBox());
    			((WatchBoxAttrDlg)attrDlg).setAttrForDlg(((WatchBoxAttrDlg)attrDlg).getWatchBox() );
    			((WatchBoxAttrDlg)attrDlg).enableButtons();
    			((WatchBoxAttrDlg)attrDlg).openSpecDlg( false );
    		}
    	}
        
    }
    
    /**
     * Returns the current mouse handler.
     * @return
     */   
    public IInputHandler getMouseHandler() {	
    
        if ( this.mouseHandler == null ) {
        	
        	this.mouseHandler = new PgenWatchBoxModifyHandler(this, mapEditor, drawingLayer, attrDlg );
        	
        }
        
        return this.mouseHandler;
        
    }
    
    /**
     * A mouse handler for the watch box modify tool
     * @author bingfan
     *
     */
	private class PgenWatchBoxModifyHandler extends PgenSelectHandler {
		
		private boolean dontMove = true;	//flag to prevent moving during selection
		private boolean simulate;
		
      	
    	public PgenWatchBoxModifyHandler( AbstractPgenTool tool, AbstractEditor mapEditor, PgenResource resource,
    					AttrDlg attrDlg){
    		super( tool, mapEditor, resource, attrDlg);
    	}
    	
        /*
         * (non-Javadoc)
         * 
         * @see com.raytheon.viz.ui.input.IInputHandler#handleMouseDown(int,
         *      int, int)
         */
        @Override	   	
        public boolean handleMouseDown(int anX, int aY, int button) { 
        	if ( !isResourceEditable() ) return false;

        	//  Check if mouse is in geographic extent
        	Coordinate loc = mapEditor.translateClick(anX, aY);
        	if ( loc == null || shiftDown || simulate ) return false;
        	
        	if ( button == 1 ) {
        		dontMove = false;
        		firstDown = loc;
        		return false;
        	}
        	// clean up
        	else if ( button == 3 ) {
            	
            	// Close the attribute dialog and do the cleanup.
            	if ( PgenWatchBoxModifyTool.this.attrDlg != null ) {
            		PgenWatchBoxModifyTool.this.attrDlg.close();
            	}

            	PgenWatchBoxModifyTool.this.attrDlg = null;
            	
        		drawingLayer.removeGhostLine();
        		ptSelected = false;
            	drawingLayer.removeSelected();
      	        mapEditor.refresh();
        		PgenUtil.setSelectingMode();

            	return true;
            	
            }
            else{
            	
               	return false;
               	
            }
        	
        }
        
	    /*
         * (non-Javadoc)
         * 
         * @see com.raytheon.viz.ui.input.IInputHandler#handleMouseDownMove(int,
         *      int, int)
         */
        @Override
        public boolean handleMouseDownMove(int x, int y, int button) {
        	if ( !isResourceEditable() ) return false;
        	if ( dontMove ) return true;
        	
        	//  Check if mouse is in geographic extent
        	Coordinate loc = mapEditor.translateClick(x, y);
        	if ( loc == null || shiftDown ) return false;

        	DrawableElement tmpEl = drawingLayer.getSelectedDE();
        	//make sure the click is close enough to the element
        	if ( drawingLayer.getDistance(tmpEl, loc) > 30 && !ptSelected ){
        		if ( firstDown != null && drawingLayer.getDistance(tmpEl, firstDown) < 30){
        			firstDown = null;
        		}
        		else {
        			return false;
        		}
        	}

        	if ( tmpEl != null && (tmpEl instanceof WatchBox) ) {

        		if ( !ptSelected ){
        			// Select the nearest point and create the ghost element.
        			ghostEl = (WatchBox)tmpEl.copy();
        			
        			if ( ghostEl != null ) {
        				ghostEl.setColors(new Color[]{ghostColor, new java.awt.Color( 255,255,255)});

        				ArrayList<Coordinate> points = new ArrayList<Coordinate>();
        				points.addAll( tmpEl.getPoints());

        				ghostEl.setPoints( points);

        				ghostEl.setPgenCategory( tmpEl.getPgenCategory());
        				ghostEl.setPgenType( tmpEl.getPgenType());

        				ptIndex = getNearestPtIndex( ghostEl, loc);

        				ptSelected = true;
        			}
        		}
        		else {
        			//create the ghost watch box
        			ghostEl.setPoints( ((WatchBox)tmpEl).createNewWatchBox(ptIndex, loc, ((IWatchBox)PgenWatchBoxModifyTool.this.attrDlg).getWatchBoxShape()));
        			drawingLayer.setGhostLine(ghostEl);
        			mapEditor.refresh();

        		}
        	}

        	simulate = true;
        	PgenUtil.simulateMouseDown(x, y, button, mapEditor);
        	simulate = false;
        	return true;
                
        }
        
        /*
         * (non-Javadoc)
         * 
         * @see com.raytheon.viz.ui.input.IInputHandler#handleMouseUp(int, int,
         *      int)
         */
        @Override
        public boolean handleMouseUp(int x, int y, int button) {
        	firstDown = null;
        	if ( !isResourceEditable() ) return false;

        	// Finish the editing
    		if (button == 1 && drawingLayer != null ){
    			
          	   	// Create a copy of the currently editing watch box
    			WatchBox el = (WatchBox) drawingLayer.getSelectedDE();

    			if ( el != null ){
    				
	    			WatchBox newEl = (WatchBox)el.copy();			

    				if ( ptSelected ) {

    					ptSelected = false;
    					
    					// re-snap the new watch box
    					resnapWatchBox( mapEditor, (WatchBox)ghostEl, newEl );
    			    	((WatchBoxAttrDlg)PgenWatchBoxModifyTool.this.attrDlg).setWatchBox(newEl);
    			    	WatchInfoDlg infoDlg = ((WatchBoxAttrDlg)PgenWatchBoxModifyTool.this.attrDlg).getWatchInfoDlg();
    			    	if ( infoDlg != null && infoDlg.getShell()!= null ){
    			    		if ( ! infoDlg.isCountyLock()){
    			    			newEl.clearCntyList();
    			    			infoDlg.clearCwaPane();
    			    		}
    			    		infoDlg.setStatesWFOs();
    			    	}
    			    	
    			    	// Replace the selected watch box with this new watch box
    					drawingLayer.replaceElement(el, newEl);
    					
    					// Set this new element as the currently selected element 
    					// Collections do not need to reset.
    					if ( !(drawingLayer.getSelectedComp() instanceof DECollection )){
    						drawingLayer.setSelected(newEl);
    					}

    					drawingLayer.removeGhostLine();

					}
    				
    				mapEditor.refresh();

    			}
    		}
        	
            return true;
            
        }

	}
	
	/**
	 * Re-snap the input watch box on the nearest anchor points
	 * @param editor - map editor
	 * @param oldBox - the input watch box
	 * @param newBox - the result watch box (can be the input watch box)
	 * @return - false if there is no anchor point in the input watch box.
	 */
	public static boolean resnapWatchBox(AbstractEditor editor, WatchBox oldBox, WatchBox newBox ){
//	public static boolean resnapWatchBox(NCMapEditor editor, WatchBox oldBox, WatchBox newBox ){
		
		Station anchor1 = null;
		Station anchor2 = null;
		ArrayList<Coordinate>watchPts = null;

		// get a list of anchor points in the watch box
		ArrayList<Station> anchorsInPoly = PgenWatchBoxDrawingTool.getAnchorsInPoly(editor, oldBox.getPoints());

		// if there is no anchor point, pop up a warning message and return false.
		if ( anchorsInPoly == null || anchorsInPoly.isEmpty() ){
			MessageDialog infoDlg = new MessageDialog( 
					PlatformUI.getWorkbench().getActiveWorkbenchWindow().getShell(), 
					"Information", null, "No anchor point in the area!",
					MessageDialog.INFORMATION, new String[]{"OK"}, 0);

			infoDlg.open();
			return false;

		}
		else {

			anchor1 = WatchBox.getNearestAnchorPt(oldBox.getPoints().get(0), anchorsInPoly);
			anchor2 = WatchBox.getNearestAnchorPt(oldBox.getPoints().get(4), anchorsInPoly);

			if ( anchor1 != null && anchor2 != null ){

				watchPts = WatchBox.generateWatchBoxPts(newBox.getWatchBoxShape(),  
						oldBox.getHalfWidth(), 
						WatchBox.snapOnAnchor(anchor1, oldBox.getPoints().get(0)), 
						WatchBox.snapOnAnchor(anchor2, oldBox.getPoints().get(4)) );

				newBox.setPoints(watchPts);
				newBox.setAnchors(anchor1, anchor2);
			}

			return true;
		}			
	}
	
	public void setAddDelCntyHandler( ){

		setHandler(new PgenWatchBoxAddDelCntyHandler(mapEditor, drawingLayer,
				((WatchBoxAttrDlg)attrDlg).getWatchBox(), this));
	}
	
	public void resetMouseHandler(){
		setHandler(new PgenWatchBoxModifyHandler(this, mapEditor, drawingLayer, attrDlg ) );
	}  
}
