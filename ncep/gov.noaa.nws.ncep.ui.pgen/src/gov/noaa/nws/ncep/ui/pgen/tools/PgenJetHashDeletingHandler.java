/*
 * gov.noaa.nws.ncep.ui.pgen.tools.PgenJetHashDeletingHandler
 * 
 * 6 December 2010
 *
 * This code has been developed by the NCEP/SIB for use in the AWIPS2 system.
 */

package gov.noaa.nws.ncep.ui.pgen.tools;

import java.awt.Color;

import com.raytheon.viz.ui.editor.AbstractEditor;
import com.vividsolutions.jts.geom.Coordinate;


import gov.noaa.nws.ncep.ui.pgen.attrdialog.JetAttrDlg;
import gov.noaa.nws.ncep.ui.pgen.display.ISinglePoint;
import gov.noaa.nws.ncep.ui.pgen.elements.AbstractDrawableComponent;
import gov.noaa.nws.ncep.ui.pgen.elements.IJetTools;
import gov.noaa.nws.ncep.ui.pgen.elements.Jet;
import gov.noaa.nws.ncep.ui.pgen.elements.Symbol;
import gov.noaa.nws.ncep.ui.pgen.elements.Jet.JetHash;
import gov.noaa.nws.ncep.ui.pgen.rsc.PgenResource;
//import gov.noaa.nws.ncep.viz.ui.display.NCMapEditor;

/**
 * Mouse handler to delete hash when drawing jet.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date       	Ticket#		Engineer	Description
 * ------------	----------	-----------	--------------------------
 * 12/10		#366	 	B. Yin   	Initial Creation.
 * 04/11		#?			B. Yin		Re-factor IAttribute
 *
 * </pre>
 * 
 * @author	B. Yin
 */
public class PgenJetHashDeletingHandler extends InputHandlerDefaultImpl {
	
//	private NCMapEditor mapEditor;
	private AbstractEditor mapEditor;
	private PgenResource drawingLayer;
	private IJetBarb prevTool;
	private AbstractDrawableComponent hashSelected;
	private JetAttrDlg jetDlg;

	/**
	 * Public constructor
	 * @param mapEditor
	 * @param drawingLayer
	 * @param prevTool
	 * @param jet
	 */
//	public PgenJetHashDeletingHandler(NCMapEditor mapEditor, PgenResource drawingLayer,
	public PgenJetHashDeletingHandler(AbstractEditor mapEditor, PgenResource drawingLayer,
			IJetBarb prevTool,  JetAttrDlg jetDlg){
		this.mapEditor= mapEditor;
		this.drawingLayer = drawingLayer;
		this.prevTool = prevTool;
		this.jetDlg = jetDlg;

		drawingLayer.removeGhostLine();
		mapEditor.refresh();
	}
	
    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.viz.ui.input.IInputHandler#handleMouseDown(int,
     *      int, int)
     */
    @Override	
    public boolean handleMouseDown(int anX, int aY, int button) {
       if ( !drawingLayer.isEditable() ) return false;
    	//  Check if mouse is in geographic extent
    	Coordinate loc = mapEditor.translateClick(anX, aY);
    	if ( loc == null || shiftDown ) return false;

    	Jet jet = prevTool.getJet();
    	if (  jet == null ) return false;
    	
    	if ( button == 1 ) {
    		if ( hashSelected != null ) {
    			Jet newJet = jet.copy();
    			// Remove the selected hash from jet
    			newJet.remove(newJet.getNearestComponent(((ISinglePoint)(hashSelected.getPrimaryDE())).getLocation()));
    			IJetTools snapTool = newJet.getSnapTool();
    			if ( snapTool != null ){
    				snapTool.snapJet(newJet);
    			}
    			
    			drawingLayer.replaceElement(jet, newJet);
    			jet = newJet;
        		prevTool.setJet(jet);
	
    			// de-select the barb
            	drawingLayer.removeSelected(hashSelected);
				drawingLayer.setGhostLine( null );

            	hashSelected = null;
        		jetDlg.updateSegmentPane();

    		}
    		else {        	
    			// Get the nearest hash and set it as the selected.
    			hashSelected = jet.getNearestDE( loc );
    			if ( !(hashSelected instanceof JetHash ) ){
    				hashSelected = null;
    			}
    			else{
    				drawingLayer.addSelected( hashSelected );
    				Symbol selectSymbol = new Symbol( null, new Color[]{Color.red},
    						2.5f, 7.5, false, ((JetHash)hashSelected).getLocation(), 
    						"Marker", "DOT");
    				drawingLayer.setGhostLine(selectSymbol);
    				
    			}
    		}
            
 	        mapEditor.refresh();  
            return true;            	
            
        }
        else if ( button == 3 ) {
        	
        	drawingLayer.removeGhostLine();
        	mapEditor.refresh();
        	
        	prevTool.resetMouseHandler();
        	return true;
        	
        }
        else{
        	
           	return false;
           	
        }
    	
    }

	@Override
	public boolean handleMouseDownMove(int x, int y, int mouseButton) {
		if ( !drawingLayer.isEditable() || shiftDown ) return false;
		else return true;
	}

}

