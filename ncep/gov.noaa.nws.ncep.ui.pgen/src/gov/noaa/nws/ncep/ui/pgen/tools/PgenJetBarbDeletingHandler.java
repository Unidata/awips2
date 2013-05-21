/*
 * gov.noaa.nws.ncep.ui.pgen.tools.PgenJetBarbDeletingHandler
 * 
 * 8 July 2009
 *
 * This code has been developed by the NCEP/SIB for use in the AWIPS2 system.
 */

package gov.noaa.nws.ncep.ui.pgen.tools;

import java.awt.Color;

import com.raytheon.viz.ui.editor.AbstractEditor;
import com.vividsolutions.jts.geom.Coordinate;


//import gov.noaa.nws.ncep.ui.display.InputHandlerDefaultImpl;
import gov.noaa.nws.ncep.ui.pgen.attrdialog.JetAttrDlg;
import gov.noaa.nws.ncep.ui.pgen.display.ISinglePoint;
import gov.noaa.nws.ncep.ui.pgen.elements.AbstractDrawableComponent;
import gov.noaa.nws.ncep.ui.pgen.elements.DECollection;
import gov.noaa.nws.ncep.ui.pgen.elements.IJetTools;
import gov.noaa.nws.ncep.ui.pgen.elements.Jet;
import gov.noaa.nws.ncep.ui.pgen.elements.Symbol;
import gov.noaa.nws.ncep.ui.pgen.rsc.PgenResource;
//import gov.noaa.nws.ncep.viz.ui.display.NCMapEditor;

/**
 * Mouse handler to delete barb when drawing jet.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date       	Ticket#		Engineer	Description
 * ------------	----------	-----------	--------------------------
 * 07/09		#135	 	B. Yin   	Initial Creation.
 * 09/30/09     #169        Greg Hull     NCMapEditor
 * 12/10		#366		B. Yin		Put a red dot for selected barb
 * 04/11		#?			B. Yin		Re-factor IAttribute
 *
 * </pre>
 * 
 * @author	B. Yin
 */
public class PgenJetBarbDeletingHandler extends InputHandlerDefaultImpl {
	
//	private NCMapEditor mapEditor;
	private AbstractEditor mapEditor;
	private PgenResource drawingLayer;
	private IJetBarb prevTool;
	private JetAttrDlg jetDlg;

	private AbstractDrawableComponent barbSelected;
	
	/**
	 * Public constructor
	 * @param mapEditor
	 * @param drawingLayer
	 * @param prevTool
	 * @param jet
	 */
//	public PgenJetBarbDeletingHandler(NCMapEditor mapEditor, PgenResource drawingLayer,
	public PgenJetBarbDeletingHandler(AbstractEditor mapEditor, PgenResource drawingLayer,
			IJetBarb prevTool, JetAttrDlg jetDlg){
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
    	if (!drawingLayer.isEditable()) return false;

    	//  Check if mouse is in geographic extent
    	Coordinate loc = mapEditor.translateClick(anX, aY);
    	if ( loc == null || shiftDown ) return false;

    	Jet jet = prevTool.getJet();
    	if (  jet == null ) return false;
    	
    	if ( button == 1 ) {
    		if ( barbSelected != null ) {
    			Jet newJet = jet.copy();
    			// Remove the selected barb from jet
    			newJet.remove(newJet.getNearestComponent(((ISinglePoint)(barbSelected.getPrimaryDE())).getLocation()));
    			IJetTools snapTool = newJet.getSnapTool();
    			if ( snapTool != null ){
    				snapTool.snapJet(newJet);
    			}
    			
    			drawingLayer.replaceElement(jet, newJet);
    			jet = newJet;
        		prevTool.setJet(jet);
	
    			// de-select the barb
            	drawingLayer.removeSelected(barbSelected);
				drawingLayer.setGhostLine(null);

            	barbSelected = null;
        		jetDlg.updateSegmentPane();

    		}
    		else {        	
    			// Get the nearest barb and set it as the selected.
    			barbSelected = jet.getNearestComponent( loc );
    			if ( !barbSelected.getName().equalsIgnoreCase("WindInfo") ){
    				barbSelected = null;
    			}
    			else{
    				drawingLayer.addSelected( barbSelected );
    				
    				//put red dots
    				DECollection dec = new DECollection();
    				for ( Coordinate pt : barbSelected.getPoints()){
    					Symbol redDot = new Symbol( null, new Color[]{Color.red},
    							2.5f, 7.5, false, pt, 
    							"Marker", "DOT");
    					dec.add(redDot);
    				}
    				drawingLayer.setGhostLine(dec);
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
