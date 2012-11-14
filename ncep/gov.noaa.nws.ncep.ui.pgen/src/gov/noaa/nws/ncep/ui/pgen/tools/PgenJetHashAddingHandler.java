/*
 * gov.noaa.nws.ncep.ui.pgen.tools.PgenJetHashAddingHandler
 * 
 * 6 December 2010
 *
 * This code has been developed by the NCEP/SIB for use in the AWIPS2 system.
 */

package gov.noaa.nws.ncep.ui.pgen.tools;

import gov.noaa.nws.ncep.ui.pgen.attrdialog.JetAttrDlg;
import gov.noaa.nws.ncep.ui.pgen.display.IVector.VectorType;
import gov.noaa.nws.ncep.ui.pgen.display.IAttribute;
import gov.noaa.nws.ncep.ui.pgen.elements.Jet;
import gov.noaa.nws.ncep.ui.pgen.elements.Jet.JetHash;
import gov.noaa.nws.ncep.ui.pgen.rsc.PgenResource;
//import gov.noaa.nws.ncep.viz.ui.display.NCMapEditor;

import java.awt.Color;

import com.raytheon.viz.ui.editor.AbstractEditor;
import com.vividsolutions.jts.geom.Coordinate;

/**
 * Mouse handler to add hash when drawing jet.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date       	Ticket#		Engineer	Description
 * ------------	----------	-----------	--------------------------
 * 12/10		#366		B. Yin   	Initial Creation.
 *
 * </pre>
 * 
 * @author	B. Yin
 */
public class PgenJetHashAddingHandler extends InputHandlerDefaultImpl {

//	private NCMapEditor mapEditor;
	private AbstractEditor mapEditor;
	private PgenResource drawingLayer;
	private IJetBarb prevTool;
	private JetAttrDlg jetDlg;
	
	/**
	 * public constructor
	 * @param mapEditor
	 * @param drawingLayer
	 * @param prevTool
	 * @param jet
	 * @param jetDlg
	 */
//	public PgenJetHashAddingHandler(NCMapEditor mapEditor, PgenResource drawingLayer,
	public PgenJetHashAddingHandler(AbstractEditor mapEditor, PgenResource drawingLayer,
			IJetBarb prevTool, JetAttrDlg jetDlg){
		this.mapEditor= mapEditor;
		this.drawingLayer = drawingLayer;
		this.prevTool = prevTool;
		this.jetDlg = jetDlg;
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
    	if ( jet == null ) return false;
    	
    	if ( button == 1 ) {

    		Jet newJet = jet.copy();
    		
    		newJet.addHash( createHash(newJet, loc) );

    		drawingLayer.replaceElement(jet, newJet);
    		jet = newJet;
    		
    		prevTool.setJet(jet);
    		
    		mapEditor.refresh();
    		jetDlg.updateSegmentPane();

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

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.viz.ui.input.IInputHandler#handleMouseMove(int,
     *      int)
     */
    @Override
    public boolean handleMouseMove(int x, int y) {

    	if ( !drawingLayer.isEditable() ) return false;
    	
    	//  Check if mouse is in geographic extent
    	Coordinate loc = mapEditor.translateClick(x, y);
    	if ( loc == null ) return false;

    	Jet jet = prevTool.getJet();
    	if ( jet == null ) return false;
    	
        drawingLayer.setGhostLine( createHash( jet, loc ) );      
      	mapEditor.refresh();
   	
    	return true;
    	
    }
    
    @Override
	public boolean handleMouseDownMove(int x, int y, int mouseButton) {
    	if ( !drawingLayer.isEditable() || shiftDown ) return false;
    	else return true;
	}

	private JetHash createHash( Jet aJet, Coordinate loc  ){
    	JetHash hash = aJet.new JetHash(null, new Color[]{ new Color(0,255,0), new Color(255,0,0)},
				2.0f, 2.0, true, loc, VectorType.HASH_MARK,
				100, 0, 1.0, false, "Vector", "Hash");
    	
 		IAttribute hashAttr = jetDlg.getHashAttr();
		
		if ( hashAttr != null ){
			hash.update(hashAttr);
		}
		
		return hash;
    }
   	
}
