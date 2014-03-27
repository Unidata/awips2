/*
 * gov.noaa.nws.ncep.ui.pgen.rsc.PgenSelectingTool
 * 
 * 2 February 2009
 *
 * This code has been developed by the NCEP/SIB for use in the AWIPS2 system.
 */

package gov.noaa.nws.ncep.ui.pgen.tools;

import gov.noaa.nws.ncep.ui.pgen.PgenSession;
import gov.noaa.nws.ncep.ui.pgen.attrdialog.AttrDlg;
import gov.noaa.nws.ncep.ui.pgen.attrdialog.AttrDlgFactory;
import gov.noaa.nws.ncep.ui.pgen.attrdialog.ContoursAttrDlg;
import gov.noaa.nws.ncep.ui.pgen.attrdialog.GfaAttrDlg;
import gov.noaa.nws.ncep.ui.pgen.attrdialog.JetAttrDlg;
import gov.noaa.nws.ncep.ui.pgen.contours.Contours;
import gov.noaa.nws.ncep.ui.pgen.display.IAttribute;
import gov.noaa.nws.ncep.ui.pgen.elements.DrawableElement;
import gov.noaa.nws.ncep.ui.pgen.elements.Jet;
import gov.noaa.nws.ncep.ui.pgen.elements.Line;
import gov.noaa.nws.ncep.ui.pgen.elements.Text;
import gov.noaa.nws.ncep.ui.pgen.elements.Vector;
import gov.noaa.nws.ncep.ui.pgen.gfa.Gfa;

import java.util.Iterator;

import org.eclipse.ui.IEditorPart;
import org.eclipse.ui.PlatformUI;

import com.raytheon.uf.viz.core.rsc.IInputHandler;
import com.raytheon.viz.ui.EditorUtil;
import com.raytheon.viz.ui.editor.AbstractEditor;

/**
 * Implements a modal map tool for PGEN selecting functions.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date       	Ticket#		Engineer	Description
 * ------------	----------	-----------	--------------------------
 * 02/09					B. Yin   	Initial Creation.
 * 04/09           72       S. Gilbert	Modified to use PgenSession
 * 04/09		  103		B. Yin		Extends from AbstractPgenTool
 * 05/09		   79		B. Yin		Close attribute dialog in deactivateTool
 * 06/09		  125		B. Yin		Allow single point DE to be moved
 * 06/09		  116		B. Yin		Use AbstractDrawableComponent
 * 07/09		  #131		J. Wu		Added check for null "elSelected" 
 * 08/09		  135		B. Yin		Handle jet and undo/redo
 * 10/09		  #160		G. Zhang	Added Sigmet support
 * 12/09		  #167		J. Wu		Added support for Contours
 * 01/10		  #182		G. Zhang	Added support for ConvSigmet
 * 06/10		  #215		J. Wu		Added support for Contours Min/Max
 * 07/10		  #223		M.Laryukhin	Added support for GFA
 * 10/10          #289      Archana     Added overridden method handleKeyDown() to
 *                                                      PgenSelectHandler to accept the delete key.
 *                                                      Updated the method handleMouseDown() 
 *                                                      to give the focus to the mapEditor.
 * 11/10		  #345		J. Wu		Added support for Contours Circle
 * 12/10		  #321		J. Wu		Added support moving contour labels
 *
 *  02/10/2011                 Chin Chen   fixed null pointer exception issue                                                  
 * 02/11			?		B. Yin		Automatically add/del ]/[ for front labels
 * 04/11			?		B. Yin		Re-factor IAttribute
 * 02/12          TTR456    Q.Zhou      Added parameters to setTrack()
 * 02/12          #597      S. Gurung   Removed snapping for CONV_SIGMET and NCON_SIGMET.
 * 										Moved snap functionalities to SnapUtil from SigmetInfo. 
 * 02/12		 TTR 525	B. Yin		Mkae sure points don't move when selecting.
 * 04/12           #750     Q. Zhou     Modified handleMouseDownMove for loc null. 
 * 										Added tempLoc and inOut to trace position of single position element
 * 05/12		 TTR 310	B. Yin		added a method to change line type.
 * 05/12			#808	J. Wu		Update Gfa vor text
 * 05/12		    #610	J. Wu   	Add warning when GFA FROM lines > 3
 * 06/12         #777       Q. Zhou     Added isNewTrack flag on Track to get first time differently
 * 07/12         #663       Q. Zhou     Modified handleMouseDown for preselected DE.  
 * 08/12         #803       Q. Zhou     Fixed Front text of 2 lines. Modified handleMouseDownMove. 
 * 11/12		 #911		J. Wu   	TTR 652 - prevent invalid GFA polygon when dragging GFA points.
 * 03/13		 #927		B. Yin		Moved out the handler class.
 * 11/13		 #1063		B. Yin		Modified getMouseHandler to handle non-PgenSelectingMouseHandler
 * 11/13		 #1081		B. Yin		Removed prevDe and added getSelectedDE method.  
 * </pre>
 * 
 * @author	B. Yin
 */

public class PgenSelectingTool extends AbstractPgenDrawingTool 
						implements IJetBarb {
	
	
	/**
	 * Input handler for mouse event
	 */
    private IInputHandler selectHandler; 

    // if a jet is selected, it needs to be stored
    private Jet jet;
    
    private Contours selectedContours;
    
    private boolean selectInContours;
    
    public PgenSelectingTool(){
    	
    	super();
    	
    }
    
    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.viz.ui.tools.AbstractTool#runTool()
     */
    @Override
    protected void activateTool( ) {
    	
    	if ( PgenSession.getInstance().getPgenPalette() == null ) return;
    	
    	IEditorPart ep = EditorUtil.getActiveEditor();
        if (!(ep instanceof AbstractEditor) ){
            return;
        }
        //close attr dialog except contour dialog
        if ( attrDlg != null && !(attrDlg instanceof ContoursAttrDlg) )attrDlg.close();
    	attrDlg = null;
    	if ( buttonName == null ) buttonName = new String("Select");
    	PgenSession.getInstance().getPgenPalette().setDefaultAction();

    	super.activateTool();
    	
    	/*
    	 *  Check if the trigger object is a Contours
    	 */
    	Object de = event.getTrigger();
    	if ( de instanceof Contours ) {
    		selectInContours = true;
     		selectedContours = (Contours)de;
     	}
    	else if ( de instanceof Gfa ) { //Added for gfa move text.
    		attrDlg = AttrDlgFactory.createAttrDlg( ((Gfa)de).getPgenCategory(), ((Gfa)de).getPgenType(),
        			PlatformUI.getWorkbench().getActiveWorkbenchWindow().getShell() );
    		attrDlg.setBlockOnOpen(false);
    		if (attrDlg.getShell() == null) attrDlg.open();
    		((GfaAttrDlg)attrDlg).enableMoveTextBtn(true);
    		drawingLayer.setSelected((Gfa)de);
    		editor.refresh();
     	}
	    else {
     		selectInContours = false;  
     		this.resetMouseHandler();
	    }
        
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.viz.ui.tools.AbstractModalTool#deactivateTool()
     */
    @Override
    public void deactivateTool() {

    	super.deactivateTool();
    	if ( mouseHandler != null && mouseHandler instanceof PgenSelectHandler) {
    		((PgenSelectHandler)mouseHandler).closeDlg();
    	}
    }
    
    /**
     * Returns the current mouse handler.
     * @return
     */   
    public IInputHandler getMouseHandler() {	
    
        if ( this.mouseHandler == null || 
        		!(this.mouseHandler instanceof PgenSelectHandler) ||                  //e.g. deleting barb when a jet is selected (TTR 902)
        		this.mapEditor != ((PgenSelectHandler)mouseHandler).getMapEditor() 
        		|| this.drawingLayer != ((PgenSelectHandler)mouseHandler).getPgenrsc() ) {
  //  	 if ( this.mouseHandler == null ) {	
        	this.mouseHandler = new PgenSelectHandler( this, mapEditor, drawingLayer, attrDlg);
        	selectHandler = mouseHandler;
        }

        return this.mouseHandler;
        
    }
    
    @Override
    public void resetMouseHandler(){

    	setHandler( selectHandler);
    }
    
    /**
     * Set adding barb handler for jet
     */
    public void setAddingBarbHandler(){
    	
    	setHandler( (IInputHandler) new PgenJetBarbAddingHandler(mapEditor, drawingLayer,
    					this, ((JetAttrDlg)attrDlg)));

    }
    
    /**
     * set deleting barb handler for jet
     */
    public void setDeletingBarbHandler(){
    	
    	setHandler( (IInputHandler) new PgenJetBarbDeletingHandler(mapEditor, drawingLayer,
    								this, ((JetAttrDlg)attrDlg)));

    }
    
    /**
	 * set adding hash handler for jet
	 */
   public void setAddingHashHandler(){
    	
    	setHandler( (IInputHandler) new PgenJetHashAddingHandler(mapEditor, drawingLayer,
    					this, ((JetAttrDlg)attrDlg)));

    }
    
    /**
     * set deleting hash handler for jet
     */
    public void setDeletingHashHandler(){
    	
    	setHandler( (IInputHandler) new PgenJetHashDeletingHandler(mapEditor, drawingLayer,
    								this, ((JetAttrDlg)attrDlg)));

    }   
    /**
     * Apply attr for all barbs in the selected jet
     * @param attr
     */
    public void applyBarbAttrOnJet(IAttribute attr, String type){

    	if ( jet != null ){

    		Jet newJet = jet.copy();
    		
    		Iterator<DrawableElement> it = newJet.createDEIterator();

    		while ( it.hasNext()){
    			DrawableElement de = it.next();
    			if ( de.getPgenType().equalsIgnoreCase(type)){
    				double sp = ((Vector)de).getSpeed();
    				double dir = ((Vector)de).getDirection();
    				de.update(attr);
    				if( type.equalsIgnoreCase("Barb")){
    					((Jet.JetBarb)de).setSpeedOnly(sp);
    				}
    				((Vector)de).setDirection(dir);
    			}
    		}

    		drawingLayer.replaceElement(jet, newJet);
    		drawingLayer.setSelected(newJet);
    		jet = newJet;
    		
    		mapEditor.refresh();
    	}
   }
    
   /**
    * Apply attr for all FL texts in the selected jet
    * @param attr
    */
   public void applyFLAttrOnJet(IAttribute attr){
    	
    	if ( jet != null ){
    		Jet newJet = jet.copy();

    		Iterator<DrawableElement> it = newJet.createDEIterator();
    		
    		while ( it.hasNext()){
    			DrawableElement de = it.next();
    			if ( de.getPgenType().equalsIgnoreCase("General Text")){
    				String[] txt =((Text)de).getText();
    				double rot = ((Text)de).getRotation();
    				de.update(attr);
    				((Text)de).setText(txt);
    				((Text)de).setRotation(rot);
    			}
    		}
    		drawingLayer.replaceElement(jet, newJet);
    		drawingLayer.setSelected(newJet);
    		jet = newJet;	
    		mapEditor.refresh();
    	}
    	
    }   
   
   /**
    * set jet instance
    */
   public void setJet( Jet jet ){
	   this.jet = jet;
	   drawingLayer.setSelected(jet);
   }
   
   /**
    * get jet instance
    */
   public Jet getJet(){
	   return jet;
   }  

    /**
		Change the line type of the selected element.
	 */
    public void changeSelectedLineType(String type){
    	
    	DrawableElement currentDe = getSelectedDE();
    	if ( currentDe != null && ( currentDe.getPgenCategory().equalsIgnoreCase("Lines") || 
    			currentDe.getPgenCategory().equalsIgnoreCase("Front") ) ){

        	AttrDlg dlg = AttrDlgFactory.createAttrDlg( currentDe.getPgenCategory(), type,
        			PlatformUI.getWorkbench().getActiveWorkbenchWindow().getShell() );
        	
        	DrawableElement de = currentDe;
    		//DrawableElement de = drawingLayer.getSelectedDE();
    		activateTool();

    		attrDlg = dlg;
    		
        	attrDlg.open();
    		attrDlg.setPgenType(type); 
    		attrDlg.setDefaultAttr();
    		attrDlg.enableButtons();

   // 		attrDlg.setAttrForDlg( de );
    		
    		Line ln = (Line)((Line)de).copy();
    		ln.setPgenType(type);

    		
    		ln.update(attrDlg);

    		drawingLayer.replaceElement(de, ln);
    		drawingLayer.setSelected(ln);
    		mapEditor.refresh();
    	}
    }
    
    /**
     * Returns the selected drawable element.
     * @return
     */
    public DrawableElement getSelectedDE(){
    	DrawableElement de = null;
    	
    	if ( drawingLayer != null ){
    		de = drawingLayer.getSelectedDE();
    	}
    	
    	return de;
    }
}
