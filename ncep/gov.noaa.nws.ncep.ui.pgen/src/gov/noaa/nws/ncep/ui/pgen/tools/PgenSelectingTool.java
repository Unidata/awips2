/*
 * gov.noaa.nws.ncep.ui.pgen.rsc.PgenSelectingTool
 * 
 * 2 February 2009
 *
 * This code has been developed by the NCEP/SIB for use in the AWIPS2 system.
 */

package gov.noaa.nws.ncep.ui.pgen.tools;

import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;
import java.awt.Color;

import org.eclipse.swt.SWT;
import org.eclipse.ui.IEditorPart;
import org.eclipse.ui.PlatformUI;
import org.geotools.referencing.GeodeticCalculator;

import com.raytheon.uf.viz.core.rsc.IInputHandler;
import com.raytheon.viz.ui.EditorUtil;
import com.raytheon.viz.ui.editor.AbstractEditor;
import com.vividsolutions.jts.geom.Coordinate;
import com.vividsolutions.jts.geom.GeometryFactory;
import com.vividsolutions.jts.geom.Point;

import gov.noaa.nws.ncep.ui.pgen.PgenSession;
import gov.noaa.nws.ncep.ui.pgen.PgenUtil;
import gov.noaa.nws.ncep.ui.pgen.contours.Contours;
import gov.noaa.nws.ncep.ui.pgen.contours.ContourLine;
import gov.noaa.nws.ncep.ui.pgen.contours.ContourMinmax;
import gov.noaa.nws.ncep.ui.pgen.contours.ContourCircle;
import gov.noaa.nws.ncep.ui.pgen.display.IAttribute;
import gov.noaa.nws.ncep.ui.pgen.display.IText;

import gov.noaa.nws.ncep.ui.pgen.elements.AbstractDrawableComponent;
import gov.noaa.nws.ncep.ui.pgen.elements.ComboSymbol;
import gov.noaa.nws.ncep.ui.pgen.elements.DECollection;
import gov.noaa.nws.ncep.ui.pgen.elements.DrawableType;
import gov.noaa.nws.ncep.ui.pgen.elements.Jet;
import gov.noaa.nws.ncep.ui.pgen.elements.Outlook;
import gov.noaa.nws.ncep.ui.pgen.elements.Arc;
import gov.noaa.nws.ncep.ui.pgen.elements.Symbol;
import gov.noaa.nws.ncep.ui.pgen.elements.SinglePointElement;
import gov.noaa.nws.ncep.ui.pgen.elements.MultiPointElement;
import gov.noaa.nws.ncep.ui.pgen.elements.DrawableElement;
import gov.noaa.nws.ncep.ui.pgen.elements.Text;
import gov.noaa.nws.ncep.ui.pgen.elements.Track;
import gov.noaa.nws.ncep.ui.pgen.rsc.PgenResource;
import gov.noaa.nws.ncep.ui.pgen.sigmet.*;
import gov.noaa.nws.ncep.ui.pgen.elements.Vector;
import gov.noaa.nws.ncep.ui.pgen.elements.Line;
import gov.noaa.nws.ncep.ui.pgen.elements.WatchBox;
import gov.noaa.nws.ncep.ui.pgen.elements.labeledlines.LabeledLine;
import gov.noaa.nws.ncep.ui.pgen.elements.tcm.Tcm;
import gov.noaa.nws.ncep.ui.pgen.filter.AcceptFilter;
import gov.noaa.nws.ncep.ui.pgen.gfa.Gfa;
import gov.noaa.nws.ncep.ui.pgen.gfa.GfaReducePoint;
import gov.noaa.nws.ncep.ui.pgen.gfa.IGfa;
import gov.noaa.nws.ncep.ui.pgen.tca.TCAElement;
import gov.noaa.nws.ncep.ui.pgen.attrdialog.AttrDlg;
import gov.noaa.nws.ncep.ui.pgen.attrdialog.AttrDlgFactory;
import gov.noaa.nws.ncep.ui.pgen.attrdialog.ContoursAttrDlg;
import gov.noaa.nws.ncep.ui.pgen.attrdialog.GfaAttrDlg;
import gov.noaa.nws.ncep.ui.pgen.attrdialog.JetAttrDlg;
import gov.noaa.nws.ncep.ui.pgen.attrdialog.LabeledSymbolAttrDlg;
import gov.noaa.nws.ncep.ui.pgen.attrdialog.OutlookAttrDlg;
import gov.noaa.nws.ncep.ui.pgen.attrdialog.SymbolAttrDlg;
import gov.noaa.nws.ncep.ui.pgen.attrdialog.TextAttrDlg;
import gov.noaa.nws.ncep.ui.pgen.attrdialog.TrackAttrDlg;
import gov.noaa.nws.ncep.ui.pgen.attrdialog.TrackExtrapPointInfoDlg;
import gov.noaa.nws.ncep.ui.pgen.attrdialog.WatchBoxAttrDlg;
import gov.noaa.nws.ncep.viz.common.SnapUtil;

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
    
        if ( this.mouseHandler == null ) {
        	
        	this.mouseHandler = new PgenSelectHandler();
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
     * Implements input handler for mouse events.
     * @author bingfan
     *
     */
    public class PgenSelectHandler extends InputHandlerDefaultImpl {
 
    	private boolean preempt;
    	
    	private boolean dontMove = false; //flag to prevent moving when selecting
    	/**
    	 * Attribute dialog for displaying track points info
    	 */
    	TrackExtrapPointInfoDlg trackExtrapPointInfoDlg = null;
    	
    	/**
    	 * Flag if any point of the element is selected.
    	 */
    	protected boolean	ptSelected	= false;
    	
    	/**
    	 * instance variable to store the pgenType of the selected drawableElement
    	 */
    	String	pgenType;
    	
    	/**
    	 * Index of the selected point.
    	 */
     	protected int	ptIndex = 0;
   	
    	/**
    	 * ghost element that shows the modified element.
    	 */
    	protected MultiPointElement ghostEl = null;
    	
    	/**
    	 * Color of the ghost element. 
    	 */
    	protected Color ghostColor = new java.awt.Color( 255,255,255);
    	
    	/**
    	 * For single point element, the original location is needed for undo.
    	 */
    	protected Coordinate oldLoc = null;
    	
    	protected Coordinate firstDown = null;

    	
    	/**
    	 * the mouse downMove position. Used for crossing the screen border, for single point element.
    	 */
    	protected Coordinate tempLoc = null;
    	
    	/**
    	 * Flag for point moving cross the screen. inOut=0: outside the map bound, inOut=1: inside the map bound..
    	 */
    	protected int inOut = 1;
    	
    	protected boolean simulate = false;
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
        	preempt = false;
	
        	if ( button == 1 ) {

        		//reset ptSelected flag in case the dialog is closed without right-mouse click.
        		if ( drawingLayer.getSelectedDE() == null ) ptSelected = false;
        		
        		// Return if an element or a point has been selected
        		if ( ptSelected || drawingLayer.getSelectedDE() != null ) {
        			dontMove =false;
        			preempt = true;

        			if ( drawingLayer.getSelectedDE() instanceof SinglePointElement
        					&& drawingLayer.getDistance(drawingLayer.getSelectedDE(), loc) > drawingLayer.getMaxDistToSelect() ) {
        				ptSelected = false;   //prevent SPE from moving when selecting it and then click far away and hold to  move.
        			}
        			
        			if ( !(drawingLayer.getSelectedDE() instanceof SinglePointElement) && !ptSelected ){
        				firstDown = loc;
        			}
        			
        			return false;
        		}
        		        	
        		// Get the nearest element and set it as the selected element.
        		DrawableElement elSelected = drawingLayer.getNearestElement( loc );
       		    
    			if ( elSelected instanceof SinglePointElement) {
    				ptSelected = true;   //prevent map from moving when holding and dragging too fast.
    			}
    			
        	//	AbstractDrawableComponent adc = drawingLayer.getNearestComponent( loc );
       		    
        		AbstractDrawableComponent adc = null;
        		if ( elSelected != null && elSelected.getParent() != null && 
        			 !elSelected.getParent().getName().equals( "Default" ) ) {
 
        			adc = drawingLayer.getNearestComponent( loc, new AcceptFilter(), true );       			
        		}
        		
//    		    AbstractDrawableComponent adc = drawingLayer.getNearestComponent( loc, new AcceptFilter(), true );

        		if ( elSelected == null ) return false;
        		preempt = true;
                
        		/*
                 *  Get the PGEN type category and bring up the attribute dialog
                 */
        		String pgCategory = null;

        		if ( elSelected instanceof TCAElement ) {
        			PgenUtil.loadTCATool(elSelected);
        		}
        		else if ( elSelected instanceof WatchBox ){
            		WatchBoxAttrDlg.getInstance(PlatformUI.getWorkbench().getActiveWorkbenchWindow().getShell()).setWatchBox( (WatchBox)elSelected );
        			PgenUtil.loadWatchBoxModifyTool(elSelected);
        		}
        		else if ( elSelected instanceof Tcm ){
        			PgenUtil.loadTcmTool(elSelected);
        		}
        		/*
        		 *  Select from within a given Contours or within the PgenResource 
        		 */
        		if ( selectInContours ) {
        			
        			DECollection dec = selectedContours;
           			elSelected = drawingLayer.getNearestElement( loc, dec );
           			
           			if ( elSelected != null ) {          				
   			            pgCategory = dec.getPgenCategory(); 
    			        pgenType = dec.getPgenType();
           			}
        			
        		}
        		else {

     		        if ( adc != null && adc instanceof Jet ) {
        			
        			    if(  adc.getPrimaryDE() == elSelected){
        		
        				    pgCategory = adc.getPgenCategory();
        				    pgenType = adc.getPgenType();
        				    jet = (Jet) adc;
        				}	
        			    else {
        			        pgCategory = elSelected.getPgenCategory();
        			        pgenType = elSelected.getPgenType(); 
        			        if ( ((Jet)adc).getSnapTool() == null ){
        			            ((Jet)adc).setSnapTool( new PgenSnapJet(drawingLayer.getDescriptor(), mapEditor, null));
        			        }
        			    }
       		        }
      		        else if ( adc != null && adc instanceof LabeledLine 
      		        		&& (elSelected.getParent() == adc || elSelected.getParent().getParent() == adc )){
            			PgenUtil.loadLabeledLineModifyTool((LabeledLine)adc);
            			drawingLayer.removeSelected();
            			
            			Iterator<DrawableElement> it = adc.createDEIterator();
            			while( it.hasNext()){
            				drawingLayer.addSelected(it.next());
            			}
            			
            			elSelected = null;
      		        	
      		        }
                    else if ( adc != null && adc.getName().equalsIgnoreCase("Contours") ) {                  			       			
            	        pgCategory = adc.getPgenCategory();
            		    pgenType = adc.getPgenType();
        		    }
                    else if ( adc!= null && elSelected instanceof Line && 
                    		  adc instanceof Outlook ){
                    	pgenType = "Outlook";
                    	pgCategory = adc.getPgenCategory();
                    }
        		    else {
                       if ( elSelected != null ) {
        			        pgCategory = elSelected.getPgenCategory();
        			        pgenType = elSelected.getPgenType(); 
                        }
        		    }
                }
        		
        		if ( elSelected != null ){
        			drawingLayer.setSelected( elSelected );
        			dontMove = true;
        		}

                if ( pgCategory != null ){
               	
                	if ( attrDlg != null ) {
                		closeAttrDlg(attrDlg, elSelected.getPgenType()); 
                		attrDlg = null; 
                	}
                	attrDlg = AttrDlgFactory.createAttrDlg( pgCategory, pgenType,
                			PlatformUI.getWorkbench().getActiveWorkbenchWindow().getShell() );  	

	                if (attrDlg == null){
	            		mapEditor.refresh();  
	                    return false;
	            	}
	                
                	attrDlg.setBlockOnOpen(false);
                	
                	attrDlg.setMouseHandlerName("Pgen Select");                	
                	attrDlg.setDrawableElement(elSelected);               	
                	
                	if ( attrDlg != null && attrDlg.getShell() == null ) attrDlg.open();
              		mapEditor.setFocus();  //archana
                	
                	if ( adc != null && adc.getName().equalsIgnoreCase("Contours") ) {              		     

             			((ContoursAttrDlg)attrDlg).setAttrForDlg( (Contours)adc );
                                     			
             			if ( elSelected != null ) {             				
            				updateContoursAttrDlg( elSelected );
              			}
            			
              		}
              		else {
                	    attrDlg.setAttrForDlg( elSelected );
              		}

                	if ( elSelected instanceof SinglePointElement &&
                	     ! (elSelected.getParent() instanceof ContourMinmax ) ) {
 
                		if ( (elSelected instanceof Symbol) ||
                			 (elSelected instanceof ComboSymbol) ) {
              		    	
               		    	SymbolAttrDlg sDlg = (SymbolAttrDlg)attrDlg;
                		    
               		    	sDlg.setLongitude( ((SinglePointElement)elSelected).getLocation().x);
                		    sDlg.setLatitude( ((SinglePointElement)elSelected).getLocation().y);  
                		    
                		    if ( attrDlg instanceof LabeledSymbolAttrDlg ){
                		    	((LabeledSymbolAttrDlg)attrDlg).setLabelChkBox( false );
                		    }
               		    	
               		    }
                	} else if(elSelected instanceof Track) {
                		TrackAttrDlg trackAttrDlg = (TrackAttrDlg)attrDlg; 
                		trackAttrDlg.isNewTrack = false;
                		trackAttrDlg.initializeTrackAttrDlg((Track)elSelected); 
                		displayTrackExtrapPointInfoDlg(trackAttrDlg, (Track)elSelected); 
                	}
                	
              		attrDlg.enableButtons();
              		attrDlg.setPgenCategory(pgCategory);
              		
              		if ( adc != null && adc instanceof Contours ) {              		     
              		    attrDlg.setPgenType( pgenType );
              		}
              		else {
              			attrDlg.setPgenType(elSelected.getPgenType());
              		}
                	
                	attrDlg.setDrawingLayer( drawingLayer );
                	attrDlg.setMapEditor( mapEditor );
                	if ( attrDlg instanceof JetAttrDlg ){
                		((JetAttrDlg)attrDlg).setJetDrawingTool(PgenSelectingTool.this);
                		((JetAttrDlg)attrDlg).updateSegmentPane();
                		if ( jet.getSnapTool() == null ){
        					jet.setSnapTool( new PgenSnapJet(drawingLayer.getDescriptor(), mapEditor, (JetAttrDlg)attrDlg));
        				}
                	}	
                	else if ( adc != null && attrDlg instanceof OutlookAttrDlg ){
                 		((OutlookAttrDlg)attrDlg).setOtlkType(((Outlook)adc).getOutlookType());              	                		
                		String lbl = null; 
                		Iterator<DrawableElement> it = elSelected.getParent().createDEIterator();
                		while( it.hasNext() ){
                			DrawableElement de = it.next();
                			if ( de instanceof Text ){
                				lbl = ((Text)de).getText()[0];
                				break;
                			}
                		}
                		if ( lbl != null ){
                			((OutlookAttrDlg)attrDlg).setLabel(lbl);
                			
                		}
                	    attrDlg.setAttrForDlg( elSelected );
                		((OutlookAttrDlg)attrDlg).setAttrForDlg( ((Outlook)adc) );

                	} else if(attrDlg instanceof GfaAttrDlg){
                		((GfaAttrDlg)attrDlg).redrawHazardSpecificPanel();
                		attrDlg.setAttrForDlg( elSelected );
                		((GfaAttrDlg)attrDlg).enableMoveTextBtn(true);
                	}
                }
                
                mapEditor.setFocus();
                mapEditor.refresh();  
                
                return preempt;
                
            }
            else if ( button == 3 ) {
            	
            	// Close the attribute dialog and do the cleanup.
            	if ( attrDlg != null ) {
            		closeAttrDlg(attrDlg, pgenType); 
            		if ( attrDlg instanceof GfaAttrDlg ){
            			//re-set event trigger 
            			PgenUtil.setSelectingMode();
            		}
            	}

            	attrDlg = null;
          	    
            	// reset to normal selecting mode
            	if ( selectInContours ) {
            		 selectInContours = false;
            		 PgenUtil.setSelectingMode();
          	    }
         	              	
            	if(trackExtrapPointInfoDlg != null)
            		trackExtrapPointInfoDlg.close(); 
            	trackExtrapPointInfoDlg = null; 
            	
        		drawingLayer.removeGhostLine();
        		ptSelected = false;
            	drawingLayer.removeSelected();
      	        mapEditor.refresh();
      	        
            	return false;
            	
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
        	
        	if ( shiftDown ) return false;
        	if ( dontMove  && drawingLayer.getSelectedDE() != null) return true;
        	//  Check if mouse is in geographic extent
        	Coordinate loc = mapEditor.translateClick(x, y);
        	//if ( loc == null ) return false;

        	DrawableElement tmpEl = drawingLayer.getSelectedDE();        	       	
        	if( isUnmovable(tmpEl) ) return false;

        	//
        	if (loc != null )
        		tempLoc = loc;

        	if (loc != null && inOut == 1) {
        		//make sure the click is close enough to the element
        		if ( drawingLayer.getDistance(tmpEl, loc) > drawingLayer.getMaxDistToSelect() && !ptSelected ) {
        		//	if (( firstDown != null && drawingLayer.getDistance(tmpEl, firstDown) > drawingLayer.getMaxDistToSelect() && !ptSelected) ||
        		//			tmpEl instanceof SinglePointElement ){
        			if ( firstDown != null && drawingLayer.getDistance(tmpEl, firstDown) < drawingLayer.getMaxDistToSelect() ){
        				firstDown = null;
        			}
        			else {
        			return false;
        	}
        		}
        	}
        	else if (loc != null && inOut == 0) {       		
            	inOut = 1;            
        	}
        	else { 
        		if (inOut != 0)
        			inOut = 0;
            	
        		if ( tmpEl == null ) //make sure if no DE is selected, no moving the DE
        			return false;	 //for pan
        		else {
        			simulate = true;
        			PgenUtil.simulateMouseDown(x, y, button, mapEditor);
        			simulate = false;
        			
        			return true;	 //for DE
        		}
        	}

        	if ( tmpEl != null ) {
        		if (tmpEl instanceof SinglePointElement ){
        			
        			ptSelected = true;  // to prevent map shifting when cursor moving too fast for single point elements. 
        			
        			if ( oldLoc == null ){
        				oldLoc = new Coordinate(((SinglePointElement)tmpEl).getLocation());
        			}

        			//get current layer and search the tmpEl
        			if ( tmpEl instanceof Jet.JetBarb ){
        				((Jet.JetBarb) tmpEl).setLocationOnly(loc);
        				Jet.JetText  jt = ((Jet.JetBarb) tmpEl).getFlightLvl();
        				if ( jt != null ){
        					drawingLayer.resetElement(jt);
        				}
        			}
        			else if ( tmpEl.getParent() != null &&
        					  tmpEl.getParent().getParent() != null &&
        					  tmpEl.getParent().getParent().getName().equalsIgnoreCase("Contours")) {
    			        
	           			drawingLayer.resetADC( tmpEl.getParent() );  // reset display of the DECollecion.
	        			
	        			((SinglePointElement)tmpEl).setLocationOnly( loc );
	        			ContoursAttrDlg cdlg = (ContoursAttrDlg)attrDlg;
        			    
    			    	if ( tmpEl instanceof Symbol ) {    	        			   	        			
    			    		tmpEl.setPgenCategory( cdlg.getActiveSymbolClass() );
    	           			tmpEl.setPgenType( cdlg.getActiveSymbolObjType() );
        			    }
        			    else if ( tmpEl instanceof Text ) {
        			    	((Text)tmpEl).setText( new String[] { cdlg.getLabel()} );
	    	        		((Text)tmpEl).setAuto( false );
        			    }
     			    	
        			}
        			else if ( tmpEl instanceof Text &&
        					tmpEl.getParent() instanceof DECollection &&
        					tmpEl.getParent().getPgenCategory() != null &&
        					tmpEl.getParent().getPgenCategory().equalsIgnoreCase("Front")){
        				
        				String[] text = ((IText)attrDlg).getString();
        				
        				//add "[" or "]" to front labels. The rule: If the label is only number and in one line, will be surrounded by [,]. 					
        				if ( text.length == 1 ) {         					
        					StringBuffer lbl = new StringBuffer(((TextAttrDlg)attrDlg).getString()[0]);
        							
        					if ( lbl.length() > 0 ){
        						if ( lbl.charAt(0) == '[')  lbl.deleteCharAt(0);
        						if ( lbl.charAt(lbl.length()-1) == ']') lbl.deleteCharAt(lbl.length()-1);
        						try {
        							Integer.parseInt(lbl.toString());
        						//check if the text is right or left of the front
        						if ( PgenTextDrawingTool.rightOfLine(mapEditor, loc, (Line)tmpEl.getParent().getPrimaryDE()) >= 0 ){
        							((TextAttrDlg)attrDlg).setText(new String[]{lbl+"]"});
        						}
        						else {
        							((TextAttrDlg)attrDlg).setText(new String[]{"[" + lbl});
        						}
        						} catch (NumberFormatException e) {
        							/*do nothing*/}
        					}
        					}
        				
        					((Text)tmpEl).setText(((TextAttrDlg)attrDlg).getString());
        					((SinglePointElement)tmpEl).setLocationOnly(loc);
        				}

        			else {
	        			((SinglePointElement)tmpEl).setLocationOnly(loc);
        			}
        			
        			if ( attrDlg instanceof SymbolAttrDlg ){

        				((SymbolAttrDlg)attrDlg).setLatitude(loc.y);
        				((SymbolAttrDlg)attrDlg).setLongitude(loc.x);

        			}
			    	
      			    drawingLayer.resetElement(tmpEl);   // reset display of this element       			
    			           			
        			mapEditor.refresh();

        		}
        		else {
        			if ( ptSelected ){
						//Replace the selected point and repaint.
        				ghostEl.removePoint( ptIndex );
        				ghostEl.addPoint(ptIndex, loc);
        				if(ghostEl instanceof Gfa && !((Gfa)ghostEl).isSnapshot()) {
    						((GfaAttrDlg)attrDlg).setEnableStatesButton(true);
    					}
        				drawingLayer.setGhostLine(ghostEl);
        				mapEditor.refresh();

        			}
        			else if ( tmpEl != null &&
        					(tmpEl instanceof MultiPointElement) ){

        				// Select the nearest point and create the ghost element.
        				ghostEl = (MultiPointElement)tmpEl.copy();

        				if ( ghostEl != null ) {
        					ghostEl.setColors(new Color[]{ghostColor, new java.awt.Color( 255,255,255)});

        					ArrayList<Coordinate> points = new ArrayList<Coordinate>();
        					points.addAll( tmpEl.getPoints());

        					ghostEl.setPoints( points);

        					ghostEl.setPgenCategory( tmpEl.getPgenCategory());
        					ghostEl.setPgenType( tmpEl.getPgenType());

        					ptIndex = getNearestPtIndex( ghostEl, loc);
        					
        					
        				   	double [] locScreen = mapEditor.translateInverseClick(loc);
       				    	double [] pt = mapEditor.translateInverseClick((ghostEl.getPoints().get(ptIndex)));

        					Point ptScreen = new GeometryFactory().createPoint(new Coordinate(pt[0], pt[1]));
        					double dist = ptScreen.distance(new GeometryFactory().createPoint(new Coordinate(locScreen[0], locScreen[1])));
        						dist = 0;
        					if ( dist <= drawingLayer.getMaxDistToSelect() ){ 
        						ghostEl.setPoints( points );

        						setGhostLineColorForTrack(ghostEl, ptIndex); 

        						ptSelected = true;
        					}

        				}
        			}
        			
        			simulate = true;
        			PgenUtil.simulateMouseDown(x, y, button, mapEditor);
        			simulate = false;
        			return true;
        		}     	
        	}

        	if ( preempt ) {
        		simulate = true;
        		PgenUtil.simulateMouseDown(x, y, button, mapEditor);
        		simulate = false;
        	}
            return preempt;
         
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
  			
          	   	// Create a copy of the currently selected element
    			DrawableElement el = drawingLayer.getSelectedDE();

    			if ( el != null ){
    				
	    			DrawableElement newEl = (DrawableElement)el.copy();			

    				if ( el instanceof SinglePointElement ){
    					
    					if ( oldLoc != null ) {
    						
    						drawingLayer.resetElement(el);   // reset display of this element
    						
    						if ( el instanceof Jet.JetBarb ){
    	        				DECollection dec = (DECollection) el.getParent();
    	        				if (  dec != null && dec.getCollectionName().equalsIgnoreCase("WindInfo")){
    	        					DECollection parent = (DECollection) dec.getParent();
    	        					if( parent != null && parent.getCollectionName().equalsIgnoreCase("jet")){
    	        						Jet oldJet = (Jet)parent;
    	        						Jet newJet = oldJet.copy();

    	        						// to make undo work, replace the whole jet
    	        						// and replace the barb in the collection
    	        						DECollection newWind = dec.copy();
    	        						newJet.replace(newJet.getNearestComponent(((SinglePointElement)el).getLocation()), newWind);
    	        						drawingLayer.replaceElement(oldJet, newJet);
    	        						
    	        				    	newWind.replace(newWind.getNearestComponent(((SinglePointElement)el).getLocation()), newEl);
    	        				    	
    	        						//set the old windinfo to the original location
    	        						Iterator<DrawableElement> it  = dec.createDEIterator();
    	        						while ( it.hasNext()){
    	        							DrawableElement de  = it.next();
    	        							if ( de instanceof SinglePointElement ){
    	        								((SinglePointElement) de).setLocationOnly(oldLoc);
    	        							}
    	        						}
    	        						oldLoc = null;
    	        					}
    	        				}
    						}
    						else if ( el.getParent() != null &&  
    								  el.getParent().getParent() instanceof Contours ) {
    							
    							drawingLayer.resetADC( el.getParent() );
    							
   						        AbstractDrawableComponent oldAdc = el.getParent();
   						        Contours oldContours = (Contours)oldAdc.getParent();
   								
   						        if ( oldContours != null ) {
   						        	
   						        	Contours newContours = oldContours.copy();
	  						        AbstractDrawableComponent newAdc = oldAdc.copy();
	  						        
	  						        newContours.replace( newContours.getNearestComponent( ((SinglePointElement)el).getLocation() ), newAdc ); 						        	
	  						        ((DECollection)newAdc).replace( ((DECollection)newAdc).getNearestComponent( ((SinglePointElement)el).getLocation() ), newEl);
	  	    						
	  						        if ( newEl instanceof Text ) {
	  						        	((Text) newEl).setAuto( false );
	  						        }
	  						        
	  						        if ( newEl.getParent() instanceof ContourMinmax ) {
	  	    						    ContoursAttrDlg cdlg = (ContoursAttrDlg)attrDlg;
	  	    	        			    newEl.setPgenCategory( cdlg.getActiveSymbolClass() );
	  	    	           			    newEl.setPgenType( cdlg.getActiveSymbolObjType() );
	  	    		       			}
	  						        
	  						        drawingLayer.replaceElement( oldContours, newContours );
	        						Iterator<DrawableElement> it  = oldAdc.createDEIterator();
	        						while ( it.hasNext() ) {
	        							DrawableElement de  = it.next();
	        							if ( de.equals( el )) {
	        								((SinglePointElement) de).setLocationOnly( oldLoc );	        	    	           			
	        							}
	        						}
	        						
        						    oldLoc = null;
   						        
   						        }  						          								
 
    						}
   						    else {
    							
    							drawingLayer.replaceElement(el, newEl);
    							((SinglePointElement) el).setLocationOnly(oldLoc);
    							
    							oldLoc = null;
    						}
    						
    						Coordinate loc = mapEditor.translateClick(x, y);
    						if (loc != null) 
    						    ((SinglePointElement) newEl).setLocation(loc);
    						else
    							((SinglePointElement) newEl).setLocation(tempLoc);
    						drawingLayer.setSelected(newEl);
    					}

    				}
    				else if ( ptSelected ) {
 
    					ptSelected = false;

			        	if ( el instanceof Jet.JetLine && 
    							jet.getPrimaryDE() == el){
    						Jet newJet = jet.copy();
    						drawingLayer.replaceElement(jet, newJet);
    						newJet.getPrimaryDE().setPoints(ghostEl.getPoints() );
    						jet  = newJet;
    						drawingLayer.setSelected(newJet.getPrimaryDE());

    					}
			        	else if  ( el.getParent() instanceof ContourLine ||
			        			   el.getParent() instanceof ContourCircle ) {                            
			        		editContoursLineNCircle( el, ghostEl.getPoints() );
			        	}
			        	else {
			        		
    						/*
    						 *  Replace the selected element with this new element
    						 *  Note: for GFA, check if the new point will make the GFA
    						 *        polygon invalid - J. Wu.
    						 */
    						if ( !(newEl instanceof Gfa) || 
    							 (newEl instanceof Gfa  && ((Gfa) ghostEl).isValid() ) ) {
   						
    						drawingLayer.replaceElement(el, newEl);
    						
    						// Update the new Element with the new points
    						/*if("CONV_SIGMET".equalsIgnoreCase(newEl.getPgenType() ) ||
    								"NCON_SIGMET".equalsIgnoreCase(newEl.getPgenType())) {	
    							newEl.setPoints( SnapUtil.getSnapWithStation(ghostEl.getPoints(),SnapUtil.VOR_STATION_LIST,10,8) );
    						} else*/ 

    							if("GFA".equalsIgnoreCase(newEl.getPgenType()) && ((IGfa)attrDlg).getGfaFcstHr().indexOf("-") > -1) {
    							ArrayList<Coordinate> points = ghostEl.getPoints();
    							int nearest = getNearestPtIndex(ghostEl, mapEditor.translateClick(x, y));
    							Coordinate toSnap = ghostEl.getPoints().get(nearest);
    							List<Coordinate> tempList = new ArrayList<Coordinate>();
    							tempList.add(toSnap);
    							tempList = SnapUtil.getSnapWithStation(tempList,SnapUtil.VOR_STATION_LIST,10,16); 
    							Coordinate snapped = tempList.get(0);
    							// update the coordinate
    							points.get(nearest).setCoordinate(snapped);
    							newEl.setPoints(points);
    						} else { 
								newEl.setPoints(ghostEl.getPoints());
    						}
    						
    						///update Gfa vor text         		    				
    						if ( newEl instanceof Gfa ) {
    							GfaReducePoint.WarningForOverThreeLines( (Gfa)newEl );           		    				
    							((Gfa)newEl).setGfaVorText( Gfa.buildVorText( (Gfa)newEl ) );
    							if ( attrDlg instanceof GfaAttrDlg ) {
    			            		((GfaAttrDlg)attrDlg).setVorText( ((Gfa)newEl).getGfaVorText() );
    							}

    						}
    						
    						if(attrDlg != null ) 
    							attrDlg.setDrawableElement(newEl);    						
    						
    						// Set this new element as the currently selected element 
    						// Collections do not need to reset.
    						if ( !(drawingLayer.getSelectedComp() instanceof DECollection )){
                                drawingLayer.setSelected(newEl);
    						}
    						
    						}
    					}
    					
    					if(newEl instanceof Track) {
    						if(isModifiedPointOneOfTheLastTwoInitPoint(newEl, ptIndex)) 
    							((Track) newEl).calculateExtrapTrackPoints(); 
    						displayTrackExtrapPointInfoDlg((TrackAttrDlg)attrDlg, (Track)newEl); 
    					} else if (newEl instanceof Gfa) {
        					attrDlg.setAttrForDlg(newEl);
    					}
    					
    					drawingLayer.removeGhostLine();

					}
    				
    				mapEditor.refresh();

    			}
    		}
        	
            return false;
            
        }
        
        @Override
        public boolean handleKeyDown(int keyCode) {
        	if ( !isResourceEditable() ) return false;

	          if(keyCode == SWT.DEL){
	        	  PgenResource pResource = PgenSession.getInstance().getPgenResource();
	        	  pResource.deleteSelectedElements();
	        	  refresh();
	        	  return true;
              }
	          else super.handleKeyDown(keyCode);
	          return false;
        }
        
        
        
        private void setGhostLineColorForTrack(MultiPointElement multiPointElement, int nearestPointIndex) {
        	/*
        	 * If multiPointElement is not a type of Track, do nothing
        	 */
        	if(multiPointElement == null || !(multiPointElement instanceof Track)) 
        		return; 
       	
        	Track track = (Track)multiPointElement; 
        	int initialTrackPointSize = 0; 
        	if(track.getInitialPoints() != null)
        		initialTrackPointSize = track.getInitialPoints().length; 
        	if(isInitialPointSelected(initialTrackPointSize, nearestPointIndex))
        		track.setInitialColor(new java.awt.Color( 255,255,255)); 
        	else 
        		track.setExtrapColor(new java.awt.Color( 255,255,255)); 
        }
        
        private boolean isInitialPointSelected(int initialPointSize, int nearestPointIndex) {
        	if(nearestPointIndex < initialPointSize)
        		return true; 
        	return false; 
        }

        
        private boolean isModifiedPointOneOfTheLastTwoInitPoint(DrawableElement drawableElement, int nearestPointIndex) {
        	boolean isOneOfTheLastTwoInitPoint = false; 
        	/*
        	 * If multiPointElement is not a type of Track, return false
        	 */
        	if(drawableElement == null || !(drawableElement instanceof Track)) 
        		return isOneOfTheLastTwoInitPoint; 
        	
        	Track track = (Track)drawableElement; 
        	int initialTrackPointSize = 0; 
        	if(track.getInitialPoints() != null)
        		initialTrackPointSize = track.getInitialPoints().length; 
        	if(nearestPointIndex == (initialTrackPointSize - 1) || 
        			nearestPointIndex == (initialTrackPointSize - 2))
        		isOneOfTheLastTwoInitPoint = true; 
        		
        	return isOneOfTheLastTwoInitPoint; 
        }

        /**
         * Gets the nearest point of an selected element to the input point
         * @param el 	element
         * @param pt 	input point
         * @return
         */
        protected int getNearestPtIndex( MultiPointElement el, Coordinate pt ){
        	
        	int		ptId = 0;
        	double	minDistance = -1; 	
          	GeodeticCalculator gc;
    		gc = new GeodeticCalculator(drawingLayer.getCoordinateReferenceSystem());
    		gc.setStartingGeographicPoint(pt.x, pt.y);
	
    		int index  = 0;
			for ( Coordinate elPoint : el.getPoints() ){
				
				gc.setDestinationGeographicPoint( elPoint.x, elPoint.y);
				
				double dist = gc.getOrthodromicDistance();

				if ( minDistance <  0 || dist < minDistance ) {
					
					minDistance = dist;
					ptId = index; 
					
				}
				
				index++;
				
			}					
			
			return ptId;
			
        }
        
        public void closeDlg(){
        	if ( attrDlg !=null ) attrDlg.close();
        }

        private void displayTrackExtrapPointInfoDlg(TrackAttrDlg attrDlgObject, 
        		Track trackObject) {

        	if ( attrDlgObject == null ) 
        		return; 
        	TrackExtrapPointInfoDlg extrapPointInfoDlg = attrDlgObject.getTrackExtrapPointInfoDlg(); 
        	if(extrapPointInfoDlg != null) {
        		extrapPointInfoDlg.close(); 
        	} else { 
       			extrapPointInfoDlg = (TrackExtrapPointInfoDlg)AttrDlgFactory.createAttrDlg(Track.TRACK_INFO_DLG_CATEGORY_NAME,
           				pgenType, PlatformUI.getWorkbench().getActiveWorkbenchWindow().getShell() );
        		attrDlgObject.setTrackExtrapPointInfoDlg(extrapPointInfoDlg); 
           		trackExtrapPointInfoDlg = extrapPointInfoDlg; 
       		}
        		
           	/*
           	 * Open the dialog and set the default attributes.
           	 * Note: must call "attrDlg.setBlockOnOpen(false)" first.
           	 */
           	extrapPointInfoDlg.setBlockOnOpen( false );
           	extrapPointInfoDlg.open();
           	
           	extrapPointInfoDlg.setTrack(trackObject, attrDlgObject.getUnitComboSelectedIndex(), 
           		attrDlgObject.getRoundComboSelectedIndex(), attrDlgObject.getRoundDirComboSelectedIndex()); 

       		extrapPointInfoDlg.setBlockOnOpen( true );

        }
        
        private boolean isTrackElement(DrawableType drawableType) {
        	if(drawableType == DrawableType.TRACK)
        		return true; 
        	return false; 
        }
        
        private DrawableType getDrawableType(String pgenTypeString) {
        	if(Track.TRACK_PGEN_TYPE.equalsIgnoreCase(pgenTypeString))
        		return DrawableType.TRACK; 
        	return DrawableType.LINE; 
        }
        
        private boolean closeAttrDlg(AttrDlg attrDlgObject, String pgenTypeString) {
        	if(attrDlgObject == null)
        		return false; 
        	if(isTrackElement(getDrawableType(pgenTypeString))) {
        		TrackAttrDlg tempTrackAttrDlg = (TrackAttrDlg)attrDlgObject;  
        		TrackExtrapPointInfoDlg tempTrackExtrapPointInfoDlg = tempTrackAttrDlg.getTrackExtrapPointInfoDlg(); 
        		tempTrackAttrDlg.setTrackExtrapPointInfoDlg(null); 
        		trackExtrapPointInfoDlg = null;
        		closeTrackExtrapPointInfoDlg(tempTrackExtrapPointInfoDlg); 
        	}
        	return attrDlgObject.close(); 
        }
        
        private void closeTrackExtrapPointInfoDlg(TrackExtrapPointInfoDlg dlgObject) {
        	if(dlgObject != null)
        		dlgObject.close(); 
        }
    }
    
    /**
     * telling if the DE is NOT movable.    
     * @param DrawableElement: the DE to be judged.
     * @return boolean: true not movable.
     */
    private boolean isUnmovable(DrawableElement tmpEl){
    	if(tmpEl instanceof Volcano )
    		return true;
    	
    	if(tmpEl instanceof Sigmet ){
    		Sigmet vaCloud = (Sigmet)tmpEl;
    		String type = vaCloud.getType();
    		
    		if(type != null && (type.contains("WINDS")||(type.contains(Sigmet.ISOLATED))))
    			return true;   		
    	}
    	
    	return false;
    }
    
    /**
     * Update the ContoursAttrDlg if the selected DE is within a Contours.    
     * @param DrawableElement: the selected DE.
     * @return 
     */
    private void updateContoursAttrDlg( DrawableElement elSelected ){
		
    	AbstractDrawableComponent pele = elSelected.getParent();
    	
    	if ( pele != null && pele.getParent() instanceof Contours ) {
    		
    	    ContoursAttrDlg cdlg = (ContoursAttrDlg)attrDlg;
   		
    		if ( elSelected instanceof Arc ) {
				cdlg.setDrawingCircle();
   			    Text lbl = ((ContourCircle)pele).getLabel();
    			if ( lbl != null ) {
    				cdlg.setLabel( lbl.getText()[0] );
    				cdlg.setNumOfLabels( 1 );
    				cdlg.setHideCircleLabel(lbl.getHide());
    			}
    		}             				
    		else if ( elSelected instanceof Line ) {

    			ArrayList<Text> lbls = ((ContourLine)pele).getLabels();
    			if ( lbls != null && lbls.size() > 0 ) {
    				cdlg.setLabel( lbls.get(0).getText()[0] );
    			} 

    			cdlg.setNumOfLabels( ((ContourLine)pele).getNumOfLabels() );
    			cdlg.setClosed( ((Line)elSelected).isClosedLine() );
    			cdlg.setContourLineType( elSelected.getPgenType() );
    			cdlg.setDrawingLine();
    		}              		                  			             				
    		else if ( elSelected instanceof Symbol ) {
    			Text lbl = ((ContourMinmax)pele).getLabel();
				cdlg.setDrawingSymbol();
    			if ( lbl != null ) {
    				cdlg.setLabel( lbl.getText()[0] );
    				cdlg.setNumOfLabels( 1 );
    				cdlg.setActiveSymbol( elSelected );
    			}               				    
    		}
    		else if ( elSelected instanceof Text ) {
    			cdlg.setLabel( ((Text)elSelected).getText()[0] );

    			if ( pele instanceof ContourLine ) {
    				cdlg.setDrawingLine();
    				cdlg.setNumOfLabels( ((ContourLine)pele).getNumOfLabels() ); 
    				cdlg.setClosed( ((ContourLine)pele).getLine().isClosedLine() );
        			cdlg.setContourLineType( ((ContourLine)pele).getLine().getPgenType() );
   			    }
    			else if ( pele instanceof ContourMinmax ) {
    				cdlg.setDrawingSymbol();
    				cdlg.setNumOfLabels( 1 );
    				cdlg.setActiveSymbol( ((ContourMinmax)pele).getSymbol() );
    			}
    			else if ( pele instanceof ContourCircle ) {
    				cdlg.setDrawingCircle();
    				cdlg.setNumOfLabels( 1 );
    				cdlg.setHideCircleLabel(((Text)elSelected).getHide());
    			}
    		}   	
    	}   	
    }
    
    /**
     * Edit lines/circle in a Contours.    
     * @param el: the selected DE.
     * @param points: new locations.
     * @return 
     */
    private void editContoursLineNCircle( DrawableElement el, ArrayList<Coordinate> points ) {
		 
    	if  (  el.getParent() instanceof ContourLine ||
 			   el.getParent() instanceof ContourCircle ) {
    	
    		Contours oldContours = (Contours)el.getParent().getParent();

    		DrawableElement selElem; 
    		if ( el.getParent() instanceof ContourLine ) {
    			selElem = ((ContourLine)el.getParent()).getLine();
    		}
    		else {
    			selElem = ((ContourCircle)el.getParent()).getCircle(); 
    		}

    		if ( oldContours != null ) {
    			Contours newContours = new Contours();

    			Iterator<AbstractDrawableComponent> it = oldContours.getComponentIterator();

    			while ( it.hasNext() ) {    				        					        					        	

    				AbstractDrawableComponent oldAdc = it.next();	        	
    				AbstractDrawableComponent newAdc = oldAdc.copy();

    				if ( oldAdc.equals( el.getParent() ) ) {

    					if ( el.getParent() instanceof ContourLine ) {
    						((ContourLine)newAdc).getLine().setPoints( points );
    						selElem = ((ContourLine)newAdc).getLine();
    					}
    					else {
    						((ContourCircle)newAdc).getCircle().setPoints( points );
    						selElem = ((ContourCircle)newAdc).getCircle(); 
    					}
    				}

    				newAdc.setParent( newContours );
    				newContours.add( newAdc );
    			}

    			newContours.update( oldContours );
    			drawingLayer.replaceElement( oldContours, newContours );
    			drawingLayer.setSelected( selElem );
    			((ContoursAttrDlg)attrDlg).setCurrentContours( newContours );
    		}
    	}
    	
    }
    
    public AttrDlg getAttrDlg(){
    	return attrDlg;
    }
    /**
		Change the line type of the selected element.
	 */
    public void changeSelectedLineType(String type){
    	DrawableElement de = drawingLayer.getSelectedDE();
    	if (  de != null && (de instanceof Line )) {
        	attrDlg.setPgenType(type); 
        	attrDlg.setDefaultAttr();

    		Line ln = (Line)((Line)de).copy();
    		ln.setPgenType(type);
    		
    		ln.update(attrDlg);
    		
    		drawingLayer.replaceElement(de, ln);
    		drawingLayer.setSelected(ln);
    		mapEditor.refresh();
    	}
    }
    
}
