
/*
 * gov.noaa.nws.ncep.ui.pgen.rsc.PgenInterpolationTool
 * 
 * AUGUST 2009
 *
 * This code has been developed by the NCEP/SIB for use in the AWIPS2 system.
 */

package gov.noaa.nws.ncep.ui.pgen.tools;

import java.awt.Color;
import java.util.List;

import com.raytheon.uf.viz.core.drawables.IRenderableDisplay;
import com.raytheon.uf.viz.core.map.IMapDescriptor;
import com.raytheon.uf.viz.core.rsc.IInputHandler;
import com.raytheon.viz.ui.editor.AbstractEditor;
import com.vividsolutions.jts.geom.Coordinate;

//import gov.noaa.nws.ncep.ui.display.InputHandlerDefaultImpl;
import gov.noaa.nws.ncep.ui.pgen.PgenUtil;

import gov.noaa.nws.ncep.ui.pgen.annotation.Operation;
import gov.noaa.nws.ncep.ui.pgen.attrdialog.PgenInterpDlg;
import gov.noaa.nws.ncep.ui.pgen.display.ILine;
import gov.noaa.nws.ncep.ui.pgen.elements.AbstractDrawableComponent;
import gov.noaa.nws.ncep.ui.pgen.elements.DrawableElement;
import gov.noaa.nws.ncep.ui.pgen.elements.Symbol;
import gov.noaa.nws.ncep.ui.pgen.filter.OperationFilter;
import gov.noaa.nws.ncep.ui.pgen.gfa.Gfa;


/**
 * Implements a modal map tool for PGEN line Interpolation.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date       	Ticket#		Engineer	Description
 * ------------	----------	-----------	--------------------------
 * 06/09			#142	S. Gilbert 	Created from PgenExtrapTool
 * 04/10			#165	G. Zhang	Added isInterpolableSigmet()
 * 04/11		#?			B. Yin		Re-factor IAttribute
 * 11/12		#?			J. Wu		Added GFA
 * </pre>
 * 
 * @author	S. Gilbert
 */
public class PgenInterpolationTool extends AbstractPgenDrawingTool {
	
	/**
	 * Possible states to be in when selecting elements for interpolation
	 * @author sgilbert
	 *
	 */
	private enum SELECT_STATUS { START, SELECTED_1, VERIFIED_1, SELECTED_2, VERIFIED_2 };
	
	private SELECT_STATUS status;

	/*
	 * list of selected elements from the PGEN resource
	 */
	List<AbstractDrawableComponent> selectedEls = null;
	
	private PgenInterpDlg interpDlg = null;
	Symbol verifySymbol = null;
	
    /**
     * Constructor.  Sets current status to START and creates a symbol to use for verified elements
     */ 
    public PgenInterpolationTool(){
    	
    	super();
    	status = SELECT_STATUS.START;
    	verifySymbol =  new Symbol(null, new Color[] { new Color(255,0,0) }, 1.0f, 1.0, 
    			false, null, "Marker", "FILLED_BOX" );
    }
       
    /**
     * Creates and returns the current mouse handler.
     * @return
     */   
    public IInputHandler getMouseHandler() {	

    	if ( this.mouseHandler == null ) {
    	
    	    this.mouseHandler = new PgenInterpHandler();
    	
        }

        return this.mouseHandler;
        
   }
        
    
    /**
     * Implements input handler for mouse events.
     * @author sgilbert
     *
     */       
    public class PgenInterpHandler extends InputHandlerDefaultImpl {
    	    	
    	OperationFilter interpFilter = new OperationFilter( Operation.INTERPOLATE );
    	
        /*
         * (non-Javadoc)
         * 
         * @see com.raytheon.viz.ui.input.IInputHandler#handleMouseDown(int,
         *      int, int)
         */
        @Override	   	
        public boolean handleMouseDown( int anX, int aY, int button ) { 
        	if ( !isResourceEditable() ) return false;
       	
        	//  Check if mouse is in geographic extent
        	Coordinate loc = mapEditor.translateClick(anX, aY);
        	if ( loc == null ) return false;
        	       	        	
    		selectedEls = drawingLayer.getAllSelected();
			interpDlg = (PgenInterpDlg)attrDlg;
    		
			/*
			 * Left mouse button pressed
			 */
        	if ( button == 1 ) {

            	switch (status) {
            	case START:
            		/*
            		 * If the nearest element is valid, set it as a "selected" element
            		 */
            		DrawableElement el1 = drawingLayer.getNearestElement( loc, interpFilter );
            		if ( selectionIsValid(el1) ) {
            			drawingLayer.setSelected( el1 );
            			if ( useGfaFcsthr( el1 ) ) {
            				 interpDlg.setStartTime( ((Gfa)el1).getGfaFcstHr() );
            			}
                		status = SELECT_STATUS.SELECTED_1;
            		}
            		break;
            	case SELECTED_1:
            		/*
            		 * register the verify symbol with this element to show it in "verify" mode
            		 */
            		drawingLayer.registerSelectedSymbol(selectedEls.get(0), verifySymbol);
            		status = SELECT_STATUS.VERIFIED_1;
            		break;
            	case VERIFIED_1:
            		/*
            		 * If the nearest element is of same category and closed status AND
            		 * is different than the first "selected" element, then set this also as
            		 * a "selected" element
            		 */
            		DrawableElement el2 = drawingLayer.getNearestElement( loc, interpFilter);
            		DrawableElement first = selectedEls.get(0).getPrimaryDE();
            		if ( comparisonIsValid(el2,first) ) {
            			drawingLayer.addSelected(el2);
            			if ( useGfaFcsthr(el2) ) {
           				    interpDlg.setEndTime( ((Gfa)el2).getGfaFcstHr() );
           			    }
                		status = SELECT_STATUS.SELECTED_2;
            		}
            		break;
            	case SELECTED_2:
            		/*
            		 * register the verify symbol with the second element to show it in "verify" mode
            		 */
            		drawingLayer.registerSelectedSymbol(selectedEls.get(1), verifySymbol);
            		status = SELECT_STATUS.VERIFIED_2;
            		armDialog();         // Enable Interpolation button
            		break;
            	}
        	    
                mapEditor.refresh();               
                //System.out.println("Button 1111111 status = "+status);    		    
                return false;	
       		                
            }
        	
        	/*
        	 * Right mouse button pressed
            */          	
            else if ( button == 3 ) {
	            	            
            	switch (status) {
            	case START:
            		/*
            		 * return to Pgen Select mode
            		 */
            		PgenUtil.setSelectingMode();
            		break;
            	case SELECTED_1:
            		/*
            		 * Remove currently selected element from selected list
            		 */
            		drawingLayer.removeSelected();
            		status = SELECT_STATUS.START;
            		break;
            	case VERIFIED_1:
            		/*
            		 * Remove currently verified element from selected list
            		 */
            		drawingLayer.removeSelected();
            		status = SELECT_STATUS.START;
            		break;
            	case SELECTED_2:
            		/*
            		 * Remove second selected element from selected list
            		 */
            		drawingLayer.removeSelected( selectedEls.get(1) );
            		status = SELECT_STATUS.VERIFIED_1;
            		break;
            	case VERIFIED_2:
            		/*
            		 * remove all elements from the selected list
            		 */
            		drawingLayer.removeSelected();
            		interpDlg.disarm();              //  Disable Interpolation button
            		status = SELECT_STATUS.START;
            		break;
            	}

            	mapEditor.refresh();
            	//System.out.println("Button 3333333 status = "+status);    		    
            	return true;          
            
            } 

            else {            	
               	return false;               	          
            }
        	
        }

    }
    
    /*
     * Enable the Interpolation Button on the Dialog
     */
    private void armDialog() {
		interpDlg.arm(this);
    }
    
    /**
     * Compares a new element with an original one to make sure that both 
     * are of the same Pgen Category AND that both are open or both are closed
     * 
     * Note: For GFA, both should have the same hazard type and tag/desk.
     * @param el2 new element
     * @param orig original element
     * @return true if the comparison indicates the second element is valid
     */
    public boolean comparisonIsValid(DrawableElement el2, DrawableElement orig) {
    	
    	if ( orig==null || el2==null ) return false; 
    	if ( orig == el2 ) return false;
    	
    	if ( orig instanceof Gfa ) {
    		
    		if ( el2 instanceof Gfa && 
    			((Gfa)el2).getGfaHazard().equals( ((Gfa)orig).getGfaHazard() ) &&
    			((Gfa)el2).getGfaTag().equals( ((Gfa)orig).getGfaTag() ) && 
    			((Gfa)el2).getGfaDesk().equals( ((Gfa)orig).getGfaDesk() ) ) {
    			return true;
    		}
    		else 
    			return false;

    	}
    	else {  	  	
		if ( el2.getPgenCategory().equals( orig.getPgenCategory() ) ) {
			
				if ( bothClosed(orig, el2) ||  bothOpen(orig, el2) ) {
					return true;
				}
				else
					return false;
		}		
		else
			return false;
    	}
		
	}

    /**
     * Checks if an element is in Pgen Category "Lines" or "Front"
     * @param el1 drawable lement
     * @return  true if element is Line or Front
     */
	public boolean selectionIsValid(DrawableElement el1) {
		
		if ( el1 == null ) return false;
		
    	if ( el1.getPgenCategory().equals("Lines") ||  el1.getPgenCategory().equals("Front") 
    		 || isInterpolableSigmet(el1) || el1 instanceof Gfa ) 	
    		return true;
    	else 
    		return false;
	}

	
	/**
	 * Performs the interpolation between the two selected elements using the properties(parameters)
	 * specified in the Interpolation Dialog.  New elements are created and are added 
	 * to the Pgen Resource.
	 * This method is invoked when the Interpolation button on the dialog is pressed.
	 */
	public void performInterpolation() {
    	
		/*
		 * get interpolation properties from the dialog
		 * reverse the interval is starting time > end time.
		 */
		InterpolationProperties props = new InterpolationProperties( interpDlg.getStartTime(),
									interpDlg.getEndTime(), interpDlg.getInterval() );
    	
    	/*
    	 * perform the interpolation and get back new elements
    	 */
    	List<AbstractDrawableComponent> deList = PgenInterpolator.interpolate(selectedEls.get(0).getPrimaryDE(), selectedEls.get(1).getPrimaryDE(), 
    			props, getDescriptor( mapEditor ) );

    	/*
    	 * Add any new elements to the Pgen Resource
    	 */
    	if ( deList.size() > 0 ) drawingLayer.addElements(deList);
    	
    	/*
    	 * clean up
    	 */
    	drawingLayer.removeSelected();
    	interpDlg.disarm();           // disable Interpolation button
    	status = SELECT_STATUS.START;
	    mapEditor.refresh();
    	
    }

	/*
	 * Checks whether two multipoint elements are both closed
	 */
	private boolean bothClosed(DrawableElement el1,	DrawableElement el2) {
		if ( ((ILine)el1).isClosedLine() && ((ILine)el2).isClosedLine() ) {
			return true;
		}
		else			
			return false;
	}

	/*
	 * Checks whether two multipoint elements are both open
	 */
	private boolean bothOpen(DrawableElement el1, DrawableElement el2) {
		if ( ((ILine)el1).isClosedLine() || ((ILine)el2).isClosedLine() ) {
			return false;
		}
		else			
			return true;
	}
	
	/*
	 * Checks whether a SIGMET PgenCategory element interpolation eligible
	 */
	private boolean isInterpolableSigmet(DrawableElement el){
				
		if( el.getPgenCategory().equals("SIGMET")){
			
			gov.noaa.nws.ncep.ui.pgen.sigmet.Sigmet sig = (gov.noaa.nws.ncep.ui.pgen.sigmet.Sigmet) el;
			
			if( ! sig.getType().contains("Text") && ! sig.getType().contains("Isolated"))
				return true;
		}
		/*
		if ( el.getPgenType().equals("VACL_SIGMET") 
				&& ! ((gov.noaa.nws.ncep.ui.pgen.sigmet.Sigmet)el).getType().contains("Text") ) 
			return true;
		*/
		
		return false;
	}
	
    // Convenience method to get the map descriptor
    private IMapDescriptor getDescriptor( AbstractEditor editor ) {
        // bsteffen ported this function from NCPaneManager
        // return (IMapDescriptor) getPaneManager().getDescriptor();
        IMapDescriptor descriptor = null;
        IRenderableDisplay display = editor.getActiveDisplayPane()
                .getRenderableDisplay();
        if (display != null) {
            descriptor = (IMapDescriptor) display.getDescriptor();
        }
        return descriptor;
    }

    /*
     * Check if it is a GFA and if its forecast hour can be used.
     */
	private boolean useGfaFcsthr( DrawableElement el ){
	    if ( !(el instanceof Gfa) ) {
	    	return false;
	    }
	    else {
	    	String fcstHr = ((Gfa)el).getGfaFcstHr();
	    	if ( fcstHr.contains("-") || fcstHr.contains(":") ) {
	    		return false;
	    	}
	    	else 
	    		return true;
	    }

	}
   


}


