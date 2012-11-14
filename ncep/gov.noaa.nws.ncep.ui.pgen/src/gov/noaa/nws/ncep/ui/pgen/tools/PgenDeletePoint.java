/*
 * gov.noaa.nws.ncep.ui.pgen.tools.PgenDeletePoint
 * 
 * 1 May 2009
 *
 * This code has been developed by the NCEP/SIB for use in the AWIPS2 system.
 */

package gov.noaa.nws.ncep.ui.pgen.tools;

import com.raytheon.uf.viz.core.rsc.IInputHandler;
import com.vividsolutions.jts.geom.Coordinate;

import gov.noaa.nws.ncep.ui.pgen.PgenUtil;
import gov.noaa.nws.ncep.ui.pgen.annotation.Operation;
import gov.noaa.nws.ncep.ui.pgen.display.IMultiPoint;
import gov.noaa.nws.ncep.ui.pgen.elements.Jet;
import gov.noaa.nws.ncep.ui.pgen.elements.MultiPointElement;
import gov.noaa.nws.ncep.ui.pgen.elements.DrawableElement;
import gov.noaa.nws.ncep.ui.pgen.elements.WatchBox;
import gov.noaa.nws.ncep.ui.pgen.filter.OperationFilter;
import gov.noaa.nws.ncep.ui.pgen.gfa.Gfa;
import gov.noaa.nws.ncep.ui.pgen.gfa.GfaReducePoint;

/**
 * Implements a modal map tool for PGEN deleting point function.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date       	Ticket#		Engineer	Description
 * ------------	----------	-----------	--------------------------
 * 05/09			79		B. Yin   	Initial Creation.
 * 08/09			79		B. Yin   	Handle jet.
 * 11/10		#332		B. Yin		Added cleanup() for handler
 * 04/11		#?			B. Yin		Re-factor IAttribute
 * 02/12		#665		J. Wu		Back to "Select" if no DE selected
 * 05/12		#610		J. Wu   	Add warning when GFA FROM lines > 3
 *
 * </pre>
 * 
 * @author	B. Yin
 */

public class PgenDeletePoint extends PgenSelectingTool {
	
	
    /**
     * Input handler for mouse events.
     */ 
    protected IInputHandler delPtHandler = null;

    
    public PgenDeletePoint(){
    	
    	super();
    	
    }
    
    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.viz.ui.tools.AbstractModalTool#deactivateTool()
     */
    @Override
    public void deactivateTool() {

    	if ( delPtHandler != null ) ((PgenDelPtHandler) delPtHandler).cleanup();
    		
    	super.deactivateTool();
        
    }
    
    /**
     * Returns the current mouse handler.
     * @return
     */   
    public IInputHandler getMouseHandler() {	
    
        if ( this.delPtHandler == null ) {
        	
        	this.delPtHandler = new PgenDelPtHandler();
        	
        }

        return this.delPtHandler;
        
    }
        
    /**
     * Implements input handler for mouse events.
     * @author bingfan
     *
     */
    public class PgenDelPtHandler extends PgenSelectingTool.PgenSelectHandler {
   	
    	OperationFilter delPointFilter = new OperationFilter( Operation.DELETE_POINT );
    	
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
        	if ( loc == null || shiftDown ) return false;
        	
        	if ( button == 1 ) {

        		if ( drawingLayer.getSelectedDE() == null ){ 
        		        	
        			// Get the nearest element and set it as the selected element.
        			DrawableElement elSelected = drawingLayer.getNearestElement( loc, delPointFilter );
        			if ( elSelected instanceof MultiPointElement &&
        					!(elSelected instanceof WatchBox )) {
        				drawingLayer.setSelected( elSelected );   
        			}
        			else { 
        				return false;
        			}
        		}
        		else if ( !ptSelected ) {
        			
        			//select the nearest point
        			ptIndex = getNearestPtIndex((MultiPointElement)drawingLayer.getSelectedDE(), loc); 
        			drawingLayer.addPtSelected( ptIndex );
        			ptSelected = true;
        			
        		}
        		else {
        			
        			//remove the selected point
        			if ( drawingLayer.getSelectedDE() instanceof MultiPointElement ){
        				DrawableElement newEl = (DrawableElement)drawingLayer.getSelectedDE().copy();
        				if (((IMultiPoint)newEl).getLinePoints().length <= 2 )return true;
        				newEl.getPoints().remove( ptIndex );
        				
        				if ( newEl instanceof Gfa ) {
        				    ((Gfa)newEl).setGfaVorText( Gfa.buildVorText( (Gfa)newEl));
		    				 GfaReducePoint.WarningForOverThreeLines( (Gfa)newEl );
        				}

        				if ( newEl instanceof Jet.JetLine ){

        					Jet jet = (Jet)drawingLayer.getActiveLayer().search(drawingLayer.getSelectedDE());
    						Jet newJet = jet.copy();
    						drawingLayer.replaceElement(jet, newJet);
    						newJet.getPrimaryDE().setPoints( ((MultiPointElement)newEl).getPoints());
    						drawingLayer.setSelected(newJet.getPrimaryDE());

    					}
    					else {
    						drawingLayer.replaceElement(drawingLayer.getSelectedDE(), newEl);
    						//setPoints will do snap
    						//((MultiPointElement)newEl).setPoints(((MultiPointElement)newEl).getPoints());
    						drawingLayer.setSelected( newEl );
    					}
        				
        				drawingLayer.removePtsSelected();
        				ptSelected = false;
        			}
        			
        		}
        		
     	        mapEditor.refresh();  
                return true;
                
            }
            else if ( button == 3 ) {
            	
            	if ( drawingLayer.getSelectedDE() != null ) {
            		ptSelected = false;
            		drawingLayer.removeSelected();
            		mapEditor.refresh();
            	}
            	else {
            		// set selecting mode
            		PgenUtil.setSelectingMode();  
            	}
     	        
            	return true;
            	
            }
            else{
            	            	
               	return false;
               	
            }
        	
        }
        
        /*
         * overrides the function in selecting tool
         */
        @Override
        public boolean handleMouseDownMove(int anX, int aY, int button){
        	if (  !isResourceEditable() || shiftDown ) return false;
        	else return true;
        }  
        
        /*
         * overrides the function in selecting tool
         */
        @Override
        public boolean handleMouseUp(int x, int y, int button){
        	return false;
        }
        
        private void cleanup(){
        	ptSelected = false;
        	drawingLayer.removeSelected();
        }
        
    }

}
	
