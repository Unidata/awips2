/*
 * gov.noaa.nws.ncep.ui.pgen.rsc.PgenCopyElement
 * 
 * 22 April 2009
 *
 * This code has been developed by the NCEP/SIB for use in the AWIPS2 system.
 */

package gov.noaa.nws.ncep.ui.pgen.tools;

import java.awt.Color;
import java.util.Iterator;

import com.raytheon.uf.viz.core.rsc.IInputHandler;
import com.vividsolutions.jts.geom.Coordinate;

import gov.noaa.nws.ncep.ui.pgen.PgenUtil;
import gov.noaa.nws.ncep.ui.pgen.annotation.Operation;
import gov.noaa.nws.ncep.ui.pgen.elements.AbstractDrawableComponent;
import gov.noaa.nws.ncep.ui.pgen.elements.DECollection;
import gov.noaa.nws.ncep.ui.pgen.elements.DrawableElement;
import gov.noaa.nws.ncep.ui.pgen.elements.Outlook;
import gov.noaa.nws.ncep.ui.pgen.elements.WatchBox;
import gov.noaa.nws.ncep.ui.pgen.filter.OperationFilter;
import gov.noaa.nws.ncep.ui.pgen.gfa.Gfa;
import gov.noaa.nws.ncep.ui.pgen.contours.Contours;
import gov.noaa.nws.ncep.viz.common.SnapUtil;
import gov.noaa.nws.ncep.ui.pgen.sigmet.SigmetInfo;

/**
 * Implements a modal map tool for the PGEN copy element function.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date       	Ticket#		Engineer	Description
 * ------------	----------	-----------	--------------------------
 * 04/09			78		B. Yin   	Initial Creation.
 * 04/09			103		B. Yin		Extends from AbstractPgenTool
 * 06/09			106		B. Yin		Use AbstractDrawableCoponent
 * 09/10			?		B. Yin		Using Screen coordinates 
 * 12/10			321		J. Wu		Select/Copy subcomponent instead
 * 										of the whole Contours. 
 * 02/11					J. Wu		Move Gfa text box with the polygon.										 
 * 02/11			?		B. Yin		Made it work for Outlooks
 * 02/12					J. Wu		Make the new copy as the newly-selected.
 * 02/12            597     S. Gurung   Moved snap functionalities to SnapUtil from SigmetInfo. 
 * 02/12                    S. Gurung   Moved isSnapADC() and getNumOfCompassPts() to SigmeInfo.		
 * 								 
 * </pre>
 * 
 * @author	B. Yin
 */

public class PgenCopyElement extends AbstractPgenTool {
	
    /**
     * Input handler for mouse events.
     */ 
    protected IInputHandler copyHandler = null;

    public PgenCopyElement(){
    	
    	super();
    	
    }
    
    /**
     * Returns the current mouse handler.
     * @return
     */   
    public IInputHandler getMouseHandler() {	
    
        if ( this.copyHandler == null ) {
        	
        	this.copyHandler = new PgenCopyHandler();
        	
        }
        
        return this.copyHandler;
        
    }
        
    /**
     * Implements input handler for mouse events.
     * @author bingfan
     *
     */
    public class PgenCopyHandler extends InputHandlerDefaultImpl {
    	
    	private boolean preempt;
    	private OperationFilter copyFilter= new OperationFilter( Operation.COPY_MOVE ); ;
    	
    	//DrawableElement elSelected = null;
    	AbstractDrawableComponent ghostEl = null;
    	Color ghostColor = new java.awt.Color( 255,255,255);

    	Coordinate ptSelected = null;
    	
        /*
         * (non-Javadoc)
         * 
         * @see com.raytheon.viz.ui.input.IInputHandler#handleMouseDown(int,
         *      int, int)
         */
        @Override	   	
        public boolean handleMouseDown(int anX, int aY, int button) {
        	
        	preempt = false;
        	
           	//  Check if mouse is in geographic extent
        	Coordinate loc = mapEditor.translateClick(anX, aY);
        	if ( loc == null ) return false;
        	
        	if ( button == 1 ) {

        		if ( drawingLayer.getSelectedComp() == null ) {
        			/*
        			 *  Get the nearest element and set it as the selected element.
        			 *  Note: for Contours, we should select the nearest ContourLine, 
                     *        ContourMinmax or ContourCircle.
        			 */
        			AbstractDrawableComponent nadc = drawingLayer.getNearestComponent( loc, copyFilter, true );
        			if ( nadc instanceof Contours ) {
        				nadc = drawingLayer.getNearestElement( loc, (DECollection)nadc ).getParent();
        			}

        			drawingLayer.setSelected( nadc );
        			if ( nadc != null ) preempt = true;
        			mapEditor.refresh();
        		}
                return preempt;
                
            }
            else if ( button == 3 ) {
            	
            	if (  drawingLayer.getSelectedComp() != null ){
            		// de-select element
            		drawingLayer.removeSelected();
            		drawingLayer.removeGhostLine();
            		ghostEl = null;
            		mapEditor.refresh();
            	}
            	else {
            		// set selecting mode
            		PgenUtil.setSelectingMode();
            	}
      	        
            	return true;
            	
            }
            else{
            	
               	return true;
               	
            }
        	
        }

        /*
         * (non-Javadoc)
         * 
         * @see com.raytheon.viz.ui.input.IInputHandler#handleMouseDownMove(int,
         *      int, int)
         */
        @Override
        public boolean handleMouseDownMove(int anX, int aY, int button) {
        	
        	//  Check if mouse is in geographic extent
        	Coordinate loc = mapEditor.translateClick(anX, aY);
        	if ( loc == null ) return false;
        	
        	AbstractDrawableComponent elSelected = drawingLayer.getSelectedComp();
        	Color ghostColor = new java.awt.Color( 255,255,255);
        	double distanceToSelect = 20;

    		if ( elSelected != null ) {

    			preempt = true;
    			// start to copy if the click is near any points of the selected el.
    			for ( Coordinate elPoint : elSelected.getPoints() ){
    				
    				double[] screenPt = mapEditor.translateInverseClick( elPoint );
    				
    				double distance = Math.sqrt( (screenPt[0] - anX ) *  (screenPt[0] - anX )
    											+(screenPt[1] - aY ) *  (screenPt[1] - aY ) );
    				
    				if( distance < distanceToSelect && ghostEl == null ) {
    					ptSelected = elPoint;
    					ghostEl = drawingLayer.getSelectedComp().copy();   					
    					break;
    				}
    				
    			}
    			
    			if ( ghostEl != null ) {
    				// use screen coordinate to copy/move
    				//double[] locScreen = mapEditor.translateInverseClick(loc);
    				double[] ptScreen = mapEditor.translateInverseClick(ptSelected);
    				
    				double deltaX = anX - ptScreen[0];
    				double deltaY = aY - ptScreen[1];
    				
       				// calculate locations of the ghost el
    				for ( int idx = 0; idx < elSelected.getPoints().size(); idx++  ){
    					double[] scnPt = mapEditor.translateInverseClick( elSelected.getPoints().get(idx) );
    					scnPt[0] += deltaX;
    					scnPt[1] += deltaY;
    					
    					Coordinate cord = mapEditor.translateClick(scnPt[0], scnPt[1]);
    					if(cord==null) continue;
    					ghostEl.getPoints().get(idx).x = cord.x;
    					ghostEl.getPoints().get(idx).y = cord.y;

    				}
    				
    				if ( elSelected instanceof Gfa ) {
    					
    					double[] scnPt = mapEditor.translateInverseClick( ((Gfa) elSelected).getGfaTextCoordinate() );
    					scnPt[0] += deltaX;
    					scnPt[1] += deltaY;
    					
    					Coordinate cord = mapEditor.translateClick(scnPt[0], scnPt[1]);
    					if( cord != null ) {
    					    ((Gfa)ghostEl).getGfaTextCoordinate().x = cord.x;
    					    ((Gfa)ghostEl).getGfaTextCoordinate().y = cord.y;
    					}
   					
    				}
    			/*	
    			// use map coordinate to copy/move
    				double deltaX = loc.x - ptSelected.x;
    				double deltaY = loc.y - ptSelected.y;
    				
    				// calculate locations of the ghost el
    				for ( int idx = 0; idx < elSelected.getPoints().size(); idx++  ){
    						
    					ghostEl.getPoints().get(idx).x = elSelected.getPoints().get(idx).x + deltaX;
    					ghostEl.getPoints().get(idx).y = elSelected.getPoints().get(idx).y + deltaY;

    				}
    			*/	
    				// set ghost color
    				ghostEl.setColors(new Color[]{ghostColor, new java.awt.Color( 255,255,255)});
    				
    		       	drawingLayer.setGhostLine(ghostEl);
    	        	mapEditor.refresh();
    					
    			}

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
        	
        	if ( ghostEl != null ) {
       		
        		// reset color for the el and add it to PGEN resource
        		
        		Iterator<DrawableElement> iterator1 = ghostEl.createDEIterator();
        		Iterator<DrawableElement> iterator2 = drawingLayer.getSelectedComp().createDEIterator();

        		while( iterator1.hasNext() && iterator2.hasNext()){
        			iterator1.next().setColors( iterator2.next().getColors());
        		}
        		
        	    AbstractDrawableComponent parent = drawingLayer.getSelectedComp().getParent();
        		
        		if ( ghostEl instanceof WatchBox ){
        			if (PgenWatchBoxModifyTool.resnapWatchBox(mapEditor, (WatchBox)ghostEl, (WatchBox)ghostEl))
        				{ drawingLayer.addElement( ghostEl );
    				      drawingLayer.setSelected( ghostEl );
        				}
        		}
        		else if ( parent.getName().equalsIgnoreCase("Contours") ) {
        			copyContoursComponent( parent );
        		}
        		else if ( parent instanceof Outlook ){
        			((Outlook) parent).add(ghostEl);
    				drawingLayer.setSelected( ghostEl );
        		}
        		else {
        			if ( SigmetInfo.isSnapADC(ghostEl)){
        				java.util.ArrayList<Coordinate> list = SnapUtil.getSnapWithStation(
        						ghostEl.getPoints(), 
        						SnapUtil.VOR_STATION_LIST, 
        						10, 
        						SigmetInfo.getNumOfCompassPts(ghostEl));
        				
        				AbstractDrawableComponent ghostElCp = ghostEl.copy();
        				((DrawableElement)ghostElCp).setPoints(list);
        				drawingLayer.addElement(ghostElCp);
	    				drawingLayer.setSelected( ghostElCp );
	    			}
        			else {
        				drawingLayer.addElement( ghostEl );
	    				drawingLayer.setSelected( ghostEl );
       			    }
        		}
        		
        		drawingLayer.removeGhostLine();
        		ghostEl = null;
        		mapEditor.refresh();
        		
        	}

           return true;
            
        }
        
        /*
         * Copy a contour line/symbol/circle component
         */
        private void copyContoursComponent( AbstractDrawableComponent adc ) {
        	        	
        	/*
        	 * Replace the whole contours to make UNDO/REDO work. 
        	 * 
        	 */
        	Contours newContours = new Contours();
	        Iterator<AbstractDrawableComponent> iterator = ((Contours)adc).getComponentIterator();
	        
	        while ( iterator.hasNext() ) {    				        					        					        	
	        	
	        	AbstractDrawableComponent oldAdc = iterator.next();	        	
	        	AbstractDrawableComponent newAdc = oldAdc.copy();
		        	
	        	/*
	        	 * Add the ghost element as a new component into the new Contour and reset the 
	        	 * selected element to the new copy in the new Contours for the next "COPY".
	        	 */
	        	if ( oldAdc.equals( drawingLayer.getSelectedComp() ) ) {
			        
	        		AbstractDrawableComponent dup = ghostEl.copy();
			        dup.setParent( newContours );
			        newContours.add( dup );

			        drawingLayer.setSelected( dup );
			            
	        	}
	        	
	        	newAdc.setParent( newContours ); 
	        	newContours.add( newAdc );
	        }    
			
	        newContours.update((Contours)adc );
 			drawingLayer.replaceElement( adc, newContours );

        }
        
    }

}
