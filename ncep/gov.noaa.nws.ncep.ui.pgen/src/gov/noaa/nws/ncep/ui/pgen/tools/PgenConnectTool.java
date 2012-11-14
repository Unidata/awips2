/*
 * gov.noaa.nws.ncep.ui.pgen.tools.PgenConnectTool
 * 
 * July 2009
 *
 * This code has been developed by the NCEP/SIB for use in the AWIPS2 system.
 */

package gov.noaa.nws.ncep.ui.pgen.tools;

import java.util.ArrayList;
import java.util.Iterator;
import java.awt.Color;

import org.geotools.referencing.GeodeticCalculator;

import com.raytheon.uf.viz.core.rsc.IInputHandler;
import com.vividsolutions.jts.geom.Coordinate;

import gov.noaa.nws.ncep.ui.pgen.PgenUtil;
import gov.noaa.nws.ncep.ui.pgen.elements.AbstractDrawableComponent;
import gov.noaa.nws.ncep.ui.pgen.elements.Arc;
import gov.noaa.nws.ncep.ui.pgen.elements.DECollection;
import gov.noaa.nws.ncep.ui.pgen.elements.Jet;
import gov.noaa.nws.ncep.ui.pgen.elements.KinkLine;
import gov.noaa.nws.ncep.ui.pgen.elements.Line;
import gov.noaa.nws.ncep.ui.pgen.elements.MultiPointElement;
import gov.noaa.nws.ncep.ui.pgen.elements.DrawableElement;
import gov.noaa.nws.ncep.ui.pgen.filter.OperationFilter;
import gov.noaa.nws.ncep.ui.pgen.annotation.Operation;
import gov.noaa.nws.ncep.ui.pgen.contours.ContourLine;
import gov.noaa.nws.ncep.ui.pgen.contours.Contours;


/**
 * Implements a modal map tool for PGEN connecting function.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date       	Ticket#		Engineer	Description
 * ------------	----------	-----------	--------------------------
 * 07/09		#141		J. Wu  		Initial Creation.
 * 12/09		#167		J. Wu  		Connect contour lines within the
 * 										same Contours with the same label.
 * 01/12		TTR342		J. Wu  		Update for connecting Contours lines.
 * 
 * </pre>
 * 
 * @author	J. Wu
 */

public class PgenConnectTool extends AbstractPgenDrawingTool {		    
    
    public PgenConnectTool(){
    	
    	super();
    	
    }
    
    /**
     * Returns the current mouse handler.
     * @return
     */   
    public IInputHandler getMouseHandler() {	
    
        if ( this.mouseHandler == null ) {       	
        	this.mouseHandler = new PgenConnectHandler();       	
        }

        return this.mouseHandler;
        
    }
                   
    
    /**
     * Implements input handler for mouse events.
     * @author jwu
     *
     */
    public class PgenConnectHandler extends InputHandlerDefaultImpl {
     	    	
    	/**
    	 * instance variable to store the pgenType of the selected drawableElement
    	 */
    	String	pgenCategory;
    	String	pgenType;
    	
    	private OperationFilter connectFilter = new OperationFilter( Operation.CONNECT );
    	
    	/**
    	 * Index of the selected point.
    	 */
     	protected int	nearPt = -1;
     	protected int	secondPt = -1;
     	
   	
    	/**
    	 * ghost element that shows the modified element.
    	 */
     	private MultiPointElement ghostEl = null;
       	
     	/**
    	 * Elements to be connected.
    	 */
    	private MultiPointElement firstEl = null;
    	private MultiPointElement secondEl = null;
   	    
    	private Jet firstJet = null;
    	private Jet secondJet = null;
    	   	
    	/**
    	 * Color of the ghost element. 
    	 */
    	private Color ghostColor = new java.awt.Color( 255,255,255);
    	    	
    	   	
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
        	if ( loc == null ) return false;
        	
        	if ( button == 1 ) {

                if ( firstEl == null ) { 
        		    
        			// Get the nearest element and set it as the selected element.
        			DrawableElement elSelected = drawingLayer.getNearestElement( loc, connectFilter );
            		AbstractDrawableComponent adc = drawingLayer.getNearestComponent( loc, connectFilter, false );
            		            		
            		pgenCategory = getPgenCategory( elSelected, adc );
            		pgenType = getPgenType( elSelected, adc );
            		            		
        			if ( (elSelected instanceof Line) && !(elSelected instanceof Arc ) &&
        					!(elSelected instanceof KinkLine ) ) {
        				
                		if ( adc != null && adc instanceof Jet ) {
                			firstJet = (Jet)adc;
                		}
                		              		       				
        				firstEl = (MultiPointElement)elSelected;        				
        				drawingLayer.setSelected( firstEl );        				
        				
        				nearPt = findNearEnd( 0, getNearestPtIndex( firstEl, loc), 
        						                 firstEl.getPoints().size() - 1 );
    					
               	    	ghostEl = createGhostElement( firstEl, nearPt, loc );
        				
               	    	drawingLayer.setGhostLine( ghostEl) ; 
        		       	
        				mapEditor.refresh();
        			}
        			else {
        				return false;
        			}
        		}
        		else {
        			
        			if ( secondEl == null ) {
        			    
        				DrawableElement elSelected = drawingLayer.getNearestElement( loc, connectFilter );
                   		AbstractDrawableComponent adc = drawingLayer.getNearestComponent( loc, connectFilter, false );

        			    if ( elSelected != firstEl && elSelected != null &&
           					 getPgenCategory( elSelected, adc ).equals( pgenCategory ) &&
           				     getPgenType(elSelected, adc ).equals( pgenType ) && 
           				     selectableContourLine( (MultiPointElement)elSelected, firstEl ) )  {        				   
      			       
        			        //Connect the second element to the first element for ghosting.
                    		if ( adc != null && adc instanceof Jet ) {
                    			secondJet = (Jet)adc;
                    		}
                    		
        			    	secondEl = (MultiPointElement)elSelected; 
        			    
        			        MultiPointElement tmpEl = (MultiPointElement)secondEl.copy();
        			    
 				            secondPt = findNearEnd( 0, getNearestPtIndex( secondEl, loc), 
 				            		                secondEl.getPoints().size() - 1 );
                            
 				            // reverse the second line if needed.
        				    if ( secondPt > 0 ) {
       				            int np = tmpEl.getPoints().size();
        					    for ( int ii = 0; ii < np; ii++ ) {
        					        tmpEl.getPoints().set( ii, secondEl.getPoints().get( np - ii - 1 ) );
       				            }
        				    }
        				    
        				    // remove the last point on the unconnected ghost line
        				    if ( ghostEl.getPoints().size() > firstEl.getPoints().size() ) {
        				        ghostEl.removePoint( ghostEl.getPoints().size() - 1  );
        				    }
        				    
        				    // add all points in the second element to the first element
        				    ghostEl.getPoints().addAll( tmpEl.getPoints() );
      		       	    
        				    drawingLayer.setGhostLine( ghostEl) ; 
        		       	
        				    mapEditor.refresh();
        			    
        			    }
        			}
      			    else { 
      			    	
      			    	/*
      			    	 * Connect and replace the selected elements.
      			    	 */      			    	                        
        		    	ArrayList<AbstractDrawableComponent> oldElem = new ArrayList<AbstractDrawableComponent>();
        		    	ArrayList<AbstractDrawableComponent> newElem = new ArrayList<AbstractDrawableComponent>();
        		    	
        		    	if ( firstJet != null && secondJet != null ) {  // Jet    		    	    
        		    		
        		    		oldElem.add( firstJet );
        		    	    oldElem.add( secondJet );        		    		
       		    	    
            		    	newElem.add( connectJet( firstJet, secondJet ) );       		    		

        		    	}
        		    	else if ( firstEl.getParent() instanceof ContourLine ) {
        		    		
        		    		// replace the old Contours with the a new Contours.
        		    		oldElem.add( firstEl.getParent().getParent() );	  
        		    		newElem.add( connectContourLine( firstEl, secondEl ) );	
        		    	}
        		    	else {  // Lines & Fronts
        		    	    oldElem.add( firstEl );
        		    	    oldElem.add( secondEl );
            		    	
        		    	    MultiPointElement mpe = (MultiPointElement)ghostEl.copy();
            		    	mpe.setColors( firstEl.getColors() );
            		      	
            		    	newElem.add( mpe );
        		    	}
        		    	
        		    	
        		    	drawingLayer.replaceElements( oldElem, newElem ); 
        		    	
        		    	/*
        		    	 * Clean up for next "connect".
        		    	 */
            		    drawingLayer.removeGhostLine();           		    
            		    drawingLayer.removeSelected();
            		    
            		    nearPt = -1;
            		    firstEl = null; 
            		    secondEl = null;
            		    firstJet = null;
            		    secondJet = null;
            			
            		    mapEditor.refresh();          				
        			}
				           			
        			return false;
        			
        		}
                
                return false;
              
            }
            else if ( button == 3 ) {           	            	
            	
            	if ( secondEl != null ) {  
            		// reselect the second element          				
            		ghostEl = createGhostElement( firstEl, nearPt, loc );
                	drawingLayer.setGhostLine( ghostEl ); 
                	
            		secondEl = null;
            		secondJet = null;
            	}
            	else {
            		
            		if ( firstEl != null ) { 
            			// unselect the first element but stay in "Connect" mode
            			drawingLayer.removeGhostLine();
            		    drawingLayer.removeSelected();
            		    nearPt = 0;
            		    firstEl = null;
            		    firstJet = null;
            		}
            		else { 
            		    // Exit "Connect" mode & set selecting mode
            		    PgenUtil.setSelectingMode();
            		}
            		
            	}				          	           	
      	        
            	mapEditor.refresh();
      	        
            	return false;
            	
            }
            else {  // Button 2 - ignore
            	
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
        public boolean handleMouseMove(int x, int y ) {
        	if ( !isResourceEditable() ) return false;

        	//  Check if mouse is in geographic extent
        	Coordinate loc = mapEditor.translateClick(x, y);
        	if ( loc == null ) return false;
        	
       	    if ( firstEl != null && secondEl == null ) {

       	    	ghostEl = createGhostElement( firstEl, nearPt, loc );
				drawingLayer.setGhostLine( ghostEl) ; 
		       	
				mapEditor.refresh();
        		
            }      

            return false;
                
        }
        
        /*
         * (non-Javadoc)
         * 
         * @see com.raytheon.viz.ui.input.IInputHandler#handleMouseUp(int, int,
         *      int)
         */
        @Override
        public boolean handleMouseUp(int x, int y, int button) {        	        	
        	if ( !isResourceEditable() ) return false;
            return true;           
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
        
        /**
         * Finds the "near" point is close to the low end or the high end.
         * @param lowEnd 
         * @param near
         * @param highEnd
         * @return
         */
        private int findNearEnd( int lowEnd, int near, int highEnd ) {       	
        	return  ((near - lowEnd ) < (highEnd - near) ) ? lowEnd : highEnd ;
        }

        /**
         * Create a new MultiPointElement for ghosting.
         * @param fstEl		Element to start ghosting 
         * @param nearPt	Index of the near point
         * @param clickPt	The last clicked point
         * @return gstEl	The new ghosting element
         */
        private MultiPointElement createGhostElement( MultiPointElement fstEl, int nearPt,
        		                  Coordinate clickPt ) {       	
        	
        	MultiPointElement gstEl = null;
        	
            if ( fstEl != null ) {   		      				     				     				
  				
   				gstEl = (MultiPointElement)( fstEl.copy() );
				gstEl.setColors(new Color[]{ ghostColor, new java.awt.Color( 255,255,255)} );
 								
  				if ( nearPt == 0 ) {
 				    int np = gstEl.getPoints().size();
  					for ( int ii = 0; ii < np; ii++ ) {
  					    gstEl.getPoints().set( ii, fstEl.getPoints().get( np - ii - 1 ) );
 				    }
  				}
  				
  				gstEl.getPoints().add( clickPt );	
            }
            
            return gstEl;
        }    
        
        /**
         * Get PGEN category from a AbstractDrawableComponent.
         * @param  elSel	Selected DrawableElement
         * @param  adc		Selected AbstractDrawableComponent
         * @return cat		The PGEN categoryJet
         */
        private String getPgenCategory( DrawableElement elSel, 
        		                        AbstractDrawableComponent adc ) {       	
            String cat = null;
            
    		if ( adc instanceof Jet && adc.getPrimaryDE() == elSel ){				
    			cat = adc.getName();
		    }
		    else {
                if ( elSel != null ) cat = elSel.getPgenCategory();
            }
    		
    		return cat;
        }
       
        /**
         * Get PGEN type from a AbstractDrawableComponent.
         * @param  elSel	Selected DrawableElement
         * @param  adc		Selected AbstractDrawableComponent
         * @return cat		The PGEN type
         */
        private String getPgenType( DrawableElement elSel, 
        		                    AbstractDrawableComponent adc ) {       	
            String type = null;
            
    		if ( adc instanceof Jet && adc.getPrimaryDE() == elSel ){				
    			type = adc.getName();
		    }
		    else {
                if ( elSel != null ) type = elSel.getPgenType();
            }
    		
    		return type;
        }
        
        
        /**
         * Connect two jets.
         * @param fstJet  The Jet to be connected to
         * @param sedJet  The Jet to be connected
         * @return mpe    The connected jet
         */
        private Jet connectJet( Jet fstJet, Jet sedJet ) {       	
            
        	// Create a new Jet through copying.
    	    Jet mpe = fstJet.copy();

    	    // The new Jet line should have all points as in the ghost line.  		
    	    mpe.getJetLine().getPoints().clear();
    	    
    	    mpe.getJetLine().getPoints().addAll( ghostEl.getPoints() );
    		
    	    // Add all barbs, flight levels (windInfo) in second Jet to the new Jet.  		
    	    Iterator<AbstractDrawableComponent> it = sedJet.getComponentIterator();       		    	    
    	    
    		while ( it.hasNext()){
    			AbstractDrawableComponent de = it.next().copy();
    			if ( de.getName().equalsIgnoreCase("windInfo")) {			       		    				
    				
    				// Must set parent of this bard to the new Jet ???
    				//((JetBarb)de.getPrimaryDE()).setParent( mpe );
    				mpe.addBarb ( (DECollection)de );
    			
    			}
    			
    		}
    		
    		mpe.getSnapTool().snapJet(mpe);
    		
    		return mpe;
	
        }
        
        /**
         * Check if a contour line is select-able.
         * 
         * Note: only a contour line within the same Contours and with the same label
         *       string could be selected.
         * 
         * @param el2bSelected  The contour line to be selected
         * @param elSelected    The contour line selected
         * @return  selectableContourLine()  if el2bSelected is select-able 
         */
        private boolean selectableContourLine( MultiPointElement el2bSelected, MultiPointElement elSelected ) {
        	
        	boolean selectable;
        	
        	if ( !(el2bSelected.getParent() instanceof ContourLine ) &&
        		 !(elSelected.getParent() instanceof ContourLine ) ) {
        		selectable = true;
        	}
        	else {
        		
        		selectable = false;
        		
        		if ( (el2bSelected.getParent() instanceof ContourLine ) &&
       		         (elSelected.getParent() instanceof ContourLine )   &&
       		         (el2bSelected.getParent().getParent().equals( 
       		        		      elSelected.getParent().getParent() ) ) ) {
        			 
        		    String[] label1 = ((ContourLine)(el2bSelected.getParent())).getLabelString();
        		    String[] label2 = ((ContourLine)(elSelected.getParent())).getLabelString();
        			
        		    selectable = true;
        		    
        		    if ( label1.length != label2.length ) {
        		    	selectable = false;
        		    }
        		    else {
        		    	for ( int ii = 0; ii < label1.length; ii++ ) {
            		    	if ( !( label1[ii].equals( label2[ii] ) ) ) {
            		    		selectable = false;
            		    		break;
            		    	}
        		    	}
        		    }
        		    
        		}
       		
        	}
       	       	
        	return selectable;
        }
        
    	/**
    	 *  Connect two contour lines in a Contours element.
         * @param clines  The contour line to be connected to
         * @param cline2  The contour line to be connected
         * @return newContours    The connected Contours
   	     */
    	private Contours connectContourLine( MultiPointElement cline1, MultiPointElement cline2 ) {		
    		
   		    Contours newContours = new Contours();
    		Contours oldContours = (Contours)cline1.getParent().getParent();
   		   		    		
            Iterator<AbstractDrawableComponent> iterator =  oldContours.getComponentIterator();
               		           
            boolean connected = false;           
            
            while ( iterator.hasNext() ) {    				        					        					        	
            	
            	AbstractDrawableComponent oldContourComp = iterator.next();
        		AbstractDrawableComponent newContourComp = oldContourComp.copy();
           	
            	if ( oldContourComp instanceof ContourLine ) {
            	
            		Line oldLine = ((ContourLine)oldContourComp).getLine();

            		if ( oldLine.equals( (Line)cline1 ) || oldLine.equals( cline2 ) ) {

            			if ( !connected ) {           			            			            			

            				Line ln = ((ContourLine)newContourComp).getLine();
            				ln.getPoints().clear();
            				ln.getPoints().addAll( ghostEl.getPoints() );

            				newContourComp.setParent( newContours );
            				newContours.add( newContourComp );       		

            				connected = true;

            			}

            		}
            		else {           	           	           	    
            			newContourComp.setParent( newContours );
            			newContours.add( newContourComp );              	    
            		}
            	}
            	else {
        			newContourComp.setParent( newContours );
        			newContours.add( newContourComp );              	               		
            	}

            }
                				
    		newContours.update( oldContours );
             		        
            return newContours;
    		
    	}
   
    }
    
}


