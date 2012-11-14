
/*
 * gov.noaa.nws.ncep.ui.pgen.rsc.PgenModifyTool
 * 
 * May 2009
 *
 * This code has been developed by the NCEP/SIB for use in the AWIPS2 system.
 */

package gov.noaa.nws.ncep.ui.pgen.tools;

import java.util.ArrayList;
import java.awt.Color;

import com.raytheon.uf.viz.core.rsc.IInputHandler;
import com.vividsolutions.jts.geom.Coordinate;

//import gov.noaa.nws.ncep.ui.display.InputHandlerDefaultImpl;
import gov.noaa.nws.ncep.ui.pgen.PgenUtil;
import gov.noaa.nws.ncep.ui.pgen.annotation.Operation;
import gov.noaa.nws.ncep.ui.pgen.elements.Jet;
import gov.noaa.nws.ncep.ui.pgen.elements.Line;
import gov.noaa.nws.ncep.ui.pgen.elements.Arc;
import gov.noaa.nws.ncep.ui.pgen.elements.MultiPointElement;
import gov.noaa.nws.ncep.ui.pgen.elements.DrawableElement;
import gov.noaa.nws.ncep.ui.pgen.filter.OperationFilter;
import gov.noaa.nws.ncep.ui.pgen.gfa.Gfa;
import gov.noaa.nws.ncep.ui.pgen.gfa.GfaReducePoint;
import gov.noaa.nws.ncep.ui.pgen.tools.PgenModifyLine;


/**
 * Implements a modal map tool for PGEN Line Modification function.
 * 
 * Only Line/Front can be selected to be modified now (not Arc).
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date       	Ticket#		Engineer	Description
 * ------------	----------	-----------	--------------------------
 * 05/09		#120		J. Wu   	Initial Creation.
 * 04/10		#165		G. Zhang	Added isModifiableSigmet()
 * 02/12        #597        S. Gurung   Removed snapping while modification for all sigmets. 
 * 										Moved snap functionalities to SnapUtil from SigmetInfo. 
 * 05/12		#808		J. Wu   	Update GFA vor text
 * 05/12		#610		J. Wu   	Add warning when GFA FROM lines > 3
 *
 * </pre>
 * 
 * @author	J. Wu
 */

public class PgenModifyTool extends AbstractPgenTool {
	
    /**
     * Input handler for mouse events.
     */ 
    protected IInputHandler modifyHandler = null;
    
    
    public PgenModifyTool() {
    	
    	super();
    	
    }
    
    /**
     * Returns the current mouse handler.
     * @return
     */   
    public IInputHandler getMouseHandler() {	
    
        if ( this.modifyHandler == null ) {
        	
        	this.modifyHandler = new PgenModifyHandler();
        	
        }

        return this.modifyHandler;
        
    }
        
    /**
     * Implements input handler for mouse events.
     * @author bingfan
     *
     */
    public class PgenModifyHandler extends InputHandlerDefaultImpl {
  	    	
    	private boolean preempt;
    	OperationFilter modifyFilter = new OperationFilter( Operation.MODIFY );
    	
       	/**
    	 * Array list to hold clicked points.
    	 */
        ArrayList<Coordinate> clickPts = null;
        
    	/**
    	 * Ghost element that shows the modified element.
    	 */
    	MultiPointElement ghostEl = null;
  	        	
    	/**
    	 * Instance for performing modification.
    	 */   	
    	PgenModifyLine pml = null;
    	
    	/**
    	 * Color of the ghost element. 
    	 */
    	Color ghostColor = new java.awt.Color( 255,255,255);
    	 	
        /*
         * (non-Javadoc)
         * 
         * @see com.raytheon.viz.ui.input.IInputHandler#handleMouseDown(int,
         *      int, int)
         */
        @Override	   	
        public boolean handleMouseDown( int anX, int aY, int button ) { 
        	if ( !isResourceEditable() ) return false;
       	
        	preempt = false;
        	//  Check if mouse is in geographic extent
        	Coordinate loc = mapEditor.translateClick(anX, aY);
        	if ( loc == null || shiftDown ) return false;
        	
        	if ( button == 1 ) {

                if ( drawingLayer.getSelectedDE() == null ) { 
        		    
        			// Get the nearest element and set it as the selected element.
        			DrawableElement elSelected = drawingLayer.getNearestElement( loc, modifyFilter);
        			if (( (elSelected instanceof Line) && !(elSelected instanceof Arc ) ) || isModifiableSigmet(elSelected)) {
        				drawingLayer.setSelected( elSelected ); 
        				mapEditor.refresh();
        				preempt = true;
        			}
        			else { 
        				return false;
        			}
        		}
        		else {
        			preempt = true;
                   	                          			
    			    if ( clickPts == null ) {
    				    clickPts = new ArrayList<Coordinate>();
    			    }    			    
    			
    			    clickPts.add( loc );
    			    
    			    if ( pml == null ) {
    			    	pml = new PgenModifyLine();
    			    }
    			    
               		pml.setClickPts( latlonToPixel( clickPts.toArray( new Coordinate[clickPts.size()] ) ) );
    		       
               		ModifyLine();           		                  		            		    
        		           		    
        			ghostEl.setColors(new Color[]{ ghostColor, new java.awt.Color( 255,255,255)});

    		       	drawingLayer.setGhostLine( ghostEl) ;
    	        	mapEditor.refresh();
        		    
       		    }
                
    		    return preempt;	
       		                
            }
            else if ( button == 3 ) {
	            	            
            	if ( drawingLayer.getSelectedDE() != null ) {
           	    
                    if ( clickPts != null && !clickPts.isEmpty() ) {
      		    
            		    pml.setClickPts( latlonToPixel( clickPts.toArray( new Coordinate[clickPts.size()] ) ) );
        		    
            		    ModifyLine();
            		    
            		    if ( ! ( ((Line)drawingLayer.getSelectedDE()).isClosedLine() && 
            		    		 ((MultiPointElement)ghostEl).getLinePoints().length < 3 ) ) {
    				        
            		    	MultiPointElement selected = (MultiPointElement)drawingLayer.getSelectedDE();
            		    	
            		    	if ( selected instanceof Jet.JetLine ){
                					
                					Jet jet = (Jet)drawingLayer.getActiveLayer().search(selected);
            						Jet newJet = jet.copy();
            						drawingLayer.replaceElement(jet, newJet);
            						newJet.getPrimaryDE().setPoints( ghostEl.getPoints());
            						drawingLayer.setSelected(newJet.getPrimaryDE());
            		    	}
            		    	else {
            		    		MultiPointElement mpe = (MultiPointElement)drawingLayer.getSelectedDE().copy();

            		    		drawingLayer.replaceElement( drawingLayer.getSelectedDE(), mpe ); 
            		    		
            		    		//need snapping, get ghostEl's points with mpe's pgenType
            		    		/*if( isModifiableSigmet(mpe)){ 
									ArrayList<Coordinate> list = SigmetInfo.getSnapWithStation(
											ghostEl.getPoints(), 
											SigmetInfo.VOR_STATION_LIST, 
											10, 
											SigmetInfo.getNumOfCompassPts(mpe));
									//ArrayList<Coordinate> list2 = SigmetInfo.getNonDplicList(list);
									mpe.setPoints(list);//2);
								}else*/
            		    		
            		    		mpe.setPoints(ghostEl.getPoints());
            		    		if ( mpe instanceof Gfa ) {
            		    			if( ((Gfa)mpe).getGfaFcstHr().indexOf("-") > -1 ){
            		    				// snap
            		    				((Gfa)mpe).snap();
            		    				
            		    				 GfaReducePoint.WarningForOverThreeLines( (Gfa)mpe );           		    				
            		    			}
            		    			

            		    			((Gfa)mpe).setGfaVorText( Gfa.buildVorText( (Gfa)mpe ));
            		    		}
            		    		
            		    		drawingLayer.setSelected( mpe );
            		    	}
            		    }
            		    
        			    drawingLayer.removeGhostLine();
        			    clickPts.clear();            		
               		
           		        mapEditor.refresh(); 
                		
            	    }
            	    else {               		          		
               		
            		    ghostEl = null;
               		
            		    drawingLayer.removeGhostLine();
                	    drawingLayer.removeSelected();
          	            mapEditor.refresh();       		
            		
            	    }
                                	
                }
                else {
      		
           	        drawingLayer.removeSelected();
     	            mapEditor.refresh();
        		
        		    // set selecting mode
        		    PgenUtil.setSelectingMode();            		
              
                }
            	    
            	return true;          
            }
            	
            else {            	
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
        	if ( !isResourceEditable() ) return false;
       	
        	//  Check if mouse is in geographic extent
        	Coordinate loc = mapEditor.translateClick(x, y);
        	if ( loc == null ) return false;
        	
        	// create the ghost element and put it in the drawing layer           	
            if ( clickPts != null && clickPts.size() >= 1 ) {
                
            	ArrayList<Coordinate>  newPts = new ArrayList<Coordinate>( clickPts );
            	newPts.add( loc );
            	
            	pml.setClickPts( latlonToPixel( newPts.toArray( new Coordinate[newPts.size()] ) ) );
            	
    		    ModifyLine();    		    
    		               
		       	ghostEl.setColors(new Color[]{ ghostColor, new java.awt.Color( 255,255,255)});
    		    drawingLayer.setGhostLine( ghostEl) ;
	        	mapEditor.refresh();           
            
            }
           
        	return true;
        	
        }

        /*
         * (non-Javadoc)
         * 
         * @see com.raytheon.viz.ui.input.IInputHandler#handleMouseUp(int, int,
         *      int)
         */
        @Override
        public boolean handleMouseUp( int x, int y, int button) {
       	      	
            return false;
            
        }
        
    	@Override
		public boolean handleMouseDownMove(int x, int y, int mouseButton) {
    		if (  !isResourceEditable() ||  shiftDown ) return false;
    		else return preempt;
		}

		/**
    	 *  Set up a "modify" instance, perform modification and 
    	 *  build a new modified element
    	 */
    	private void ModifyLine( ) {   		
    		  		
    		pml.setOriginalPts( latlonToPixel( ((Line)drawingLayer.getSelectedDE()).getLinePoints() ) );
 
    		pml.setSmoothLevel ( ((Line)drawingLayer.getSelectedDE()).getSmoothFactor()  );

    		pml.setClosed (  ((Line)drawingLayer.getSelectedDE()).isClosedLine()  );   		   		
    		
    		pml.PerformModify();
    		
    		buildNewElement();
    		
       	}
   	
    	/**
    	 * Converts an array of lat/lons to pixel coordinates
    	 * @param pts An array of points in lat/lon coordinates
    	 * @return The array of points in pixel coordinates
    	 */
    	private double[][] latlonToPixel( Coordinate[] pts ) {
    		
    		double[] point = new double[2];
    		double[][] pixels = new double[pts.length][2];
            
    		int ii = 0;
    		for ( Coordinate crd : pts ) {
    			
    			point = mapEditor.translateInverseClick( crd );
    			pixels[ii][0] = point[0];
    			pixels[ii][1] = point[1]; 
    			
    			ii++;
    		}
    		    		
    		return pixels;
    	}

    	
    	/**
    	 * Converts an array of pixel coordinates to lat/lons
    	 * @param pts An array of points in pixel coordinates
    	 * @return The array of points in Lat/Lons
    	 */
    	private ArrayList<Coordinate> pixelToLatlon( double[][] pixels ) {
    		
    		ArrayList<Coordinate>  crd = new ArrayList<Coordinate>();
            
    		for ( int ii = 0; ii < pixels.length; ii++ ) {
    		    crd.add( mapEditor.translateClick( pixels[ii][0], pixels[ii][1] ) );
    		}
    				    		
    		return crd;
    		
    	}
       	
    	/**
    	 * Build a new modified DrawableElement from a set of lat/lons
    	 * @param pts An array of points in pixel coordinates
    	 * @return The array of points in Lat/Lons
    	 */
    	private void buildNewElement() {
    		
    		ghostEl = (MultiPointElement)( drawingLayer.getSelectedDE().copy() );
			
    		if ( ghostEl != null && pml.getModifiedPts() != null && pml.getModifiedPts().length > 1 ) {
  
 				ghostEl.setLinePoints( pixelToLatlon( pml.getModifiedPts() ) );
								
				if ( ((Line)drawingLayer.getSelectedDE()).isClosedLine() ) {
				    if ( pml.getModifiedPts().length < 3 ) {
				        ghostEl.setClosed( false );
				    }
				}
			}   		
    	}   	

    }
    
    /**
     * check if the DE is modifiable Sigmet.
     * @param DrawableElement: DE to be checked.
     * @return boolean: true: the Sigmet is Modifiable.
     */
    private boolean isModifiableSigmet(DrawableElement el){
    	
    	if(el instanceof gov.noaa.nws.ncep.ui.pgen.sigmet.Sigmet){
    		gov.noaa.nws.ncep.ui.pgen.sigmet.Sigmet sig = (gov.noaa.nws.ncep.ui.pgen.sigmet.Sigmet) el;
    		
    		if( ! sig.getType().contains("Text") && ! sig.getType().contains("Isolated"))
    			return true;
    	}
    	
    	return false;    	
    }
    
}

