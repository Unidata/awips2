
/*
 * gov.noaa.nws.ncep.ui.pgen.rsc.PgenGfaDrawingTool
 * 
 * 22 March 2010
 *
 * This code has been developed by the NCEP/SIB for use in the AWIPS2 system.
 */

package gov.noaa.nws.ncep.ui.pgen.tools;

import gov.noaa.nws.ncep.ui.pgen.PgenUtil;
import gov.noaa.nws.ncep.ui.pgen.attrdialog.AttrDlg;
import gov.noaa.nws.ncep.ui.pgen.attrdialog.AttrDlgFactory;
import gov.noaa.nws.ncep.ui.pgen.attrdialog.GfaAttrDlg;
import gov.noaa.nws.ncep.ui.pgen.attrdialog.PgenFilterDlg;
import gov.noaa.nws.ncep.ui.pgen.elements.AbstractDrawableComponent;
import gov.noaa.nws.ncep.ui.pgen.elements.DrawableElementFactory;
import gov.noaa.nws.ncep.ui.pgen.elements.DrawableType;
import gov.noaa.nws.ncep.ui.pgen.display.IAttribute;
import gov.noaa.nws.ncep.ui.pgen.elements.Line;
import gov.noaa.nws.ncep.ui.pgen.gfa.Gfa;
import gov.noaa.nws.ncep.ui.pgen.gfa.GfaClip;
import gov.noaa.nws.ncep.ui.pgen.gfa.GfaSnap;
import gov.noaa.nws.ncep.ui.pgen.gfa.IGfa;
import gov.noaa.nws.ncep.viz.common.SnapUtil;

import java.util.ArrayList;

import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.ui.PlatformUI;

import com.raytheon.uf.viz.core.rsc.IInputHandler;
import com.vividsolutions.jts.geom.Coordinate;
import com.vividsolutions.jts.geom.Polygon;

/**
 * Implements a modal map tool for PGEN multiple points drawing.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date       	Ticket#		Engineer	Description
 * ------------	----------	-----------	--------------------------
 * 03/10					M.Laryukhin Initial Creation.
 * 04/11		#?			B. Yin		Re-factor IAttribute
 * 12/11		#?			B. Yin		Sets vorText 
 * 02/12        #597        S. Gurung   Moved snap functionalities to SnapUtil from SigmetInfo. 
 * 05/12		#808		J. Wu		Tune GFA performance for vor text
 * 07/12        #663        Q. Zhou     Modified handleMouseDown left btn to add moveText situation. 
 *                                      Added code to right btn to 1)exit moving text mode. 2)exit selecting mode.
 * 										Modified handleGfaMouseDown to activate moveText btn, and move startGfaText out.
 * 										Modified activateTool to handle non-delete situation.
 * 										Modified handleMouseMove to add moveText situation. 
 *										Create and modified handleGfaTextMouseMove and handleGfaMouseMove
 * 11/12		#911		J. Wu   	TTR 652 - prevent invalid GFA polygon when the new point is
 *                                                clustering with last point (30NM) and first point.
 * </pre>
 * 
 * @author	M.Laryukhin
 */

public class PgenGfaDrawingTool extends AbstractPgenDrawingTool {
	
	
    private boolean startGfaText;
	private static String msg = "This Gfa is invalid and will be excluded in any future " +
								"FROM actions. Please correct it before starting FROM.";
    
    public PgenGfaDrawingTool(){
    	
    	super();
    	
    }
    
    @Override
	protected void activateTool() {
		super.activateTool();
		
		if(attrDlg == null){		
			attrDlg = AttrDlgFactory.createAttrDlg( pgenCategory, pgenType, 
					PlatformUI.getWorkbench().getActiveWorkbenchWindow().getShell() );
		}
		
		if (attrDlg != null && !isDelObj()) {

			String param;
			if ((param = event.getParameter("startGfaText")) != null
					&& param.equalsIgnoreCase("true")) {
				// coming from PgenUtil.setDrawingGfaTextMode
				Gfa gfa = (Gfa)event.getParameters().get("lastUsedGfa");
				startGfaText = true;
				PgenGfaDrawingHandler handler = (PgenGfaDrawingHandler)getMouseHandler();
				if(gfa != null) {
					handler.points.addAll(gfa.getPoints());
					if ( handler.elem == null ) {	//for moving gfa text from selecting mode
						handler.elem = gfa;
					}
					GfaAttrDlg dlg = (GfaAttrDlg)attrDlg;
					dlg.setGfaArea(gfa.getGfaArea());
					dlg.setGfaBeginning(gfa.getGfaBeginning());
					dlg.setGfaEnding(gfa.getGfaEnding());
					dlg.setGfaStates(gfa.getGfaStates());
					drawingLayer.setSelected(gfa);
					editor.refresh();
				}

			} else {
				startGfaText = false;
			}
		}
		return;
	}
    
    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.viz.ui.tools.AbstractModalTool#deactivateTool()
     */
    @Override
    public void deactivateTool() {
    	super.deactivateTool();
        	
	    PgenGfaDrawingHandler mph = (PgenGfaDrawingHandler) mouseHandler;
        if (mph != null) mph.clearPoints();
    }

    /**
     * Returns the current mouse handler.
     * @return
     */   
    public IInputHandler getMouseHandler() {	
        if ( this.mouseHandler == null ) {
        	this.mouseHandler = new PgenGfaDrawingHandler();
        }
        return this.mouseHandler;
    }
   
    
    /**
     * Implements input handler for mouse events.
     * @author bingfan
     *
     */
          
    public class PgenGfaDrawingHandler extends InputHandlerDefaultImpl {
    	
    	/**
    	 * Points of the new element.
    	 */
        protected ArrayList<Coordinate> points = new ArrayList<Coordinate>();
        
       	/**
    	 * Current element.
    	 */     
        protected AbstractDrawableComponent elem;
        
       	/**
    	 * An instance of DrawableElementFactory, which is used to 
    	 * create new elements.
    	 */
    	protected DrawableElementFactory def = new DrawableElementFactory();

        /*
         * (non-Javadoc)
         * 
         * @see com.raytheon.viz.ui.input.IInputHandler#handleMouseDown(int,
         *      int, int)
         */
        @Override	
        public boolean handleMouseDown(int anX, int aY, int button) {
            
        	//  Check if mouse is in geographic extent
        	Coordinate loc = mapEditor.translateClick(anX, aY);
        	if ( loc == null || shiftDown ) return false;
    		DrawableType drawableType = getDrawableType(pgenType); 

        	if ( button == 1 ) {
        		if ( startGfaText && ((GfaAttrDlg)attrDlg).isMoveTextEnable() ) {      //if moveText button is clicked
  
        			Gfa newGfa = (Gfa)elem.copy();
        			drawingLayer.replaceElement(elem, newGfa);
        			elem = newGfa;
        			
        			handleGfaMouseDown(loc, drawableType); 
        			drawingLayer.setSelected((Gfa)elem);
        		}
        		 
        		else if (startGfaText && drawableType == DrawableType.GFA){			//after a GFA polygon is created
        			startGfaText = false;
        			drawingLayer.addElement(elem);
        			
        			validateGfa( (Gfa)elem) ;
        			handleGfaMouseDown(loc, drawableType); 
        			((GfaAttrDlg)attrDlg).enableMoveTextBtn(true);
        			
        		}
        		else {
        			if ( points.size() == 0 ) {
        				//When drawing GFA polygon, disable the moveText button in GFA dialog
            			((GfaAttrDlg)attrDlg).enableMoveTextBtn(false);
    			}
        			
        			if ( isValidPoint( points, loc ) ) {
                points.add( loc );     
        		}
                
//        			points.add( loc ); 
        		}
        		
                attrDlg.setAttrForDlg(attrDlg); // update the parameters in GfaAttrDlg
                return true;
            }
            else if ( button == 3 ) {

        		//exit drawing text mode
        		if ( startGfaText ) {
        			startGfaText = false;

        			if ( ((Gfa)elem).getGfaTextCoordinate() == null ){
        				//if right click before adding the text
        				((Gfa)elem).setGfaTextCoordinate(loc);
            			drawingLayer.addElement(elem);
            			validateGfa( (Gfa)elem) ;
            			mapEditor.refresh();
        				points.clear();
        			}
        			else {
        				//change to selecting mode to move points
        				PgenUtil.setSelectingMode(elem);
        				elem = null;
        				return true;
        			}
        		}

            	if ( points.size() == 0 ) {

            		closeAttrDlg(attrDlg); 
            		attrDlg = null; 
						drawingLayer.removeSelected((Gfa)elem);
						elem = null;
            		PgenUtil.setSelectingMode();
					

            	}
            	else if ( points.size() < 2 ){
                    removeClearRefresh();
            	}
            	else {
            		//Use pgenType value to decide if the DrawableType should be TRACK or LINE

            		if (drawableType == DrawableType.GFA && points.size() == 2 ) {
            			removeClearRefresh();
            	        return true;
            		}
            		if(!((GfaAttrDlg)attrDlg).validateRequiredFields()) return false;

            		if(((IGfa)attrDlg).getGfaFcstHr().indexOf("-") > -1) {
	            		// snap
	        			points = SnapUtil.getSnapWithStation(points,SnapUtil.VOR_STATION_LIST,10,16);
            		}
            		
            		// create a new DrawableElement.    
            		elem = def.create( drawableType, (IAttribute)attrDlg,
            				pgenCategory, pgenType, points, drawingLayer.getActiveLayer());
            		
            		//set the hour filter
            		if ( PgenFilterDlg.isFilterDlgOpen()){
            			PgenFilterDlg.getInstance(null).setHourChkBox(((Gfa)elem).getForecastHours(), true);
            		}
            		
            		//set from line
            		String vorText = Gfa.buildVorText( (Gfa)elem );
            		((Gfa)elem).setGfaVorText( vorText );
            		((GfaAttrDlg)attrDlg).setVorText( vorText );
            		
					startGfaText = true;
					attrDlg.setAttrForDlg(attrDlg); // update the parameters in GfaAttrDlg

					return true;
            	}

            	return true;
            }
        	else if ( button == 2 ){
        		if(startGfaText) {
        			return handleMouseDown(anX, aY, 1);
        		} else if(points.size() >= 2) {
        			return handleMouseDown(anX, aY, 3);
        		}
        		return true;
        	}

        	return false;
        }

		private boolean handleGfaMouseDown(Coordinate loc, DrawableType drawableType) {
			
			((Gfa)elem).setGfaTextCoordinate(loc);
    		
			removeClearRefresh();
			
			((GfaAttrDlg)attrDlg).populateTagCbo();
			((GfaAttrDlg)attrDlg).setDrawableElement((Gfa)elem);
			
			return true;
		}

		private void removeClearRefresh() {
			drawingLayer.removeGhostLine();
			points.clear();
			mapEditor.refresh();
		}
        
        private DrawableType getDrawableType(String pgenTypeString) {
        	if (pgenTypeString.equalsIgnoreCase("GFA"))
        		return DrawableType.GFA;
        	return DrawableType.LINE; 
        }
        
        private void closeAttrDlg(AttrDlg attrDlgObject) {
        	if(attrDlgObject == null)
        		return; 
        	attrDlgObject.close(); 
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
        	
        	
        	AbstractDrawableComponent ghost = null;
    		if(startGfaText && ((GfaAttrDlg)attrDlg).isMoveTextEnable() ){
    			return handleGfaTextMouseMove(loc);
    		} 
    		else if( startGfaText){
    			return handleGfaMouseMove(loc);
    		}    		
    		else {
    			ghost = def.create(DrawableType.GFA, (IAttribute)attrDlg,
        			pgenCategory, pgenType, points, drawingLayer.getActiveLayer());
    		}
        	
            if ( points != null && points.size() >= 1) {
            	
                ArrayList<Coordinate> ghostPts = new ArrayList<Coordinate>(points);
                ghostPts.add(loc);
                Line ln = (Line)ghost;
            	ln.setLinePoints( new ArrayList<Coordinate>( ghostPts ) );
            	
            	drawingLayer.setGhostLine(ghost);
            	mapEditor.refresh();
            	
            }
            
        	return false;
        	
        }

		@Override
		public boolean handleMouseDownMove(int x, int y, int mouseButton) {
			if ( !isResourceEditable() ||  shiftDown ) return false;
			else return true;
		}

		private boolean handleGfaTextMouseMove(Coordinate loc) {

			AbstractDrawableComponent ghost = null;
//			if (drawingLayer.getActiveLayer().getDrawables().contains(elem)) {
//				drawingLayer.removeElement(elem);
//			}
			
			if (elem != null && ((Gfa)elem).getGfaTextCoordinate() != null) {
				ghost = (Gfa)elem.copy();
			}
			else {
				ghost = (Gfa)def.create(DrawableType.GFA, (IAttribute)attrDlg,
						pgenCategory, pgenType, points, drawingLayer.getActiveLayer());
			}
			
			((Gfa)ghost).setGfaTextCoordinate(loc);
			
			drawingLayer.setGhostLine(ghost);
        	mapEditor.refresh();
			return false;
		}
		
		private boolean handleGfaMouseMove(Coordinate loc) {

			Gfa gfa = (Gfa)def.create(DrawableType.GFA, (IAttribute)attrDlg,
					pgenCategory, pgenType, points, drawingLayer.getActiveLayer());
			gfa.setGfaTextCoordinate(loc);
			drawingLayer.setGhostLine(gfa);
			mapEditor.refresh();
			return false;
		}
        
        public void clearPoints(){
        	points.clear();
        }

        
        /*
         * Warning if a GFA is invalid (self-crossing).
         */
        private void validateGfa( Gfa gfa ) {
			if ( !gfa.isValid() ) {
				MessageDialog confirmDlg = new MessageDialog( 
						PlatformUI.getWorkbench().getActiveWorkbenchWindow().getShell(), 
						"Invalid GFA Polygon", null, msg,
						MessageDialog.WARNING, new String[]{"OK"}, 0 );

				confirmDlg.open();
			}
        }
        
        /*
        * Checks if adding a new point to the end will make the polygon invalid.
        * 
        * Invalid if: (1) the point is in the clustering distance with the last point &&
        *             (2) the point is in the clustering distance with the first point &&
        *             (3) the point makes the polygon self-crossing - both in map/grid coordinate.
        * 
        */
       private boolean isValidPoint( ArrayList<Coordinate> points, Coordinate loc ) {
       	
    	   boolean valid = true;

    	   if ( loc == null ) {
    		   valid = false;
    	   }
    	   else {
    		   int ll = points.size();
    		   if ( ll > 1 ) {

    			   if ( GfaSnap.getInstance().isCluster( loc, points.get(ll -1) ) ||
    					   GfaSnap.getInstance().isCluster( loc, points.get(0) ) ) {

    				   Coordinate[] pts = new Coordinate[ points.size() + 1 ];
    				   points.toArray( pts);
    				   pts[ pts.length - 1 ] = loc;

    				   Polygon poly1 = GfaClip.getInstance().pointsToPolygon( pts );

    				   valid = poly1.isValid();

    				   if ( valid ) {
    					   Polygon poly2 = GfaClip.getInstance().pointsToPolygon( PgenUtil.latlonToGrid( pts ) );
    					   valid = poly2.isValid();
    				   }
    			   }    	    	
    		   }
    	   }

    	   return valid;
       }
       
    }
    
}
