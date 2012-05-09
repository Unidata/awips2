
/*
 * gov.noaa.nws.ncep.ui.pgen.rsc.PgenGfaDrawingTool
 * 
 * 22 March 2010
 *
 * This code has been developed by the NCEP/SIB for use in the AWIPS2 system.
 */

package gov.noaa.nws.ncep.ui.pgen.tools;

import gov.noaa.nws.ncep.ui.pgen.PgenUtil;
import gov.noaa.nws.ncep.ui.pgen.attrDialog.AttrDlg;
import gov.noaa.nws.ncep.ui.pgen.attrDialog.AttrDlgFactory;
import gov.noaa.nws.ncep.ui.pgen.attrDialog.GfaAttrDlg;
import gov.noaa.nws.ncep.ui.pgen.attrDialog.PgenFilterDlg;
import gov.noaa.nws.ncep.ui.pgen.elements.AbstractDrawableComponent;
import gov.noaa.nws.ncep.ui.pgen.elements.DrawableElementFactory;
import gov.noaa.nws.ncep.ui.pgen.elements.DrawableType;
import gov.noaa.nws.ncep.ui.pgen.display.IAttribute;
import gov.noaa.nws.ncep.ui.pgen.elements.Line;
import gov.noaa.nws.ncep.ui.pgen.gfa.Gfa;
import gov.noaa.nws.ncep.ui.pgen.gfa.IGfa;
import gov.noaa.nws.ncep.viz.common.SnapUtil;

import java.util.ArrayList;
import org.eclipse.ui.PlatformUI;

import com.raytheon.uf.viz.core.rsc.IInputHandler;
import com.vividsolutions.jts.geom.Coordinate;

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
 * 
 * </pre>
 * 
 * @author	M.Laryukhin
 */

public class PgenGfaDrawingTool extends AbstractPgenDrawingTool {
	
	
    private boolean startGfaText;
    
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
					GfaAttrDlg dlg = (GfaAttrDlg)attrDlg;
					dlg.setGfaArea(gfa.getGfaArea());
					dlg.setGfaBeginning(gfa.getGfaBeginning());
					dlg.setGfaEnding(gfa.getGfaEnding());
					dlg.setGfaStates(gfa.getGfaStates());
				}
				drawingLayer.removeElement(gfa);
				drawingLayer.setGhostLine(gfa);
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
        	if ( loc == null ) return false;
    		DrawableType drawableType = getDrawableType(pgenType); 

        	if ( button == 1 ) {
    			if (startGfaText && drawableType == DrawableType.GFA){
            		return handleGfaMouseDown(loc, drawableType);
    			}
                points.add( loc );     
                
                attrDlg.setAttrForDlg(attrDlg); // update the parameters in GfaAttrDlg
                return true;
            }
            else if ( button == 3 ) {
            	if ( points.size() == 0 ) {
            		closeAttrDlg(attrDlg); 
            		attrDlg = null; 
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

            		if(((IGfa)attrDlg).getGfaFcstHr().indexOf("-") > -1){
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
            		((GfaAttrDlg)attrDlg).setVorText((Gfa)elem);
            		
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
			elem = def.create( drawableType, (IAttribute)attrDlg,
					pgenCategory, pgenType, points, drawingLayer.getActiveLayer());

			((Gfa)elem).setGfaTextCoordinate(loc);
			
			drawingLayer.addElement(elem);

			removeClearRefresh();
			
			((GfaAttrDlg)attrDlg).populateTagCbo();
			
			startGfaText = false;
			
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
        	//  Check if mouse is in geographic extent
        	Coordinate loc = mapEditor.translateClick(x, y);
        	if ( loc == null ) return false;
        	
        	
        	AbstractDrawableComponent ghost = null;
    		if(startGfaText){
    			return handleGfaTextMouseMove(loc);
    		} else {
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
			return true;
		}

		private boolean handleGfaTextMouseMove(Coordinate loc) {
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

    }
    
}
