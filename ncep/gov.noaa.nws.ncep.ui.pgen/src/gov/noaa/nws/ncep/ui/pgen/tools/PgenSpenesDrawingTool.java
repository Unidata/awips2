package gov.noaa.nws.ncep.ui.pgen.tools;

import gov.noaa.nws.ncep.ui.pgen.PgenStaticDataProvider;
import gov.noaa.nws.ncep.ui.pgen.PgenUtil;
import gov.noaa.nws.ncep.ui.pgen.display.IAttribute;
import gov.noaa.nws.ncep.ui.pgen.elements.AbstractDrawableComponent;
import gov.noaa.nws.ncep.ui.pgen.elements.DrawableElement;
import gov.noaa.nws.ncep.ui.pgen.elements.DrawableElementFactory;
import gov.noaa.nws.ncep.ui.pgen.elements.DrawableType;
import gov.noaa.nws.ncep.ui.pgen.elements.Line;

import java.util.ArrayList;

import com.raytheon.uf.viz.core.rsc.IInputHandler;
import com.vividsolutions.jts.geom.Coordinate;

public class PgenSpenesDrawingTool extends AbstractPgenDrawingTool {

	
    public PgenSpenesDrawingTool(){
    	
    	super();
    	
    }

    /*
     * Invoked by the CommandService when starting this tool
     * 
     * @see com.raytheon.viz.ui.tools.AbstractTool#runTool()
     */
    @Override
    protected void activateTool( ) {
    	
    	super.activateTool();
       	new Thread(){
			public void run(){
				PgenStaticDataProvider.getProvider().loadCwaTable();
				PgenStaticDataProvider.getProvider().loadStateTable();
				PgenStaticDataProvider.getProvider().loadRfcTable();
			}
		}.start();
    
    }
    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.viz.ui.tools.AbstractModalTool#deactivateTool()
     */
    @Override
    public void deactivateTool() {
    	
    	super.deactivateTool();
    	
	    if ( mouseHandler instanceof PgenSpenesDrawingHandler ){
	    	PgenSpenesDrawingHandler mph = (PgenSpenesDrawingHandler) mouseHandler;
	    	if (mph != null) mph.clearPoints();
	    }
    }

    /**
     * Returns the current mouse handler.
     * @return
     */   
    public IInputHandler getMouseHandler() {	
    
        if ( this.mouseHandler == null ) {
        	
        	this.mouseHandler = new PgenSpenesDrawingHandler();
        	
        }
        
        return this.mouseHandler;
    }
   
    private class PgenSpenesDrawingHandler extends InputHandlerDefaultImpl {
    	
    	/**
    	 * Points of the new element.
    	 */
        private ArrayList<Coordinate> points = new ArrayList<Coordinate>();
        
       	/**
    	 * Current element.
    	 */     
        private AbstractDrawableComponent elem;
        
       	/**
    	 * An instance of DrawableElementFactory, which is used to 
    	 * create new elements.
    	 */
    	private DrawableElementFactory def = new DrawableElementFactory();
    	
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
        	
        	if ( button == 1 ) {
        		
                points.add( loc );                
                
                return true;
                
            }
            else if ( button == 3 ) {
            	
            	if ( points.size() == 0 ) {
            		
            		if (attrDlg != null) attrDlg.close(); 
            		attrDlg = null; 
            		PgenUtil.setSelectingMode();

            	}
            	else if ( points.size() < 2 ){
            		
                    drawingLayer.removeGhostLine();
                    points.clear();
                    
        	        mapEditor.refresh();
        	        
            	}
            	else {

            		// create a line    
            		elem = def.create( DrawableType.SPENES, (IAttribute)attrDlg,
            				pgenCategory, pgenType, points, drawingLayer.getActiveLayer());

            		attrDlg.setDrawableElement((DrawableElement)elem);
            		//	AttrSettings.getInstance().setSettings((DrawableElement)elem);

            		// add the product to PGEN resource
            		drawingLayer.addElement( elem );

            		//System.out.println(USState.statesInGeometry(((Spenes)elem).toJTSPolygon()));
            		drawingLayer.removeGhostLine();
        			points.clear();

            		mapEditor.refresh();

            	}

            	return true;
            	
            }
        	else if ( button == 2 ){
        		
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
        	//  Check if mouse is in geographic extent
        	Coordinate loc = mapEditor.translateClick(x, y);
        	if ( loc == null ) return false;
        	
        	// create the ghost element and put it in the drawing layer
            AbstractDrawableComponent ghostline = def.create(DrawableType.SPENES, (IAttribute)attrDlg,
        			pgenCategory, pgenType, points, drawingLayer.getActiveLayer());
           	
            if ( points != null && points.size() >= 1) {
            	
                ArrayList<Coordinate> ghostPts = new ArrayList<Coordinate>(points);
                ghostPts.add(loc);
                Line ln = (Line)ghostline;
            	ln.setLinePoints( new ArrayList<Coordinate>( ghostPts ) );
            	
            	/*
            	 * Ghost distance and direction to starting point, if requested
            	 */
            	drawingLayer.setGhostLine(ghostline);
            	mapEditor.refresh();
            	
            }
            
        	return false;
        	
        }
        
        @Override
        public boolean handleMouseDownMove(int aX, int aY, int button) {
        	if ( shiftDown ) return false;
        	return true;
        }
        
        private void clearPoints(){
        	points.clear();
        }
    }

}
