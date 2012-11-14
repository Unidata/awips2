/*
 * gov.noaa.nws.ncep.ui.pgen.tools.PgenWatchBoxDrawingTool
 * 
 * 5 December 2008
 *
 * This code has been developed by the NCEP/SIB for use in the AWIPS2 system.
 */

package gov.noaa.nws.ncep.ui.pgen.tools;

import java.awt.Polygon;
import java.util.ArrayList;
import java.util.List;

import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.ui.PlatformUI;

import com.raytheon.uf.viz.core.rsc.IInputHandler;
import com.raytheon.viz.ui.editor.AbstractEditor;
import com.vividsolutions.jts.geom.Coordinate;

import gov.noaa.nws.ncep.edex.common.stationTables.Station;
import gov.noaa.nws.ncep.edex.common.stationTables.StationTable;
import gov.noaa.nws.ncep.ui.pgen.PgenStaticDataProvider;
import gov.noaa.nws.ncep.ui.pgen.PgenUtil;
import gov.noaa.nws.ncep.ui.pgen.elements.*;
import gov.noaa.nws.ncep.ui.pgen.attrdialog.WatchBoxAttrDlg;
import gov.noaa.nws.ncep.ui.pgen.elements.DECollection;
import gov.noaa.nws.ncep.ui.pgen.elements.DrawableType;
import gov.noaa.nws.ncep.ui.pgen.display.IAttribute;
import gov.noaa.nws.ncep.ui.pgen.elements.Line;
import gov.noaa.nws.ncep.ui.pgen.elements.WatchBox.WatchShape;
//import gov.noaa.nws.ncep.viz.ui.display.NCMapEditor;

/**
 * Implements a modal map tool to draw PGEN watch boxes.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date       	Ticket#		Engineer	Description
 * ------------	----------	-----------	--------------------------
 * 09/09					B. Yin   	Initial Creation.
 * 03/10					B. Yin   	Put watch element in a DECollection
 * 04/11		?			B. Yin		load modify-tool once the 2nd point dropped
 * 07/11		?			B. Yin		Pre-load county table
 *
 * </pre>
 * 
 * @author	B. Yin
 */

public class PgenWatchBoxDrawingTool extends AbstractPgenDrawingTool {
	
	/**
	 * public constructor
	 */
    public PgenWatchBoxDrawingTool(){
    	
    	super();
    	
    }
    
    @Override
    protected void activateTool( ) {
    	super.activateTool();
    	if (super.isDelObj()) return;

    	if ( attrDlg != null ){
    		((WatchBoxAttrDlg)attrDlg).enableDspBtn(false);
    	}

    	new Thread(){
			public void run(){
				PgenStaticDataProvider.getProvider().getSPCCounties();
			}
		}.start();
		
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
    	
        PgenWatchBoxDrawingHandler wbh = (PgenWatchBoxDrawingHandler) mouseHandler;
        if (wbh != null) wbh.clearPoints();
        
    }

    /**
     * Returns the current mouse handler.
     * @return
     */   
    public IInputHandler getMouseHandler() {	
    
        if ( this.mouseHandler == null ) {
        	
        	this.mouseHandler = new PgenWatchBoxDrawingHandler();
        	
        }
        
        return this.mouseHandler;
    }
   
    
    /**
     * Implements input handler for mouse events.
     * @author bingfan
     *
     */
          
    private class PgenWatchBoxDrawingHandler extends InputHandlerDefaultImpl {
    	
    	/**
    	 * Points of the new watch box.
    	 */
        private ArrayList<Coordinate> points = new ArrayList<Coordinate>();
        
       	/**
    	 * An instance of DrawableElementFactory, which is used to 
    	 * create a new watch box.
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
        	if ( !isResourceEditable() ) return false;
            
        	//  Check if mouse is in geographic extent
        	Coordinate loc = mapEditor.translateClick(anX, aY);
        	if ( loc == null || shiftDown ) return false;
        	
        	if ( button == 1 ) {

        		/*
        		 * Select watch box shape first.
        		 */
        		if (((WatchBoxAttrDlg)attrDlg).getWatchBoxShape() == null ) {
        			MessageDialog infoDlg = new MessageDialog( 
							PlatformUI.getWorkbench().getActiveWorkbenchWindow().getShell(), 
							"Information", null, "Please select a watch shape before drawing.",
							MessageDialog.INFORMATION, new String[]{"OK"}, 0);

					infoDlg.open();
        		}
        		else {
        			points.add( loc );
        		}
                
                return true;
                
            }
            else if ( button == 3 ) {
            	
            	if ( points.size() == 0 ) {
            		
            		attrDlg.close();
            		attrDlg = null; 
            		PgenUtil.setSelectingMode();

            	}
            	else if ( points.size() < 2 ){
            		
                    drawingLayer.removeGhostLine();
                    points.clear();
                    
        	        mapEditor.refresh();
        	        
            	}
            	
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
        public boolean handleMouseUp( int x, int y, int button ) {
        	if ( !isResourceEditable() ) return false;
        	
        	if ( button == 1 ){
        		if ( points != null && points.size() == 2) {
        			
                	WatchShape ws = ((WatchBoxAttrDlg)attrDlg).getWatchBoxShape();

                	/*
                	 * Get all anchor points in the polygon.
                	 */
                	ArrayList<Station> anchorsInPoly = getAnchorsInPoly(mapEditor, 
                			WatchBox.generateWatchBoxPts(ws,WatchBox.HALF_WIDTH * PgenUtil.SM2M, points.get(0), points.get(1)));
                	
                	/*
                	 * If there is no anchor points in the polygon, pop up a warning message.
                	 * Otherwise, get the two closest anchor points 
                	 * and generate the eight points of the watch box.
                	 */
                	if ( anchorsInPoly == null || anchorsInPoly.isEmpty() ){
                		MessageDialog infoDlg = new MessageDialog( 
                				PlatformUI.getWorkbench().getActiveWorkbenchWindow().getShell(), 
                				"Information", null, "No anchor point in the area!",
                				MessageDialog.INFORMATION, new String[]{"OK"}, 0);

                		infoDlg.open();
                	}
                	else {

                		DECollection dec = def.createWatchBox( pgenCategory, pgenType, ws, 
                					points.get(0), points.get(1), 
                					anchorsInPoly, (IAttribute)attrDlg);
                		if ( dec != null ) {
                			drawingLayer.addElement(dec);
                			
                			//load modify tool
                			((WatchBoxAttrDlg)attrDlg).enableDspBtn(true);
                			((WatchBoxAttrDlg)attrDlg).setWatchBox((WatchBox)dec.getPrimaryDE());
                   			points.clear();
                			PgenUtil.loadWatchBoxModifyTool(dec.getPrimaryDE());
                		}

                	}
                	
            		drawingLayer.removeGhostLine();
            		points.clear();

            		mapEditor.refresh();
            		

        		}
        	}
        	
        	return true;
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
        	
        	// create the ghost line and put it in the drawing layer
           	AbstractDrawableComponent ghost = def.create(DrawableType.LINE, (IAttribute)attrDlg,
        			"Line", "LINE_SOLID", points, drawingLayer.getActiveLayer());
           	
            if ( points != null && points.size() >= 1) {
            	
                ArrayList<Coordinate> ghostPts = new ArrayList<Coordinate>(points);
                ghostPts.add(loc);
                ((Line)ghost).setLinePoints( new ArrayList<Coordinate>( ghostPts ) );
            	
            	drawingLayer.setGhostLine(ghost);
            	mapEditor.refresh();
            	
            }
            
        	return false;
        	
        }
        


  
        @Override
		public boolean handleMouseDownMove(int x, int y, int mouseButton) {
        	if ( !drawingLayer.isEditable() || shiftDown ) return false;
        	else return true;
		}

		/**
         * Remove all points from the mouse handler.
         */
        public void clearPoints(){
        	points.clear();
        }
      
        
      /*  
       *  GeoTools' getDestinationGeoGraphicPoint() is similar to this function
       * 
         public Coordinate getLocAtDistance(Coordinate loc, double distance, double direction ){
        	if ( loc.x > 180 || loc.x < -180 || loc.y > 90 || loc.y < -90 ){
        		return null;
        	}
        	else {
        		double dtr = Math.PI / 180;
        		double locLon = loc.x * dtr;
        		double locLat = loc.y * dtr;
        		direction *= dtr;
        		distance /= 6371200.0;
        		
        		double lat = Math.asin(Math.sin(locLat)*Math.cos(distance)
        						+ Math.cos(locLat)*Math.sin(distance)*Math.cos(direction));
        		double lon;
        		
        		if ( Math.abs((Math.abs(locLat) - Math.PI/2)) < 0.000001 ){
        			lon = locLon;
        		}
        		else {
        			lon = Math.atan2(Math.cos(locLat)*Math.sin(distance)*Math.sin(direction), 
        					Math.cos(distance)-Math.sin(locLat)*Math.sin(lat));
        			lon = Math.IEEEremainder(locLon+lon+Math.PI, 2*Math.PI) - Math.PI;
        		}
        		return new Coordinate(lon/dtr, lat/dtr);
        		
        	}
        }
	*/
    }

    /**
     * Get a list of anchor points in the polygon.
     * @param editor - map editor
     * @param polyPoints - list of coordinates of the polygon
     * @return - list of anchor points in the polygon
     */
    public static ArrayList<Station> getAnchorsInPoly(AbstractEditor editor, List<Coordinate> polyPoints ){
//    public static ArrayList<Station> getAnchorsInPoly(NCMapEditor editor, List<Coordinate> polyPoints ){
    	
    	ArrayList<Station> stnList = new ArrayList<Station>();
    	
    	// construct the polygon
		int[] xpoints = new int[polyPoints.size()];
		int[] ypoints = new int[polyPoints.size()];
		for ( int ii = 0; ii < polyPoints.size(); ii++){
			double pt[] = editor.translateInverseClick(polyPoints.get(ii));
			xpoints[ii] = (int) pt[0];
			ypoints[ii] = (int) pt[1];
		}

		Polygon poly = new Polygon(xpoints, ypoints, polyPoints.size());
		
		// get a list of all anchor points
		StationTable anchorTbl = PgenStaticDataProvider.getProvider().getAnchorTbl();
    	List<Station> anchorList = anchorTbl.getStationList();
    	
    	// if an anchor point is in the polygon, add it to the return list.
    	for ( Station stn : anchorList ){
    		double loc[] = editor.translateInverseClick(new Coordinate(stn.getLongitude(), stn.getLatitude()));
    		if ( poly.contains(loc[0], loc[1])){
    			stnList.add(stn);
    		}
    	}
    	
    	return stnList;
    }
    
}
