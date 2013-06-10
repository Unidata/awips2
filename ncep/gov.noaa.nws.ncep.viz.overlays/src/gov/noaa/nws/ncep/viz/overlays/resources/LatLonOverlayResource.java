package gov.noaa.nws.ncep.viz.overlays.resources; 

import java.util.ArrayList;
import java.util.List;

import gov.noaa.nws.ncep.viz.resources.INatlCntrsResource;

import org.opengis.referencing.crs.CoordinateReferenceSystem;

import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.IGraphicsTarget;
import com.raytheon.uf.viz.core.PixelExtent;
import com.raytheon.uf.viz.core.rsc.LoadProperties;
import com.raytheon.uf.viz.core.drawables.IWireframeShape;
import com.raytheon.uf.viz.core.drawables.PaintProperties;
import com.raytheon.uf.viz.core.map.IMapDescriptor;
import com.raytheon.uf.viz.core.rsc.AbstractVizResource;

import com.vividsolutions.jts.geom.Coordinate;

/**
 * Implements a drawing layer to draw lat/lon lines 
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#     Engineer    Description
 * ------------ ----------  ----------- --------------------------
 *   06/15/09    #127        M. Gao	     Initial Creation
 *   06/17/09    #115        Greg Hull   Integrate with AbstractNatlCntrsResource
 *   08/07/09                Greg Hull   remove unused variables and methods
 *   11/18/09                Greg Hull   Incorporate to11d6 changes 
 *   11/04/13    #880        Xiaochuan   set one wireframeShape for one lat or Lon lines.
 *                                       Set spatialChopFlag to be false.
 * </pre>
 * 
 * @author mgao
 * 
 */
public class LatLonOverlayResource extends AbstractVizResource<LatLonOverlayResourceData, IMapDescriptor> 
                            implements INatlCntrsResource {

	private final static org.apache.log4j.Logger log = 
		org.apache.log4j.Logger.getLogger(LatLonOverlayResource.class);
	
	private LatLonOverlayResourceData latLonOverlayResourceData; 
	
    /** The wireframe object for drawing Latitude lines*/
    private IWireframeShape wireframeShapeForLatLineArray;

    /** The wireframe object for drawing Longitude lines*/
    private IWireframeShape wireframeShapeForLonLineArray;
    
    private List<Coordinate[]> latitudeCoordinatePointArrayList; 

    private List<Coordinate[]> longitudeCoordinatePointArrayList; 
    
    private double offset = 0; //50000; 
    
	private double mapMinX; 
	private double mapMaxY; 
	private double mapMinY; 
	private double mapMaxX; 
    
    private double viewMinX; 
	private double viewMaxY; 
	private double viewMinY; 
	private double viewMaxX; 
    
	/*
	 * The four minimum and maximum of X and Y used to paint label
	 */
    private double effectiveMinX; 
	private double effectiveMaxY; 
	private double effectiveMinY; 
	private double effectiveMaxX; 
    
	private double latLonDrawingPointInterval = 1.0; //0.7;  //1.0;  //0.5 
    
	private boolean needsUpdate = true;
	
    protected LatLonOverlayResource(LatLonOverlayResourceData llRscData, LoadProperties props) {
        super(llRscData, props);
        latLonOverlayResourceData = llRscData;
    }
        
    /*
     * (non-Javadoc)
     * 
     * @seecom.raytheon.viz.core.rsc.IVizResource#init(com.raytheon.viz.core.
     * IGraphicsTarget)
     */
    public void initInternal(IGraphicsTarget target) throws VizException {
    	initializeMapMinAndMaxXAndY(); 
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.viz.core.rsc.IVizResource#paint(com.raytheon.viz.core.
     * IGraphicsTarget, com.raytheon.viz.core.PixelExtent, double, float)
     */
    public void paintInternal(IGraphicsTarget target, PaintProperties paintProps) throws VizException {

		float zoomFactor = paintProps.getZoomLevel();
		
		initializeViewMinAndMaxXAndY(paintProps); 
		int latitudeDrawingLineNumber = getLatitudeDrawingLineNumber(
    			latLonOverlayResourceData.getLatitudeInterval()); 
    	int longitudeDrawingLineNumber = getLongitudeDrawingLineNumber(
    			latLonOverlayResourceData.getLongitudeInterval()); 
   	
		// Only need to recreate the wireframeShapes if the intervals or line types changed.
		if( needsUpdate ) {
			needsUpdate = false;			

	    	/*
	    	 * necessary???
	    	 */
	    	clearWireFrameShapeArray(wireframeShapeForLatLineArray);
	    	clearWireFrameShapeArray(wireframeShapeForLonLineArray); 
	      	clearCoordinatePointArrayList(latitudeCoordinatePointArrayList); 
	    	clearCoordinatePointArrayList(longitudeCoordinatePointArrayList); 
 	
	       	latitudeCoordinatePointArrayList = new ArrayList<Coordinate[]>(latitudeDrawingLineNumber); 
	    	longitudeCoordinatePointArrayList = new ArrayList<Coordinate[]>(longitudeDrawingLineNumber); 
	    	
	    	wireframeShapeForLatLineArray = target.createWireframeShape(false,
					descriptor, 4.0f, false, new PixelExtent(
							getViewMinX()+offset, getViewMaxX()-offset, getViewMinY()+offset, getViewMaxY()-offset));
	    	
			double latitudeValue = -90;  
			for(int i=0; i<latitudeDrawingLineNumber && latitudeValue <= 90; i++) {
				Coordinate[] latLonCoordinateArray = createCoordinateArrayForLatitudeLine(latitudeValue, latLonDrawingPointInterval); 
				latitudeCoordinatePointArrayList.add(latLonCoordinateArray); 
				
				if (!( latitudeValue == -90 || latitudeValue == 90 )){
					wireframeShapeForLatLineArray.addLineSegment(latLonCoordinateArray);
				}
				
				latitudeValue += latLonOverlayResourceData.getLatitudeInterval(); 
			}
			
			wireframeShapeForLatLineArray.compile(); 

			wireframeShapeForLonLineArray = target.createWireframeShape(false,
					descriptor, 4.0f, false, new PixelExtent(
							getViewMinX()+offset, getViewMaxX()-offset, getViewMinY()+offset, getViewMaxY()-offset));
			
			double longitudeValue = -180;  
			for(int i=0; i<longitudeDrawingLineNumber && longitudeValue <= 180; i++) {
				
				Coordinate[] latLonCoordinateArray = createCoordinateArrayLongitudeLine(longitudeValue, latLonDrawingPointInterval); 
				longitudeCoordinatePointArrayList.add(latLonCoordinateArray); 
				wireframeShapeForLonLineArray.addLineSegment(latLonCoordinateArray); 
				longitudeValue += latLonOverlayResourceData.getLongitudeInterval(); 
			}
			wireframeShapeForLonLineArray.compile(); 
		}
    	
		double latitudeValue = -90;
		updateEffectiveMinX(getMapMinX(), getViewMinX(), getViewMaxX());

    	for(int i=0; i<	latitudeDrawingLineNumber && latitudeValue <= 90; i++) {
    		addDefaultLabelByPointIndex(wireframeShapeForLatLineArray, String.valueOf((int)latitudeValue), latitudeCoordinatePointArrayList.get(i), 0); 
    		int pointIndexForAddingLabel = getPointLabelIndexForAddingLatitudeLabel(latitudeCoordinatePointArrayList.get(i), zoomFactor, 
    				getEffectiveMinX(), getEffectiveMaxX(), getEffectiveMinY(), getEffectiveMaxY()); 
    		if(!(pointIndexForAddingLabel < 0)) {
    			addLabelOnLatLonLine( wireframeShapeForLatLineArray, latitudeCoordinatePointArrayList.get(i), pointIndexForAddingLabel, 
    					String.valueOf((int)latitudeValue)); 
    		}
    		target.drawWireframeShape(wireframeShapeForLatLineArray, 
    				latLonOverlayResourceData.getColor(), 
    				latLonOverlayResourceData.getLineWidth(), 
    				latLonOverlayResourceData.getLineStyle()); 
			latitudeValue += latLonOverlayResourceData.getLatitudeInterval(); 
    	}

		double longitudeValue = -180;  
		updateEffectiveMaxY(getMapMaxY(), getViewMinY(), getViewMaxY());

		for(int i=0; i<longitudeDrawingLineNumber && longitudeValue <= 180; i++) {	
    		int pointIndexForAddingLabel = getPointLabelIndexForAddingLongitudeLabel(longitudeCoordinatePointArrayList.get(i), zoomFactor, 
    				getEffectiveMinX(), getEffectiveMaxX(), getEffectiveMinY(), getEffectiveMaxY()); 
    		if(!(pointIndexForAddingLabel < 0)) {
    			addLabelOnLatLonLine(wireframeShapeForLonLineArray, longitudeCoordinatePointArrayList.get(i), pointIndexForAddingLabel, 
    					String.valueOf((int)longitudeValue)); 
    		}
    		target.drawWireframeShape(wireframeShapeForLonLineArray, 
    				latLonOverlayResourceData.getColor(), 
    				latLonOverlayResourceData.getLineWidth(), 
    				latLonOverlayResourceData.getLineStyle()); 
			longitudeValue += latLonOverlayResourceData.getLongitudeInterval(); 
    	}
    }

    private void addLabelOnLatLonLine( IWireframeShape wireframeShape, Coordinate[] coordinateArray, int coordiantePointIndex, String label) {
		double[] tmp = { coordinateArray[coordiantePointIndex].x, coordinateArray[coordiantePointIndex].y};
		double[] screenPixel = descriptor.worldToPixel(tmp);
		if(screenPixel != null) {
			wireframeShape.clearLabels(); 
			wireframeShape.addLabel(label, screenPixel); 	
		}
    }
    
    private void initializeMapMinAndMaxXAndY() {
    	mapMinX = descriptor.getGridGeometry().getGridRange().getLow(0);  
    	mapMaxX = descriptor.getGridGeometry().getGridRange().getHigh(0); 
    	mapMinY = descriptor.getGridGeometry().getGridRange().getLow(1); 
    	mapMaxY = descriptor.getGridGeometry().getGridRange().getHigh(1); 
    }
    
    private void initializeViewMinAndMaxXAndY(PaintProperties paintProps) {
    	viewMinX = paintProps.getView().getExtent().getMinX(); 
    	viewMaxX = paintProps.getView().getExtent().getMaxX(); 
    	viewMinY = paintProps.getView().getExtent().getMinY(); 
    	viewMaxY = paintProps.getView().getExtent().getMaxY(); 

    	effectiveMinX = viewMinX; 
    	effectiveMaxX = viewMaxX; 
    	effectiveMinY = viewMinY; 
    	effectiveMaxY = viewMaxY; 
    }
    
    private void updateEffectiveMinX(double mapMinXValue, double viewMinXValue, double viewMaxXValue) {
    	if(isLeftEdgeOfMapInsideCurrentView(mapMinXValue, viewMinXValue, viewMaxXValue)) {
    		effectiveMinX = mapMinXValue; 
    	}
    }
    
    private boolean isLeftEdgeOfMapInsideCurrentView(double minXOfMap, double minXOfCurrentView, double maxXOfCurrentView) {
    	boolean isInsideResult = false; 
    	if(minXOfMap > minXOfCurrentView && minXOfMap < maxXOfCurrentView)
    		isInsideResult = true; 
    	return isInsideResult; 
    }
    
    private void updateEffectiveMaxY(double mapMaxYValue, double viewMinYValue, double viewMaxYValue) {
    	if(isBottomEdgeOfMapInsideCurrentView(mapMaxYValue, viewMinYValue, viewMaxYValue))
    		effectiveMaxY = mapMaxYValue;
    }
    
    private boolean isBottomEdgeOfMapInsideCurrentView(double maxYOfMap, double minYOfCurrentView, double maxYOfCurrentView) {
    	boolean isInsideResult = false; 
    	if(maxYOfMap > minYOfCurrentView && maxYOfMap < maxYOfCurrentView) {
    		isInsideResult = true; 
    	}
    	return isInsideResult; 
    }
    
    private int getPointLabelIndexForAddingLongitudeLabel(Coordinate[] latLonCoordinateArray, 
			float zoomFactor, double minX, double maxX, double minY, double maxY) {
    	int pointIndex = -1; 
    	if(latLonCoordinateArray == null || latLonCoordinateArray.length ==  0)
    		return pointIndex; 
    	
		double positionOffset = 120; 
    	for(int i=0; i<latLonCoordinateArray.length; i++) {
			double[] tmp = { latLonCoordinateArray[i].x, latLonCoordinateArray[i].y};
			double[] screenPixel = descriptor.worldToPixel(tmp);
			if(isPointForPlacingLongituteLabel(screenPixel, positionOffset, zoomFactor, minX, maxX, minY, maxY)) {
				pointIndex = i; 
				break;  
			}
    	}
    	return pointIndex; 
    }
    
    private int getPointLabelIndexForAddingLatitudeLabel(Coordinate[] latLonCoordinateArray, 
			float zoomFactor, double minX, double maxX, double minY, double maxY) {
    	int pointIndex = -1; 
    	if(latLonCoordinateArray == null || latLonCoordinateArray.length ==  0)
    		return pointIndex; 
    	
		double positionOffset = 120; 
    	for(int i=0; i<latLonCoordinateArray.length; i++) {
			double[] tmp = { latLonCoordinateArray[i].x, latLonCoordinateArray[i].y};
			double[] screenPixel = descriptor.worldToPixel(tmp);
			if(isPointForPlacingLatituteLabel(screenPixel, positionOffset, zoomFactor, minX, maxX, minY, maxY)) {
				pointIndex = i; 
				break;  
			}
    	}
    	return pointIndex; 
    }
    
	private void addDefaultLabelByPointIndex(IWireframeShape wireframeShape, String labelValue, 
			Coordinate[] latLonCoordinateArray, int defaultPointIndex) {
		double[] tmp = { latLonCoordinateArray[defaultPointIndex].x, latLonCoordinateArray[defaultPointIndex].y};
		double[] screenPixel = descriptor.worldToPixel(tmp);
		if(screenPixel != null)
			wireframeShape.addLabel(labelValue, screenPixel); 	
	}

    private boolean isPointForPlacingLongituteLabel(double[] pixelValueArray, double positionOffset, float zoomFactor, 
    		double minX, double maxX, double minY, double maxY) {
		double delta = 200; 
    	boolean isPointForLabel = false; 
    	if(pixelValueArray != null) {
        	if(pixelValueArray[0] > minX && pixelValueArray[0] < maxX &&
        			pixelValueArray[1] > minY && pixelValueArray[1] < maxY) {
        		if(Math.abs(pixelValueArray[1] - maxY) < delta) {
        			isPointForLabel = true; 
        		}
        	}
    	}
    	return isPointForLabel; 
    }

	private boolean isPointForPlacingLatituteLabel(double[] pixelValueArray, double positionOffset, float zoomFactor, 
    		double minX, double maxX, double minY, double maxY) {
		double delta = 200; 
		double adjustedOffset = positionOffset * zoomFactor; 
    	boolean isPointForLabel = false; 
    	if(pixelValueArray != null) {
        	if(pixelValueArray[0] > (minX + adjustedOffset) && pixelValueArray[0] < maxX &&
        			pixelValueArray[1] > minY && pixelValueArray[1] < maxY) {
        		if(Math.abs(pixelValueArray[0] - minX - (positionOffset * zoomFactor)) < delta) {
        			isPointForLabel = true; 
        		}
        	}
    	}
    	return isPointForLabel; 
    }

	private int getLatitudeDrawingLineNumber(int latInterval) {
		int latLineNumber = 180/15;  // set a default value
		if(latInterval>0 && latInterval<=180) {
			latLineNumber = 180/latInterval; 
		}
		return latLineNumber + 1; 
	}
	
	private int getLongitudeDrawingLineNumber(int lonInterval) {
		int lonLineNumber = 360/15;   // set a default value
		if(lonInterval>0 && lonInterval<=360) {
			lonLineNumber = 360 / lonInterval; 
			if(lonLineNumber > 360)
				lonLineNumber = 360;  // if we draw 360 lines, the last line will overlap with the first line
		}
		return lonLineNumber + 1; 
	}
	
	private Coordinate[] createCoordinateArrayForLatitudeLine(double latitudeValue, double latLonPointInterval) {
		int coordinateArrayLength = (int)(360 / latLonPointInterval) + 1; 
		Coordinate[] coordinateArray = new Coordinate[coordinateArrayLength]; 
		double longitude = -180; 
		for(int i=0; i<coordinateArray.length && longitude <= 180; i++) {
			coordinateArray[i] = new Coordinate(longitude, latitudeValue); 
			longitude += latLonPointInterval;  
		}
		return coordinateArray; 
	}
	
	private Coordinate[] createCoordinateArrayLongitudeLine(double longitudeValue, double latLonPointInterval) {
		int coordinateArrayLength = (int)((180 - 10) / latLonPointInterval); 
		Coordinate[] coordinateArray = new Coordinate[coordinateArrayLength]; 
		double latitude = -90 + latLonPointInterval; 
		for(int i=0; i<coordinateArray.length && latitude <= 90; i++) {
			coordinateArray[i] = new Coordinate(longitudeValue, latitude); 
			latitude += latLonPointInterval; 
		}
		return coordinateArray; 
	}

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.viz.core.rsc.IVizResource#dispose()
     */
    public void disposeInternal() {
//   		clearWireFrameShapeArray(wireframeShapeForLatLineArray); 
//   		clearWireFrameShapeArray(wireframeShapeForLonLineArray); 
    		
    }
    
    private void clearWireFrameShapeArray(IWireframeShape wireframeShapeArray) {
    	if(wireframeShapeArray != null) {
    		wireframeShapeArray.dispose();
    		wireframeShapeArray = null;
    	}
    }
    
    private void clearCoordinatePointArrayList(List<Coordinate[]> coordinatePointArrayList) {
    	if(coordinatePointArrayList != null) {
    		for(Coordinate[] eachCoordinateArray : coordinatePointArrayList) {
    			eachCoordinateArray = null; 
    		}
    		coordinatePointArrayList = null; 
    	}
    }

	/*
	 * the getters for Map's Min and Max X and Y, View's Min and Max X and Y
	 */
	public double getViewMinX() {
		return viewMinX;
	}

	public double getViewMaxY() {
		return viewMaxY;
	}

	public double getViewMinY() {
		return viewMinY;
	}

	public double getViewMaxX() {
		return viewMaxX;
	}

    public double getMapMinX() {
		return mapMinX;
	}

	public double getMapMaxY() {
		return mapMaxY;
	}

	public double getMapMinY() {
		return mapMinY;
	}

	public double getMapMaxX() {
		return mapMaxX;
	}

	public double getEffectiveMinX() {
		return effectiveMinX;
	}

	public double getEffectiveMaxY() {
		return effectiveMaxY;
	}

	public double getEffectiveMinY() {
		return effectiveMinY;
	}

	public double getEffectiveMaxX() {
		return effectiveMaxX;
	}

//    @Override
    public void resourceAttrsModified() {
    	needsUpdate = true; 
    }

//	@Override
	public boolean isProjectable(CoordinateReferenceSystem mapData) {
		return true;
	}

	// TODO : This has not been tested. 
	@Override
	public void project(CoordinateReferenceSystem mapData) throws VizException {
		needsUpdate = true;
	}	
}
