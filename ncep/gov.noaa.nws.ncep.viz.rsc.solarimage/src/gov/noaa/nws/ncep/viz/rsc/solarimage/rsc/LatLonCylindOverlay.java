package gov.noaa.nws.ncep.viz.rsc.solarimage.rsc;


import gov.noaa.nws.ncep.viz.rsc.solarimage.util.HeaderData;

import java.util.ArrayList;
import java.util.List;

import org.eclipse.swt.graphics.RGB;
import org.opengis.referencing.operation.TransformException;

import com.raytheon.uf.viz.core.IGraphicsTarget;
import com.raytheon.uf.viz.core.IGraphicsTarget.LineStyle;
import com.raytheon.uf.viz.core.drawables.IDescriptor;
import com.raytheon.uf.viz.core.drawables.IFont;
import com.raytheon.uf.viz.core.drawables.IWireframeShape;
import com.raytheon.uf.viz.core.drawables.PaintProperties;
import com.raytheon.uf.viz.core.exception.VizException;
import com.vividsolutions.jts.geom.Coordinate;

/**
 * The class that draws the latitude and longitude overlays.
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer         Description
 * ------------ ---------- -----------      --------------------------
 * 04/04/2013   958        qzhou            Initial creation
 * 
 * </pre>
 * 
 * @author qzhou
 * @version 1.0
 */

public class LatLonCylindOverlay {
	
	  // Latitude lines
    private IWireframeShape[] wireframeShapeForLatLineArray;
    
    private List<Coordinate[]> latCoordPointArrayList; 
    
    private IWireframeShape[] wireframeShapeForLonLineArray;
    
    private List<Coordinate[]> lonCoordPointArrayList; 
    
    private double latLonDrawingPointInterval = 1.0;  

    protected int latLonInterval;
    
    private CylindCedDisplay imageDisplay;
    
    HeaderData headerData;
    
    private IDescriptor descriptor;
    
    private PaintProperties paintProps;
    
    private float zoomFactor;
   
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
   
	boolean isCarrington;
	
	public LatLonCylindOverlay(CylindCedDisplay imageDisplay, IDescriptor descriptor, int latLonInterval, PaintProperties paintProps)  {
		this.imageDisplay = imageDisplay;
		this.descriptor = descriptor;
		this.latLonInterval = latLonInterval;
		this.paintProps = paintProps;
		this.zoomFactor = paintProps.getZoomLevel();

//		initializeMapMinAndMaxXAndY();
	}

	private void initializeMapMinAndMaxXAndY() {
    	mapMinX = descriptor.getGridGeometry().getGridRange().getLow(0);  
    	mapMaxX = descriptor.getGridGeometry().getGridRange().getHigh(0); 
    	mapMinY = descriptor.getGridGeometry().getGridRange().getLow(1); 
    	mapMaxY = descriptor.getGridGeometry().getGridRange().getHigh(1);     	
    }
    
    private void initializeViewMinAndMaxXAndY() {
    	viewMinX = paintProps.getView().getExtent().getMinX(); 
    	viewMaxX = paintProps.getView().getExtent().getMaxX(); 
    	viewMinY = paintProps.getView().getExtent().getMinY(); 
    	viewMaxY = paintProps.getView().getExtent().getMaxY(); 

    	effectiveMinX = viewMinX; 
    	effectiveMaxX = viewMaxX; 
    	effectiveMinY = viewMinY; 
    	effectiveMaxY = viewMaxY; 
    	
    	//System.out.println(" *** viewMinX = " + viewMinX + " " + viewMaxX + " " + viewMinY + " " +  viewMaxY);
    }
    
	public void drawLatLines(IGraphicsTarget target) throws VizException, TransformException {	    
		 
		 	IFont.Style [] fontStyle = new IFont.Style[1];
	    	fontStyle[0] = IFont.Style.BOLD;
		    IFont labelFont = target.initializeFont(target.getDefaultFont()
	                .getFontName(), 12, fontStyle);
		    labelFont.setSmoothing(false);
		    labelFont.setScaleFont(false);
		    
		 	initializeViewMinAndMaxXAndY();
	       
	        int latDrawingLineNumber = getLatDrawingLineNumber(latLonInterval); 
	        
	        latCoordPointArrayList = new ArrayList<Coordinate[]>(latDrawingLineNumber);
	    	wireframeShapeForLatLineArray = new IWireframeShape[latDrawingLineNumber];
	    	
	    	int latValue = -90;  
			for(int i=0; i<wireframeShapeForLatLineArray.length && latValue<= 90; i++) {				
				wireframeShapeForLatLineArray[i] = target.createWireframeShape(false, descriptor);
				Coordinate[] latLonCoordArray = createCoordArrayForLatLine(imageDisplay, latValue, latLonDrawingPointInterval); 
				if (latLonCoordArray !=null) {
					latCoordPointArrayList.add(latLonCoordArray); 
					wireframeShapeForLatLineArray[i].addLineSegment(latLonCoordArray); 
					wireframeShapeForLatLineArray[i].compile();
				}
				latValue += latLonInterval; 
			}
			
			latValue = -90; 
	    	for(int i=0; i<wireframeShapeForLatLineArray.length && latValue<= 90; i++) { 
	    		wireframeShapeForLatLineArray[i].clearLabels();
				addDefaultLabelByPointIndex(wireframeShapeForLatLineArray[i], String.valueOf((int)latValue), latCoordPointArrayList.get(i), 0); 
					
				updateEffectiveMinX(getMapMinX(), getViewMinX(), getViewMaxX()); 
				int pointIndexForAddingLabel = getPointLabelIndexForAddingLatLabel(latCoordPointArrayList.get(i), zoomFactor, 
	    				getEffectiveMinX(), getEffectiveMaxX(), getEffectiveMinY(), getEffectiveMaxY()); 
	    		
				if(!(pointIndexForAddingLabel < 0))
				addLabelOnLatLine( wireframeShapeForLatLineArray[i], latCoordPointArrayList.get(i), pointIndexForAddingLabel, 
	    				String.valueOf((int)latValue)); 
				
	    		target.drawWireframeShape(wireframeShapeForLatLineArray[i], 
	    				new RGB(255, 0, 0), 
	    				1.0f, 
	    				LineStyle.MEDIUM_DASHED, labelFont); 
				latValue += latLonInterval; 
	    	}
	    	

	    }
 
	    public void drawLonLines(IGraphicsTarget target) throws VizException, TransformException {

	    	IFont.Style [] fontStyle = new IFont.Style[1];
	    	fontStyle[0] = IFont.Style.BOLD;
		    IFont labelFont = target.initializeFont(target.getDefaultFont()
	                .getFontName(), 12, fontStyle);
		    labelFont.setSmoothing(false);
		    labelFont.setScaleFont(false);

	    	int lonDrawingLineNumber = getLonDrawingLineNumber(latLonInterval); 
	        
	        lonCoordPointArrayList = new ArrayList<Coordinate[]>(lonDrawingLineNumber);
	    	wireframeShapeForLonLineArray = new IWireframeShape[lonDrawingLineNumber]; 
	    	

			int lonValue = -180;  // 180 will not be displayed
			for (int i=0; i<wireframeShapeForLonLineArray.length && lonValue <= 180; i++) {
				wireframeShapeForLonLineArray[i] = target.createWireframeShape(false, descriptor);
				Coordinate[] latLonCoordArray = createCoordArrayForLonLine(imageDisplay, lonValue, latLonDrawingPointInterval); 
				lonCoordPointArrayList.add(latLonCoordArray); 
				wireframeShapeForLonLineArray[i].addLineSegment(latLonCoordArray); 
				wireframeShapeForLonLineArray[i].compile(); 
				lonValue += latLonInterval; 
			}

			lonValue = -180; 
	    	for (int i=0; i<wireframeShapeForLonLineArray.length && lonValue <= 180 ; i++) { 
				wireframeShapeForLonLineArray[i].clearLabels();
				updateEffectiveMaxY(getMapMaxY(), getViewMinY(), getViewMaxY()); 
				int pointIndexForAddingLabel = getPointLabelIndexForAddingLonLabel(lonCoordPointArrayList.get(i), zoomFactor, 
	    				getEffectiveMinX(), getEffectiveMaxX(), getEffectiveMinY(), getEffectiveMaxY());  

				if(!(pointIndexForAddingLabel+1 < 0))
	    		addLabelOnLonLine(wireframeShapeForLonLineArray[i], lonCoordPointArrayList.get(i), pointIndexForAddingLabel+1, //-1
	    				String.valueOf(lonValue)); 
	
	    		target.drawWireframeShape(wireframeShapeForLonLineArray[i], 
	    				new RGB(255, 0, 0), 
	    				1.0f, 
	    				LineStyle.MEDIUM_DASHED, labelFont); 
				lonValue += latLonInterval; 
	    	}
	    	

	    	// won't draw on 180
	    	wireframeShapeForLonLineArray[0] = target.createWireframeShape(false, descriptor);
			Coordinate[] latLonCoordArray = createCoordArrayForLonLine(imageDisplay, -179.4, latLonDrawingPointInterval); 
			lonCoordPointArrayList.add(latLonCoordArray); 
			wireframeShapeForLonLineArray[0].addLineSegment(latLonCoordArray); 
			wireframeShapeForLonLineArray[0].compile(); 			
			
			wireframeShapeForLonLineArray[0].clearLabels();
			updateEffectiveMaxY(getMapMaxY(), getViewMinY(), getViewMaxY()); 
			int pointIndexForAddingLabel = getPointLabelIndexForAddingLonLabel(lonCoordPointArrayList.get(0), zoomFactor, 
    				getEffectiveMinX(), getEffectiveMaxX(), getEffectiveMinY(), getEffectiveMaxY()); 
			
    		addLabelOnLonLine(wireframeShapeForLonLineArray[0], lonCoordPointArrayList.get(0), pointIndexForAddingLabel+1, //-1
    				String.valueOf(-180)); 

    		target.drawWireframeShape(wireframeShapeForLonLineArray[0], 
    				new RGB(255, 0, 0), 
    				1.0f, 
    				LineStyle.MEDIUM_DASHED, labelFont); 
			
			
			wireframeShapeForLonLineArray[0] = target.createWireframeShape(false, descriptor);
			latLonCoordArray = createCoordArrayForLonLine(imageDisplay, 179.4, latLonDrawingPointInterval); //180 won't draw
			lonCoordPointArrayList.add(latLonCoordArray); 
			wireframeShapeForLonLineArray[0].addLineSegment(latLonCoordArray); 
			wireframeShapeForLonLineArray[0].compile();
			
			wireframeShapeForLonLineArray[0].clearLabels();
			updateEffectiveMaxY(getMapMaxY(), getViewMinY(), getViewMaxY()); 
			pointIndexForAddingLabel = getPointLabelIndexForAddingLonLabel(lonCoordPointArrayList.get(0), zoomFactor, 
    				getEffectiveMinX(), getEffectiveMaxX(), getEffectiveMinY(), getEffectiveMaxY()); 
			
    		addLabelOnLonLine(wireframeShapeForLonLineArray[0], lonCoordPointArrayList.get(360/latLonInterval), pointIndexForAddingLabel+1, //-1
    				String.valueOf(180)); 

    		target.drawWireframeShape(wireframeShapeForLonLineArray[0], 
    				new RGB(255, 0, 0), 
    				1.0f, 
    				LineStyle.MEDIUM_DASHED, labelFont);
	    }
	    
	    private Coordinate[] createCoordArrayForLatLine(CylindCedDisplay imageDisplay, double latValue, double latLonPointInterval) throws VizException {
	
	    	int coordArrayLength = (int)(360 / latLonPointInterval) + 1; 
	    	
			Coordinate[] coordArray = new Coordinate[coordArrayLength]; 
			Coordinate[] coordPixelArray = new Coordinate[coordArrayLength]; 

			double lon = -180; 			
			for(int i=0; i<coordArray.length && lon <= 180; i++) {
				coordArray[i] = new Coordinate(lon, latValue); 
				
				double[] coord = new double[2];
				coord[0] = coordArray[i].x;
				coord[1] = coordArray[i].y;
				
				double[] pixel = new double[2];
				try {
					imageDisplay.getWorldToPixel().transform(coord, 0, pixel, 0, 1);
				} catch (TransformException e) {
					// TODO Auto-generated catch block
					e.printStackTrace();
				}
				
				coordPixelArray[i] = new Coordinate(pixel[0], pixel[1]);//latLon;
				
				lon += latLonPointInterval;  					
			}
			
			return coordPixelArray; 

		}
	    
	    private Coordinate[] createCoordArrayForLonLine(CylindCedDisplay imageDisplay,double lonValue, double latLonPointInterval) throws VizException {
	    	int coordArrayLength = (int)(180 / latLonPointInterval) ;//+1;
	    	
			Coordinate[] coordArray = new Coordinate[coordArrayLength];
			Coordinate[] coordPixelArray = new Coordinate[coordArrayLength];
			
			double lat = -90; //+ latLonPointInterval; 
			for(int i=0; i<coordArray.length && lat <= 90; i++) {
				coordArray[i] = new Coordinate(lonValue, lat); 
				
				double[] coord = new double[2];
				coord[0] = coordArray[i].x;
				coord[1] = coordArray[i].y;
				
				double[] pixel = new double[2];
				try {
					imageDisplay.getWorldToPixel().transform(coord, 0, pixel, 0, 1);
				} catch (TransformException e) {
					// TODO Auto-generated catch block
					e.printStackTrace();
				}
				
				coordPixelArray[i] = new Coordinate(pixel[0],pixel[1]);
				lat += latLonPointInterval; 				
			}			

			return coordPixelArray; 
		}

	    
	    private int getLatDrawingLineNumber(int latInterval) {
			int latLineNumber = 180/15;  // set a default value
			if(latInterval>0 && latInterval<=180) {
				latLineNumber = 180/latInterval; 
			}
			return latLineNumber + 1; 
		}
	    
	    private int getLonDrawingLineNumber(int lonInterval) {
			int lonLineNumber = 360/15;   // set a default value
			if(lonInterval>0 && lonInterval<=360) {
				lonLineNumber = 360 / lonInterval; 
				if(lonLineNumber > 360)
					lonLineNumber = 360;  // if we draw 360 lines, the last line will overlap with the first line
			}
			return lonLineNumber + 1; 
		}
	    
		private void addDefaultLabelByPointIndex(IWireframeShape wireframeShape, String labelValue, 
				Coordinate[] latLonCoordinateArray, int defaultPointIndex) throws TransformException {
			double[] tmp = { latLonCoordinateArray[defaultPointIndex].x, latLonCoordinateArray[defaultPointIndex].y};
			double[] screenPixel = descriptor.worldToPixel(tmp);//new double[2];
			
			if(screenPixel != null)
				wireframeShape.addLabel(labelValue, screenPixel); 	
		}

		private void addLabelOnLatLine( IWireframeShape wireframeShape, Coordinate[] coordinateArray, int coordiantePointIndex, String label) throws TransformException {
			 
//			 if (coordiantePointIndex < 175) {
			 
				 double[] tmp = { coordinateArray[coordiantePointIndex].x -20, coordinateArray[coordiantePointIndex].y};//move label down
				double[] screenPixel = descriptor.worldToPixel(tmp);//new double[2];
				//imageDisplay.getWorldToPixel().transform(tmp, 0, screenPixel, 0, 1);//descriptor.worldToPixel(tmp);
				if(screenPixel != null) {
					wireframeShape.clearLabels(); 
					wireframeShape.addLabel(label, screenPixel); 	
				}
//			}
	    }
		
		private void addLabelOnLonLine( IWireframeShape wireframeShape, Coordinate[] coordinateArray, int coordiantePointIndex, String label) throws TransformException {
			 
//			 if (coordiantePointIndex < 175) {
			 
				 double[] tmp = { coordinateArray[coordiantePointIndex].x, coordinateArray[coordiantePointIndex].y + 10};//move label down
				double[] screenPixel = descriptor.worldToPixel(tmp);//new double[2];
				//imageDisplay.getWorldToPixel().transform(tmp, 0, screenPixel, 0, 1);//descriptor.worldToPixel(tmp);
				if(screenPixel != null) {
					wireframeShape.clearLabels(); 
					wireframeShape.addLabel(label, screenPixel); 	
				}
//			}
	    }
		
		  private int getPointLabelIndexForAddingLatLabel(Coordinate[] latLonCoordinateArray, 
					float zoomFactor, double minX, double maxX, double minY, double maxY) throws TransformException {
		    	int pointIndex = -1; 
		    	if(latLonCoordinateArray == null || latLonCoordinateArray.length ==  0)
		    		return pointIndex; 
		    	
				double positionOffset = 0; //120 adjust label zooming appearance
		    	for(int i=0; i<latLonCoordinateArray.length; i++) {
					double[] tmp = { latLonCoordinateArray[i].x, latLonCoordinateArray[i].y};
					double[] screenPixel = descriptor.worldToPixel(tmp);//new double[2];
					//imageDisplay.getWorldToPixel().transform(tmp, 0, screenPixel, 0, 1);//descriptor.worldToPixel(tmp);
					if(isPointForPlacingLatituteLabel(screenPixel, positionOffset, zoomFactor, minX, maxX, minY, maxY)) {
						pointIndex = i; 
						break;  
					}
		    	}
		    	return pointIndex; 
		    }  
		  

		  private int getPointLabelIndexForAddingLonLabel(Coordinate[] latLonCoordinateArray, 
				float zoomFactor, double minX, double maxX, double minY, double maxY) throws TransformException {
		    	int pointIndex = -1; 
		    	if(latLonCoordinateArray == null || latLonCoordinateArray.length ==  0)
		    		return pointIndex; 
		    			    	
		    	double positionOffset = 0; 
		    	for(int i=0; i<latLonCoordinateArray.length; i++) {
					double[] tmp = { latLonCoordinateArray[i].x, latLonCoordinateArray[i].y};
					double[] screenPixel = descriptor.worldToPixel(tmp);// new double[2];
					//imageDisplay.getWorldToPixel().transform(tmp, 0, screenPixel, 0, 1);//descriptor.worldToPixel(tmp);
					if(isPointForPlacingLongituteLabel(screenPixel, positionOffset, zoomFactor, minX, maxX, minY, maxY)) {
						pointIndex = i; 					
						break;  	
					}
		    	}
		    	return pointIndex; 
	    }
		  	  
	    
		  private boolean isPointForPlacingLatituteLabel(double[] pixelValueArray, double positionOffset, float zoomFactor, 
	    		double minX, double maxX, double minY, double maxY) {
/* */				double delta = 200; 
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
		  

		    private boolean isPointForPlacingLongituteLabel(double[] pixelValueArray, double positionOffset, float zoomFactor, 
		    		double minX, double maxX, double minY, double maxY) {
				double delta = 20; 
				double adjustedOffset = positionOffset * zoomFactor; 
		    	boolean isPointForLabel = false; 
		    	if(pixelValueArray != null) {
		        	if(pixelValueArray[0] > (minX-adjustedOffset) && pixelValueArray[0] < maxX &&
		        			pixelValueArray[1] > minY && pixelValueArray[1] < maxY) {
		        		if(Math.abs(pixelValueArray[1] + maxY-adjustedOffset) < delta) {
		        			isPointForLabel = true; 
		        		}
		        	}
		    	}
		    	return isPointForLabel; 
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
			    
}
