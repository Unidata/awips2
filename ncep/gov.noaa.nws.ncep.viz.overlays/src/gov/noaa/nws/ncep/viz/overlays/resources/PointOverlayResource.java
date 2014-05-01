package gov.noaa.nws.ncep.viz.overlays.resources;

import gov.noaa.nws.ncep.ui.pgen.display.DisplayElementFactory;
import gov.noaa.nws.ncep.ui.pgen.display.IDisplayable;
import gov.noaa.nws.ncep.ui.pgen.elements.SymbolLocationSet;
import gov.noaa.nws.ncep.viz.common.staticPointDataSource.IStaticPointDataSource;
import gov.noaa.nws.ncep.viz.common.staticPointDataSource.LabeledPoint;
import gov.noaa.nws.ncep.viz.common.staticPointDataSource.StaticPointDataSourceMngr;
import gov.noaa.nws.ncep.viz.common.staticPointDataSource.IStaticPointDataSource.StaticPointDataSourceType;
import gov.noaa.nws.ncep.viz.common.ui.Markers.MarkerState;
import gov.noaa.nws.ncep.viz.common.ui.Markers.MarkerTextSize;
import gov.noaa.nws.ncep.viz.localization.NcPathManager;
import gov.noaa.nws.ncep.viz.localization.NcPathManager.NcPathConstants;
import gov.noaa.nws.ncep.viz.resources.INatlCntrsResource;

import java.awt.Color;
import java.awt.geom.Rectangle2D;
import java.io.BufferedReader;
import java.io.File;
import java.io.FileNotFoundException;
import java.io.FileReader;
import java.io.IOException;
import java.util.ArrayList;
import java.util.List;
import java.util.Scanner;

import org.opengis.referencing.crs.CoordinateReferenceSystem;

import com.raytheon.uf.viz.core.DrawableString;
import com.raytheon.uf.viz.core.IExtent;
import com.raytheon.uf.viz.core.IGraphicsTarget;
import com.raytheon.uf.viz.core.IGraphicsTarget.HorizontalAlignment;
import com.raytheon.uf.viz.core.IGraphicsTarget.TextStyle;
import com.raytheon.uf.viz.core.IGraphicsTarget.VerticalAlignment;
import com.raytheon.uf.viz.core.PixelExtent;
import com.raytheon.uf.viz.core.drawables.IFont;
import com.raytheon.uf.viz.core.drawables.PaintProperties;
import com.raytheon.uf.viz.core.drawables.ResourcePair;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.map.MapDescriptor;
import com.raytheon.uf.viz.core.rsc.AbstractVizResource;
import com.raytheon.uf.viz.core.rsc.LoadProperties;
import com.raytheon.uf.viz.core.rsc.ResourceProperties;
import com.vividsolutions.jts.geom.Coordinate;

/**
 *  
 * 
 * <pre>
 *   
 *    SOFTWARE HISTORY
 *   
 *    Date          Ticket#     Engineer    Description
 * ----------- ---------- ----------- --------------------------
 * 07/03/13     #1010     ghull       Initial creation
 *    
 * </pre>
 * 
 * @author randerso
 * 
 */
public class PointOverlayResource extends AbstractVizResource<PointOverlayResourceData, MapDescriptor> 
	implements INatlCntrsResource {

	private PointOverlayResourceData ptOvrlyRscData; 
	
    private List<LabeledPoint> labeledPoints;

    /** The set of symbols with similar attributes across many locations */
    private SymbolLocationSet symbolSet = null;
    
    /** A flag indicating new symbols are needed next time we repaint with markers active */
    private boolean symbolSetRegenerationNeeded = true;

	//  Whether to draw marker symbol and draw ID at each point
	// These are set from the MarkerState enum in the resourceData
	private boolean drawSymbols = true;
	private boolean drawLabels = true;
        
//	private int pixelSizeHint = 45;
	private IFont font=null;
	double charWidth;
	double charHeight;

	private List<DrawableString> labelStrings = null;
	private ArrayList<IDisplayable> symDispElmtsList = null;
	private IExtent prevExtent = null;
	private Integer numVisPoints = 0;
	

    protected PointOverlayResource( PointOverlayResourceData resourceData,
            LoadProperties loadProperties) {
        super(resourceData, loadProperties);
        ptOvrlyRscData = resourceData;         
    	updateDrawFlagsFromMarkerState();
    }
    
    @Override
    public String getName() {
    	//     	
    	String numPoints = null;
    	
//    	if( drawSymbols && 
//    		symDispElmtsList != null && !symDispElmtsList.isEmpty() ) {
//    		numPoints = Integer.toString( symDispElmtsList.size() );
//    	}
//    	if( numPoints == null && drawLabels &&
//    		labelStrings != null && !labelStrings.isEmpty() ) { 
//    		numPoints = Integer.toString( labelStrings.size() );
//    	}
    	// 
    	if( numVisPoints == 0 ) {
    		if( labelStrings == null || labelStrings.isEmpty() ) {
        		numPoints = "No Points";    			
    		}
    		else {
    			numPoints = Integer.toString( labelStrings.size() );
    		}
    	}
    	else {
    		numPoints = Integer.toString( numVisPoints );
    	}
    	
    	String mapName = ptOvrlyRscData.getMapName(); 
//    	if( mapName == null || mapName.isEmpty() ) {
//    		return ptOvrlyRscData.getFilename() + " ("+numPoints+" points)";
//    	}
//    	else {
    		return mapName + " ("+numPoints+")";    		
//    	}
    }
    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.viz.core.rsc.AbstractVizResource#init(com.raytheon.uf
     * .viz.core.IGraphicsTarget)
     */
	public void initInternal( IGraphicsTarget target ) throws VizException {
//		List<String> initParams = new ArrayList<String>();
		
		IStaticPointDataSource ptSrc = StaticPointDataSourceMngr.createPointDataSource( 
				ptOvrlyRscData.getSourceType(), ptOvrlyRscData.getSourceName(), 
				ptOvrlyRscData.getSourceParams() );	
		ptSrc.loadData();

		labeledPoints = ptSrc.getPointData();
		
		// set the pixel values from the lat/lons
		project( this.descriptor.getCRS() );
	}


	/*
	 * (non-Javadoc)
	 * 
	 * @see com.raytheon.viz.core.rsc.IVizResource#paint(com.raytheon.viz.core.IGraphicsTarget,
	 *      com.raytheon.viz.core.PixelExtent, double, float)
	 */
	public void paintInternal(IGraphicsTarget target, PaintProperties paintProps)
			throws VizException {
       
		// This could be implemented in ProgressiveDisclosureProperties(in ResourceProperties) 
		// but it is an attribute. 
		int displayWidth = (int) (descriptor.getMapWidth() * paintProps.getZoomLevel() / 1000);
		
		if( displayWidth > ptOvrlyRscData.getMaxSymbolDisplayWidth() &&
			displayWidth > ptOvrlyRscData.getMaxLabelDisplayWidth() ) {
			return;
		}
//
//		AbstractVizResource<?, ?> resource = rp.getResource();
//		ResourceProperties properties = rp.getProperties();
//
//           if (properties.isDisplayable(displayWidth)) {
//                PaintProperties newProps = new PaintProperties(paintProps);

		// if the first time painting or if
		//    the extents have changed and we are not zooming (ie wait till last zoom to 
		//  determine the new labels/symbols
		//
		if( prevExtent == null ||
			(!paintProps.isZooming() &&
			 !(Math.abs( prevExtent.getMaxX() - paintProps.getView().getExtent().getMaxX() ) < .01 &&
			   Math.abs( prevExtent.getMaxY() - paintProps.getView().getExtent().getMaxY() ) < .01 &&
			   Math.abs( prevExtent.getMinX() - paintProps.getView().getExtent().getMinX() ) < .01 &&
			   Math.abs( prevExtent.getMinY() - paintProps.getView().getExtent().getMinY() ) < .01 ) ) ) {
			
			symbolSetRegenerationNeeded = true;
			 
			disposeSymbolElements();
			disposeLabelStrings();
		}
				
		prevExtent = paintProps.getView().getExtent().clone();
		
		double screenToWorldRatio = paintProps.getCanvasBounds().width
			/ paintProps.getView().getExtent().getWidth();

		if( font == null ) {
			font = target.initializeFont("Monospace",
					(float) (12 * ptOvrlyRscData.getMarkerTextSize().getSoftwareSize()), null);
			font.setSmoothing(false);
			font.setScaleFont(false);
			Rectangle2D charSize = target.getStringBounds(font, "N");
			charWidth = charSize.getWidth();
			charHeight = charSize.getHeight();
		}

		if( drawSymbols) {
			if( symbolSetRegenerationNeeded  ) {

				symbolSetRegenerationNeeded = false;
		    	
				disposeSymbolElements();

	    		symbolSet = null;		       
	    		numVisPoints = 0;
	    		
		    	//  SymbolLocationSet constructor requires a positive-length array of Coordinate
		    	List<Coordinate> visibleLocs = new ArrayList<Coordinate>();
		    	
		    	for( LabeledPoint lp : labeledPoints ) {
		    		double[] latlon = new double[] { lp.getLongitude(), lp.getLatitude() }; 
		    		double[] pix = this.descriptor.worldToPixel( latlon );
		    		if( pix == null ) { // there is at least one bad point at the south pole
		    			continue;
		    		}
		    		if( paintProps.getView().isVisible( pix ) &&
		    			displayWidth <= ptOvrlyRscData.getMaxSymbolDisplayWidth() ) {
		    			
		    			visibleLocs.add( new Coordinate( latlon[0], latlon[1]) );
		    		}
		    	}
		    	if( !visibleLocs.isEmpty() ) {
		    		numVisPoints = visibleLocs.size();
		    		Coordinate[] locations = visibleLocs.toArray( new Coordinate[0] );

		    		Color[] colors = new Color[] {new Color( ptOvrlyRscData.getColor().red, 
		    				ptOvrlyRscData.getColor().green, 
		    				ptOvrlyRscData.getColor().blue)};

		    		symbolSet = new SymbolLocationSet (
		    				null,
		    				colors,
		    				ptOvrlyRscData.getMarkerWidth(),
		    				ptOvrlyRscData.getMarkerSize() * 0.75,  // sizeScale
		    				false,  // clear
		    				locations,
		    				"Marker", // category
		    				ptOvrlyRscData.getMarkerType().toString() ); 		        	
		    	}
			}
			
			if( symbolSet != null ) {
				if( symDispElmtsList == null ) {				
					DisplayElementFactory df = new DisplayElementFactory (target, this.descriptor);
					symDispElmtsList = df.createDisplayElements(symbolSet, paintProps);
				}
				
				if( symDispElmtsList != null ) {
					for( IDisplayable symElmt : symDispElmtsList ) {
						symElmt.draw(target, paintProps);
					}
				}
			}
		}

		if( drawLabels ) {
			if( labelStrings == null || labelStrings.isEmpty() ) {
				double offsetX = 0.0;//charWidth / 2.0 / screenToWorldRatio;
				double offsetY = 0.0;//charHeight / screenToWorldRatio;
				
				if( drawSymbols ) {
					offsetY =  charHeight / screenToWorldRatio;
				}
				
				labelStrings = new ArrayList<DrawableString>();
				
				for( LabeledPoint lp : labeledPoints ) {
		    		double[] latlon = new double[] { lp.getLongitude(), lp.getLatitude() }; 
		    		double[] pix = this.descriptor.worldToPixel( latlon );
		    		if( pix == null ) {
		    			continue;
		    		}
					if( paintProps.getView().isVisible( pix ) &&
						displayWidth <= ptOvrlyRscData.getMaxLabelDisplayWidth()   ) {

						DrawableString drawStr = new DrawableString( lp.getName(),
								ptOvrlyRscData.getColor() );
						drawStr.font = font;
						drawStr.setCoordinates(
								pix[0]+ offsetX, pix[1]+ offsetY );
						drawStr.horizontalAlignment = HorizontalAlignment.CENTER;
						drawStr.verticallAlignment = VerticalAlignment.MIDDLE;
						// WORD WRAP looks nicer but the drawing doesn't seem to be 
						// horizontally centering
						// the wrapped labels (those with spaces) correctly. So if there
						// are no symbols I think its better to have the text centered over
						// the true location.
//						drawStr.textStyle = (drawSymbols ? TextStyle.WORD_WRAP : TextStyle.NORMAL );
						drawStr.textStyle = TextStyle.NORMAL;
						labelStrings.add(drawStr);
					}
				}
			}
			
			if( labelStrings != null ) {
				target.drawStrings( labelStrings );
			}
		}
	}

//    private void generateSymbolSet() {
//    	if( !drawSymbols || lpiPoints.size() == 0 ) {
//    		symbolSet = null;
//        	symbolSetRegenerationNeeded = false;
//    	}
//
//    	//  SymbolLocationSet constructor requires a positive-length array of Coordinate
//    	Coordinate[] locations = new Coordinate[lpiPoints.size()];
//    	int i = 0;
//    	for( LPIPoint p : lpiPoints ) {
//    		locations[i++] = p.latLon;
//    	}
//
//    	Color[] colors = new Color[] {new Color( lpiResourceData.getColor().red, 
//    			lpiResourceData.getColor().green, 
//    			lpiResourceData.getColor().blue)};
//    	float lineWidth = lpiResourceData.getMarkerWidth();
//    	double sizeScale = lpiResourceData.getMarkerSize() * 0.75;
//    	Boolean clear = false;
//    	String category = new String("Marker");
//    	String type = lpiResourceData.getMarkerType().toString();
//
//    	symbolSet = new SymbolLocationSet (
//    			null,
//    			colors,
//    			lineWidth,
//    			sizeScale,
//    			clear,
//    			locations,
//    			category,
//    			type);
//        	
//    	symbolSetRegenerationNeeded = false;
//    }


	@Override
	public void project(CoordinateReferenceSystem mapData) throws VizException {
//		for( LabeledPoint lp : labeledPoints ) {
//			lp.pixel = this.descriptor.worldToPixel(new double[] {
//					lp.latLon.x, lp.latLon.y });
//		}
	}

	public void updateDrawFlagsFromMarkerState( ) {
		MarkerState markerState = ptOvrlyRscData.getMarkerState();
		
		drawSymbols = (markerState == MarkerState.MARKER_ONLY || 
				     markerState == MarkerState.MARKER_PLUS_TEXT);
		drawLabels = (markerState == MarkerState.TEXT_ONLY   ||
				     markerState == MarkerState.MARKER_PLUS_TEXT);
	}

	/**
	 * @param markerTextSize the markerTextSize to set
	 */
	public void setMarkerTextSize(MarkerTextSize markerTextSize) {
		ptOvrlyRscData.setMarkerTextSize(markerTextSize);
	}

    public void resourceAttrsModified() {
    	
    	ResourceProperties rprop = getProperties();
    	rprop.getPdProps(); 
    	
	    symbolSetRegenerationNeeded = true;
    	updateDrawFlagsFromMarkerState();
    	
    	disposeSymbolElements();
    	disposeLabelStrings();
    	
    	if( font != null ) {
    		font.dispose();
    		font = null;
    	}
    }

	@Override
	protected void disposeInternal() {

    	if( font != null ) {
    		font.dispose();
    		font = null;
    	}
    	
    	disposeSymbolElements();
	}
	
	private void disposeLabelStrings() {
		if( labelStrings != null ) {
			labelStrings.clear();
		}
	}
	
	private void disposeSymbolElements() {
		if( symDispElmtsList != null ) {
			for( IDisplayable e : symDispElmtsList ) {
	 			e.dispose();
			}
			symDispElmtsList = null;
		}
	}
}
