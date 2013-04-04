package gov.noaa.nws.ncep.viz.overlays.resources;

import gov.noaa.nws.ncep.ui.pgen.display.DisplayElementFactory;
import gov.noaa.nws.ncep.ui.pgen.display.IDisplayable;
import gov.noaa.nws.ncep.ui.pgen.elements.SymbolLocationSet;
import gov.noaa.nws.ncep.viz.localization.NcPathManager;
import gov.noaa.nws.ncep.viz.localization.NcPathManager.NcPathConstants;
import gov.noaa.nws.ncep.viz.overlays.IPointOverlayResourceData.MarkerState;
import gov.noaa.nws.ncep.viz.overlays.IPointOverlayResourceData.MarkerTextSize;
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

import com.raytheon.uf.viz.core.IGraphicsTarget;
import com.raytheon.uf.viz.core.IGraphicsTarget.HorizontalAlignment;
import com.raytheon.uf.viz.core.drawables.IFont;
import com.raytheon.uf.viz.core.drawables.PaintProperties;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.map.MapDescriptor;
import com.raytheon.uf.viz.core.rsc.AbstractVizResource;
import com.raytheon.uf.viz.core.rsc.LoadProperties;
import com.vividsolutions.jts.geom.Coordinate;

/**
 * Reads in a "D2D"-native LPI resource
 * 
 * <pre>
 *   
 *    SOFTWARE HISTORY
 *   
 *    Date          Ticket#     Engineer    Description
 *    ------------	----------	-----------	--------------------------
 *    9/17/07                   randerso    Initial Creation.
 *    3/23/09       53          B. Hebbard  Split off separate version for NCEP
 *    3/25/09       #85         Greg Hull   markerState sets drawTicks/drawNames
 *    4/15/09       53B         B. Hebbard  Efficiency:  Generate symbols only when
 *                                          needed for attribute change
 *    4/23/09       53C         B. Hebbard  Efficiency:  Use new SymbolLocationSet;
 *                                          improve regen logic
 *    6/15/09       #115        Greg Hull   extend AbstractNatlCntrsResource for
 *                                          attribute processing. 
 *    7/31/2009					M. Gao		Integrate with TO 11
 *    12/1/2009                 ghull       to11d6 : Reference ncep data directory
 *    08/10/2010    273         ghull       get overlayData dir from 
 *    07/28/2011    450         ghull       NcPathManager
 *    04/23/2012    #744        sgurung     Display Marker Text based on user specified zoom level
 *    08/17/12      655         B. Hebbard  Added paintProps as parameter to IDisplayable draw
 *    10/19/2012    898         sgurung     Fix for fuzzy fonts
 *    
 * </pre>
 * 
 * @author randerso
 * 
 */
public class LPIResource extends AbstractVizResource<LPIResourceData, MapDescriptor> 
	implements INatlCntrsResource {

	private LPIResourceData lpiResourceData; 
	
	/** Whether the resource is ready to be drawn */
	private boolean ready = false;

	/** The list of points */
    private List<LPIPoint> points;

    /** The set of symbols with similar attributes across many locations */
    private SymbolLocationSet symbolSet = null;
    
    /** A flag indicating new symbols are needed next time we repaint with markers active */
    private boolean symbolSetRegenerationNeeded = false;

	private int maxLen = 0;

	//  Whether to draw marker symbol and draw ID at each point
	// These are set from the MarkerState enum in the resourceData
	private boolean drawTicks = true;
	private boolean drawNames = true;
        
	private int pixelSizeHint = 45;

	private class LPIPoint {
		public Coordinate latLon;

		public double[] pixel;

		public double dist;

		public String label;

		LPIPoint() {
			latLon = new Coordinate();
		}
	}

    protected LPIResource(LPIResourceData resourceData,
            LoadProperties loadProperties) {
        super(resourceData, loadProperties);
        lpiResourceData = resourceData;         
    	updateDrawFlagsFromMarkerState();
    }
    
    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.viz.core.rsc.AbstractVizResource#init(com.raytheon.uf
     * .viz.core.IGraphicsTarget)
     */
	public void initInternal( IGraphicsTarget target ) throws VizException {
		try {
			File file = new File( this.resourceData.getFilename() );
			if (!file.isAbsolute()) {
			    
				file = NcPathManager.getInstance().getStaticFile( 
						NcPathConstants.BASEMAPS_DIR+File.separator+this.resourceData.getFilename());
			}
			
			points = new ArrayList<LPIPoint>();
			BufferedReader in = new BufferedReader(new FileReader(file));

			String s = in.readLine();
			while (s != null) {
				LPIPoint p = readPoint(s);
				if (p != null)
					points.add(p);
				s = in.readLine();
			}
			in.close();

		} catch (FileNotFoundException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		} catch (IOException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}

		project(this.descriptor.getCRS());

        if (drawTicks)  //  If symbol set needed, may as well get it now...
        {
            generateSymbolSet();
		}
        else            //  ...otherwise defer until/unless needed later.
        {
            symbolSetRegenerationNeeded = true;
        }

		ready = true;
	}

    private void generateSymbolSet()
    {
    	if (points.size() == 0) {
    		symbolSet = null;
    	}
    	else {
    		//  SymbolLocationSet constructor requires a positive-length array of Coordinate
    		Coordinate[] locations = new Coordinate[points.size()];
    		int i = 0;
    		for (LPIPoint p : points) {
    			locations[i++] = p.latLon;
    		}

    		Color[] colors = new Color[] {new Color( lpiResourceData.getColor().red, 
    				                                 lpiResourceData.getColor().green, 
    				                                 lpiResourceData.getColor().blue)};
    		float lineWidth = lpiResourceData.getMarkerWidth();
    		double sizeScale = lpiResourceData.getMarkerSize() * 0.75;
    		Boolean clear = false;
    		String category = new String("Marker");
    		String type = lpiResourceData.getMarkerType().toString();

    		symbolSet = new SymbolLocationSet (
    				null,
    				colors,
    				lineWidth,
    				sizeScale,
    				clear,
    				locations,
    				category,
    				type);
    	}
    	
    	symbolSetRegenerationNeeded = false;
    }

	public LPIPoint readPoint(String s) throws IOException {

		Scanner in = new Scanner(s);

		LPIPoint p = this.new LPIPoint();

		if (!in.hasNextDouble())
			return null;
		p.latLon.y = in.nextDouble();
		if (!in.hasNextDouble())
			return null;
		p.latLon.x = in.nextDouble();

		if (!in.hasNextDouble())
			return null;
		p.dist = in.nextDouble();

		if (!in.hasNext())
			return null;
		p.label = in.findInLine("[^\\|]*").trim();
		if (p.label.length() > maxLen)
			maxLen = p.label.length();

		return p;
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see com.raytheon.viz.core.rsc.IVizResource#paint(com.raytheon.viz.core.IGraphicsTarget,
	 *      com.raytheon.viz.core.PixelExtent, double, float)
	 */
	public void paintInternal(IGraphicsTarget target, PaintProperties paintProps)
			throws VizException {
		
		if (ready) {

			int displayWidth = (int) (this.descriptor.getMapWidth() * paintProps
					.getZoomLevel());

			double metersPerPixel = displayWidth
					/ paintProps.getCanvasBounds().width;
			
//			D2DProperties props = (D2DProperties) paintProps
//					.getPerspectiveProps();

			double screenToWorldRatio = paintProps.getCanvasBounds().width
					/ paintProps.getView().getExtent().getWidth();

			//TODO:  Examine size computation.  Was hardcoded "9" in RTS version;
			//       upped to "12" by 'eyeballing' the results
//			IFont font = target.initializeFont("Monospace",
//					(float) (12 * getMarkerTextSize().getSoftwareSize() * props.getMagnification()), null);
			IFont font = target.initializeFont("Monospace",
					(float) (12 * lpiResourceData.getMarkerTextSize().getSoftwareSize()), null);
			font.setSmoothing(false);
			font.setScaleFont(false);

			Rectangle2D charSize = target.getStringBounds(font, "N");
			double charWidth = charSize.getWidth();
			double charHeight = charSize.getHeight();

			double displayHintSize = this.pixelSizeHint; 
			double minSepDist = (displayHintSize * (metersPerPixel / 1000.0)); 
			
			float markerTextAppearZoomLevel = lpiResourceData.getMarkerTextAppearanceZoomLevel() / 10.0f;
			markerTextAppearZoomLevel = Math.round(markerTextAppearZoomLevel * 100.0f) / 100.0f;
			
			float currZoomLevel = Math.round(paintProps.getZoomLevel() * 100.0f) / 100.0f;
			
			double offsetX = 0;
			double offsetY = 0;
			HorizontalAlignment align = HorizontalAlignment.CENTER;
			
			//TODO:  *Might* need to determine the size of the symbol instead of the size of a tick ("+")
			if (drawTicks) {
				offsetX = charWidth / 2.0 / screenToWorldRatio;
				offsetY = charHeight / screenToWorldRatio;
				align = HorizontalAlignment.LEFT;
				if (symbolSetRegenerationNeeded)
				{
					generateSymbolSet();
				}
				if (symbolSet != null)
				{
					DisplayElementFactory df = new DisplayElementFactory (target, this.descriptor);
					ArrayList<IDisplayable> elements = df.createDisplayElements(symbolSet, paintProps);
					for (IDisplayable each : elements)
					{
						each.draw(target, paintProps);
						each.dispose();
					}
				}
            }

			if (drawNames) {
				for (LPIPoint p : points) {
					if (p.pixel == null)
						continue;
					if ((paintProps.getView().isVisible(p.pixel))
							&& (p.dist >= minSepDist || markerTextAppearZoomLevel >= currZoomLevel)) {
						
							target.drawString(font, p.label, p.pixel[0] + offsetX,
									p.pixel[1] + offsetY, 0.0, 
									IGraphicsTarget.TextStyle.WORD_WRAP, lpiResourceData.getColor(),
									align, null);
					}
				}
			}

			font.dispose();
		}
	}

	@Override
	public void project(CoordinateReferenceSystem mapData) throws VizException {
		for (LPIPoint p : points) {
			p.pixel = this.descriptor.worldToPixel(new double[] {
					p.latLon.x, p.latLon.y });
		}
	}

	public void updateDrawFlagsFromMarkerState( ) {
		MarkerState markerState = lpiResourceData.getMarkerState();
		
		drawTicks = (markerState == MarkerState.MARKER_ONLY || 
				     markerState == MarkerState.MARKER_PLUS_TEXT);
		drawNames = (markerState == MarkerState.TEXT_ONLY   ||
				     markerState == MarkerState.MARKER_PLUS_TEXT);
	}

	/**
	 * @param markerTextSize the markerTextSize to set
	 */
	public void setMarkerTextSize(MarkerTextSize markerTextSize) {
		lpiResourceData.setMarkerTextSize(markerTextSize);
	}

    public void resourceAttrsModified() {
	    symbolSetRegenerationNeeded = true;
    	updateDrawFlagsFromMarkerState();
    }

	@Override
	protected void disposeInternal() {
	}
}
