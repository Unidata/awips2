package gov.noaa.nws.ncep.viz.overlays.resources;

import java.awt.Color;
import java.awt.geom.Rectangle2D;
import java.io.File;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.Iterator;

import org.opengis.referencing.crs.CoordinateReferenceSystem;

import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.IGraphicsTarget;
import com.raytheon.uf.viz.core.IGraphicsTarget.HorizontalAlignment;
import com.raytheon.uf.viz.core.IGraphicsTarget.TextStyle;
import com.raytheon.uf.viz.core.rsc.LoadProperties;
import com.raytheon.uf.viz.core.drawables.IFont;
import com.raytheon.uf.viz.core.drawables.PaintProperties;
import com.raytheon.uf.viz.core.map.MapDescriptor;
import com.raytheon.uf.viz.core.rsc.AbstractVizResource;

import com.raytheon.viz.pointdata.StaticPlotInfoPV;
import com.raytheon.viz.pointdata.StaticPlotInfoPV.SPIEntry;
import com.vividsolutions.jts.geom.Coordinate;

import gov.noaa.nws.ncep.viz.resources.INatlCntrsResource;
import gov.noaa.nws.ncep.viz.localization.NcPathManager;
import gov.noaa.nws.ncep.viz.localization.NcPathManager.NcPathConstants;
import gov.noaa.nws.ncep.viz.overlays.IPointOverlayResourceData.MarkerState;
import gov.noaa.nws.ncep.ui.pgen.display.DisplayElementFactory;
import gov.noaa.nws.ncep.ui.pgen.display.IDisplayable;
import gov.noaa.nws.ncep.ui.pgen.elements.SymbolLocationSet;

/**
 * Reads in a "D2D"-native SPI resource
 * 
 * <pre>
 *   
 *    SOFTWARE HISTORY
 *   
 *    Date          Ticket#     Engineer    Description
 *    ------------  ----------  ----------- --------------------------
 *    1/10/08       562         bphillip    Initial Creation.
 *    3/23/09       53          B. Hebbard  Split off separate version for NCEP
 *    3/25/09       #85         Greg Hull   markerState sets drawTicks/drawNames
 *    4/15/09       53B         B. Hebbard  Activate symbol drawing (like LPI)
 *    4/23/09       53C         B. Hebbard  Efficiency:  Use new SymbolLocationSet;
 *                                          improve regen logic
 *    6/15/09       #115        Greg Hull   extend AbstractNatlCntrsResource for
 *                                              attribute processing. 
 *    8/05/09                   M. Gao		integrate with TO 11
 *    8/07/09                   Greg Hull   rm commented out or unused code
 *    12/1/2009                 ghull       to11d6 : Reference ncep data directory
 *    07/28/2011    450         ghull       NcPathManager
 *    08/17/12      655         B. Hebbard  Added paintProps as parameter to IDisplayable draw
 *    10/19/2012    898         sgurung     Fix for fuzzy fonts
 *    
 * </pre>
 * 
 * @author bphillip
 * 
 */
public class SPIResource extends AbstractVizResource<SPIResourceData, MapDescriptor>  
			implements INatlCntrsResource  {  

    SPIResourceData spiResourceData; 

	/** Whether the resource is ready to be drawn */
	private boolean ready = false;

	private int maxLen = 0;

	HashMap<String, SPIEntry> entries;
    
    /** The set of symbols with similar attributes across many locations */
    private SymbolLocationSet symbolSet = null;
    
    /** A flag indicating new symbols are needed next time we repaint with markers active */
    private boolean symbolSetRegenerationNeeded = false;

	/** Whether to draw marker symbol at each point */
	private boolean drawTicks = true;

	/** Whether to draw ID text at each point */
	private boolean drawNames = true;
        
	private int pixelSizeHint = 45;

	public int getPixelSizeHint() {
		return pixelSizeHint;
	}

	public void setPixelSizeHint(int pixelSizeHint) {
		this.pixelSizeHint = pixelSizeHint;
	}

    public SPIResource(SPIResourceData resourceData, LoadProperties props) {
        super(resourceData, props);
        spiResourceData = resourceData; 
    }

	@Override
	public void initInternal(IGraphicsTarget target) throws VizException {
        File file = new File(this.resourceData.getFilename());
        if (!file.isAbsolute()) {
			file = NcPathManager.getInstance().getStaticFile( 
					NcPathConstants.BASEMAPS_DIR+File.separator+this.resourceData.getFilename());

        }

		entries = StaticPlotInfoPV.readStaticPlotInfoPV(
				file.getAbsolutePath(), true).getSpiList();
		String key = null;
		for (Iterator<String> iterator = entries.keySet().iterator(); iterator
				.hasNext();) {
			key = iterator.next();
			if (key.length() > maxLen) {
				maxLen = key.length();
			}

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
		if (entries.keySet().size() == 0)
		{
			symbolSet = null;
		}
		else
		{
    		//  SymbolLocationSet constructor requires a positive-length array of Coordinate
			Coordinate[] locations = new Coordinate[entries.keySet().size()];
			int i = 0;
			for (String key : entries.keySet())
			{
				locations[i++] = entries.get(key).latlon;
			}

			Color[] colors = new Color[] {new Color(spiResourceData.getColor().red, 
					spiResourceData.getColor().green, spiResourceData.getColor().blue)};
			float lineWidth = spiResourceData.getMarkerWidth(); //markerWidth;
			double sizeScale = spiResourceData.getMarkerSize() * 0.75;
			Boolean clear = false;
    		String category = new String("Marker");
			String type = spiResourceData.getMarkerType().toString();

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

	@Override
	public void paintInternal(IGraphicsTarget target, PaintProperties paintProps)
			throws VizException {

		if (ready) {

	        int displayWidth = (int) (this.descriptor.getMapWidth() * paintProps
					.getZoomLevel());

			double metersPerPixel = displayWidth
					/ paintProps.getCanvasBounds().width;

			double screenToWorldRatio = paintProps.getCanvasBounds().width
					/ paintProps.getView().getExtent().getWidth();

			//TODO:  Examine size computation.  Was hardcoded "9" in RTS version;
			//       upped to "12" by 'eyeballing' the results
//			IFont font = target.initializeFont("Monospace",
//					(float) (12 * markerTextSize.getSoftwareSize() * mapDescriptor.getMagnification()), null);
			IFont font = target.initializeFont("Monospace",
					(float) (12 * spiResourceData.getMarkerTextSize().getSoftwareSize()), null);
			font.setSmoothing(false);
			font.setScaleFont(false);

			Rectangle2D charSize = target.getStringBounds(font, "N");
			double charWidth = charSize.getWidth();
			double charHeight = charSize.getHeight();

//			double displayHintSize = this.pixelSizeHint
//					* mapDescriptor.getMagnification();
//			double minSepDist = (displayHintSize * (metersPerPixel / 1000.0))
//					/ mapDescriptor.getDensity();
			double displayHintSize = this.pixelSizeHint; 
			double minSepDist = (displayHintSize * (metersPerPixel / 1000.0)); 

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

//			if (drawNames && (mapDescriptor.getMagnification() > 0.0)) {
			if (drawNames) {
				for (Iterator<String> iterator = entries.keySet().iterator(); iterator
				.hasNext();) {
					String key = iterator.next();
					SPIEntry entry = entries.get(key);				
					if (paintProps.getView().isVisible(entry.pixel)
							&& (entry.distance >= minSepDist)) {
						target.drawString(font, key, entry.pixel[0] + offsetX,
								entry.pixel[1] + offsetY, 0.0, TextStyle.NORMAL,
								spiResourceData.getColor(), align, null);
					}
				}
			}

			font.dispose();
		}
	}

	@Override
	public void project(CoordinateReferenceSystem mapData) throws VizException {

		SPIEntry entry = null;
		for (Iterator<String> iterator = entries.keySet().iterator(); iterator
				.hasNext();) {
			entry = entries.get(iterator.next());
			entry.pixel = this.descriptor.worldToPixel(new double[] {
					entry.latlon.x, entry.latlon.y });

		}

	}

	@Override
	public void disposeInternal() {
	}

	public void updateDrawFlagsFromMarkerState( ) {
		MarkerState markerState = spiResourceData.getMarkerState();
		
		drawTicks = (markerState == MarkerState.MARKER_ONLY || 
				     markerState == MarkerState.MARKER_PLUS_TEXT);
		drawNames = (markerState == MarkerState.TEXT_ONLY   ||
				     markerState == MarkerState.MARKER_PLUS_TEXT);
	}

    public void resourceAttrsModified() {
	    symbolSetRegenerationNeeded = true;
    	updateDrawFlagsFromMarkerState();
    }
}
