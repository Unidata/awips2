package gov.noaa.nws.ncep.viz.rsc.mosaic.rsc;

import gov.noaa.nws.ncep.edex.plugin.mosaic.common.MosaicRecord;
import gov.noaa.nws.ncep.edex.plugin.mosaic.uengine.MosaicTiler;
import gov.noaa.nws.ncep.viz.common.ColorMapUtil;
import gov.noaa.nws.ncep.viz.common.ui.NmapCommon;
import gov.noaa.nws.ncep.viz.resources.AbstractNatlCntrsResource;
import gov.noaa.nws.ncep.viz.resources.colorBar.ColorBarResource;
import gov.noaa.nws.ncep.viz.resources.colorBar.ColorBarResourceData;
import gov.noaa.nws.ncep.viz.rsc.mosaic.Activator;
import gov.noaa.nws.ncep.viz.ui.display.ColorBarFromColormap;

import java.io.File;
import java.io.FileNotFoundException;
import java.text.ParseException;
import java.text.ParsePosition;
import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import javax.measure.converter.MultiplyConverter;
import javax.measure.converter.UnitConverter;
import javax.measure.unit.Unit;
import javax.measure.unit.UnitFormat;

import org.eclipse.swt.graphics.RGB;
import org.opengis.referencing.crs.CoordinateReferenceSystem;

import com.raytheon.uf.common.colormap.ColorMap;
import com.raytheon.uf.common.dataplugin.PluginDataObject;
import com.raytheon.uf.common.datastorage.DataStoreFactory;
import com.raytheon.uf.common.datastorage.IDataStore;
import com.raytheon.uf.common.datastorage.StorageException;
import com.raytheon.uf.common.geospatial.ReferencedCoordinate;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.common.time.DataTime;
import com.raytheon.uf.viz.core.HDF5Util;
import com.raytheon.uf.viz.core.IGraphicsTarget;
import com.raytheon.uf.viz.core.drawables.ColorMapParameters;
import com.raytheon.uf.viz.core.drawables.PaintProperties;
import com.raytheon.uf.viz.core.drawables.ResourcePair;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.map.MapDescriptor;
import com.raytheon.uf.viz.core.rsc.GenericResourceData;
import com.raytheon.uf.viz.core.rsc.IResourceDataChanged;
import com.raytheon.uf.viz.core.rsc.LoadProperties;
import com.raytheon.uf.viz.core.rsc.ResourceProperties;
import com.raytheon.uf.viz.core.rsc.IResourceDataChanged.ChangeType;
import com.raytheon.uf.viz.core.rsc.capabilities.ColorMapCapability;
import com.raytheon.uf.viz.core.rsc.capabilities.ColorableCapability;
import com.raytheon.uf.viz.core.rsc.capabilities.ImagingCapability;
import com.raytheon.uf.viz.core.status.StatusConstants;
import com.raytheon.uf.viz.core.style.DataMappingPreferences;
import com.raytheon.uf.viz.core.style.DataMappingPreferences.DataMappingEntry;
import com.raytheon.viz.core.rsc.hdf5.AbstractTileSet;
import com.raytheon.viz.core.style.image.ImagePreferences;
import com.raytheon.viz.core.style.image.SamplePreferences;
import com.raytheon.viz.core.units.PiecewisePixel;
import com.vividsolutions.jts.geom.Coordinate;

/**
 * Provide Radar Mosaic raster rendering support 
 * 
 * <pre>
 * 
 *  SOFTWARE HISTORY
 * 
 *  Date         Ticket#     Engineer     Description
 *  ------------ ----------  -----------  --------------------------
 *  01/2010	  	   204 	 	  M. Li       Initial Creation.
 *  03/2010                   B. Hebbard  Port TO11D6->TO11DR3; add localization
 *  04/2010        259        Greg Hull   Added Colorbar
 *  09/2010        307        Greg Hull   move getName to resourceData and base on 
 *                                        the productCode in the metadataMap 
 *  07/11/11                  Greg Hull   ColorBarResource 
 *  06-07-2012     717         Archana	  Updated setColorMapParameters() to store label information
 *                                        for the colorbar                                     
 *  06/21/2012     #825	      Greg Hull   rm mosaicInfo.txt; get legend info from the Record.
 *  07/18/12       717        Archana     Refactored a field used to align the label data      
 * 12/19/2012     #960        Greg Hull   override propertiesChanged() to update colorBar.
 *        
 * </pre>
 * 
 * @author mli
 * @version 1
 */

public class MosaicResource extends AbstractNatlCntrsResource<MosaicResourceData, MapDescriptor> implements
	IResourceDataChanged {

    private static final IUFStatusHandler statusHandler = UFStatus.getHandler(MosaicResource.class, "Mosaic");
    
	MosaicResourceData radarRscData;
	
	protected String baseFileName;

	protected IGraphicsTarget grphTarget;
	
	protected int numLevels;

    protected String viewType;

	/** The line color */
	private static final RGB DEFAULT_COLOR = new RGB(255, 255, 255);

	protected RadarTileSet baseTile;
	
    private ColorMapParameters colorMapParameters = null;
    
    protected ColorBarResource cbarResource;
    protected ResourcePair     cbarRscPair;


	protected class FrameData extends AbstractFrameData {

		RadarTileSet tileSet;
		DataTime     tileTime; // the time of the data used to create the tileset
		                       // used to determine if we need to replace the tile 
		                       // with a better timematch. 
		public int    prodCode;
		public String prodName;
		public String unitName;
		public int    numLevels;
		public String legendStr="";

		
		protected FrameData(DataTime time, int interval) {
			super(time, interval);	
			tileTime = null;
		}

		public boolean updateFrameData( IRscDataObject rscDataObj ) {
        	PluginDataObject pdo = ((DfltRecordRscDataObj)rscDataObj).getPDO();
			MosaicRecord radarRecord = (MosaicRecord) pdo;
			
			prodCode = radarRecord.getProductCode();
			prodName = radarRecord.getProdName();
			numLevels = radarRecord.getNumLevels();
			unitName  = radarRecord.getUnit();
			legendStr = createLegend( prodCode, prodName, numLevels, unitName );
			
			synchronized (this) {
				try {
					if (!(pdo instanceof MosaicRecord)) {
						statusHandler.handle(Priority.PROBLEM,""
								+ this.getClass().getName() + " expected : "
								+ MosaicRecord.class.getName() + " Got: " + pdo);
						return false;
					}

					if( radarRscData.getProductCode().intValue() != prodCode ) {
						System.out.println("??? radar product code in data Record doesn't match"+
								" the requested product code???");
						radarRscData.setProductCode( radarRecord.getProductCode() );
					}
						
					if (baseTile == null) {
						try {
							setColorMapParameters(radarRecord);
						} catch (FileNotFoundException e) {
							e.printStackTrace();
						} catch (StorageException e) {
							e.printStackTrace();
						} catch (VizException e) {
							e.printStackTrace();
						}
					}

					
					if( baseTile == null ) {
						tileSet = baseTile = createTile(radarRecord, false);
						tileTime = radarRecord.getDataTime();
					}
					else { // if the tileset is already set, and the new record is 
						   // is not a better match then return					
						if( tileSet != null && tileTime != null ) {
							if( timeMatch(radarRecord.getDataTime()) >= timeMatch(tileTime) ) {
								return false;
							}
							else {  // if this is a better match, we need to create a new tile.
								if( tileSet == baseTile ) {
									tileSet = null;
								}
								else {
									tileSet.dispose();
									tileSet = null;								
								}								
							}
						}

						tileSet = createTile(radarRecord, true);
						tileTime = radarRecord.getDataTime();
					}
					
					if( grphTarget != null ) {
						tileSet.init(grphTarget);
					}	
					
					Collections.sort(MosaicResource.this.dataTimes);
				}
				catch( VizException e ) {
					System.out.println("Error processing MosaicRecord. " + e.getMessage() );
					return false;
				}
			}
			return true;
		}
		
		public void dispose() {
			if( tileSet != baseTile && tileSet != null ) {
				tileSet.dispose();
				tileSet = null;
			}
		}
	}


	public MosaicResource(MosaicResourceData rrd, LoadProperties loadProps)
	throws VizException {
		super(rrd, loadProps);
		rrd.addChangeListener(this);

		this.radarRscData = rrd;
		
		grphTarget = null;
		this.dataTimes = new ArrayList<DataTime>();
//		this.productCode = 0;

		if (this.getCapability(ColorableCapability.class).getColor() == null) {
			this.getCapability(ColorableCapability.class).setColor(
					DEFAULT_COLOR);
		}
	}

	
	@Override
	protected AbstractFrameData createNewFrame( DataTime frameTime, int frameInterval) {
		return new FrameData( frameTime, frameInterval );
	}
	
	private String createLegend( int prodCode,  String prodName, 
						         int numLevels, String unitName) {
		if( unitName == null ) {
			unitName = " ";
		} else {
			if (!unitName.contains("(")) {
				String temp = " (";
				temp += unitName + ") ";
				unitName = temp;
			}
			if (unitName.contains("/10")) {
				unitName = unitName.replace("/10", "");
			}
			if (unitName.contains("*1000")) {
				unitName = unitName.replace("*1000", "");
			}
		}

		String legendString = new String( prodName + unitName
					+ (int) (Math.log( numLevels) / Math.log(2)) + "-bit " );
		return legendString;		
	}
	
	@Override
	public String getName() {

		FrameData fd = (FrameData) getCurrentFrame();

		if( fd == null ) {
			return "Natl Mosaic-No Data";
		}
		else if( fd.tileTime == null || 
				 fd.tileSet.getMapDescriptor().getFrameCount() == 0) {
		
			return fd.legendStr + "-No Data";
		}
		else {
			return fd.legendStr + NmapCommon.getTimeStringFromDataTime( fd.tileTime, "/");
		}
	}
	
	/*
	 * (non-Javadoc)
	 * 
	 * @see com.raytheon.viz.core.rsc.IVizResource#dispose()
	 */
	@Override
	public void disposeInternal() {
		super.disposeInternal();

		getDescriptor().getResourceList().remove( cbarRscPair );
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @seecom.raytheon.viz.core.rsc.IVizResource#init(com.raytheon.viz.core.
	 * IGraphicsTarget)
	 */
	public void initResource(IGraphicsTarget target) throws VizException {
		synchronized (this) {
			this.viewType = target.getViewType();
            this.grphTarget = target;
 
            // create the colorBar Resource and add it to the resourceList for this descriptor.
            cbarRscPair  = ResourcePair.constructSystemResourcePair( 
 		           new ColorBarResourceData( radarRscData.getColorBar() ) );
 
            getDescriptor().getResourceList().add(  cbarRscPair );
            getDescriptor().getResourceList().instantiateResources( getDescriptor(), true );

            cbarResource = (ColorBarResource) cbarRscPair.getResource();
//            cbarResource.setColorBar( radarRscData.getColorBar() );
            getCapability(ImagingCapability.class).setSuppressingMenuItems(true);
            getCapability(ColorMapCapability.class).setSuppressingMenuItems(true); 
            getCapability(ColorableCapability.class).setSuppressingMenuItems(true);
			queryRecords();

			if (this.baseTile != null) {
                this.baseTile.init(target);
            }
			
        	for( AbstractFrameData frm : frameDataMap.values() ) {
        		AbstractTileSet ts = ((FrameData)frm).tileSet; 
        		if( ts != null )
        			ts.init(target);
            }
        	
		}	
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @seecom.raytheon.viz.core.rsc.IVizResource#paint(com.raytheon.viz.core.
	 * IGraphicsTarget, com.raytheon.viz.core.PixelExtent, double, float)
	 */
	//@Override
	public void paintFrame( AbstractFrameData frmData, IGraphicsTarget target, PaintProperties paintProps)
    throws VizException {
		if (paintProps == null || paintProps.getDataTime() == null) {
			return;
		}

		FrameData currFrame = (FrameData) frmData;
		RadarTileSet tileSet = currFrame.tileSet;

		try {
			if (tileSet != null) {
	        	ImagingCapability imgCap = new ImagingCapability();
	        	imgCap.setBrightness(radarRscData.getBrightness());
	        	imgCap.setContrast(radarRscData.getContrast());
	        	imgCap.setAlpha(radarRscData.getAlpha());
	        	paintProps.setAlpha(radarRscData.getAlpha());
	        	radarRscData.fireChangeListeners(ChangeType.CAPABILITY, imgCap);				
				colorMapParameters = getCapability(ColorMapCapability.class)
				            .getColorMapParameters();
				//TODO:  Suggest making the following more resilient to errors -- handle case where
				//       "params" is null, which does occur if colormap file was not found.
				if (colorMapParameters.getColorMap() == null) {
					throw new VizException("ColorMap not specified");
				}

				tileSet.paint(target, paintProps);
			} 
			
		} catch (Exception e) {
			String msg = e.getMessage();
			if (msg == null) {
				msg = "Error rendering radar mosaic";
			}
			throw new VizException(msg, e);
		}

	}

	@SuppressWarnings("unchecked")
	private void setColorMapParameters(MosaicRecord radarRecord) throws FileNotFoundException, StorageException, VizException {
		
		File loc = HDF5Util.findHDF5Location(radarRecord);

		IDataStore dataStore = DataStoreFactory.getDataStore(loc);

		radarRecord.retrieveFromDataStore(dataStore);

		Unit<?> dataUnit = null;
		if (radarRecord.getUnit() != null) {
			try {
				dataUnit = UnitFormat.getUCUMInstance().parseProductUnit(
						radarRecord.getUnit(), new ParsePosition(0));
			} catch (ParseException e) {
				throw new VizException("Unable to parse units ", e);
			}
		} else {
			dataUnit = Unit.ONE;
		}
		int numLevels = radarRecord.getNumLevels();
		Object[] thresholds = radarRecord.getDecodedThresholds();
		DataMappingPreferences dmPref = new DataMappingPreferences();
		DataMappingEntry dmEntry;
		List<DataMappingEntry> dmEntriesList = new ArrayList<DataMappingEntry>(0);
		if (numLevels <= 16) {

			ArrayList<Integer> pixel = new ArrayList<Integer>();
			ArrayList<Float> real = new ArrayList<Float>();
			for (int i = 0; i < numLevels; i++) {
				dmEntry = new DataMappingEntry();
			    dmEntry.setPixelValue(new Double(i));
				if (thresholds[i] instanceof Float) {
					   pixel.add(i);
					   real.add((Float) thresholds[i]);
					   dmEntry.setDisplayValue(((Float)thresholds[i]).doubleValue());
					   dmEntry.setLabel(((Float)thresholds[i]).toString());
				}else{
					   dmEntry.setDisplayValue(Double.NaN);
					   
					   if ( ((String)thresholds[i]).compareToIgnoreCase("NO DATA") == 0 ){
						    dmEntry.setLabel("ND");
					   }else
						   dmEntry.setLabel((String)thresholds[i]);
				}
				
			    dmEntriesList.add(dmEntry);

			}

			double[] pix = new double[pixel.size()];
			int i = 0;
			for (Integer p : pixel) {
				pix[i++] = p;
			}

			double[] std = new double[real.size()];
			i = 0;
			for (Float r : real) {
				std[i++] = r;
			}

			dataUnit = new PiecewisePixel(dataUnit, pix, std);
		} else {
			double offset = radarRecord.getThreshold(0);
			double scale = radarRecord.getThreshold(1);
			offset -= 2 * scale;
			double[] pix = { 2, 255 };
			double[] data = { 2 * scale + offset, 255 * scale + offset };
			dataUnit = new PiecewisePixel(dataUnit, pix, data);
		}

		ColorMap colorMap;
		try {
			colorMap = (ColorMap) ColorMapUtil.loadColorMap( 
					radarRscData.getResourceName().getRscCategory(), 
					radarRscData.getColorMapName() );
		} catch (VizException e) {
			throw new VizException("Error loading colormap: "+ radarRscData.getColorMapName() );
		}


		colorMapParameters = new ColorMapParameters();
		if (colorMapParameters.getDisplayUnit() == null) {
			colorMapParameters.setDisplayUnit(dataUnit);
			
		}
		
		colorMapParameters.setColorMap( colorMap );
        colorMapParameters.setColorMapMax(255);
		colorMapParameters.setColorMapMin(0);
		colorMapParameters.setDataMax(255);
		colorMapParameters.setDataMin(0);
		getCapability(ColorMapCapability.class).setColorMapParameters(
				colorMapParameters);
		if( dmEntriesList.size() > 0 ){
		    DataMappingEntry[] dmEntryArray = new DataMappingEntry[dmEntriesList.size()];
		    dmEntriesList.toArray(dmEntryArray);
 		    dmPref.setSerializableEntries(dmEntryArray);
 		    ImagePreferences imgPref = new ImagePreferences();
 		    imgPref.setDataMapping(dmPref);
 		    SamplePreferences sampPref = new SamplePreferences();
 		    sampPref.setMinValue(0);
 		    sampPref.setMaxValue(255);
 		    imgPref.setSamplePrefs(sampPref);
 		    ColorBarFromColormap cBar = (ColorBarFromColormap)this.cbarResource.getResourceData().getColorbar();
 		    if(cBar != null ){
 		       cBar.setImagePreferences(imgPref);
 		    
 		       if(!cBar.isScalingAttemptedForThisColorMap())
 		    	   cBar.scalePixelValues();

 	       	   if(radarRecord.getUnit().compareToIgnoreCase("in") == 0 ){
		         	cBar.setDisplayUnitStr("INCHES");
		       }else{
		           cBar.setDisplayUnitStr(radarRecord.getUnit());
		       } 		       
 		       
 	       	   cBar.setAlignLabelInTheMiddleOfInterval(true);
 	       	   cBar.setColorMap(colorMap);
 
 		       this.radarRscData.setColorBar(cBar);
 		    }

		}

	}
	
	
	
	private RadarTileSet createTile(MosaicRecord radarRecord, boolean hasBasetile) throws VizException {
		File loc = HDF5Util.findHDF5Location(radarRecord);

		IDataStore dataStore = DataStoreFactory.getDataStore(loc);

		try {
			radarRecord.retrieveFromDataStore(dataStore);
		} catch (FileNotFoundException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		} catch (StorageException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
		
		RadarTileSet tileSet = null;
		
		int numLevels = radarRecord.getNumLevels();
		UnitConverter dataToImage = null;
		if (numLevels <= 16) {
			dataToImage = new MultiplyConverter(16);
		} else {
			dataToImage = colorMapParameters.getDataToImageConverter();
		}

		MosaicTiler tiler = new MosaicTiler(radarRecord, 1,	radarRecord.getNx(), dataToImage);

		if (hasBasetile) {
			tileSet = new RadarTileSet(tiler, baseTile, this,
					grphTarget.getViewType());
		} else {
			tileSet = new RadarTileSet(tiler, this, grphTarget.getViewType());
		}

		if( grphTarget != null )
			tileSet.setMapDescriptor(this.descriptor);
		
		return tileSet;
	}
	
	/*
	 * (non-Javadoc)
	 * 
	 * @see
	 * com.raytheon.viz.core.rsc.capabilities.IInspectableResource#inspect(com
	 * .vividsolutions.jts.geom.Coordinate)
	 */
	@Override
	public String inspect(ReferencedCoordinate latLon) throws VizException {

		Map<String, String> dataMap;
		try {
			dataMap = interrogate(latLon.asLatLon());
		} catch (Exception e) {
			throw new VizException("Error converting coordinate for hover", e);
		}

		if (dataMap == null) {
			return "NO DATA";
		}

		StringBuffer displayedData = new StringBuffer();
		//displayedData.append(dataMap.get("ICAO") + " ");

		displayedData.append(dataMap.get("Value"));

		if (dataMap.containsKey("Shear")) {
			displayedData.append(" " + dataMap.get("Shear"));
		}

		if (dataMap.containsKey("Azimuth")) {
			displayedData.append(" " + dataMap.get("MSL") + "MSL");
			displayedData.append(" " + dataMap.get("AGL") + "AGL");
			displayedData.append(" " + dataMap.get("Range"));
			displayedData.append("@" + dataMap.get("Azimuth"));
		}

		return displayedData.toString();
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see
	 * com.raytheon.uf.viz.core.rsc.AbstractVizResource#interrogate(com.raytheon
	 * .uf.viz.core.geospatial.ReferencedCoordinate)
	 */
	@Override
	public Map<String, Object> interrogate(ReferencedCoordinate coord)
	throws VizException {
		Map<String, String> smap;
		try {
			smap = this.interrogate(coord.asLatLon());
		} catch (Exception e) {
			throw new VizException("Error transforming", e);
		}
		Map<String, Object> rmap = new HashMap<String, Object>();
		
		if( smap == null ) 
			return rmap;

		for (String smapKey : smap.keySet()) {
			String value = smap.get(smapKey);
			rmap.put(smapKey, value);
		}
		return rmap;
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see
	 * com.raytheon.viz.core.rsc.capabilities.IInspectableResource#interrogate
	 * (com.vividsolutions.jts.geom.Coordinate)
	 */
	public Map<String, String> interrogate(Coordinate latLon)
	throws VizException {
		return null;
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see
	 * com.raytheon.viz.core.rsc.capabilities.IProjectableResource#project(org
	 * .opengis.referencing.crs.CoordinateReferenceSystem)
	 */
	@Override
	public void project(CoordinateReferenceSystem mapData) throws VizException {

		if (this.baseTile != null)
            this.baseTile.reproject();

    	for( AbstractFrameData frm : frameDataMap.values() ) {
    		AbstractTileSet ts = ((FrameData)frm).tileSet; 
    		if( ts != null )
    			ts.reproject();
    		else
    			System.out.println("ERROR reproject tiles");
        }    	
	}


	@Override
	public void resourceChanged(ChangeType type, Object object) {
        if ( type != null && type == ChangeType.CAPABILITY ){
        	if (object instanceof ImagingCapability ){
        		ImagingCapability imgCap = getCapability(ImagingCapability.class);
           		ImagingCapability newImgCap = ( ImagingCapability ) object;
        		imgCap.setBrightness(newImgCap.getBrightness(), false);
        		imgCap.setContrast(newImgCap.getContrast(), false);
        		imgCap.setAlpha(newImgCap.getAlpha(), false);
                radarRscData.setAlpha(  imgCap.getAlpha()  );
                radarRscData.setBrightness(  imgCap.getBrightness() );
                radarRscData.setContrast(  imgCap.getContrast() );
        		issueRefresh();
        		
        		
        	}
        	else if (object instanceof ColorMapCapability ){
        		
        		ColorMapCapability colorMapCap = getCapability(ColorMapCapability.class);
        		ColorMapCapability newColorMapCap = (ColorMapCapability) object;
        		colorMapCap.setColorMapParameters(newColorMapCap.getColorMapParameters(), false);
        		ColorMap theColorMap = ( ColorMap ) colorMapCap.getColorMapParameters().getColorMap();
        		String colorMapName = colorMapCap.getColorMapParameters().getColorMapName();
        		radarRscData.setColorMapName( colorMapName );
        	    radarRscData.getRscAttrSet().setAttrValue( "colorMapName", colorMapName );
        	    ColorBarFromColormap cBar = radarRscData.getColorBar();
        	    cBar.setColorMap( theColorMap );
        	    radarRscData.getRscAttrSet().setAttrValue( "colorBar", cBar );
        	    radarRscData.setIsEdited( true );
        		issueRefresh();

        	}

        }

	}

	@Override
    public void propertiesChanged(ResourceProperties updatedProps) {
    	
    	if( cbarRscPair != null ) {
    		cbarRscPair.getProperties().setVisible( updatedProps.isVisible() );
    	}
    }

    // the colorBar and/or the colormap may have changed so update the 
    // colorBarPainter and the colorMapParametersCapability which holds
    // the instance of the colorMap that Raytheon's code needs
	@Override
	public void resourceAttrsModified() {
		// update the colorbarPainter with a possibly new colorbar
    	ColorBarFromColormap colorBar = radarRscData.getColorBar();
//		ColorBarFromColormap colorBar = (ColorBarFromColormap) radarRscData.getRscAttrSet().getRscAttr("colorBar").getAttrValue();
    	cbarResource.setColorBar( colorBar );
		    	    	
    	ColorMapParameters cmapParams = getCapability(ColorMapCapability.class).getColorMapParameters();
    	cmapParams.setColorMap( colorBar.getColorMap());
    	cmapParams.setColorMapName( radarRscData.getColorMapName() );
    			
        getCapability(ColorMapCapability.class).setColorMapParameters( cmapParams );

        // TODO : how to migrate this to to11dr11? Or do we still need to do this?
//		baseTile.resourceChanged(ChangeType.CAPABILITY, this.getCapability( ColorMapCapability.class));
	}
}
