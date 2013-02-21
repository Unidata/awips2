/*
 * gov.noaa.nws.ncep.viz.rsc.mosaic.rsc.RadarImageResource
 * 
 * 03-04-2011
 *
 * This code has been developed by the NCEP/SIB for use in the AWIPS2 system.
 */


package gov.noaa.nws.ncep.viz.rsc.mosaic.rsc;


import gov.noaa.nws.ncep.viz.common.ColorMapUtil;
import gov.noaa.nws.ncep.viz.resources.colorBar.ColorBarResource;
import gov.noaa.nws.ncep.viz.resources.colorBar.ColorBarResourceData;
import gov.noaa.nws.ncep.viz.ui.display.ColorBarFromColormap;

import java.awt.Rectangle;
import java.io.File;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.text.ParseException;
import java.text.ParsePosition;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.Map;

import javax.measure.converter.MultiplyConverter;
import javax.measure.converter.UnitConverter;
import javax.measure.unit.Unit;
import javax.measure.unit.UnitFormat;

import org.eclipse.swt.graphics.RGB;
import org.geotools.coverage.grid.GridGeometry2D;
import org.geotools.geometry.jts.ReferencedEnvelope;
import org.geotools.referencing.CRS;
import org.geotools.referencing.crs.DefaultGeographicCRS;
import org.opengis.referencing.FactoryException;
import org.opengis.referencing.crs.CoordinateReferenceSystem;
import org.opengis.referencing.datum.PixelInCell;
import org.opengis.referencing.operation.MathTransform;

import com.raytheon.uf.common.colormap.ColorMap;
import com.raytheon.uf.common.dataplugin.PluginDataObject;
import com.raytheon.uf.common.dataplugin.radar.RadarRecord;
import com.raytheon.uf.common.dataplugin.radar.util.RadarDataRetriever;
import com.raytheon.uf.common.dataplugin.radar.util.RadarUtil;
import com.raytheon.uf.common.datastorage.DataStoreFactory;
import com.raytheon.uf.common.datastorage.IDataStore;
import com.raytheon.uf.common.datastorage.StorageException;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.common.time.DataTime;
import com.raytheon.uf.common.util.BufferUtil;
import com.raytheon.uf.viz.core.HDF5Util;
import com.raytheon.uf.viz.core.IGraphicsTarget;
import com.raytheon.uf.viz.core.IMesh;
import com.raytheon.uf.viz.core.IMeshCallback;
import com.raytheon.uf.viz.core.PixelCoverage;
import com.raytheon.uf.viz.core.cache.CacheObject;
import com.raytheon.uf.viz.core.data.prep.CMDataPreparerManager;
import com.raytheon.uf.viz.core.drawables.ColorMapParameters;
import com.raytheon.uf.viz.core.drawables.IDescriptor;
import com.raytheon.uf.viz.core.drawables.IWireframeShape;
import com.raytheon.uf.viz.core.drawables.PaintProperties;
import com.raytheon.uf.viz.core.drawables.ResourcePair;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.rsc.GenericResourceData;
import com.raytheon.uf.viz.core.rsc.IResourceDataChanged;
import com.raytheon.uf.viz.core.rsc.LoadProperties;
import com.raytheon.uf.viz.core.rsc.ResourceProperties;
import com.raytheon.uf.viz.core.rsc.IResourceDataChanged.ChangeType;
import com.raytheon.uf.viz.core.rsc.capabilities.ColorMapCapability;
import com.raytheon.uf.viz.core.rsc.capabilities.ColorableCapability;
import com.raytheon.uf.viz.core.rsc.capabilities.ImagingCapability;
import com.raytheon.uf.viz.core.rsc.capabilities.OutlineCapability;
import com.raytheon.uf.viz.core.rsc.hdf5.ImageTile;
import com.raytheon.uf.viz.core.style.DataMappingPreferences;
import com.raytheon.uf.viz.core.style.DataMappingPreferences.DataMappingEntry;
import com.raytheon.viz.awipstools.capabilities.RangeRingsOverlayCapability;
import com.raytheon.viz.awipstools.capabilityInterfaces.IRangeableResource;
import com.raytheon.viz.core.drawables.ColorMapParameterFactory;
import com.raytheon.viz.core.units.PiecewisePixel;
import com.raytheon.viz.radar.IRadarRecordMetadata;
import com.raytheon.viz.radar.RadarRecordDataRetriever;
import com.raytheon.viz.radar.RadarRecordMetadata;
import com.raytheon.viz.radar.RadarTimeRecord;
import com.raytheon.viz.radar.rsc.RadarTextResourceData;
import com.raytheon.viz.radar.rsc.image.IRadialMeshExtension;
import com.raytheon.viz.radar.util.DataUtilities;
import com.vividsolutions.jts.geom.Coordinate;

/**
 * This class is based on Raytheon's code.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 03-04-2011              G. Zhang     Initial creation
 * 07-11-2011              Greg Hull    create initResource to create the colorBarResource.
 * 09-04-2012              B. Hebbard   Add getGridGeometry() to descriptor per OB12.9.1 RTS
 *                                      change IRadialMeshExtension.constructMesh 2nd param
 * 12/19/2012     #960        Greg Hull   override propertiesChanged() to update colorBar.
 * 
 * </pre>
 * 
 * @author gzhang
 * @version 1.0
 */

public abstract class RadarImageResource<D extends IDescriptor> extends
        AbstractRadarResource<D>  implements IMeshCallback, IRangeableResource, IResourceDataChanged {


	RadarResourceData radarRscData;
	
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
		
		protected FrameData(DataTime time, int interval) {
			super(time, interval);	
			tileTime = null;
		}

		public boolean updateFrameData( IRscDataObject rscDataObj ) {

			if( !(rscDataObj instanceof DfltRecordRscDataObj) ) {
				System.out.println("Unrecognized Radar Image");
				return false;
			}
        	PluginDataObject pdo = ((DfltRecordRscDataObj)rscDataObj).getPDO();
			RadarRecord radarRecord = (RadarRecord) pdo;
			RadarImageResource.this.addRecord(pdo);//TO Be Tested: 2011-03-05
	       
			RadarTimeRecord rtr = radarRecords.get(pdo.getDataTime());

//			RadarTimeRecord rtr = new RadarTimeRecord();	
//			rtr.radarCacheObject = CacheObject.newCacheObject(
//					new RadarRecordMetadata(radarRecord),
//					new RadarRecordDataRetriever());
			try{
				setColorMapParameters( radarRecord,rtr );
			}
			catch (FileNotFoundException e) {
				e.printStackTrace();
			} catch (StorageException e) {
				e.printStackTrace();
			} catch (VizException e) {
				e.printStackTrace();
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
	
	@SuppressWarnings("unchecked")
	private void setColorMapParameters(RadarRecord radarRecord, RadarTimeRecord rtr) throws FileNotFoundException, StorageException, VizException {
		
		File loc = HDF5Util.findHDF5Location(radarRecord);

		IDataStore dataStore = DataStoreFactory.getDataStore(loc);

		RadarDataRetriever.populateRadarRecord(dataStore,
                radarRecord);

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
		if (numLevels <= 16) {
			ArrayList<Integer> pixel = new ArrayList<Integer>();
			ArrayList<Float> real = new ArrayList<Float>();
			for (int i = 0; i < numLevels; i++) {
				if (thresholds[i] instanceof Float) {
					pixel.add(i);
					real.add((Float) thresholds[i]);
				}
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

		ColorBarFromColormap colorBar = radarRscData.getColorBar();

		((ColorBarFromColormap)colorBar).setColorMap( colorMap );

		ColorMapParameters colorMapParameters = new ColorMapParameters();

		colorMapParameters.setColorMap( colorMap );

		if (colorMapParameters.getDisplayUnit() == null) {
			colorMapParameters.setDisplayUnit(dataUnit);
		}

		colorMapParameters.setColorMapMax(255);
		colorMapParameters.setColorMapMin(0);
		colorMapParameters.setDataMax(255);
		colorMapParameters.setDataMin(0);
		rtr.params  = colorMapParameters;
		this.getCapability(ColorMapCapability.class).setColorMapParameters(
				colorMapParameters);		
	}
    
    
//------------------------------------------------------------------- above from RadarResource.java---------------------------------
    /**
     * @param resourceData
     * @param loadProperties
     * @throws VizException
     */
    protected RadarImageResource(RadarResourceData resourceData,
            LoadProperties loadProperties)//, com.raytheon.viz.radar.interrogators.IRadarInterrogator interrogator)
            throws VizException {
        super(resourceData, loadProperties);//, interrogator);
        rangeCircle = new HashMap<Float, IWireframeShape>();
    }

	@Override
	public void initResource(IGraphicsTarget target) throws VizException {
        // create the colorBar Resource and add it to the resourceList for this descriptor.
        cbarRscPair  = ResourcePair.constructSystemResourcePair( 
		           new ColorBarResourceData( radarRscData.getColorBar() ) );

        getDescriptor().getResourceList().add(  cbarRscPair );
        getDescriptor().getResourceList().instantiateResources( getDescriptor(), true );

        cbarResource = (ColorBarResource) cbarRscPair.getResource();
	}
	
    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.viz.radar.rsc.AbstractRadarResource#disposeInternal()
     */
    @Override
    protected void disposeInternal() {
        for (Map.Entry<DataTime, RadarTimeRecord> timeRecord : radarRecords
                .entrySet()) {
            if (timeRecord.getValue().image != null) {
                timeRecord.getValue().image.dispose();
            }
            if (timeRecord.getValue().tile != null) {
                timeRecord.getValue().tile.dispose();
            }
        }
//        RadarDisplayManager.getInstance().removeListener(this);//TODO: 2011-03-08
        RadarTextResourceData.removeRadarTextResource(descriptor);
        
        getDescriptor().getResourceList().remove( cbarRscPair );        
    }


    protected void/*RadarTileSet*/ createTile(IGraphicsTarget target, 
    		RadarTimeRecord tiltRecord, RadarRecord populatedRecord)
            throws StorageException, IOException, ClassNotFoundException,
            VizException {
    	tiltRecord.params = getColorMapParameters(target, populatedRecord);
    	if (tiltRecord.tile == null) {
    		tiltRecord.tile = new ImageTile();
        }

    	tiltRecord.tile.coverage = buildCoverage(target, tiltRecord);
        if (tiltRecord.tile.coverage.getMesh() == null) {
            tiltRecord.tile.coverage.setMesh(buildMesh(target, tiltRecord));
        }
        
        Rectangle rect = new Rectangle(0, 0, populatedRecord.getNumBins(),
                populatedRecord.getNumRadials());

        tiltRecord.image = target.initializeRaster(CMDataPreparerManager
                .getDataPreparer(BufferUtil.wrapDirect(
                        toImageData(tiltRecord.params, populatedRecord),
                        new Rectangle(0, 0, populatedRecord.getNumBins(),
                                populatedRecord.getNumRadials())), rect,
                        new int[] { rect.width, rect.height }),
                tiltRecord.params);

    }

    protected ColorMapParameters getColorMapParameters(IGraphicsTarget target,
            RadarRecord record) throws VizException {

        ColorMapParameters params = getCapability(ColorMapCapability.class)
                .getColorMapParameters();
        String colorMapName = "";
        if (params != null && params.getColorMap() != null) {
            return params;
        } else if (params != null) {
            colorMapName = params.getColorMapName();
        }
        // Make sure we have all the data
        if (record.getRawData() == null) {
            File loc = HDF5Util.findHDF5Location(record);
            IDataStore dataStore = DataStoreFactory.getDataStore(loc);
            try {
                RadarDataRetriever.populateRadarRecord(dataStore, record);
            } catch (Exception e) {
                throw new VizException(e);
            }
        }

        // Setup the ColorMap settings
        int prodCode = record.getProductCode();
        Unit<?> dataUnit = DataUtilities.getDataUnit(record);

params = /*new ColorMapParameters();//*/ColorMapParameterFactory.build((Object) null, "" + prodCode,    dataUnit, null);
        if (params.getDisplayUnit() == null) {
            params.setDisplayUnit(record.getUnitObject());
        }
        if (params.getImageUnit() == dataUnit && record.getNumLevels() <= 16) {
            DataMappingPreferences dataMapping = new DataMappingPreferences();
            Object[] thresholds = record.getDecodedThresholds();
            for (int i = 1; i < record.getNumLevels(); i++) {
                DataMappingEntry entry = new DataMappingEntry();

                // Sets the position left or right, should be normalized to
                // the
                // numLevels
                entry.setPixelValue((double) (i * 16));

                // Set the data value
                if (thresholds[i] instanceof Float) {
                    entry.setDisplayValue(params.getDataToDisplayConverter()
                            .convert(i));
                } else if (thresholds[i] instanceof String) {
                    entry.setLabel((String) thresholds[i]);
                } else if (thresholds[i] == null) {
                    entry.setLabel("");
                } else {
                    entry.setLabel(thresholds[i].toString());
                }
                dataMapping.addEntry(entry);
            }
            params.setDataMapping(dataMapping);
        }
        getCapability(ColorMapCapability.class).setColorMapParameters(params);

        if (params.getColorMap() == null) {
           if (("").equals(colorMapName)) {
                colorMapName = params.getColorMapName();
            }
            if (colorMapName == null) {
                colorMapName = "Radar/Storm Clear Reflectivity";//OSF/Digital VIL";//16 Level Reflectivity";
            }

            params.setColorMap(target.buildColorMap(colorMapName));

        }
        
		ColorMap colorMap;
		try {
			colorMap = (ColorMap) ColorMapUtil.loadColorMap( 
					radarRscData.getResourceName().getRscCategory(), 
					radarRscData.getColorMapName() );
		} catch (VizException e) {
			throw new VizException("Error loading colormap: "+ radarRscData.getColorMapName() );
}//params.setColorMap(colorMap);

        params.setColorMapMax(255);
        params.setColorMapMin(0);
        params.setDataMax(255);
        params.setDataMin(0);

        return params;
    }

    public void paintRadar(IGraphicsTarget target, PaintProperties paintProps)
            throws VizException {
        displayedDate = paintProps.getDataTime();
        RadarTimeRecord record = radarRecords.get(displayedDate);
        if (record == null) {
            return;
        }

        displayedLevel = displayedDate.getLevelValue().floatValue();

        RadarRecord radarRecord = record.radarCacheObject.getObjectAsync();
        if (radarRecord == null) {
        	issueRefresh();
        	return;
        }

        if (record.params != null) {
            getCapability(ColorMapCapability.class).setColorMapParameters(
                    record.params, false);
        }
        this.actualLevel = String.format("%1.1f",
                radarRecord.getTrueElevationAngle());
        try {
            // Handle events from the radar display controls
            if (refreshDisplay) {
                refreshDisplay = false;
                if (record.image != null) {
                    record.image.dispose();
                    record.image = null;
                }
                if (record.tile != null) {
                    record.tile.dispose();
                    record.tile = null;
                }
            }

            if (record.image == null) {
                createTile(target, record, radarRecord);
            }

            if (record.image != null) {
                ImagingCapability cap = getCapability(ImagingCapability.class);
                record.image.setBrightness(cap.getBrightness());
                record.image.setContrast(cap.getContrast());
                record.image.setInterpolated(cap.isInterpolationState());
                target.drawRaster(record.image, record.tile.coverage,  paintProps);
            }

        } catch (Exception e) {
            String msg = e.getMessage();
            if (msg == null) {
                msg = "Error rendering radar";
            }
            throw new VizException(msg, e);
        }
    }

    public byte[] convertData(RadarRecord record, byte[] table) {
        return null;
    }

    /**
     * @param imageData
     */
    protected boolean createCircle(byte[] imageData, RadarRecord record, int h,
            int w, int i) {
        // do nothing
        return false;
    }

    private byte[] toImageData(ColorMapParameters params, RadarRecord record) {
        // Sometimes the data unit may not match what is in the params so always
        // use what we really have
        UnitConverter dataToImage = null;
        Unit<?> dataUnit = DataUtilities.getDataUnit(record);
        if (dataUnit != null && !dataUnit.equals(params.getDataUnit())) {
            Unit<?> imageUnit = params.getImageUnit();
            if (imageUnit != null && dataUnit.isCompatible(imageUnit)) {
                dataToImage = dataUnit.getConverterTo(imageUnit);
            } else if (imageUnit != null) {
                dataUnit = DataUtilities.getDataUnit(record, radarRscData.mode);//resourceData.mode);
                if (dataUnit.isCompatible(imageUnit)) {
                    dataToImage = dataUnit.getConverterTo(imageUnit);
                }
            }
        } else {
            dataToImage = params.getDataToImageConverter();
        }
        if (dataToImage == null && record.getNumLevels() <= 16) {
            dataToImage = new MultiplyConverter(16);
        } else if (dataToImage == null) {
            dataToImage = UnitConverter.IDENTITY;
        }
        // precompute the converted value for every possible value in the
        // record.
        byte[] table = new byte[record.getNumLevels()];
        for (int i = 0; i < record.getNumLevels(); i++) {
            double image = dataToImage.convert(i);
            if (Double.isNaN(image)) {
                double d = dataUnit.getConverterTo(params.getDisplayUnit())
                        .convert(i);
                if (Double.isNaN(d)) {
                    // This means that the data is a non-numeric value, most
                    // likely a flag of some sort
                    if (record.getNumLevels() <= 16) {
                        // For 3 and 4 bit products products try to match the
                        // flag value to a string value in the params
                        String value = record.getDecodedThreshold(i).toString();
                        for (DataMappingEntry entry : params.getDataMapping()
                                .getEntries()) {
                            if (value.equals(entry.getLabel())
                                    || value.equals(entry.getSample())) {
                                table[i] = entry.getPixelValue().byteValue();
                                break;
                            }
                        }
                    } else {
                        // For 8 bit products the flag value should be
                        // specifically handled in the style rules. For example
                        // if 1 is a flag for RF than pixel value 1 in the style
                        // rules will need to be RF. This is not
                        // a graceful seperation of data and representation but
                        // it works
                        table[i] = (byte) i;
                    }
                } else {
                    // the data value is outside the range of the colormap
                    UnitConverter image2disp = params
                            .getImageToDisplayConverter();
                    if (image2disp == null) {
                        continue;
                    }
                    for (int j = 0; j < 256; j++) {
                        double disp = image2disp.convert(j);
                        if (Double.isNaN(disp)) {
                            continue;
                        }
                        if (d < disp) {
                            // Map data values smaller than the colormap min to
                            // 0, which should be no data.
                            table[i] = (byte) 0;
                            // If we want small values to appear as the lowest
                            // data value than do this next line instead
                            // table[i] = (byte) j;
                            break;
                        }
                        if (d > disp) {
                            // map data values larger than the colormap max to
                            // the highest value
                            table[i] = (byte) j;
                        }

                    }
                }
            } else {
                table[i] = (byte) Math.round(image);
            }
        }

        return convertData(record, table);
    }

    public IMesh buildMesh(IGraphicsTarget target, RadarTimeRecord timeRecord)
            throws VizException {
        return target.getExtension(IRadialMeshExtension.class).constructMesh(
                timeRecord.radarCacheObject.getObjectSync(), descriptor.getGridGeometry());
    }

    public PixelCoverage buildCoverage(IGraphicsTarget target,
    		RadarTimeRecord timeRecord) throws VizException {
    	IRadarRecordMetadata radarRecord = timeRecord.radarCacheObject.getMetadata();
    	return new PixelCoverage(new Coordinate(
    			/* 0, 0 */radarRecord.getLongitude(), radarRecord.getLatitude()), 0, 0);
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.viz.core.IMeshCallback#meshCalculated(com.raytheon.uf
     * .viz.core.rsc.hdf5.ImageTile)
     */
    @Override
    public void meshCalculated(ImageTile tile) {
        issueRefresh();
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.viz.core.rsc.AbstractVizResource#project(org.opengis.
     * referencing.crs.CoordinateReferenceSystem)
     */
    @Override
    public void project(CoordinateReferenceSystem crs) throws VizException {
        for (IWireframeShape ring : rangeCircle.values()) {
            ring.dispose();
        }
        rangeCircle.clear();
    }
    
    @Override
	protected boolean preProcessFrameUpdate() {
		return true;
	}
    
    @Override
	protected void paintFrame(AbstractFrameData frameData,
			   IGraphicsTarget target, PaintProperties paintProps ) throws VizException{
	
		displayedDate = null;

        if ((paintProps == null) || (paintProps.getDataTime() == null)) {
            return;
        }

        displayedDate = paintProps.getDataTime();
        CacheObject<?, RadarRecord> co = getRadarRecord(displayedDate);
        if (co == null) {
        	return;
        }
        RadarRecord radarRecord = co.getObjectAsync();
        displayedLevel = displayedDate.getLevelValue().floatValue();

        if (radarRecord == null) {
        	issueRefresh();
            return;
        }

        paintRadar(target, paintProps);

        if (radarRscData.rangeRings){//resourceData.rangeRings) {
            IWireframeShape rangeCircle = this.rangeCircle.get(actualLevel);

            Float elev = radarRecord.getPrimaryElevationAngle().floatValue();

            // create range circle
            rangeCircle = this.rangeCircle.get(elev);
            if (rangeCircle == null) {
                // Attempt to create envelope, adapted from AbstractTileSet
                double maxExtent = RadarUtil.calculateExtent(radarRecord);
                rangeCircle = computeRangeCircle(target, radarRecord.getCRS(),
                        maxExtent);
                if (rangeCircle != null) {
                    this.rangeCircle.put(elev, rangeCircle);
                }
            }

            if ((rangeCircle != null)
                    && (getCapability(OutlineCapability.class).isOutlineOn())) {
                target.drawWireframeShape(rangeCircle,
                        getCapability(ColorableCapability.class).getColor(),
                        getCapability(OutlineCapability.class)
                                .getOutlineWidth(),
                        getCapability(OutlineCapability.class).getLineStyle(),
                        paintProps.getAlpha());
            }
        }
        RangeRingsOverlayCapability rrcap = getCapability(RangeRingsOverlayCapability.class);
        rrcap.setRangeableResource(this);
        rrcap.paint(target, paintProps);    	
    }    

	@Override
    public void propertiesChanged(ResourceProperties updatedProps) {
    	
    	if( cbarRscPair != null ) {
    		cbarRscPair.getProperties().setVisible( updatedProps.isVisible() );
    	}
    }

//	public void resourceChanged(ChangeType type, Object object) {
//	}

}