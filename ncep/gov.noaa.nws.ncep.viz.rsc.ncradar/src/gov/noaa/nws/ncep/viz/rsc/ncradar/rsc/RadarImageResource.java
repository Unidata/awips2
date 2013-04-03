/*
 * gov.noaa.nws.ncep.viz.rsc.ncradar.rsc.RadarImageResource
 * 
 * 12-07-2011
 *
 * This code has been developed by the NCEP/SIB for use in the AWIPS2 system.
 */

package gov.noaa.nws.ncep.viz.rsc.ncradar.rsc;

import gov.noaa.nws.ncep.viz.common.ColorMapUtil;
import gov.noaa.nws.ncep.viz.resources.colorBar.ColorBarResource;
import gov.noaa.nws.ncep.viz.resources.colorBar.ColorBarResourceData;
import gov.noaa.nws.ncep.viz.ui.display.ColorBarFromColormap;

import java.awt.Rectangle;
import java.io.File;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.nio.ByteBuffer;
import java.text.ParseException;
import java.text.ParsePosition;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;

import javax.measure.converter.MultiplyConverter;
import javax.measure.converter.UnitConverter;
import javax.measure.unit.Unit;
import javax.measure.unit.UnitFormat;

import org.eclipse.swt.graphics.RGB;

import org.geotools.referencing.CRS;

import org.opengis.referencing.FactoryException;
import org.opengis.referencing.crs.CoordinateReferenceSystem;

import org.opengis.referencing.operation.MathTransform;
import org.opengis.referencing.operation.TransformException;

import com.raytheon.uf.common.colormap.ColorMap;
import com.raytheon.uf.common.colormap.IColorMap;
import com.raytheon.uf.common.dataplugin.PluginDataObject;
import com.raytheon.uf.common.dataplugin.radar.RadarRecord;
import com.raytheon.uf.common.dataplugin.radar.util.RadarDataRetriever;
import com.raytheon.uf.common.dataplugin.radar.util.RadarUtil;
import com.raytheon.uf.common.datastorage.DataStoreFactory;
import com.raytheon.uf.common.datastorage.IDataStore;
import com.raytheon.uf.common.datastorage.StorageException;
import com.raytheon.uf.common.geospatial.MapUtil;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.common.time.DataTime;
import com.raytheon.uf.viz.core.DrawableImage;
import com.raytheon.uf.viz.core.HDF5Util;
import com.raytheon.uf.viz.core.IGraphicsTarget;
import com.raytheon.uf.viz.core.IMesh;
import com.raytheon.uf.viz.core.IMeshCallback;
import com.raytheon.uf.viz.core.PixelCoverage;
import com.raytheon.uf.viz.core.data.IColorMapDataRetrievalCallback;
import com.raytheon.uf.viz.core.drawables.ColorMapParameters;
import com.raytheon.uf.viz.core.drawables.IDescriptor;
import com.raytheon.uf.viz.core.drawables.IImage;
import com.raytheon.uf.viz.core.drawables.IWireframeShape;
import com.raytheon.uf.viz.core.drawables.PaintProperties;
import com.raytheon.uf.viz.core.drawables.ResourcePair;
import com.raytheon.uf.viz.core.drawables.ext.colormap.IColormappedImageExtension;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.rsc.IResourceDataChanged;
import com.raytheon.uf.viz.core.rsc.LoadProperties;
import com.raytheon.uf.viz.core.rsc.ResourceProperties;
import com.raytheon.uf.viz.core.rsc.capabilities.ColorMapCapability;
import com.raytheon.uf.viz.core.rsc.capabilities.ImagingCapability;
import com.raytheon.uf.viz.core.rsc.capabilities.OutlineCapability;
import com.raytheon.uf.viz.core.rsc.hdf5.ImageTile;
import com.raytheon.uf.viz.core.style.DataMappingPreferences;
import com.raytheon.uf.viz.core.style.DataMappingPreferences.DataMappingEntry;
import com.raytheon.viz.awipstools.capabilities.RangeRingsOverlayCapability;
import com.raytheon.viz.awipstools.capabilityInterfaces.IRangeableResource;
import com.raytheon.viz.core.drawables.ColorMapParameterFactory;
import com.raytheon.viz.core.style.image.ImagePreferences;
import com.raytheon.viz.core.style.image.SamplePreferences;
import com.raytheon.viz.core.units.PiecewisePixel;
import gov.noaa.nws.ncep.viz.rsc.ncradar.VizRadarRecord;
import com.raytheon.viz.radar.interrogators.IRadarInterrogator;
import com.raytheon.viz.radar.util.DataUtilities;
import com.vividsolutions.jts.geom.Coordinate;

/**
 * TODO Add Description
 *
 * This class is based on Raytheon's code.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 12/07/2011   #541       S. Gurung   Initial creation
 * 12/16/2011              S. Gurung   Added resourceAttrsModified()
 * 01/03/2011              S. Gurung   Changed circle color to black
 * 04/02/2012   #651       S. Gurung   Added fix for applying resource attributes changes.
 * 06-07-2012   #717       Archana	   Updated setColorMapParameters() to store label information
 *                                      for the colorbar 
 * 12/19/2012   #960       Greg Hull   override propertiesChanged() to update colorBar.
 *                                      
 * </pre>
 * 
 * @author sgurung
 * @version 1.0
 */

public abstract class RadarImageResource<D extends IDescriptor> extends
		AbstractRadarResource<D>  implements IMeshCallback, IRangeableResource, IResourceDataChanged {
    protected static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(RadarImageResource.class);

    private static final int RANGE_CIRCLE_PTS = 360;

    protected Map<Float, IWireframeShape> rangeCircle;

    protected Map<DataTime, DrawableImage> images = new ConcurrentHashMap<DataTime, DrawableImage>();
    
    protected RadarTileSet baseTile;
    
    /** The line color */
	private static final RGB DEFAULT_COLOR = new RGB(255, 255, 255);

    private ColorMapParameters colorMapParameters = null;
    
    protected ColorBarResource cbarResource;
    protected ResourcePair     cbarRscPair;
    
    protected IGraphicsTarget grphTarget;
	
	protected int numLevels;

    protected String viewType;
    
    protected boolean refreshImage = false;
   
    /**
     * @param resourceData
     * @param loadProperties
     * @throws VizException
     */
    protected RadarImageResource(RadarResourceData resourceData,
            LoadProperties loadProperties, IRadarInterrogator interrogator)
            throws VizException {
        super(resourceData, loadProperties, interrogator);
        rangeCircle = new HashMap<Float, IWireframeShape>();
    	refreshImage = false;
    }
    
    protected class FrameData extends AbstractFrameData {

		RadarTileSet tileSet;
		DataTime     tileTime; // the time of the data used to create the tileset
		                       // used to determine if we need to replace the tile 
		                       // with a better timematch. 
		
		public FrameData(DataTime time, int interval) {
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
	       
			VizRadarRecord rtr = radarRecords.get(pdo.getDataTime());

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
	private void setColorMapParameters(RadarRecord radarRecord, VizRadarRecord rtr) throws FileNotFoundException, StorageException, VizException {
		
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
					((RadarResourceData)resourceData).getResourceName().getRscCategory(), 
					((RadarResourceData)resourceData).getColorMapName() );
		} catch (VizException e) {
			throw new VizException("Error loading colormap: "+ ((RadarResourceData)resourceData).getColorMapName() );
		}

		ColorBarFromColormap colorBar = ((RadarResourceData)resourceData).getColorBar();

		((ColorBarFromColormap)colorBar).setColorMap( colorMap );

		colorMapParameters = new ColorMapParameters();
		colorMapParameters.setColorMap( colorMap );

		if (colorMapParameters.getDisplayUnit() == null) {
			colorMapParameters.setDisplayUnit(dataUnit);
		}

		colorMapParameters.setColorMapMax(255);
		colorMapParameters.setColorMapMin(0);
		colorMapParameters.setDataMax(255);
		colorMapParameters.setDataMin(0);
		rtr.params  = colorMapParameters;
		getCapability(ColorMapCapability.class).setColorMapParameters(
				colorMapParameters);
		if ( dmEntriesList.size() > 0 ){
		    DataMappingEntry[] dmEntryArray = new DataMappingEntry[dmEntriesList.size()];
		    dmEntriesList.toArray(dmEntryArray);
 		    dmPref.setSerializableEntries(dmEntryArray);
 		    ImagePreferences imgPref = new ImagePreferences();
 		    imgPref.setDataMapping(dmPref);
 		    SamplePreferences sampPref = new SamplePreferences();
 		    sampPref.setMinValue(0);
 		    sampPref.setMaxValue(255);
 		    imgPref.setSamplePrefs(sampPref);	
 		    colorBar.setImagePreferences(imgPref);
 		    colorBar.setDisplayUnitStr(radarRecord.getUnit());
 		    colorBar.setAlignLabelInTheMiddleOfInterval(true);
 		    this.cbarResource.getResourceData().setColorBar(colorBar);
		}		
	}
    

    @Override
    protected void disposeInternal() {
        super.disposeInternal();

        for (IWireframeShape shape : rangeCircle.values()) {
            if (shape != null) {
                shape.dispose();
            }
        }
        rangeCircle.clear();
        for (DrawableImage image : images.values()) {
            disposeImage(image);
        }
        images.clear();
        if( cbarResource.getResourceData().getColorbar() != null ){
        	cbarResource.getResourceData().getColorbar().dispose();
        	cbarResource.getResourceData().setColorBar(null);
        }
        
        issueRefresh();
    }

    @Override
	public void initResource(IGraphicsTarget target) throws VizException {
        // create the colorBar Resource and add it to the resourceList for this descriptor.
        cbarRscPair  = ResourcePair.constructSystemResourcePair( 
		           new ColorBarResourceData( ((RadarResourceData)resourceData).getColorBar() ) );

        getDescriptor().getResourceList().add(  cbarRscPair );
        getDescriptor().getResourceList().instantiateResources( getDescriptor(), true );

        cbarResource = (ColorBarResource) cbarRscPair.getResource();
        
        if (this.baseTile != null) {
            this.baseTile.init(target);
        }
        
        /*for( AbstractFrameData frm : frameDataMap.values() ) {
    		AbstractTileSet ts = ((FrameData)frm).tileSet; 
    		if( ts != null )
    			ts.init(target);
        }*/
	}
    
    /**
     * Create the radar tile given the tileRecord to populate and a RadarRecord
     * with all data populated
     * 
     * @param target
     * @param tiltRecord
     * @param populatedRecord
     * @throws StorageException
     * @throws IOException
     * @throws ClassNotFoundException
     * @throws VizException
     */
    protected void createTile(IGraphicsTarget target,
            VizRadarRecord populatedRecord) throws StorageException,
            IOException, ClassNotFoundException, VizException {
    	
    	ColorMapParameters params = getColorMapParameters(target, populatedRecord);
    	
        PixelCoverage coverage = buildCoverage(target, populatedRecord);
        if (coverage.getMesh() == null) {
            coverage.setMesh(buildMesh(target, populatedRecord));
        }
       
        IImage image = createImage(target, params, populatedRecord,
                new Rectangle(0, 0, populatedRecord.getNumBins(),
                        populatedRecord.getNumRadials()));
        DrawableImage dImage = images.put(populatedRecord.getDataTime(),
                new DrawableImage(image, coverage));
        if (dImage != null) {
            disposeImage(dImage);
        }
    }

    /**
     * Get the colormap parameters, expects a radar record populated with data
     * 
     * @param target
     * @param record
     * @return
     * @throws VizException
     */
    protected ColorMapParameters getColorMapParameters(IGraphicsTarget target,
            RadarRecord record) throws VizException {
        ColorMapParameters params = getCapability(ColorMapCapability.class)
                .getColorMapParameters();
    
        String colorMapName = "";
        IColorMap colorMap = null;
        float cMapMax = 255;
        float cMapMin = 0;
        if (params != null && params.getDataUnit() != null) {
            return params;
        } else if (params != null) {
            colorMapName = params.getColorMapName();
            colorMap = params.getColorMap();
            //if colorMap range has changed, get updated max and min
            if (refreshImage) {
	            cMapMax = params.getColorMapMax();
	            cMapMin = params.getColorMapMin();
            }
        }
             
        // Setup the ColorMap settings
        int prodCode = record.getProductCode();
        Unit<?> dataUnit = DataUtilities.getDataUnit(record);

        params = ColorMapParameterFactory.build((Object) null, "" + prodCode,
                dataUnit, null, ((RadarResourceData)resourceData).mode);
        if (params.getDisplayUnit() == null) {
            params.setDisplayUnit(record.getUnitObject());
        }
        if (params.getImageUnit() == dataUnit && record.getNumLevels() <= 16) {
            DataMappingPreferences dataMapping = new DataMappingPreferences();
            Object[] thresholds = record.getDecodedThresholds();
            for (int i = 1; i < record.getNumLevels(); i++) {
                DataMappingEntry entry = new DataMappingEntry();

                // Sets the position left or right, should be normalized to
                // the numLevels
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
            params.setColorBarIntervals(null);
        }
        
        getCapability(ColorMapCapability.class).setColorMapParameters(params);

        if (colorMap != null) {        	
            params.setColorMap(colorMap);
            params.setColorMapName(colorMapName);
        }

        if (params.getColorMap() == null) {
            if (("").equals(colorMapName)) {
                colorMapName = params.getColorMapName();
            }
            if (colorMapName == null) {
                colorMapName = "Radar/OSF/16 Level Reflectivity";
            }
         
            params.setColorMap(target.buildColorMap(colorMapName));

        }

        params.setColorMapMax(cMapMax);
	    params.setColorMapMin(cMapMin);
        params.setDataMax(255);
        params.setDataMin(0);
        
        return params;
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.viz.core.rsc.AbstractVizResource#paintInternal(com.raytheon
     * .uf.viz.core.IGraphicsTarget,
     * com.raytheon.uf.viz.core.drawables.PaintProperties)
     */
     @Override
	protected void paintFrame(AbstractFrameData frameData,
			   IGraphicsTarget target, PaintProperties paintProps ) throws VizException{
	
        displayedDate = null;
        if ((paintProps == null) || (paintProps.getDataTime() == null)) {
            return;
        }

        displayedDate = paintProps.getDataTime();
        VizRadarRecord radarRecord = getRadarRecord(displayedDate);
        displayedLevel = displayedDate.getLevelValue().floatValue();

        if (radarRecord == null) {
            issueRefresh();
            return;
        }
        paintRadar(target, paintProps);

        // Draw circle 
        if (((RadarResourceData)resourceData).rangeRings) {
            IWireframeShape rangeCircle = this.rangeCircle.get(actualLevel);

            Float elev = 0.0f;
            if (radarRecord.getPrimaryElevationAngle() != null) {
                elev = radarRecord.getPrimaryElevationAngle().floatValue();
            }
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
                		new RGB(0, 0, 0),//getCapability(ColorableCapability.class).getColor(),
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

    public void paintRadar(IGraphicsTarget target, PaintProperties paintProps)
            throws VizException {
        displayedDate = paintProps.getDataTime();
        synchronized (this.images) {
            VizRadarRecord record = getRadarRecord(displayedDate);
            if (record == null) {
                return;
            }
           
            displayedLevel = displayedDate.getLevelValue().floatValue();

            this.actualLevel = String.format("%1.1f",
                    record.getTrueElevationAngle());
            try {
                DrawableImage image = images.get(displayedDate);
                if (refreshImage) {
                	redoImage(displayedDate);
                	image = null;  
                	images.clear();
                }
                if (image == null || image.getCoverage() == null) {
                    if (record.getStoredDataAsync() == null) {
                        issueRefresh();
                        return;
                    }
                    createTile(target, record);
                    image = images.get(displayedDate);
                }               

                if (image != null) {
                	
    	        	ImagingCapability imgCap = new ImagingCapability(); //getCapability(ImagingCapability.class);//
    	        	imgCap.setBrightness(((RadarResourceData)resourceData).getBrightness());
    	        	imgCap.setContrast(((RadarResourceData)resourceData).getContrast());
    	        	imgCap.setAlpha(((RadarResourceData)resourceData).getAlpha());    	        	
    	        	//((RadarResourceData)resourceData).fireChangeListeners(ChangeType.CAPABILITY, imgCap);
    	        	
    	        	image.getImage().setBrightness(((RadarResourceData)resourceData).getBrightness());
    	        	image.getImage().setContrast(((RadarResourceData)resourceData).getContrast());
    	        	image.getImage().setInterpolated(imgCap.isInterpolationState());
    	        	
    	        	paintProps.setAlpha(((RadarResourceData)resourceData).getAlpha());    	        	
    	        	target.drawRasters(paintProps, image);
                }

                if (image == null || image.getCoverage() == null
                        || image.getCoverage().getMesh() == null) {
                    issueRefresh();
                }
            } catch (Exception e) {
                String msg = e.getMessage();
                if (msg == null) {
                    msg = "Error rendering radar";
                }
                throw new VizException(msg, e);
            }
        }

    	refreshImage = false;   
    }

    /**
     * Shared by image and non-image
     * 
     * @param target
     * @param crs
     * @param range
     * @return
     */
    public IWireframeShape computeRangeCircle(IGraphicsTarget target,
            CoordinateReferenceSystem crs, double range) {
        IWireframeShape rangeCircle = target.createWireframeShape(true,
                descriptor);

        try {
            MathTransform mt = CRS.findMathTransform(crs,
                    MapUtil.getLatLonProjection());

            double[][] pts = new double[RANGE_CIRCLE_PTS + 1][2];
            double azDelta = 2 * Math.PI / RANGE_CIRCLE_PTS;
            double az = 0.0;
            double[] input = new double[2];
            double[] output = new double[2];
            for (int i = 0; i < pts.length; i++) {
                input[0] = range * Math.cos(az);
                input[1] = range * Math.sin(az);
                mt.transform(input, 0, output, 0, 1);
                pts[i] = descriptor.worldToPixel(output);
                az += azDelta;
            }
            pts[RANGE_CIRCLE_PTS] = pts[0];

            rangeCircle.allocate(pts.length);
            rangeCircle.addLineSegment(pts);
        } catch (TransformException e) {
            statusHandler.handle(Priority.PROBLEM,
                    "Unable to compute the range circle", e);
            return null;
        } catch (FactoryException e) {
            statusHandler.handle(Priority.PROBLEM,
                    "Unable to compute the range circle", e);
            return null;
        }

        return rangeCircle;
    }

    protected byte[] createConversionTable(ColorMapParameters params,
            RadarRecord record) {
        // Sometimes the data unit may not match what is in the params so always
        // use what we really have
        UnitConverter dataToImage = null;
        Unit<?> dataUnit = DataUtilities.getDataUnit(record);
        if (dataUnit != null && !dataUnit.equals(params.getDataUnit())) {
            Unit<?> imageUnit = params.getImageUnit();
            if (imageUnit != null && dataUnit.isCompatible(imageUnit)) {
                dataToImage = dataUnit.getConverterTo(imageUnit);
            } else if (imageUnit != null) {
                dataUnit = DataUtilities.getDataUnit(record, ((RadarResourceData)resourceData).mode);
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
                        // a graceful separation of data and representation but
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
                            // table[i] = (byte) 0;
                            // If we want small values to appear as the lowest
                            // data value than do this next line instead
                            // This was changed for the DUA product so
                            // differences less than -5 get mapped to a data
                            // value.
                            table[i] = (byte) j;
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
        return table;
    }

    protected IImage createImage(IGraphicsTarget target,
            ColorMapParameters params, RadarRecord record, Rectangle rect)
            throws VizException {
        byte[] table = createConversionTable(params, record);
        return target
                .getExtension(IColormappedImageExtension.class)
                .initializeRaster(
                        new RadarImageDataRetrievalAdapter(record, table, rect),
                        params);
    }

    public IMesh buildMesh(IGraphicsTarget target, VizRadarRecord radarRecord)
            throws VizException {
        return null;
    }

    public PixelCoverage buildCoverage(IGraphicsTarget target,
            VizRadarRecord timeRecord) throws VizException {
        return new PixelCoverage(new Coordinate(0, 0), 0, 0);
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

    public void redoImage(DataTime time) {
        disposeImage(images.remove(time));
    }

    protected void disposeImage(DrawableImage image) {
        if (image != null) {
            image.dispose();
        }
    }    

    // the colorBar and/or the colormap may have changed so update the 
    // colorBarPainter and the colorMapParametersCapability which holds
    // the instance of the colorMap that Raytheon's code needs
	@Override
	public void resourceAttrsModified() {
		
		// update the colorbarPainter with a possibly new colorbar
    	ColorBarFromColormap colorBar = ((RadarResourceData)resourceData).getColorBar();

    	cbarResource.setColorBar( colorBar );
		    	    	
    	ColorMapParameters cmapParams = getCapability(ColorMapCapability.class).getColorMapParameters();
    	cmapParams.setColorMap( colorBar.getColorMap());
    	cmapParams.setColorMapName( ((RadarResourceData)resourceData).getColorMapName() );
    			
    	getCapability(ColorMapCapability.class).setColorMapParameters( cmapParams );
        refreshImage = true;
        
        // TODO : how to migrate this to to11dr11? Or do we still need to do this?
        // baseTile.resourceChanged(ChangeType.CAPABILITY, this.getCapability( ColorMapCapability.class));
	}

    /*@Override
    public void remove(DataTime dataTime) {
        super.remove(dataTime);
        final DrawableImage image = images.remove(dataTime);
        if (image == null) {
            return;
        }
        // Run this in the UI thread to avoid accidentally disposing of things
        // that are painting. This is better than synchronizing because it makes
        // it much more difficult to deadlock.
        VizApp.runAsync(new Runnable() {
            @Override
            public void run() {
                disposeImage(image);
            }
        });
    }*/
	
	@Override
    public void propertiesChanged(ResourceProperties updatedProps) {
    	
    	if( cbarRscPair != null ) {
    		cbarRscPair.getProperties().setVisible( updatedProps.isVisible() );
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
        		((RadarResourceData)resourceData).setAlpha(  imgCap.getAlpha()  );
        		((RadarResourceData)resourceData).setBrightness(  imgCap.getBrightness() );
        		((RadarResourceData)resourceData).setContrast(  imgCap.getContrast() );
        		issueRefresh();        		
        		
        		
        	}
        	else if (object instanceof ColorMapCapability ){
        		
        		ColorMapCapability colorMapCap = getCapability(ColorMapCapability.class);
        		ColorMapCapability newColorMapCap = (ColorMapCapability) object;
        		ColorMapParameters newColorMapParms = newColorMapCap.getColorMapParameters();
        		                
        		if (newColorMapParms != null) {        			
	        		colorMapCap.setColorMapParameters(newColorMapParms, false);
	        		ColorMap theColorMap = ( ColorMap ) colorMapCap.getColorMapParameters().getColorMap();
	        		String colorMapName = colorMapCap.getColorMapParameters().getColorMapName();
	        		((RadarResourceData)resourceData).setColorMapName( colorMapName );
	        		((RadarResourceData)resourceData).getRscAttrSet().setAttrValue( "colorMapName", colorMapName );
	        	    ColorBarFromColormap cBar = ((RadarResourceData)resourceData).getColorBar();
	        	    cBar.setColorMap( theColorMap );
	        	    ((RadarResourceData)resourceData).getRscAttrSet().setAttrValue( "colorBar", cBar );
	        	    ((RadarResourceData)resourceData).setIsEdited( true );        	    
	        	    
	        	    refreshImage = true;
	        		issueRefresh();
        		}

        	}

        }

	}

   
    protected static class RadarImageDataRetrievalAdapter implements
            IColorMapDataRetrievalCallback {

        protected final RadarRecord record;

        protected final byte[] table;

        protected Rectangle rect;
        
        private final int hashCode;

        public RadarImageDataRetrievalAdapter(RadarRecord record, byte[] table,
        		Rectangle rect) {
            this.record = record;
            this.table = table;
            this.rect = rect;

            final int prime = 31;
            int hashCode = 1;
            hashCode = prime * hashCode
                    + ((record == null) ? 0 : record.hashCode());
            hashCode = prime * hashCode + Arrays.hashCode(table);
            hashCode = prime * hashCode + rect.hashCode();
            this.hashCode = hashCode;
        }

        @Override
        public ColorMapData getColorMapData() {
            return new ColorMapData(ByteBuffer.wrap(convertData()), new int[] {
                    rect.width, rect.height });
        }
        
        public byte[] convertData() {
            return record.getRawData();
        }

        protected boolean createCircle(byte[] imageData, int h, int w, int i) {
            // do nothing
            return false;
        }
        
        @Override
        public int hashCode() {
            return hashCode;
        }

        @Override
        public boolean equals(Object obj) {
            if (this == obj)
                return true;
            if (obj == null)
                return false;
            if (getClass() != obj.getClass())
                return false;
            RadarImageDataRetrievalAdapter other = (RadarImageDataRetrievalAdapter) obj;
            if (record == null) {
                if (other.record != null)
                    return false;
            } else if (!record.equals(other.record))
                return false;
            if (!Arrays.equals(table, other.table))
                return false;
            return true;
        }

    }

}

