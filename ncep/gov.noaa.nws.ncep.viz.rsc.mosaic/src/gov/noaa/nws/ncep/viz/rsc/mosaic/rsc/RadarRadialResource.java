/*
 * gov.noaa.nws.ncep.viz.rsc.mosaic.rsc.RadarRadialResource
 * 
 * 03-04-2011
 *
 * This code has been developed by the NCEP/SIB for use in the AWIPS2 system.
 */

package gov.noaa.nws.ncep.viz.rsc.mosaic.rsc;

import gov.noaa.nws.ncep.viz.ui.display.ColorBarFromColormap;

import java.text.DecimalFormat;
import java.util.HashMap;
import java.util.Map;

import javax.measure.unit.NonSI;

import org.geotools.referencing.CRS;
import org.geotools.referencing.crs.DefaultGeographicCRS;
import org.opengis.referencing.FactoryException;
import org.opengis.referencing.crs.CoordinateReferenceSystem;

import com.raytheon.uf.common.colormap.ColorMap;
import com.raytheon.uf.common.dataplugin.IDecoderGettable.Amount;
import com.raytheon.uf.common.dataplugin.radar.RadarRecord;
import com.raytheon.uf.common.dataquery.requests.RequestConstraint;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.common.time.DataTime;
import com.raytheon.uf.viz.core.IGraphicsTarget;
import com.raytheon.uf.viz.core.catalog.LayerProperty;
import com.raytheon.uf.viz.core.catalog.ScriptCreator;
import com.raytheon.uf.viz.core.comm.Connector;
import com.raytheon.uf.viz.core.drawables.ColorMapParameters;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.map.MapDescriptor;
import com.raytheon.uf.viz.core.rsc.LoadProperties;
import com.raytheon.uf.viz.core.rsc.ResourceProperties;
import com.raytheon.uf.viz.core.rsc.ResourceType;
import com.raytheon.uf.viz.core.rsc.IResourceDataChanged.ChangeType;
import com.raytheon.uf.viz.core.rsc.capabilities.ColorMapCapability;
import com.raytheon.uf.viz.core.rsc.capabilities.ImagingCapability;
import com.raytheon.viz.awipstools.capabilities.EAVCapability;
import com.raytheon.viz.awipstools.common.EstimatedActualVelocity;
import com.raytheon.viz.radar.IRadarRecordMetadata;
import com.raytheon.viz.radar.RadarTimeRecord;
import com.vividsolutions.jts.geom.Coordinate;

/**
 * Based on Raytheon's code
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 03-04-2011              G. Zhang     Initial creation
 * 12/19/2012     #960     Greg Hull   override propertiesChanged() to update colorBar.
 * 
 * </pre>
 * 
 * @author gzhang
 * @version 1.0
 */

public class RadarRadialResource  extends RadarImageResource<MapDescriptor> {
	
    private static String EAV_VALUE = "EAC.Value";

    /**
     * @param rrd
     * @param loadProps
     * @throws VizException
     */
    public RadarRadialResource(RadarResourceData rrd, LoadProperties loadProps//,
    		/*com.raytheon.viz.radar.interrogators.IRadarInterrogatorIRadarInterrogatorinterrogator*/ ) throws VizException {
        super(rrd, loadProps/*);//, interrogator*/);
        this.radarRscData = rrd;
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.viz.radar.rsc.AbstractRadarResource#getElevation()
     */
    @Override
    public Amount getElevation() {
        RadarTimeRecord record = radarRecords.get(displayedDate);

        if (record != null) {
        	IRadarRecordMetadata radarRecord = record.radarCacheObject.getMetadata();
        	if (record.tile != null) {
        		return new Amount(radarRecord.getElevation(), NonSI.FOOT);
        	}
        }
        return super.getElevation();
    }
    
    public String getTrueElevText(){
        RadarTimeRecord record = radarRecords.get(displayedDate);
        String s = "";//matching: x.x DEG
        if (record != null) {
        	IRadarRecordMetadata radarRecord = record.radarCacheObject.getMetadata();
            if (record != null) {
            	DecimalFormat df = new DecimalFormat("0.0");
                
            	return df.format( radarRecord.getTrueElevationAngle()) + NonSI.DEGREE_ANGLE.toString();                
            }
        }
        return s;    	
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.viz.radar.rsc.RadarImageResource#createCircle(byte[])
     */
    @Override
    protected boolean createCircle(byte[] imageData, RadarRecord record, int h,
            int w, int i) {
        int height = record.getNumBins();

        return false;
    }

    @Override
    public byte[] convertData(RadarRecord record, byte[] table) {
        int i = 0;
        int width = record.getNumRadials();
        int height = record.getNumBins();
        byte[] imageData = new byte[width * height];
        for (int h = 0; h < height; ++h) {
            for (int w = 0; w < width; ++w) {
                if (!createCircle(imageData, record, height - h - 1, w, w
                        * record.getNumBins() + h)) {
                    imageData[i] = table[record.getRawIntDataValue(w, height
                            - h - 1)];
                }
                i++;
            }
        }
        return imageData;
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.viz.radar.rsc.RadarImageResource#project(org.opengis.referencing
     * .crs.CoordinateReferenceSystem)
     */
    @Override
    public void project(CoordinateReferenceSystem crs) throws VizException {
        super.project(crs);
        for (RadarTimeRecord rtr : radarRecords.values()) {
            if (rtr.tile != null && rtr.tile.coverage.getMesh() != null) {
                rtr.tile.coverage.getMesh().reproject(
                        descriptor.getGridGeometry());
            } else {
                refreshDisplay = true;
            }
        }
    }

    @Override
    public String inspect(Map<String, String> dataMap) {
        StringBuilder sb = new StringBuilder(super.inspect(dataMap));

        if (dataMap != null && dataMap.containsKey(EAV_VALUE)) {
            sb.append(" ").append(dataMap.get(EAV_VALUE));
        }
        return sb.toString();
    }

    @Override
    public Map<String, String> interrogate(Coordinate latLon) {
        Map<String, String> dataMap = super.interrogate(latLon);

        // add EAC values to dataMap, if necessary
        if (hasCapability(EAVCapability.class)) {
            EAVCapability eavCap = getCapability(EAVCapability.class);
            if (eavCap.isCapabilityActive()) {
                double numericValue = 0;

                if (dataMap != null) {
                    if (dataMap.get("numericValue") != null) {
                        numericValue = Double.parseDouble((String) dataMap
                                .get("numericValue"));
                    }
                    EstimatedActualVelocity eav = eavCap.getEav();
//dataMap.put(EAV_VALUE,    eav.getEAVValue(latLon, numericValue));
                }
            }
        }
        return dataMap;
    }
    

protected HashMap<String, RequestConstraint> queryList;//for legend DEG 2011-03-14	

	@Override
	public void queryRecords() throws VizException {

		/*HashMap<String, RequestConstraint>*/ queryList = new HashMap<String, RequestConstraint>(
				resourceData.getMetadataMap());

		LayerProperty prop = new LayerProperty();
		prop.setDesiredProduct(ResourceType.PLAN_VIEW);
		prop.setEntryQueryParameters(queryList, false);
		prop.setNumberOfImages(15000); // TODO: max # records ?? should we cap
										// this ?
		String script = null;
		script = ScriptCreator.createScript(prop);

		if (script == null)
			return;

		Object[] pdoList = Connector.getInstance().connect(script, null, 60000);
//ArrayList<PluginDataObject> pdos = new ArrayList<PluginDataObject>();
		for (Object pdo : pdoList) {
			for( IRscDataObject dataObject : processRecord( pdo ) )	{	
				newRscDataObjsQueue.add(dataObject);//LOG.info("--- dataObject: "+dataObject.getDataTime());
			}
/*pdos.add((PluginDataObject)pdo);*/		}
		
//resourceChanged(ChangeType.DATA_UPDATE, pdos.toArray(new PluginDataObject[]{}));//see getRadarRecord(DataTime)&Map<DataTime, RadarTimeRecord> radarRecords of AbstractRadarResource
	}

	@Override
	public void initResource(IGraphicsTarget target) throws VizException {
		
		synchronized (this) {
			super.initResource( target );
			
			this.viewType = target.getViewType();
            this.grphTarget = target;            
			queryRecords();       	
		}		
	}


	@Override
	protected gov.noaa.nws.ncep.viz.resources.AbstractNatlCntrsResource.AbstractFrameData createNewFrame(
			DataTime frameTime, int frameInterval) {
		// TODO Auto-generated method stub
		return new FrameData(frameTime, frameInterval );//return null;
	}
	
	
    // the colorBar and/or the colormap may have changed so update the 
    // colorBarPainter and the colorMapParametersCapability which holds
    // the instance of the colorMap that Raytheon's code needs
	@Override
	public void resourceAttrsModified() {
		// update the colorbarPainter with a possibly new colorbar
    	ColorBarFromColormap colorBar = radarRscData.getColorBar();

    	cbarResource.setColorBar( colorBar );
		    	    	
    	ColorMapParameters cmapParams = getCapability(ColorMapCapability.class).getColorMapParameters();
    	cmapParams.setColorMap( colorBar.getColorMap());
    	cmapParams.setColorMapName( radarRscData.getColorMapName() );
    			
        getCapability(ColorMapCapability.class).setColorMapParameters( cmapParams );

        // TODO : how to migrate this to to11dr11? Or do we still need to do this?
//		baseTile.resourceChanged(ChangeType.CAPABILITY, this.getCapability( ColorMapCapability.class));
	}

	@Override
    public void propertiesChanged(ResourceProperties updatedProps) {

		cbarRscPair.getProperties().setVisible( updatedProps.isVisible() );
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
}