/*
 * gov.noaa.nws.ncep.viz.rsc.ncradar.rsc.image.RadarRadialResource
 * 
 * 12-07-2011
 *
 * This code has been developed by the NCEP/SIB for use in the AWIPS2 system.
 */
package gov.noaa.nws.ncep.viz.rsc.ncradar.rsc.image;

import java.awt.Rectangle;
import java.util.HashMap;
import java.util.Map;

import javax.measure.unit.NonSI;

import org.opengis.referencing.crs.CoordinateReferenceSystem;

import com.raytheon.uf.common.dataplugin.IDecoderGettable.Amount;
import com.raytheon.uf.common.dataplugin.radar.RadarRecord;
import com.raytheon.uf.common.dataquery.requests.RequestConstraint;
import com.raytheon.uf.common.time.DataTime;
import com.raytheon.uf.viz.core.DrawableImage;
import com.raytheon.uf.viz.core.IGraphicsTarget;
import com.raytheon.uf.viz.core.IMesh;
import com.raytheon.uf.viz.core.catalog.LayerProperty;
import com.raytheon.uf.viz.core.catalog.ScriptCreator;
import com.raytheon.uf.viz.core.comm.Connector;
import com.raytheon.uf.viz.core.drawables.ColorMapParameters;
import com.raytheon.uf.viz.core.drawables.IImage;
import com.raytheon.uf.viz.core.drawables.ext.colormap.IColormappedImageExtension;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.map.IMapDescriptor;
import com.raytheon.uf.viz.core.map.MapDescriptor;
import com.raytheon.uf.viz.core.rsc.LoadProperties;
import com.raytheon.uf.viz.core.rsc.ResourceType;
import com.raytheon.viz.awipstools.capabilities.EAVCapability;
import com.raytheon.viz.awipstools.common.EstimatedActualVelocity;

import gov.noaa.nws.ncep.viz.rsc.ncradar.VizRadarRecord;
import com.raytheon.viz.radar.interrogators.IRadarInterrogator;
import gov.noaa.nws.ncep.viz.rsc.ncradar.rsc.RadarImageResource;
import gov.noaa.nws.ncep.viz.rsc.ncradar.rsc.RadarResourceData;
import com.raytheon.viz.radar.rsc.image.IRadialMeshExtension;

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
 * 12/09/2011   #541       S. Gurung   Initial creation
 * 12/16/2011              S. Gurung   Removed resourceAttrsModified()
 * 03/30/2012   #651       S. Gurung   Removed method resourceChanged
 * 09-04-2012              B. Hebbard  Add getGridGeometry() to descriptor per OB12.9.1 RTS
 *                                     change IRadialMeshExtension.constructMesh 2nd param
 * 
 * </pre>
 * 
 * @author sgurung
 * @version 1.0
 */

public class RadarRadialResource extends RadarImageResource<MapDescriptor> {

    private static final String EAV_VALUE = "EAC.Value";

    /**
     * @param rrd
     * @param loadProps
     * @throws VizException
     */
    public RadarRadialResource(RadarResourceData rrd, LoadProperties loadProps,
            IRadarInterrogator interrogator) throws VizException {
        super(rrd, loadProps, interrogator);
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.viz.radar.rsc.AbstractRadarResource#getElevation()
     */
    @Override
    public Amount getElevation() {
        RadarRecord radarRecord = getRadarRecord(displayedDate);

        if (radarRecord != null) {
            return new Amount(radarRecord.getElevation(), NonSI.FOOT);
        }
        return super.getElevation();
    }

    @Override
    protected IImage createImage(IGraphicsTarget target,
            ColorMapParameters params, RadarRecord record, Rectangle rect)
            throws VizException {
        byte[] table = createConversionTable(params, record);
        return target
                .getExtension(IColormappedImageExtension.class)
                .initializeRaster(
                        new RadarRadialDataRetrievalAdapter(record, table, rect),
                        params);
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * gov.noaa.nws.ncep.viz.rsc.ncradar.rsc.RadarImageResource#project(org.opengis.referencing
     * .crs.CoordinateReferenceSystem)
     */
    @Override
    public void project(CoordinateReferenceSystem crs) throws VizException {
        super.project(crs);
        // TODO dispose just the coverage, not the image.
        for (DrawableImage image : images.values()) {
            if (image != null) {
                image.dispose();
            }
        }
        images.clear();
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

        // add EAV values to dataMap, if necessary
        if (hasCapability(EAVCapability.class)) {
            EAVCapability eavCap = getCapability(EAVCapability.class);
            if (eavCap.isCapabilityActive()) {
                if (dataMap != null) {
                    EstimatedActualVelocity eav = eavCap.getEav();
                    dataMap.put(EAV_VALUE, eav.getEAVValue(latLon,
                            new HashMap<String, Object>(dataMap)));
                }
            }
        }
        return dataMap;
    }

    protected static class RadarRadialDataRetrievalAdapter extends
            RadarImageDataRetrievalAdapter {

        public RadarRadialDataRetrievalAdapter(RadarRecord record, byte[] table,
        		Rectangle rect) {
            super(record, table, rect);
        }

        @Override
        public byte[] convertData() {
            byte[] imageData = new byte[record.getNumBins()
                    * record.getNumRadials()];
            int i = 0;
            for (int h = 0; h < record.getNumRadials(); ++h) {
                for (int w = 0; w < record.getNumBins(); ++w) {
                    if (!createCircle(imageData, h, w, i)) {
                        imageData[i] = table[record.getRawIntDataValue(h, w)];
                    }
                    ++i;
                }
            }
            return imageData;
        }

        @Override
        protected boolean createCircle(byte[] imageData, int h, int w, int i) {
            if (w == 0) {
                imageData[0] = (byte) 0;
                if (record.getRawData() != null) {
                    record.getRawData()[i] = (byte) 0;
                }
                if (record.getRawShortData() != null) {
                    record.getRawShortData()[i] = (byte) 0;
                }
                return true;
            }
            return false;
        }

    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * gov.noaa.nws.ncep.viz.rsc.ncradar.rsc.RadarImageResource#buildMesh(com.raytheon.
     * uf.viz.core.IGraphicsTarget, gov.noaa.nws.ncep.viz.rsc.ncradar.VizRadarRecord)
     */
    @Override
    public IMesh buildMesh(IGraphicsTarget target, VizRadarRecord radarRecord)
            throws VizException {
        return target.getExtension(IRadialMeshExtension.class).constructMesh(
                radarRecord, ((IMapDescriptor) descriptor).getGridGeometry());
    }

	@Override
	public void updateConfig() {
		// TODO Auto-generated method stub
		
	}

	
	protected HashMap<String, RequestConstraint> queryList;	

	@Override
	public void queryRecords() throws VizException {

		queryList = new HashMap<String, RequestConstraint>(
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
				newRscDataObjsQueue.add(dataObject);
			}
			/*pdos.add((PluginDataObject)pdo);*/		
		}
		
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
	   

}

