/*
 * gov.noaa.nws.ncep.viz.rsc.mosaic.rsc.AbstractNatlCntrsResource
 * 
 * 03-03-2011
 *
 * This code has been developed by the NCEP/SIB for use in the AWIPS2 system.
 */

package gov.noaa.nws.ncep.viz.rsc.mosaic.rsc;


import gov.noaa.nws.ncep.viz.resources.AbstractNatlCntrsResource;
import gov.noaa.nws.ncep.viz.ui.display.NCMapDescriptor;

import java.io.File;
import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.Map;

import javax.measure.unit.NonSI;

import org.geotools.referencing.CRS;
import org.opengis.referencing.FactoryException;
import org.opengis.referencing.crs.CoordinateReferenceSystem;
import org.opengis.referencing.operation.MathTransform;
import org.opengis.referencing.operation.TransformException;

import com.raytheon.uf.common.dataplugin.IDecoderGettable.Amount;
import com.raytheon.uf.common.dataplugin.PluginDataObject;
import com.raytheon.uf.common.dataplugin.radar.RadarRecord;
import com.raytheon.uf.common.dataplugin.radar.util.RadarInfoDict;
import com.raytheon.uf.common.geospatial.MapUtil;
import com.raytheon.uf.common.geospatial.ReferencedCoordinate;
import com.raytheon.uf.common.localization.PathManagerFactory;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.common.time.DataTime;
import com.raytheon.uf.viz.core.IGraphicsTarget;
import com.raytheon.uf.viz.core.cache.CacheObject;
import com.raytheon.uf.viz.core.drawables.IDescriptor;
import com.raytheon.uf.viz.core.drawables.IWireframeShape;
import com.raytheon.uf.viz.core.drawables.PaintProperties;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.map.MapDescriptor;
import com.raytheon.uf.viz.core.rsc.IResourceDataChanged;
import com.raytheon.uf.viz.core.rsc.LoadProperties;
import com.raytheon.uf.viz.core.rsc.capabilities.ColorableCapability;
import com.raytheon.uf.viz.core.status.StatusConstants;
import com.raytheon.uf.viz.d2d.core.map.IDataScaleResource;
import com.raytheon.viz.awipstools.capabilityInterfaces.IRangeableResource;
import com.raytheon.viz.radar.IRadarConfigListener;
import com.raytheon.viz.radar.IRadarRecordMetadata;
import com.raytheon.viz.radar.RadarRecordDataRetriever;
import com.raytheon.viz.radar.RadarRecordMetadata;
import com.raytheon.viz.radar.RadarTimeRecord;
import com.raytheon.viz.radar.rsc.RadarTextResource.IRadarTextGeneratingResource;
import com.raytheon.viz.radar.ui.RadarDisplayManager;
import com.vividsolutions.jts.geom.Coordinate;

/**
 * Top level radar resource that contains the code that is shared by all below
 * resources.
 * 
 * This class is based on Raytheon's code.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Mar. 3, 2011            G. Zhang	   Initial creation
 * 
 * </pre>
 * 
 * @author gzhang
 * @version 1.0
 */

public abstract class AbstractRadarResource<D extends IDescriptor> extends AbstractNatlCntrsResource<RadarResourceData, NCMapDescriptor> 
        implements IRadarConfigListener, IResourceDataChanged, IRangeableResource,
                   IDataScaleResource, IRadarTextGeneratingResource {
	
    protected static final IUFStatusHandler statusHandler = UFStatus.getHandler(AbstractRadarResource.class);
    
	protected static final IUFStatusHandler radarHandler = UFStatus.getHandler(AbstractRadarResource.class, "Radar");
	
	protected static final IUFStatusHandler dataHandler = UFStatus.getHandler(AbstractRadarResource.class, StatusConstants.SUBCATEGORY_DATAAVAIL);

//    public com.raytheon.viz.radar.interrogators.IRadarInterrogator interrogator;

    protected int productCode;

    public String icao;

    private static final int RANGE_CIRCLE_PTS = 360;

    protected DataTime displayedDate;

    protected float displayedLevel = -1;

    protected String actualLevel = "";

    protected Map<DataTime, RadarTimeRecord> radarRecords;

    protected RadarRecord baseRecord = null;


    protected Map<RadarTimeRecord, String[]> radarInfoText = new HashMap<RadarTimeRecord, String[]>();

    public boolean refreshDisplay = false;

    public Map<Float, IWireframeShape> rangeCircle;

    protected String mode;

    /**
     * @param resourceData
     * @param loadProperties
     */
    protected AbstractRadarResource(RadarResourceData resourceData,
            LoadProperties loadProperties) {
        super(resourceData, loadProperties);
        getCapability(ColorableCapability.class);
        resourceData.addChangeListener(this);
        RadarDisplayManager.getInstance().addListener(this);

        dataTimes = new ArrayList<DataTime>();
        radarRecords = Collections
                .synchronizedMap(new HashMap<DataTime, RadarTimeRecord>());
        icao = "";
        productCode = 0;
    }

    @Override
    protected void disposeInternal() {
    }

    /**
     * @param record
     */
    protected void addRecord(PluginDataObject record) {
        if (!(record instanceof RadarRecord)) {
            statusHandler.handle(Priority.PROBLEM, ""
                            + this.getClass().getName() + " expected : "
                            + RadarRecord.class.getName() + " Got: " + record);
            return;
        }
        RadarRecord radarRecord = (RadarRecord) record;
        if (baseRecord == null) {
            baseRecord = radarRecord;
        }
        radarRecord.setAddSpatial(false);//!resourceData.latest);
        icao = radarRecord.getIcao();
        productCode = radarRecord.getProductCode();
        DataTime d = radarRecord.getDataTime();

        RadarTimeRecord radarTimeRecord = radarRecords.get(d);
        if (radarTimeRecord == null) {
        	radarTimeRecord = new RadarTimeRecord();
        	radarTimeRecord.radarCacheObject = CacheObject.newCacheObject(
        			new RadarRecordMetadata(radarRecord),
        			new RadarRecordDataRetriever());
        	radarRecords.put(d, radarTimeRecord);

            dataTimes.add(d);
            Collections.sort(dataTimes);
        } else {
            // Use the newest one
        	IRadarRecordMetadata existing = radarTimeRecord.radarCacheObject.getMetadata();
        	if (existing.getInsertTime().getTimeInMillis() < radarRecord
        			.getInsertTime().getTimeInMillis()) {
        		radarTimeRecord = new RadarTimeRecord();
                radarTimeRecord.radarCacheObject = CacheObject.newCacheObject(
                		new RadarRecordMetadata(radarRecord),
                		new RadarRecordDataRetriever());
                radarRecords.put(d, radarTimeRecord);
            }
        }
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.viz.core.rsc.AbstractVizResource#paintInternal(com.raytheon
     * .uf.viz.core.IGraphicsTarget,
     * com.raytheon.uf.viz.core.drawables.PaintProperties)
     */
//    @Override
//    protected void paintInternal(IGraphicsTarget target,
//            PaintProperties paintProps) throws VizException {
//    }

    protected boolean handleProductSpecificPaint(IGraphicsTarget target,
            PaintProperties paintProps, RadarTimeRecord tiltRecord)
            throws VizException {
        return false;
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
            MathTransform mt = CRS.findMathTransform(crs, MapUtil
                    .getLatLonProjection());

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

            rangeCircle.addLineSegment(pts);
        } catch (TransformException e) {
            dataHandler.handle(Priority.PROBLEM,
                    "Unable to compute the range circle", e);
            return null;
        } catch (FactoryException e) {
            dataHandler.handle(Priority.PROBLEM,
                    "Unable to compute the range circle", e);
            return null;
        }

        return rangeCircle;
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.viz.radar.IRadarConfigListener#updateConfig()
     */
    @Override
    public void updateConfig() {
        refreshDisplay = true;
        this.issueRefresh();

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

        return inspect(dataMap);
    }

    /**
     * Given the map of data values, return the inspection string
     * 
     * @param dataMap
     * @return
     */
    public String inspect(Map<String, String> dataMap) {
        if (dataMap == null) {
            return "NO DATA";
        }

        StringBuffer displayedData = new StringBuffer();

        displayedData.append(dataMap.get("Value"));

        if (dataMap.containsKey("Shear")) {
            displayedData.append(" " + dataMap.get("Shear"));
        }

        if (dataMap.containsKey("MSL")) {
            displayedData.append(" " + dataMap.get("MSL") + "MSL");
            displayedData.append(" " + dataMap.get("AGL") + "AGL");
        }

        if (dataMap.containsKey("Azimuth")) {
            displayedData.append(" " + dataMap.get("Range"));
            displayedData.append("@" + dataMap.get("Azimuth"));
        }

        displayedData.append(' ').append(dataMap.get("ICAO"));

        if (displayedData.toString().contains("null")) {
            displayedData.replace(0, displayedData.length(), "NO DATA");
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

        if (smap != null) {
            for (String smapKey : smap.keySet()) {
                String value = smap.get(smapKey);
                rmap.put(smapKey, value);
            }
        }
        return rmap;
    }

    public Map<String, String> interrogate(Coordinate latLon) {
//        if (interrogator == null) {
//            return null;
//        }
//        ColorMapParameters params = getCapability(ColorMapCapability.class)
//                .getColorMapParameters();
//
//        if (displayedDate == null) {
//            displayedDate = descriptor.getTimeForResource(this);
//        }
//
//        RadarRecord radarRecord = getRadarRecord(displayedDate);
//        if (radarRecord == null) {
//            return null;
//        }
//        Map<String, String> dataMap = interrogator.sample(radarRecord, latLon,
//                params);
        return new HashMap<String,String>();//dataMap;
    }

    /**
     * Given the dataTime, returns the upper text info for that time
     * 
     * @param time
     * @return
     */
    public String[] getUpperText(DataTime time) {
//        RadarRecord record = getRadarRecord(time);
//        if (record == null) {
//            return null;
//        }
//        List<IRadarTextContributor> lines = UpperTextSet.getContributors(record
//                .getProductCode());
//        if (lines == null) {
//            return null;
//        }
//        String[] result = new String[lines.size()];
//        for (int i = 0; i < result.length; i++) {
//            result[i] = lines.get(i).contributeText(record);
//        }
//        // Remove blank lines from the end.
//        while (result[result.length - 1].isEmpty()) {
//            result = Arrays.copyOfRange(result, 0, result.length - 1);
//        }
return new String[]{};//result;
    }

    public CacheObject<? extends IRadarRecordMetadata, RadarRecord> getRadarRecord(DataTime time) {
        RadarTimeRecord record = radarRecords.get(time);
        if (record == null) {
            return null;
        }
        return record.radarCacheObject;
    }

    public Map<DataTime, RadarTimeRecord> getRadarRecords() {
        return radarRecords;
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.viz.awipstools.capabilityInterfaces.IRangeableResource#getCenter
     * ()
     */
    @Override
    public Coordinate getCenter() {
        RadarTimeRecord record = radarRecords.get(displayedDate);

        if (record != null) {
        	IRadarRecordMetadata radarRecord = record.radarCacheObject.getMetadata();
            if (record.tile != null) {
                return new Coordinate(radarRecord.getLongitude(), radarRecord
                        .getLatitude());
            }
        }
        return new Coordinate();
    }

    /*
     * (non-Javadoc)
     * 
     * @seecom.raytheon.viz.awipstools.capabilityInterfaces.IRangeableResource#
     * getElevation()
     */
    @Override
    public Amount getElevation() {
        return new Amount(0.0, NonSI.FOOT);
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.viz.awipstools.capabilityInterfaces.IRangeableResource#getTilt
     * ()
     */
    @Override
    public double getTilt() {
        double tilt = 0.0;
        if (displayedDate != null) {
            tilt = displayedDate.getLevelValue();
        }
        return tilt;
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.viz.d2d.core.map.IDataScaleResource#getCenterLocation()
     */
    @Override
    public Coordinate getCenterLocation() {
        return baseRecord != null ? new Coordinate(baseRecord.getLongitude(),
                baseRecord.getLatitude()) : null;
    }

    public String getMode() {
        return mode;
    }

    public void setMode(String mode) {
        this.mode = mode;
    }
}


