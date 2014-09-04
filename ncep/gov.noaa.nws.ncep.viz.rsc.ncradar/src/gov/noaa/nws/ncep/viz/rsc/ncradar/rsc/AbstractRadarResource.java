/*
 * gov.noaa.nws.ncep.viz.rsc.ncradar.rsc.AbstractRadarResource
 * 
 * 12-07-2011
 *
 * This code has been developed by the NCEP/SIB for use in the AWIPS2 system.
 */

package gov.noaa.nws.ncep.viz.rsc.ncradar.rsc;

import gov.noaa.nws.ncep.viz.resources.AbstractNatlCntrsResource;
import gov.noaa.nws.ncep.viz.ui.display.NCMapDescriptor;

import java.io.File;
import java.util.ArrayList;
import java.util.Date;
import java.util.HashMap;

import javax.measure.unit.NonSI;

import com.raytheon.uf.common.dataplugin.IDecoderGettable.Amount;
import com.raytheon.uf.common.dataplugin.PluginDataObject;
import com.raytheon.uf.common.dataplugin.radar.RadarRecord;
import com.raytheon.uf.common.dataplugin.radar.util.RadarInfoDict;
import com.raytheon.uf.common.dataquery.requests.RequestConstraint;
import com.raytheon.uf.common.localization.PathManagerFactory;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.common.time.DataTime;
import com.raytheon.uf.viz.core.catalog.LayerProperty;
import com.raytheon.uf.viz.core.catalog.ScriptCreator;
import com.raytheon.uf.viz.core.comm.Connector;
import com.raytheon.uf.viz.core.drawables.IDescriptor;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.rsc.IResourceDataChanged;
import com.raytheon.uf.viz.core.rsc.LoadProperties;
import com.raytheon.uf.viz.core.rsc.ResourceType;
import com.raytheon.uf.viz.core.rsc.capabilities.ColorMapCapability;
import com.raytheon.uf.viz.core.rsc.capabilities.ColorableCapability;
import com.raytheon.uf.viz.core.rsc.capabilities.ImagingCapability;
import com.raytheon.viz.awipstools.capabilityInterfaces.IRangeableResource;
import com.raytheon.viz.radar.DefaultVizRadarRecord;
import com.raytheon.viz.radar.VizRadarRecord;
import com.vividsolutions.jts.geom.Coordinate;

/**
 * Top level radar resource that contains the code that is shared by all below
 * resources
 * 
 * This class is based on Raytheon's code.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 12/07/2011    #541      S. Gurung   Initial creation
 * 02/01/13      #972      G. Hull     define on IDescriptor
 * 06/07/13      #999      G. Hull     removed displayedLevel, displayedDate.moved RadarRecord from
 *                                     radarRecords to RadarFrameData. add queryRecords based on timeline.
 * 06/10/13      #999      G. Hull     rm interrogate and inspect. (add back when supported by NCP.)
 * 06/10/13      #999      G. Hull     rm IRadarTextGeneratingResource and IRadarConfigListener since not supported by NCP.
 * 06/10/2013    #999      G. Hull     rm IDataScaleResource
 * 06/18/2014    TTR1026   J. Wu (/bh) Fixed potential exceptions for loading local radar data.
 * 
 * </pre>
 * 
 * @author sgurung
 * @version 1.0
 */

public abstract class AbstractRadarResource<D extends IDescriptor> extends
        AbstractNatlCntrsResource<RadarResourceData, NCMapDescriptor> implements
        IResourceDataChanged, IRangeableResource {
    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(AbstractRadarResource.class);

    public String icao;

    protected String actualLevel = "";

    protected Coordinate centerLocation = null;

    protected static final RadarInfoDict infoDict;

    static {
        File radarInfo = PathManagerFactory.getPathManager().getStaticFile(
                "radarInfo.txt");
        if (radarInfo != null) {
            infoDict = RadarInfoDict.getInstance(radarInfo.getParent());
        } else {
            infoDict = null;
        }
    }

    /**
     * @param resourceData
     * @param loadProperties
     */
    protected AbstractRadarResource(RadarResourceData resourceData,
            LoadProperties loadProperties) {
        super(resourceData, loadProperties);
        resourceData.addChangeListener(this);

        dataTimes = new ArrayList<DataTime>();
        icao = "";
        getCapability(ColorMapCapability.class).setSuppressingMenuItems(true);
        getCapability(ImagingCapability.class).setSuppressingMenuItems(true);
        getCapability(ColorableCapability.class).setSuppressingMenuItems(true);
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.uf.viz.core.rsc.AbstractVizResource#disposeInternal()
     */
    @Override
    protected void disposeInternal() {
    }

    // override base version to constrain on the selected timeline
    @Override
    public void queryRecords() throws VizException {

        HashMap<String, RequestConstraint> queryList = new HashMap<String, RequestConstraint>(
                resourceData.getMetadataMap());

        Long stl = Long.MAX_VALUE;
        Long etl = Long.MIN_VALUE;

        for (AbstractFrameData afd : frameDataMap.values()) {
            if (stl > afd.getFrameStartTime().getRefTime().getTime()) {
                stl = afd.getFrameStartTime().getRefTime().getTime();
            }
            if (etl < afd.getFrameEndTime().getRefTime().getTime()) {
                etl = afd.getFrameEndTime().getRefTime().getTime();
            }
        }

        DataTime timelineStart = new DataTime(new Date(stl));
        DataTime timelineEnd = new DataTime(new Date(etl));
        RequestConstraint reqConstr = new RequestConstraint();

        // only query records that
        String[] dts = timelineStart.toString().split(" ");
        String startTimeStr = dts[0] + " "
                + dts[1].substring(0, dts[1].length() - 2);
        dts = timelineEnd.toString().split(" ");
        String endTimeStr = dts[0] + " "
                + dts[1].substring(0, dts[1].length() - 2);
        String[] constraintList = { startTimeStr, endTimeStr };
        reqConstr.setBetweenValueList(constraintList);
        reqConstr.setConstraintType(RequestConstraint.ConstraintType.BETWEEN);

        queryList.put("dataTime.refTime", reqConstr);

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

        for (Object pdo : pdoList) {
            for (IRscDataObject dataObject : processRecord(pdo)) {
                newRscDataObjsQueue.add(dataObject);
            }
        }
        // preProcess
    }

    // TODO : extend this for derived classes to hold DrawableImage,
    // RadarGraphicsDisplay,
    // Wireframes ....
    //
    public class RadarFrameData extends AbstractFrameData {

        public RadarFrameData(DataTime ftime, int frameInterval) {
            super(ftime, frameInterval);
        }

        // one record per frame but can be changed if appropriate.
        private VizRadarRecord radarRecord = null;

        public VizRadarRecord getRadarRecord() {
            return radarRecord;
        }

        public boolean updateFrameData(IRscDataObject rscDataObj) {

            if (!(rscDataObj instanceof DfltRecordRscDataObj)) {
                System.out.println("Unrecognized Radar Image");
                return false;
            }
            PluginDataObject pdo = ((DfltRecordRscDataObj) rscDataObj).getPDO();

            if (!(pdo instanceof RadarRecord)) {
                statusHandler.handle(Priority.PROBLEM, ""
                        + this.getClass().getName() + " expected : "
                        + RadarRecord.class.getName() + " Got: " + pdo);
                return false;
            }

            RadarRecord newRdrRec = (RadarRecord) pdo;

            newRdrRec.setAddSpatial(false);// !((RadarResourceData)resourceData).latest);
            icao = newRdrRec.getIcao();

            if (newRdrRec.getLatitude() != null
                    && newRdrRec.getLongitude() != null) {
                centerLocation = new Coordinate(newRdrRec.getLongitude(),
                        newRdrRec.getLatitude());
            }

            // if there is already a record for this frame then determine if the
            // new
            // one is a better time match.
            // Do we need to worry about adding unused records to the cache if
            // we
            // end up replacing alot of the records??

            if (radarRecord != null) {
                long timeDiff = timeMatch(radarRecord.getDataTime());

                if (timeMatch(newRdrRec.getDataTime()) < timeDiff) {
                    radarRecord = null; // ? can we or do we need to
                                        // dispose/uncache this?
                    // Raytheon's logic for when to update a radarRecord (same
                    // time though)
                    // if (existing.getNumLevels() != null
                    // && !existing.getNumLevels().equals(
                    // radarRecord.getNumLevels())) {
                    // // Use the one with the most levels
                    // if (existing.getNumLevels().intValue() < radarRecord
                    // .getNumLevels().intValue()) {
                    // remove(d);
                    // existing = null;
                    // }
                    // } else if (existing.getGateResolution() != null
                    // && !existing.getGateResolution().equals(
                    // radarRecord.getGateResolution())) {
                    // // use the one with the smallest resolution
                    // if (existing.getGateResolution().intValue() > radarRecord
                    // .getGateResolution().intValue()) {
                    // remove(d);
                    // existing = null;
                    // }
                    // } else if (existing.getNumBins() *
                    // existing.getNumRadials() != radarRecord
                    // .getNumBins() * radarRecord.getNumRadials()) {
                    // // use the one with the most pixels
                    // if (existing.getNumBins() * existing.getNumRadials() <
                    // radarRecord
                    // .getNumBins() * radarRecord.getNumRadials()) {
                    // remove(d);
                    // existing = null;
                    // }
                    // } else if (existing.getInsertTime().getTimeInMillis() <
                    // radarRecord
                    // .getInsertTime().getTimeInMillis()) {
                    // // Use the newest one
                    // remove(d);
                    // existing = null;
                    // }
                }
            }

            if (radarRecord == null) {
                radarRecord = createVizRadarRecord(newRdrRec);
                // radarRecords.put(d, existing);
                // synchronized (dataTimes) {
                // dataTimes.add(d);
                // Collections.sort(dataTimes);
                // }
            }

            // VizRadarRecord rtr = radarRecords.get(pdo.getDataTime());

            return true;
        }

        public void dispose() {
            // if( tileSet != baseTile && tileSet != null ) {
            // tileSet.dispose();
            // tileSet = null;
            // }
        }
    }

    protected VizRadarRecord createVizRadarRecord(RadarRecord radarRecord) {
        return new DefaultVizRadarRecord(radarRecord);
    }

    public VizRadarRecord getCurrentRadarRecord() {

        // Guard against NUll pointer.
        if (getCurrentFrame() == null) {
            return null;
        }

        return ((RadarFrameData) getCurrentFrame()).getRadarRecord();
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
        RadarRecord record = ((RadarFrameData) getCurrentFrame()).radarRecord;

        if (record != null) {
            return new Coordinate(record.getLongitude(), record.getLatitude());
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
        // NOT Tested (Not implemented for NCP) :
        // changed to get primaryElevationAngle from the record instead of the
        // DataTime
        // DataTime displayedDate = currFrameTime;
        // if (displayedDate != null) {
        // tilt = displayedDate.getLevelValue();
        // }
        RadarRecord rdrec = getCurrentRadarRecord();
        if (rdrec != null) {
            return rdrec.getPrimaryElevationAngle();
        }
        return 0.0;
    }
}
