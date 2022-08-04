/**
 * This software was developed and / or modified by NOAA/NWS/OCP/ASDT
 **/
package com.raytheon.viz.mpe.ui.rsc;

import java.awt.Rectangle;
import java.io.IOException;
import java.nio.FloatBuffer;
import java.util.Collection;
import java.util.Date;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import javax.measure.UnitConverter;

import com.raytheon.uf.common.dataplugin.PluginDataObject;
import com.raytheon.uf.common.dataplugin.grid.GridRecord;
import com.raytheon.uf.common.dataplugin.grid.datastorage.GridDataRetriever;
import com.raytheon.uf.common.dataplugin.grid.derivparam.data.GridRequestableData;
import com.raytheon.uf.common.dataplugin.shef.tables.Colorvalue;
import com.raytheon.uf.common.dataquery.requests.RequestConstraint;
import com.raytheon.uf.common.datastorage.Request;
import com.raytheon.uf.common.datastorage.StorageException;
import com.raytheon.uf.common.datastorage.records.FloatDataRecord;
import com.raytheon.uf.common.datastorage.records.IDataRecord;
import com.raytheon.uf.common.mpe.constants.RFCQPEConstants;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.time.DataTime;
import com.raytheon.uf.common.xmrg.XmrgFile;
import com.raytheon.uf.viz.core.alerts.AlertMessage;
import com.raytheon.uf.viz.core.drawables.IDescriptor;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.rsc.AbstractRequestableResourceData;
import com.raytheon.uf.viz.core.rsc.AbstractVizResource;
import com.raytheon.uf.viz.core.rsc.LoadProperties;
import com.raytheon.viz.grid.record.RequestableDataRecord;
import com.raytheon.viz.hydrocommon.HydroDisplayManager;
import com.raytheon.viz.hydrocommon.whfslib.colorthreshold.GetColorValues;
import com.raytheon.viz.mpe.ui.MPEDisplayManager;
import com.raytheon.viz.mpe.ui.rfcmask.RfcMask;

import si.uom.SI;
import systems.uom.common.USCustomary;
import tec.uom.se.unit.MetricPrefix;

/**
 * 
 * RFC QPE resource data
 * 
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jun 20, 2017 17911      wkwock      Initial creation
 * Aug 14, 2018 20851      wkwock      Use period end time instead of reference time
 *
 * </pre>
 *
 * @author wkwock
 */
public class RFCQPEResourceData extends AbstractRequestableResourceData {
    private static final IUFStatusHandler logger = UFStatus
            .getHandler(RFCQPEResourceData.class);

    /** data buffer */
    private FloatBuffer dataBuffer = null;

    /** last display date */
    private Date lastDisplayDate = null;

    /** color list */
    private List<Colorvalue> colorList;

    /** RFC site */
    private String rfcSite;

    /** mask for the RFC site in XMRG */
    private XmrgFile xmrgMask = null;

    /** pluginDataObject list */
    private Map<Date, PluginDataObject> pdos = new HashMap<>();

    /** resource */
    private RFCQPEResource resource;

    public RFCQPEResourceData(String rfcSite) {
        this.rfcSite = rfcSite;
        createMetadataMap();
        try {
            xmrgMask = RfcMask.getRFCMask(rfcSite);
        } catch (IOException e) {
            logger.error("Failed to read " + rfcSite + "RFC mask file.", e);
        }
    }

    /**
     * create metadataMap
     */
    private void createMetadataMap() {
        HashMap<String, RequestConstraint> metadataMap = new HashMap<>();
        RequestConstraint paramConstraint = new RequestConstraint(
                "QPE01-" + rfcSite + "1hr");
        metadataMap.put("info.parameter.abbreviation", paramConstraint);

        RequestConstraint pluginConstraint = new RequestConstraint("grid");
        metadataMap.put("pluginName", pluginConstraint);

        RequestConstraint datasetIdConstraint = new RequestConstraint(
                "QPE-" + rfcSite);
        metadataMap.put("info.datasetId", datasetIdConstraint);

        RequestConstraint levelConstraint = new RequestConstraint("SFC");
        metadataMap.put("info.level.masterLevel.name", levelConstraint);
        setMetadataMap(metadataMap);
    }

    @Override
    public AbstractVizResource<?, ?> construct(LoadProperties loadProperties,
            IDescriptor descriptor) throws VizException {
        DataTime[] availableTimes = this.getAvailableTimes();
        // Perform the initial load
        PluginDataObject[] pdoList = getLatestPluginDataObjects(availableTimes,
                new DataTime[0]);
        for (PluginDataObject pdo : pdoList) {
            pdos.put(pdo.getDataTime().getValidPeriod().getEnd(), pdo);
        }

        if (colorList == null) {
            String user_id = System.getProperty("user.name");
            int duration = 1;
            colorList = GetColorValues.get_colorvalues(user_id,
                    HydroDisplayManager.MPE_APPLICATION_NAME,
                    RFCQPEConstants.CVUSE, duration, "E", HydroDisplayManager
                            .getInstance().getNamedColorUseSetList());
        }
        resource = (RFCQPEResource) constructResource(loadProperties, pdoList);
        return resource;
    }

    @Override
    protected AbstractVizResource<?, ?> constructResource(
            LoadProperties loadProperties, PluginDataObject[] objects)
            throws VizException {

        return new RFCQPEResource(this, loadProperties, colorList, rfcSite);
    }

    @Override
    public void update(Object updateData) {
        if (!(updateData instanceof AlertMessage[])) {
            return;
        }

        boolean issueRefresh = false;
        try {
            AlertMessage[] alertMsgs = (AlertMessage[]) updateData;

            DataTime[] newTimes = new DataTime[alertMsgs.length];
            for (int i = 0; i < alertMsgs.length; i++) {
                newTimes[i] = (DataTime) alertMsgs[i].decodedAlert
                        .get("dataTime");

                Date displayDate = MPEDisplayManager.getCurrent()
                        .getCurrentDisplayedDate();
                if (displayDate.equals(newTimes[i].getValidPeriod().getEnd())) {
                    issueRefresh = true;
                }
            }

            // merge with main pdoList
            PluginDataObject[] newPdos = getLatestPluginDataObjects(newTimes,
                    new DataTime[0]);
            for (PluginDataObject newPdo : newPdos) {
                // merge and use the new PDO for same datatime
                pdos.put(newPdo.getDataTime().getValidPeriod().getEnd(), newPdo);
            }

        } catch (VizException e) {
            logger.error("Failed to update RFC QPE data.", e);
        }

        if (issueRefresh) {
            dataBuffer = null;
            resource.issueRefresh();
        }
    }

    /**
     * Get QPE data
     * 
     * @return QPE data
     */
    public FloatBuffer getQPEData() {
        Date displayDate = MPEDisplayManager.getCurrent()
                .getCurrentDisplayedDate();
        if (displayDate.equals(lastDisplayDate) && dataBuffer != null) {
            return dataBuffer;
        }
        // Update dataBuffer since the display time is different
        lastDisplayDate = displayDate;

        // initialized QPE data
        dataBuffer = FloatBuffer.allocate(xmrgMask.getHrapExtent().width
                * xmrgMask.getHrapExtent().height);

        try {
            FloatDataRecord dataRecord = null;
            PluginDataObject pdo = pdos.get(displayDate);
            if (pdo != null) {
                try {
                    IDataRecord dataRec = GridDataRetriever
                            .retrieve((GridRecord) pdo, Request.ALL);
                    dataRecord = (FloatDataRecord) dataRec;
                    RequestableDataRecord rdr = ((RequestableDataRecord) pdo);
                    Collection<GridRequestableData> grd = rdr.getGridRequests();
                    grd.forEach((temp) -> {
                        resource.setInsertTime(
                                temp.getGridSource().getInsertTime().getTime());
                    });
                } catch (StorageException e) {
                    logger.error("Failed to get RFC QPE data", e);
                }
            }

            float[] floatData = null;
            if (dataRecord == null) {
                this.resource.setInsertTime(null);
            } else {
                floatData = dataRecord.getFloatData();
            }

            short[] maskData = xmrgMask.getData();
            int width = (int) xmrgMask.getHrapExtent().getWidth();
            int height = (int) xmrgMask.getHrapExtent().getHeight();
            for (int i = 0; i < height; i++) {
                for (int j = 0; j < width; j++) {
                    int origIndex = i * width + j;
                    if (maskData[origIndex] == 0) {
                        dataBuffer.put(RFCQPEConstants.MISSING_VALUE);
                    } else {
                        if (floatData == null) {
                            dataBuffer
                                    .put(RFCQPEConstants.OPAQUE_MISSING_VALUE);
                        } else {
                            // convert mm to inch
                            UnitConverter toInch = MetricPrefix.MILLI(SI.METRE)
                                    .getConverterTo(USCustomary.INCH);
                            dataBuffer.put((float) toInch
                                    .convert(floatData[origIndex]));
                        }
                    }
                }
            }
            dataBuffer.rewind();
        } catch (Exception e) {
            logger.error("Failed to get QPE Data.", e);
        }

        return dataBuffer;
    }

    /**
     * Get the location and size of the this RFC HRAP
     * 
     * @return the extent
     */
    public Rectangle getExtent() {
        return xmrgMask.getHrapExtent();
    }
}
