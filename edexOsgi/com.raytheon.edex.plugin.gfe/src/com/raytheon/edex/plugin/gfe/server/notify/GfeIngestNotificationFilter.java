/**
 * This software was developed and / or modified by Raytheon Company,
 * pursuant to Contract DG133W-05-CQ-1067 with the US Government.
 * 
 * U.S. EXPORT CONTROLLED TECHNICAL DATA
 * This software product contains export-restricted data whose
 * export/transfer/disclosure is restricted by U.S. law. Dissemination
 * to non-U.S. persons whether in the United States or abroad requires
 * an export license or other authorization.
 * 
 * Contractor Name:        Raytheon Company
 * Contractor Address:     6825 Pine Street, Suite 340
 *                         Mail Stop B8
 *                         Omaha, NE 68106
 *                         402.291.0100
 * 
 * See the AWIPS II Master Rights File ("Master Rights File.pdf") for
 * further licensing information.
 **/
package com.raytheon.edex.plugin.gfe.server.notify;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Date;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

import com.raytheon.edex.msg.DataURINotificationMessage;
import com.raytheon.edex.plugin.gfe.cache.d2dparms.D2DParmIdCache;
import com.raytheon.edex.plugin.gfe.config.GFESiteActivation;
import com.raytheon.edex.plugin.gfe.config.IFPServerConfig;
import com.raytheon.edex.plugin.gfe.config.IFPServerConfigManager;
import com.raytheon.edex.plugin.gfe.exception.GfeConfigurationException;
import com.raytheon.edex.plugin.gfe.server.D2DSatParm;
import com.raytheon.edex.plugin.gfe.server.GridParmManager;
import com.raytheon.edex.plugin.gfe.server.database.D2DSatDatabase;
import com.raytheon.edex.plugin.gfe.server.database.D2DSatDatabaseManager;
import com.raytheon.edex.plugin.gfe.smartinit.SmartInitQueue;
import com.raytheon.edex.plugin.gfe.smartinit.SmartInitRecord;
import com.raytheon.edex.plugin.gfe.smartinit.SmartInitRecordPK;
import com.raytheon.edex.plugin.gfe.util.GridTranslator;
import com.raytheon.edex.plugin.gfe.util.SendNotifications;
import com.raytheon.uf.common.dataplugin.gfe.GridDataHistory;
import com.raytheon.uf.common.dataplugin.gfe.db.objects.DatabaseID;
import com.raytheon.uf.common.dataplugin.gfe.db.objects.DatabaseID.DataType;
import com.raytheon.uf.common.dataplugin.gfe.db.objects.ParmID;
import com.raytheon.uf.common.dataplugin.gfe.server.notify.DBInvChangeNotification;
import com.raytheon.uf.common.dataplugin.gfe.server.notify.GfeNotification;
import com.raytheon.uf.common.dataplugin.gfe.server.notify.GridUpdateNotification;
import com.raytheon.uf.common.dataplugin.grid.GridRecord;
import com.raytheon.uf.common.dataplugin.satellite.SatelliteRecord;
import com.raytheon.uf.common.message.WsId;
import com.raytheon.uf.common.parameter.mapping.ParameterMapper;
import com.raytheon.uf.common.serialization.SerializationUtil;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.common.time.TimeRange;
import com.raytheon.uf.common.util.mapping.MultipleMappingException;
import com.raytheon.uf.edex.core.EDEXUtil;

/**
 * TODO Add Description
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Aug 12, 2011            dgilling     Initial creation
 * Sep 19, 2012			   jdynina		DR 15442 fix
 * Jan 18, 2013      #1504 randerso     Moved D2D to GFE parameter name translation from
 *                                      D2DParmIdCache to GfeIngestNotificationFilter
 * 
 * </pre>
 * 
 * @author dgilling
 * @version 1.0
 */

public class GfeIngestNotificationFilter {

    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(GfeIngestNotificationFilter.class);

    private SmartInitQueue smartInitQueue = null;

    public void filterDataURINotifications(DataURINotificationMessage message)
            throws Exception {
        Date arrivalTime = new Date();
        List<GridRecord> gridRecords = new ArrayList<GridRecord>(500);
        List<SatelliteRecord> satRecords = new ArrayList<SatelliteRecord>(100);

        for (String dataURI : message.getDataURIs()) {
            if (dataURI.startsWith("/grid/")) {
                gridRecords.add(new GridRecord(dataURI));
            } else if (dataURI.startsWith("/satellite/")) {
                satRecords.add(new SatelliteRecord(dataURI));
            }
        }
        if (!gridRecords.isEmpty()) {
            filterGridRecords(gridRecords, arrivalTime);
        }
        if (!satRecords.isEmpty()) {
            filterSatelliteRecords(satRecords, arrivalTime);
        }
    }

    public void filterGridRecords(List<GridRecord> gridRecords, Date arrivalTime)
            throws Exception {
        StringBuilder initNameBuilder = new StringBuilder(120);

        Set<String> activeSites = GFESiteActivation.getInstance()
                .getActiveSites();
        for (String site : activeSites) {
            // using a map so that the instances can be easily looked up and
            // updated
            Map<SmartInitRecordPK, SmartInitRecord> inits = new HashMap<SmartInitRecordPK, SmartInitRecord>();
            // Loop through each record received and construct a ParmID
            Map<ParmID, List<TimeRange>> gridInv = new HashMap<ParmID, List<TimeRange>>();
            List<GridUpdateNotification> guns = new ArrayList<GridUpdateNotification>();
            Set<DatabaseID> newDbs = new HashSet<DatabaseID>();

            IFPServerConfig config = null;
            try {
                config = IFPServerConfigManager.getServerConfig(site);
            } catch (GfeConfigurationException e) {
                statusHandler.error("Unable to retrieve site config for "
                        + site, e);
                continue;
            }
            for (GridRecord grid : gridRecords) {
                String gfeModel = config.gfeModelNameMapping(grid
                        .getDatasetId());

                // ignore if no mapping
                if (gfeModel != null && gfeModel.length() > 0) {
                    DatabaseID dbId = new DatabaseID(site, DataType.GRID,
                            "D2D", gfeModel, grid.getDataTime().getRefTime());

                    if ((!D2DParmIdCache.getInstance().getDatabaseIDs()
                            .contains(dbId))
                            && (!newDbs.contains(dbId))) {
                        List<DatabaseID> fullInv = GridParmManager
                                .getDbInventory(site).getPayload();
                        fullInv.add(dbId);
                        newDbs.add(dbId);
                        GfeNotification dbInv = new DBInvChangeNotification(
                                fullInv, Arrays.asList(dbId),
                                new ArrayList<DatabaseID>(0), site);
                        sendNotification(dbInv);
                    }

                    String abbrev = grid.getParameter().getAbbreviation();
                    String gfeParmName = null;
                    try {
                        gfeParmName = ParameterMapper.getInstance()
                                .lookupAlias(abbrev, "gfeParamName");
                    } catch (MultipleMappingException e) {
                        statusHandler.handle(Priority.WARN,
                                e.getLocalizedMessage(), e);
                        gfeParmName = e.getArbitraryMapping();
                    }

                    String level = GridTranslator.getShortLevelName(grid
                            .getLevel().getMasterLevel().getName(), grid
                            .getLevel().getLevelonevalue(), grid.getLevel()
                            .getLeveltwovalue());
                    ParmID parmID = new ParmID(gfeParmName, dbId, level);

                    List<TimeRange> trs = gridInv.get(parmID);
                    if (trs == null) {
                        trs = new ArrayList<TimeRange>();
                        gridInv.put(parmID, trs);
                    }
                    TimeRange validPeriod = grid.getDataTime().getValidPeriod();
                    if (validPeriod.getDuration() > 0) {
                        trs.add(validPeriod);
                    } else {
                        trs.add(new TimeRange(grid.getDataTime()
                                .getValidPeriod().getStart(), 3600 * 1000));
                    }

                    List<String> siteInitModules = config.initModels(gfeModel);
                    for (String modelName : siteInitModules) {
                        initNameBuilder.delete(0, initNameBuilder.length());
                        initNameBuilder.append(site);
                        initNameBuilder.append("_GRID_D2D_");
                        initNameBuilder.append(modelName);
                        initNameBuilder.append('_');
                        initNameBuilder.append(dbId.getModelTime());

                        SmartInitRecordPK id = new SmartInitRecordPK(
                                initNameBuilder.toString(), grid.getDataTime()
                                        .getValidPeriod().getStart());

                        SmartInitRecord record = inits.get(id);
                        if (record == null) {
                            record = new SmartInitRecord();
                            record.setId(id);
                            record.setInsertTime(arrivalTime);
                            record.setSmartInit(modelName);
                            record.setDbName(dbId.toString());
                            record.setPriority(SmartInitRecord.LIVE_SMART_INIT_PRIORITY);
                            inits.put(id, record);
                        }
                    }
                }
            }

            // DR 15442 - move last for loop out of the for loop at line 110
            for (ParmID parmId : gridInv.keySet()) {
                try {
                    List<TimeRange> trs = gridInv.get(parmId);
                    for (TimeRange time : trs) {
                        List<GridDataHistory> histList = new ArrayList<GridDataHistory>();
                        histList.add(new GridDataHistory(
                                GridDataHistory.OriginType.INITIALIZED, parmId,
                                time, null, (WsId) null));
                        Map<TimeRange, List<GridDataHistory>> hist = new HashMap<TimeRange, List<GridDataHistory>>();
                        hist.put(time, histList);
                        guns.add(new GridUpdateNotification(parmId, time, hist,
                                null, parmId.getDbId().getSiteId()));
                    }
                } catch (Exception e) {
                    statusHandler.error("Unable to retrieve grid history for "
                            + parmId.toString(), e);
                }
            }

            try {
                sendNotifications(guns);
            } catch (Exception e) {
                statusHandler.error("Unable to send grib ingest notifications",
                        e);
            }

            smartInitQueue.addInits(inits.values());
        }
    }

    public void filterSatelliteRecords(List<SatelliteRecord> records,
            Date arrivalTime) throws Exception {
        StringBuilder initNameBuilder = new StringBuilder(120);

        Set<String> activeSites = GFESiteActivation.getInstance()
                .getActiveSites();
        for (String site : activeSites) {
            // using a map so that the instances can be easily looked up and
            // updated
            Map<SmartInitRecordPK, SmartInitRecord> inits = new HashMap<SmartInitRecordPK, SmartInitRecord>();
            List<GridUpdateNotification> guns = new ArrayList<GridUpdateNotification>();

            IFPServerConfig config = null;
            try {
                config = IFPServerConfigManager.getServerConfig(site);
            } catch (GfeConfigurationException e) {
                statusHandler.error(
                        "Error retrieiving site config for " + site, e);
                continue;
            }

            List<String> siteInitModules = config.initModels("Satellite");
            Map<String, String> satData = config.satDirs();
            D2DSatDatabase satDb = D2DSatDatabaseManager.getSatDatabase(site);

            for (SatelliteRecord msg : records) {
                Date validTime = msg.getDataTime().getValidPeriod().getStart();
                String product = msg.getSource() + "/"
                        + msg.getCreatingEntity() + "/" + msg.getSectorID()
                        + "/" + msg.getPhysicalElement();
                if (satData.containsKey(product)) {
                    ParmID pid = new ParmID(satData.get(product),
                            satDb.getDbId());
                    D2DSatParm satParm = satDb.findParm(pid);
                    TimeRange tr = new TimeRange(validTime, satParm
                            .getGridParmInfo().getPayload()
                            .getTimeConstraints().getDuration() * 1000);
                    GridUpdateNotification notify = new GridUpdateNotification(
                            pid, tr, satParm.getGridHistory(Arrays.asList(tr))
                                    .getPayload(), null, site);
                    guns.add(notify);

                    for (String init : siteInitModules) {
                        initNameBuilder.delete(0, initNameBuilder.length());
                        initNameBuilder.append(site);
                        initNameBuilder.append("_GRID_D2D_");
                        initNameBuilder.append(init);
                        initNameBuilder.append("_00000000_0000");
                        SmartInitRecordPK id = new SmartInitRecordPK(
                                initNameBuilder.toString(), validTime);
                        if (!inits.containsKey(id)) {
                            SmartInitRecord record = new SmartInitRecord();
                            record.setId(id);
                            record.setInsertTime(arrivalTime);
                            record.setSmartInit(init);
                            record.setDbName(satDb.getDbId().toString());
                            record.setPriority(SmartInitRecord.LIVE_SMART_INIT_PRIORITY);
                            inits.put(id, record);
                        }
                    }
                }
            }

            try {
                sendNotifications(guns);
            } catch (Exception e) {
                statusHandler.error(
                        "Unable to send satellite ingest notifications", e);
            }

            smartInitQueue.addInits(inits.values());
        }
    }

    private void sendNotification(GfeNotification notification)
            throws Exception {
        sendNotifications(Arrays.asList(notification));
    }

    private void sendNotifications(List<? extends GfeNotification> notifications)
            throws Exception {
        byte[] message = SerializationUtil.transformToThrift(notifications);
        EDEXUtil.getMessageProducer().sendAsyncUri(
                "jms-generic:topic:gfeGribNotification", message);
        SendNotifications.send(notifications);
    }

    public SmartInitQueue getSmartInitQueue() {
        return smartInitQueue;
    }

    public void setSmartInitQueue(SmartInitQueue smartInitQueue) {
        this.smartInitQueue = smartInitQueue;
    }
}
