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
package com.raytheon.edex.plugin.gfe.server;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.Date;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.concurrent.ConcurrentSkipListSet;

import com.raytheon.edex.plugin.gfe.config.IFPServerConfig;
import com.raytheon.edex.plugin.satellite.dao.SatelliteDao;
import com.raytheon.uf.common.dataplugin.PluginException;
import com.raytheon.uf.common.dataplugin.gfe.GridDataHistory;
import com.raytheon.uf.common.dataplugin.gfe.RemapGrid;
import com.raytheon.uf.common.dataplugin.gfe.db.objects.DatabaseID;
import com.raytheon.uf.common.dataplugin.gfe.db.objects.GridLocation;
import com.raytheon.uf.common.dataplugin.gfe.db.objects.GridParmInfo;
import com.raytheon.uf.common.dataplugin.gfe.db.objects.GridParmInfo.GridType;
import com.raytheon.uf.common.dataplugin.gfe.db.objects.ParmID;
import com.raytheon.uf.common.dataplugin.gfe.db.objects.TimeConstraints;
import com.raytheon.uf.common.dataplugin.gfe.exception.GfeException;
import com.raytheon.uf.common.dataplugin.gfe.grid.Grid2DByte;
import com.raytheon.uf.common.dataplugin.gfe.grid.Grid2DFloat;
import com.raytheon.uf.common.dataplugin.gfe.server.message.ServerResponse;
import com.raytheon.uf.common.dataplugin.gfe.server.notify.GridUpdateNotification;
import com.raytheon.uf.common.dataplugin.gfe.server.request.GetGridRequest;
import com.raytheon.uf.common.dataplugin.gfe.slice.IGridSlice;
import com.raytheon.uf.common.dataplugin.gfe.slice.ScalarGridSlice;
import com.raytheon.uf.common.dataplugin.satellite.SatelliteRecord;
import com.raytheon.uf.common.datastorage.records.ByteDataRecord;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.time.TimeRange;
import com.raytheon.uf.common.time.util.TimeUtil;
import com.raytheon.uf.edex.database.plugin.PluginFactory;

/**
 * Server side parm implementation for satellite data in GFE
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * May 16, 2011            bphillip     Initial creation
 * Mar 25, 2013  1823      dgilling     Disassociate data from Source and
 *                                      CreatingEntity metadata, rely only
 *                                      on SectorId and PhysicalElement as in A1.
 * Jun 13, 2013  2044      randerso     Fixed satellite time matching
 * 
 * </pre>
 * 
 * @author bphillip
 * @version 1.0
 */

public class D2DSatParm {

    /** The statusHandler */
    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(D2DSatParm.class);

    /** The ParmID associated with this D2DSatParm */
    private ParmID pid;

    /** The site server configuration */
    private IFPServerConfig config;

    /** Time constraints used by satellite data */
    private static final TimeConstraints tc = new TimeConstraints(60, 60, 0);

    /** The sector ID for this satellite data */
    private String sectorID;

    /** The physical element for this satellite data */
    private String physicalElement;

    private ConcurrentSkipListSet<TimeRange> inventory;

    private SatelliteDao satDao;

    /**
     * Creates a new D2DSatParm
     * 
     * @param config
     *            The server configuration for this site
     * @param productURI
     *            The URI segment describing the source of the satellite data
     * @param dbId
     *            The databaseID of the satellite database
     * @param parmName
     *            The parm name
     * @throws GfeException
     */
    public D2DSatParm(IFPServerConfig config, String productURI,
            DatabaseID dbId, String parmName) throws GfeException {
        this.config = config;
        this.pid = new ParmID(parmName, dbId);
        try {
            this.satDao = (SatelliteDao) PluginFactory.getInstance()
                    .getPluginDao("satellite");
        } catch (PluginException e) {
            throw new GfeException("Unable to create SatelliteDao", e);
        }

        if ((productURI != null) && productURI.contains("/")) {
            if (productURI.startsWith("/")) {
                productURI = productURI.substring(1);
            }
            String[] tokens = productURI.split("/");
            sectorID = tokens[0];
            physicalElement = tokens[1];
            this.inventory = getDbInventory();
        }
    }

    /**
     * Gets the parmID
     * 
     * @return The parmID
     */
    public ParmID pid() {
        return this.pid;
    }

    /**
     * @return ServerResponse containing list of available time ranges if
     *         successful
     */
    public ServerResponse<List<TimeRange>> getGridInventory() {
        ServerResponse<List<TimeRange>> sr = new ServerResponse<List<TimeRange>>();
        sr.setPayload(new ArrayList<TimeRange>(this.inventory));
        return sr;
    }

    /**
     * @return
     * @throws GfeException
     */
    private ConcurrentSkipListSet<TimeRange> getDbInventory()
            throws GfeException {
        List<Date> satInventory = new ArrayList<Date>();
        int desiredVersions = config.desiredDbVersions(pid.getDbId());

        try {
            satInventory = satDao.getSatelliteInventory(null, null, sectorID,
                    physicalElement, desiredVersions);
        } catch (Exception e) {
            throw new GfeException("Error getting inventory for sectorID ["
                    + sectorID + "] and physicalElement [" + physicalElement
                    + "].", e);
        }

        ConcurrentSkipListSet<TimeRange> dbInventory = new ConcurrentSkipListSet<TimeRange>();
        for (Date d : satInventory) {
            Date start = truncateSeconds(d);
            dbInventory.add(new TimeRange(start, tc.getDuration() * 1000));
        }
        return dbInventory;
    }

    private Date truncateSeconds(Date date) {
        // truncate seconds
        return new Date((date.getTime() / TimeUtil.MILLIS_PER_MINUTE)
                * TimeUtil.MILLIS_PER_MINUTE);
    }

    /**
     * @return the grid parm info
     */
    public ServerResponse<GridParmInfo> getGridParmInfo() {
        ServerResponse<GridParmInfo> gpi = new ServerResponse<GridParmInfo>();
        GridParmInfo info = new GridParmInfo(pid, config.dbDomain(),
                GridType.SCALAR, "unknown", "sat data", 0, 255, 0, false, tc,
                false);
        gpi.setPayload(info);
        return gpi;
    }

    /**
     * @param inventory
     * @return map of time ranges to lists of grid histories
     */
    public ServerResponse<Map<TimeRange, List<GridDataHistory>>> getGridHistory(
            List<TimeRange> inventory) {
        ServerResponse<Map<TimeRange, List<GridDataHistory>>> sr = new ServerResponse<Map<TimeRange, List<GridDataHistory>>>();
        Map<TimeRange, List<GridDataHistory>> history = new HashMap<TimeRange, List<GridDataHistory>>();

        for (TimeRange time : inventory) {
            List<GridDataHistory> indvHistory = new ArrayList<GridDataHistory>();
            indvHistory.add(new GridDataHistory(
                    GridDataHistory.OriginType.INITIALIZED, pid, time));
            history.put(time, indvHistory);
        }
        sr.setPayload(history);
        return sr;
    }

    /**
     * @param getRequest
     * @param badDataTimes
     * @return ServerResponse containing list of grid slices if successful
     */
    public ServerResponse<List<IGridSlice>> getGridData(
            GetGridRequest getRequest, List<TimeRange> badDataTimes) {
        List<TimeRange> timeRanges = getRequest.getTimes();
        ServerResponse<List<IGridSlice>> sr = new ServerResponse<List<IGridSlice>>();
        List<IGridSlice> gridSlices = new ArrayList<IGridSlice>();

        SatelliteDao dao = null;
        try {
            dao = (SatelliteDao) PluginFactory.getInstance().getPluginDao(
                    "satellite");
            List<SatelliteRecord> satRecords = dao.getSatelliteData(sectorID,
                    physicalElement, timeRanges);
            for (int i = 0; i < satRecords.size(); i++) {
                GridLocation satGridLoc = new GridLocation(this.pid.toString(),
                        satRecords.get(i));
                ByteDataRecord hdf5Record = (ByteDataRecord) satRecords.get(i)
                        .getMessageData();
                Grid2DByte rawData = new Grid2DByte(
                        (int) hdf5Record.getSizes()[0],
                        (int) hdf5Record.getSizes()[1],
                        hdf5Record.getByteData());
                RemapGrid remap = new RemapGrid(satGridLoc,
                        this.config.dbDomain());
                Grid2DByte remappedGrid = remap.remap(rawData, 0, 0);
                GridParmInfo gpi = new GridParmInfo(pid, config.dbDomain(),
                        GridType.SCALAR, "unknown", "sat data", 0, 255, 0,
                        false, tc);
                GridDataHistory[] history = new GridDataHistory[] { new GridDataHistory(
                        GridDataHistory.OriginType.SCRATCH, pid,
                        timeRanges.get(i)) };
                ScalarGridSlice slice = new ScalarGridSlice(timeRanges.get(i),
                        gpi, history, bytesToFloats(remappedGrid.getBuffer()
                                .array(), config.dbDomain().getNx(), config
                                .dbDomain().getNy()));
                gridSlices.add(slice);
            }

        } catch (Exception e) {
            statusHandler.error(
                    "Error retrieving satellite data for GFE!"
                            + e.getLocalizedMessage(), e);
        }
        sr.setPayload(gridSlices);
        return sr;
    }

    /**
     * Utility function to convert byte data to floats
     * 
     * @param rawBytes
     *            The byte data to be converted
     * @param nx
     *            The x dimension of the data
     * @param ny
     *            The y dimension of the data
     * @return The converted array
     */
    private Grid2DFloat bytesToFloats(byte[] rawBytes, int nx, int ny) {
        float[] floats = new float[rawBytes.length];
        for (int idx = 0; idx < rawBytes.length; idx++) {
            floats[idx] = (rawBytes[idx] & 0xff);
        }
        return new Grid2DFloat(nx, ny, floats);
    }

    /**
     * Update inventory based on uri notification
     * 
     * @param record
     * @return GridUpdateNotification to be sent if inventory updated or null
     */
    public GridUpdateNotification update(SatelliteRecord record) {
        GridUpdateNotification notify = null;

        Date validTime = record.getDataTime().getValidPeriod().getStart();
        Date start = truncateSeconds(validTime);
        TimeRange tr = new TimeRange(start, tc.getDuration() * 1000);
        if (!inventory.contains(tr)) {
            this.inventory.add(tr);
            notify = new GridUpdateNotification(pid, tr, getGridHistory(
                    Arrays.asList(tr)).getPayload(), null, pid.getDbId()
                    .getSiteId());
        }
        return notify;
    }

    /**
     * Update inventory from database and return GridUpdateNotifications
     * 
     * @return the list of GridUpdateNotifications
     */
    public List<GridUpdateNotification> updateFromDb() {
        List<GridUpdateNotification> notifs;

        try {
            ConcurrentSkipListSet<TimeRange> newInventory = getDbInventory();

            List<TimeRange> adds = new ArrayList<TimeRange>(newInventory);
            adds.removeAll(inventory);

            List<TimeRange> deletes = new ArrayList<TimeRange>(inventory);
            deletes.removeAll(newInventory);

            this.inventory = newInventory;

            notifs = new ArrayList<GridUpdateNotification>(adds.size()
                    + deletes.size());
            for (TimeRange tr : adds) {
                notifs.add(new GridUpdateNotification(pid, tr, getGridHistory(
                        Arrays.asList(tr)).getPayload(), null, pid.getDbId()
                        .getSiteId()));
            }

            // empty histories map for deletes
            Map<TimeRange, List<GridDataHistory>> histories = Collections
                    .emptyMap();
            for (TimeRange tr : deletes) {
                notifs.add(new GridUpdateNotification(pid, tr, histories, null,
                        pid.getDbId().getSiteId()));
            }
        } catch (GfeException e) {
            statusHandler.error(e.getLocalizedMessage(), e);
            notifs = Collections.emptyList();
        }

        return notifs;
    }

    /**
     * Update inventory from GridUpdateNotification
     * 
     * @param gun
     *            the GridUpdateNotification
     */
    public void update(GridUpdateNotification gun) {
        TimeRange replace = gun.getReplacementTimeRange();

        Iterator<TimeRange> iter = inventory.iterator();
        while (iter.hasNext()) {
            TimeRange tr = iter.next();
            if (replace.contains(tr)) {
                iter.remove();
            }
        }

        for (TimeRange tr : gun.getHistories().keySet()) {
            inventory.add(tr);
        }
    }
}
