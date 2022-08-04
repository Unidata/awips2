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

import javax.measure.IncommensurableException;
import javax.measure.UnconvertibleException;
import javax.measure.Unit;
import javax.measure.UnitConverter;

import org.opengis.referencing.FactoryException;
import org.opengis.referencing.operation.TransformException;

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
import com.raytheon.uf.common.dataplugin.satellite.units.SatelliteUnitsUtil;
import com.raytheon.uf.common.datastorage.records.ByteDataRecord;
import com.raytheon.uf.common.datastorage.records.ShortDataRecord;
import com.raytheon.uf.common.status.IPerformanceStatusHandler;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.PerformanceStatus;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.common.time.TimeRange;
import com.raytheon.uf.common.time.util.TimeUtil;
import com.raytheon.uf.edex.database.DataAccessLayerException;
import com.raytheon.uf.edex.database.plugin.PluginFactory;
import com.raytheon.uf.edex.database.query.SpatialConstraint;

import tec.uom.se.AbstractUnit;
import tec.uom.se.format.SimpleUnitFormat;

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
 * Jun 06, 2018  7310      mapeters     Support GOES-R data
 * Jun 15, 2018  7310      mapeters     Add spatial constraint to queries to
 *                                      improve GOES-R performance
 * Apr 15, 2019  7596      lsingh       Updated units framework to JSR-363.
 *                                      Handled unit conversion.
 *
 * </pre>
 *
 * @author bphillip
 */

public class D2DSatParm {

    private static final IUFStatusHandler statusHandler = UFStatus
            .getHandler(D2DSatParm.class);

    private static final IPerformanceStatusHandler perfLog = PerformanceStatus
            .getHandler(D2DSatParm.class.getSimpleName() + ": ");

    private static final int DEFAULT_INTERVAL = TimeUtil.SECONDS_PER_MINUTE;

    private final SpatialConstraint spatialConstraint;

    /** The ParmID associated with this D2DSatParm */
    private final ParmID pid;

    /** The site server configuration */
    private final IFPServerConfig config;

    private final SatelliteDao satDao;

    /** Time constraints for this satellite data */
    private TimeConstraints tc;

    /** The sector ID for this satellite data */
    private String sectorID;

    /** The physical element for this satellite data */
    private String physicalElement;

    private ConcurrentSkipListSet<TimeRange> inventory;

    private ParmMetadata metadata;

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
        this.spatialConstraint = new SpatialConstraint(
                config.dbDomain().getGeometry(),
                SpatialConstraint.Type.INTERSECTS, "coverage.location");

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
        ServerResponse<List<TimeRange>> sr = new ServerResponse<>();
        sr.setPayload(new ArrayList<>(this.inventory));
        return sr;
    }

    /**
     * @return
     * @throws GfeException
     */
    private ConcurrentSkipListSet<TimeRange> getDbInventory()
            throws GfeException {
        List<Date> satInventory = new ArrayList<>();
        int desiredVersions = config.desiredDbVersions(pid.getDbId());

        try {
            satInventory = satDao.getSatelliteInventory(null, null, sectorID,
                    physicalElement, spatialConstraint, desiredVersions);
        } catch (Exception e) {
            throw new GfeException("Error getting inventory for sectorID ["
                    + sectorID + "] and physicalElement [" + physicalElement
                    + "].", e);
        }

        /*
         * Determine time constraints from inventory in order to determine time
         * range that each date falls into
         */
        tc = getTimeConstraints(satInventory);

        ConcurrentSkipListSet<TimeRange> dbInventory = new ConcurrentSkipListSet<>();
        for (Date d : satInventory) {
            dbInventory.add(getTimeRange(d));
        }
        return dbInventory;
    }

    /**
     * Get the time range containing the given date. The time range will match
     * the interval that we want to show the data in. For instance, if we want 5
     * minute intervals, then 17:24:38 will result in 17:20:00-17:25:00.
     *
     * @param date
     * @return the time range that contains the given date and corresponds to
     *         the desired data interval
     */
    private TimeRange getTimeRange(Date date) {
        long durationMillis = tc.getDuration() * TimeUtil.MILLIS_PER_SECOND;
        Date start = new Date(
                (date.getTime() / durationMillis) * durationMillis);
        return new TimeRange(start, durationMillis);

    }

    /**
     * @return the grid parm info
     */
    public ServerResponse<GridParmInfo> getGridParmInfo() {
        ParmMetadata metadata = getMetadata();
        ServerResponse<GridParmInfo> gpi = new ServerResponse<>();
        GridParmInfo info = new GridParmInfo(pid, config.dbDomain(),
                GridType.SCALAR, metadata.units, "sat data", metadata.minVal,
                metadata.maxVal, 0, false, tc, false);
        gpi.setPayload(info);
        return gpi;
    }

    /**
     * @param inventory
     * @return map of time ranges to lists of grid histories
     */
    public ServerResponse<Map<TimeRange, List<GridDataHistory>>> getGridHistory(
            List<TimeRange> inventory) {
        ServerResponse<Map<TimeRange, List<GridDataHistory>>> sr = new ServerResponse<>();
        Map<TimeRange, List<GridDataHistory>> history = new HashMap<>();

        for (TimeRange time : inventory) {
            List<GridDataHistory> indvHistory = new ArrayList<>();
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
        long start = System.currentTimeMillis();

        List<TimeRange> timeRanges = getRequest.getTimes();

        List<IGridSlice> data = new ArrayList<>();
        try {
            List<SatelliteRecord> satRecords = satDao.getSatelliteData(sectorID,
                    physicalElement, timeRanges, spatialConstraint);
            if (!satRecords.isEmpty()) {
                statusHandler.debug("Retrieved " + satRecords.size()
                        + " satellite records for " + getRequest);
                SatelliteRecord sampleRecord = satRecords.get(0);
                setMetadata(sampleRecord);
                Object sampleHdf5Record = sampleRecord.getMessageData();

                if (sampleHdf5Record instanceof ShortDataRecord) {
                    data = getGoesrGridData(satRecords, timeRanges);
                } else {
                    // ByteDataRecord
                    data = getNonGoesrGridData(satRecords, timeRanges);
                }
            }
        } catch (Exception e) {
            String errorMsg = "Error retrieving satellite data for GFE request: "
                    + getRequest + "\n" + e.getLocalizedMessage();
            statusHandler.error(errorMsg, e);
            return ServerResponse.errorResponseList(errorMsg);
        }

        long duration = System.currentTimeMillis() - start;
        perfLog.logDuration("Getting satellite data for " + getRequest,
                duration);

        ServerResponse<List<IGridSlice>> sr = new ServerResponse<>();
        sr.setPayload(data);
        return sr;
    }

    /**
     * Get grid data from non-GOES-R satellite records. The records are expected
     * to contain ByteDataRecords, and it is assumed there is a one-to-one
     * relationship between time ranges and satellite records.
     *
     * @param satRecords
     * @param timeRanges
     * @return grid slices
     * @throws FactoryException
     * @throws TransformException
     * @throws GfeException
     */
    private List<IGridSlice> getNonGoesrGridData(
            List<SatelliteRecord> satRecords, List<TimeRange> timeRanges)
            throws FactoryException, TransformException, GfeException {
        List<IGridSlice> gridSlices = new ArrayList<>();
        for (int i = 0; i < satRecords.size(); i++) {
            SatelliteRecord satRecord = satRecords.get(i);
            Object hdf5Record = satRecord.getMessageData();
            if (!(hdf5Record instanceof ByteDataRecord)) {
                throw new GfeException(
                        "Unexpected satellite record type for record "
                                + satRecord.getDataURI() + ": "
                                + hdf5Record.getClass().getSimpleName());
            }
            GridLocation satGridLoc = new GridLocation(this.pid.toString(),
                    satRecord);
            ByteDataRecord byteRecord = (ByteDataRecord) hdf5Record;
            Grid2DByte rawData = new Grid2DByte((int) byteRecord.getSizes()[0],
                    (int) byteRecord.getSizes()[1], byteRecord.getByteData());
            RemapGrid remap = new RemapGrid(satGridLoc, this.config.dbDomain());
            Grid2DByte remappedGrid = remap.remap(rawData, 0, 0);
            Grid2DFloat floatGrid = bytesToFloats(
                    remappedGrid.getBuffer().array(), config.dbDomain().getNx(),
                    config.dbDomain().getNy());

            gridSlices.add(createGridSlice(floatGrid, timeRanges.get(i)));
        }

        return gridSlices;
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
     * Get grid data from GOES-R satellite records. The records are expected to
     * contain ShortDataRecords, and there may be multiple records per time
     * range, as the data is stored in smaller tiles. To account for this, the
     * short data is consolidated into a single grid per time range after it is
     * converted to GFE's target gridspace.
     *
     * @param satRecords
     * @param timeRanges
     * @return server response containing grid slices
     * @throws FactoryException
     * @throws TransformException
     * @throws GfeException
     */
    private List<IGridSlice> getGoesrGridData(List<SatelliteRecord> satRecords,
            List<TimeRange> timeRanges)
            throws FactoryException, TransformException, GfeException {
        List<IGridSlice> gridSlices = new ArrayList<>();

        Unit<?> overallRecordUnit = null;
        Map<TimeRange, float[]> timeRangeToFloats = new HashMap<>();
        for (SatelliteRecord satRecord : satRecords) {
            Object hdf5Record = satRecord.getMessageData();
            if (!(hdf5Record instanceof ShortDataRecord)) {
                throw new GfeException(
                        "Unexpected satellite record type for record "
                                + satRecord.getDataURI() + ": "
                                + hdf5Record.getClass().getSimpleName());
            }
            ShortDataRecord shortRecord = (ShortDataRecord) hdf5Record;

            Unit<?> recordUnit = SatelliteUnitsUtil.getRecordUnit(satRecord);
            if (recordUnit == null) {
                recordUnit = AbstractUnit.ONE;
            }
            if (overallRecordUnit == null) {
                overallRecordUnit = recordUnit;
            } else if (!overallRecordUnit.equals(recordUnit)) {
                // This should never happen
                statusHandler.error("Retrieved satellite records with different"
                        + " record units, skipping second one...\n"
                        + satRecords.get(0) + "\tUnit: '" + overallRecordUnit
                        + "'\n" + satRecord + "\tUnit: '" + recordUnit + "'");
                continue;
            }

            /*
             * Convert from dataUnit to the recordUnit. This will perform the
             * scale_factor and add_offset specified in the HDF5 file
             */
            Unit<?> dataUnit = SatelliteUnitsUtil.getDataUnit(recordUnit,
                    shortRecord);
            UnitConverter unitConverter;
            try {
                unitConverter = dataUnit.getConverterToAny(recordUnit);
            } catch (UnconvertibleException | IncommensurableException e) {
                SimpleUnitFormat stringFormat = SimpleUnitFormat
                        .getInstance(SimpleUnitFormat.Flavor.ASCII);
                statusHandler.handle(Priority.ERROR,
                        "Retrieved satellite record [" + satRecord + "], however, unable to convert unit "
                                + stringFormat.format(dataUnit) + " to unit "
                                + stringFormat.format(recordUnit) +". Skipping...",
                        e);
                continue;
            }

            short[] rawShorts = shortRecord.getShortData();
            float[] convertedFloats = new float[rawShorts.length];
            for (int i = 0; i < rawShorts.length; ++i) {
                convertedFloats[i] = (float) unitConverter
                        .convert(rawShorts[i] & 0xffff);
            }
            Grid2DFloat floatGrid = new Grid2DFloat(
                    (int) shortRecord.getSizes()[0],
                    (int) shortRecord.getSizes()[1], convertedFloats);

            GridLocation satGridLoc = new GridLocation(this.pid.toString(),
                    satRecord);
            RemapGrid remap = new RemapGrid(satGridLoc, this.config.dbDomain());

            // Map the tile to GFE's grid space, using NaN as fill value
            Grid2DFloat remappedGrid = remap.remap(floatGrid, Float.NaN,
                    Float.MAX_VALUE, -Float.MAX_VALUE, Float.NaN);
            float[] remappedFloats = remappedGrid.getFloats();

            // Determine which queried time range this record fits into
            TimeRange currTimeRange = null;
            for (TimeRange timeRange : timeRanges) {
                if (timeRange.contains(satRecord.getDataTime().getRefTime())) {
                    currTimeRange = timeRange;
                    break;
                }
            }
            if (currTimeRange == null) {
                statusHandler.error("Retrieved satellite record [" + satRecord
                        + "] that didn't match requested time ranges ["
                        + timeRanges + "], skipping...");
                continue;
            }

            // Combine the tile with other tiles for the same time range
            float[] combinedFloats = timeRangeToFloats.get(currTimeRange);
            if (combinedFloats == null) {
                timeRangeToFloats.put(currTimeRange, remappedFloats);
            } else {
                for (int i = 0; i < combinedFloats.length; ++i) {
                    if (!Float.isNaN(remappedFloats[i])) {
                        combinedFloats[i] = remappedFloats[i];
                    }
                }
            }
        }

        // Build a grid slice for the combined float data of each time range
        for (Map.Entry<TimeRange, float[]> entry : timeRangeToFloats
                .entrySet()) {
            TimeRange timeRange = entry.getKey();
            float[] combinedFloats = entry.getValue();
            Grid2DFloat floatGrid = new Grid2DFloat(config.dbDomain().getNx(),
                    config.dbDomain().getNy(), combinedFloats);
            gridSlices.add(createGridSlice(floatGrid, timeRange));
        }

        return gridSlices;
    }

    private ScalarGridSlice createGridSlice(Grid2DFloat floatGrid,
            TimeRange timeRange) {
        ParmMetadata metadata = getMetadata();
        GridParmInfo gpi = new GridParmInfo(pid, config.dbDomain(),
                GridType.SCALAR, metadata.units, "sat data", metadata.minVal,
                metadata.maxVal, 0, false, tc);
        GridDataHistory[] history = new GridDataHistory[] { new GridDataHistory(
                GridDataHistory.OriginType.SCRATCH, pid, timeRange) };
        ScalarGridSlice slice = new ScalarGridSlice(timeRange, gpi, history,
                floatGrid);
        return slice;
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
        TimeRange tr = getTimeRange(validTime);
        if (!inventory.contains(tr)) {
            this.inventory.add(tr);
            notify = new GridUpdateNotification(pid, tr,
                    getGridHistory(Arrays.asList(tr)).getPayload(), null,
                    pid.getDbId().getSiteId());
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

            List<TimeRange> adds = new ArrayList<>(newInventory);
            adds.removeAll(inventory);

            List<TimeRange> deletes = new ArrayList<>(inventory);
            deletes.removeAll(newInventory);

            this.inventory = newInventory;

            notifs = new ArrayList<>(adds.size() + deletes.size());
            for (TimeRange tr : adds) {
                notifs.add(new GridUpdateNotification(pid, tr,
                        getGridHistory(Arrays.asList(tr)).getPayload(), null,
                        pid.getDbId().getSiteId()));
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

    /**
     * Gets time constraints based on a list of times (in descending order)
     *
     * @param times
     *            the times in descending order
     * @return the time constraints
     */
    private TimeConstraints getTimeConstraints(List<Date> times) {
        /*
         * If only one time, use default. Otherwise, determine time constraints
         * based off differences in times
         */
        if (times.size() <= 1) {
            return new TimeConstraints(DEFAULT_INTERVAL, DEFAULT_INTERVAL, 0);
        }

        long minInterval = TimeRange.allTimes().getDuration();
        for (int i = 0; i < times.size() - 1; ++i) {
            long millisInterval = times.get(i).getTime()
                    - times.get(i + 1).getTime();
            long secondsInterval = millisInterval / TimeUtil.MILLIS_PER_SECOND;
            minInterval = Math.min(minInterval, secondsInterval);
        }

        // Ensure interval will pass TimeConstraint's validation
        if (minInterval > TimeUtil.SECONDS_PER_DAY) {
            minInterval = TimeUtil.SECONDS_PER_DAY;
        } else if (minInterval <= 0) {
            minInterval = DEFAULT_INTERVAL;
        } else {
            // Must be factor of SECONDS_PER_DAY
            while (TimeUtil.SECONDS_PER_DAY % minInterval != 0) {
                --minInterval;
            }
        }

        int interval = (int) minInterval;
        return new TimeConstraints(interval, interval, 0);
    }

    /**
     * Get the metadata for this parm. If it hasn't been cached yet, a sample
     * satellite record will be queried in order to determine the metadata.
     *
     * @return this parm's metadata
     */
    private synchronized ParmMetadata getMetadata() {
        if (metadata == null) {
            if (!inventory.isEmpty()) {
                List<TimeRange> sampleTimes = Collections
                        .singletonList(inventory.iterator().next());
                List<SatelliteRecord> satRecords;
                try {
                    satRecords = satDao.getSatelliteData(sectorID,
                            physicalElement, sampleTimes);
                    if (!satRecords.isEmpty()) {
                        setMetadata(satRecords.get(0));
                    }
                } catch (DataAccessLayerException e) {
                    String msg = "Error getting satellite record for sectorID ["
                            + sectorID + "], physicalElement ["
                            + physicalElement + "], and timeRanges ["
                            + sampleTimes + "].";
                    statusHandler.warn(msg, e);
                }
            }
            if (metadata == null) {
                // Default values, use widest minVal/maxVal range
                return new ParmMetadata("unknown", 0, 500);
            }
        }
        return metadata;
    }

    /**
     * Use the sample satellite record to cache metadata about this parm. If
     * metadata has already been cached, this does nothing.
     *
     * @param sampleRecord
     *            sample satellite record to determine metadata from
     */
    private synchronized void setMetadata(SatelliteRecord sampleRecord) {
        if (metadata == null) {
            Unit<?> units = SatelliteUnitsUtil.getRecordUnit(sampleRecord);
            if (units == null) {
                units = AbstractUnit.ONE;
            }
            String unitsStr = units.toString();
            if (unitsStr.isEmpty()) {
                unitsStr = "unknown";
            }
            boolean goesr = sampleRecord
                    .getMessageData() instanceof ShortDataRecord;
            float minVal, maxVal;
            if (goesr) {
                /*
                 * GOES-R data is converted from shorts using variable scale
                 * factors and offsets from HDF5 files, so there is no set
                 * min/max value. Pick limits based off units.
                 */
                if (units.equals(AbstractUnit.ONE)) {
                    // Decimal values 0-1, provide some buffer
                    minVal = -1;
                    maxVal = 2;
                } else {
                    // Should be kelvin
                    minVal = 0;
                    maxVal = 500;
                }
            } else {
                /*
                 * Non-GOES-R data is just bytes casted to floats, use byte
                 * limits
                 */
                minVal = 0;
                maxVal = 255;
            }
            this.metadata = new ParmMetadata(unitsStr, minVal, maxVal);
        }
    }

    private static class ParmMetadata {

        private final String units;

        private final float minVal;

        private final float maxVal;

        public ParmMetadata(String unitsStr, float minVal, float maxVal) {
            this.units = unitsStr;
            this.minVal = minVal;
            this.maxVal = maxVal;
        }
    }
}
