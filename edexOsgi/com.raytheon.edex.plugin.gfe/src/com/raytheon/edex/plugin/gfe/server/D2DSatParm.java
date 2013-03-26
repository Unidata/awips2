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
import java.util.Date;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import com.raytheon.edex.plugin.gfe.config.IFPServerConfig;
import com.raytheon.edex.plugin.satellite.dao.SatelliteDao;
import com.raytheon.uf.common.dataplugin.gfe.GridDataHistory;
import com.raytheon.uf.common.dataplugin.gfe.RemapGrid;
import com.raytheon.uf.common.dataplugin.gfe.db.objects.DatabaseID;
import com.raytheon.uf.common.dataplugin.gfe.db.objects.GFERecord.GridType;
import com.raytheon.uf.common.dataplugin.gfe.db.objects.GridLocation;
import com.raytheon.uf.common.dataplugin.gfe.db.objects.GridParmInfo;
import com.raytheon.uf.common.dataplugin.gfe.db.objects.ParmID;
import com.raytheon.uf.common.dataplugin.gfe.db.objects.TimeConstraints;
import com.raytheon.uf.common.dataplugin.gfe.grid.Grid2DByte;
import com.raytheon.uf.common.dataplugin.gfe.grid.Grid2DFloat;
import com.raytheon.uf.common.dataplugin.gfe.server.message.ServerResponse;
import com.raytheon.uf.common.dataplugin.gfe.server.request.GetGridRequest;
import com.raytheon.uf.common.dataplugin.gfe.slice.IGridSlice;
import com.raytheon.uf.common.dataplugin.gfe.slice.ScalarGridSlice;
import com.raytheon.uf.common.dataplugin.satellite.SatMapCoverage;
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
 * 
 * </pre>
 * 
 * @author bphillip
 * @version 1.0
 */

public class D2DSatParm extends GridParm {

    /** The log handler */
    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(D2DSatParm.class);

    private static final long TIME_MATCH_FACTOR = 3 * TimeUtil.MILLIS_PER_MINUTE;

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
     */
    public D2DSatParm(IFPServerConfig config, String productURI,
            DatabaseID dbId, String parmName) {
        this.config = config;
        this.pid = new ParmID(parmName, dbId);
        if (productURI != null && productURI.contains("/")) {
            if (productURI.startsWith("/")) {
                productURI = productURI.substring(1);
            }
            String[] tokens = productURI.split("/");
            sectorID = tokens[0];
            physicalElement = tokens[1];
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

    @Override
    public boolean isValid() {
        return true;
    }

    /**
     * @return
     */
    @Override
    public ServerResponse<List<TimeRange>> getGridInventory() {
        ServerResponse<List<TimeRange>> sr = new ServerResponse<List<TimeRange>>();
        List<TimeRange> inventory = new ArrayList<TimeRange>();
        List<Date> satInventory = new ArrayList<Date>();

        SatelliteDao satDao = null;
        try {
            satDao = (SatelliteDao) PluginFactory.getInstance().getPluginDao(
                    "satellite");

            satInventory = satDao.getSatelliteInventory(null, null, sectorID,
                    physicalElement);
        } catch (Exception e) {
            statusHandler.error("Error getting inventory for sectorID ["
                    + sectorID + "] and physicalElement [" + physicalElement
                    + "].  " + e.getLocalizedMessage());
            sr.addMessage("Error getting inventory for sectorID [" + sectorID
                    + "] and physicalElement [" + physicalElement + "].  "
                    + e.getLocalizedMessage());
            return sr;
        }

        for (Date d : satInventory) {
            inventory.add(new TimeRange(d, tc.getDuration() * 1000));
        }
        sr.setPayload(inventory);
        return sr;
    }

    /**
     * @return
     * 
     */
    @Override
    public ServerResponse<GridParmInfo> getGridParmInfo() {
        ServerResponse<GridParmInfo> gpi = new ServerResponse<GridParmInfo>();
        GridParmInfo info = new GridParmInfo(pid, config.dbDomain(),
                GridType.SCALAR, "unknown", "sat data", 0, 255, 0, false, tc,
                false);
        gpi.setPayload(info);
        return gpi;
    }

    /**
     * @return
     */
    @Override
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
     * @return
     */
    @Override
    public ServerResponse<List<IGridSlice>> getGridData(
            GetGridRequest getRequest, List<TimeRange> badDataTimes) {
        List<TimeRange> timeRanges = getRequest.getTimes();
        ServerResponse<List<IGridSlice>> sr = new ServerResponse<List<IGridSlice>>();
        List<TimeRange> inventory = getGridInventory().getPayload();
        List<IGridSlice> gridSlices = new ArrayList<IGridSlice>();

        List<TimeRange> matchedTimes = matchRequestTimes(timeRanges, inventory);

        SatelliteDao dao = null;
        try {
            dao = (SatelliteDao) PluginFactory.getInstance().getPluginDao(
                    "satellite");
            List<SatelliteRecord> satRecords = dao.getSatelliteData(null, null,
                    sectorID, physicalElement, rangesToDates(matchedTimes));
            for (int i = 0; i < satRecords.size(); i++) {
                GridLocation satGridLoc = satMapCoverageToGridLocation(satRecords
                        .get(i).getCoverage());
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
     * @param reqTimeRanges
     * @param inventory
     * @return
     */
    private List<TimeRange> matchRequestTimes(List<TimeRange> reqTimeRanges,
            List<TimeRange> inventory) {
        List<TimeRange> retVal = new ArrayList<TimeRange>(reqTimeRanges.size());

        for (TimeRange tr : reqTimeRanges) {
            TimeRange matchRange = new TimeRange(tr.getStart().getTime()
                    - TIME_MATCH_FACTOR, tr.getEnd().getTime()
                    + TIME_MATCH_FACTOR);

            for (TimeRange invTR : inventory) {
                if (matchRange.contains(invTR)) {
                    retVal.add(invTR);
                    break;
                }
            }
        }

        return retVal;
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
     * Converts satellite map coverage data to GFE compatible coverage for use
     * with RemapGrid
     * 
     * @param coverage
     *            The satellite map coverage to be converted
     * @return The GFE compatible version of the satellite map coverage
     */
    private GridLocation satMapCoverageToGridLocation(SatMapCoverage coverage) {
        GridLocation location = new GridLocation();
        location.setCrsObject(coverage.getCrs());
        location.setGeometry(coverage.getGeometry());
        location.setNx(coverage.getNx());
        location.setNy(coverage.getNy());
        return location;

    }

    /**
     * Extracts the start times from a list of time ranges and places them in a
     * list
     * 
     * @param ranges
     *            The timeranges to extract the start times from
     * @return The list of start time Dates
     */
    private List<Date> rangesToDates(List<TimeRange> ranges) {
        List<Date> dates = new ArrayList<Date>();
        for (TimeRange range : ranges) {
            dates.add(range.getStart());
        }
        return dates;
    }
}
