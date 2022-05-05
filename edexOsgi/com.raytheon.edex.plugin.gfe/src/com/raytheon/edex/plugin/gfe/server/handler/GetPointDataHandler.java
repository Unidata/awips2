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
package com.raytheon.edex.plugin.gfe.server.handler;

import java.awt.Point;
import java.util.ArrayList;
import java.util.Date;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.TreeMap;

import javax.measure.Unit;

import org.locationtech.jts.geom.Coordinate;

import com.raytheon.edex.plugin.gfe.server.IFPServer;
import com.raytheon.edex.plugin.gfe.server.database.GridDatabase;
import com.raytheon.uf.common.dataplugin.gfe.db.objects.DatabaseID;
import com.raytheon.uf.common.dataplugin.gfe.db.objects.GridLocation;
import com.raytheon.uf.common.dataplugin.gfe.db.objects.GridParmInfo.GridType;
import com.raytheon.uf.common.dataplugin.gfe.db.objects.ParmID;
import com.raytheon.uf.common.dataplugin.gfe.point.GFEPointDataContainer;
import com.raytheon.uf.common.dataplugin.gfe.point.GFEPointDataContainers;
import com.raytheon.uf.common.dataplugin.gfe.point.GFEPointDataView;
import com.raytheon.uf.common.dataplugin.gfe.request.GetPointDataRequest;
import com.raytheon.uf.common.dataplugin.gfe.server.message.ServerResponse;
import com.raytheon.uf.common.dataplugin.gfe.slice.DiscreteGridSlice;
import com.raytheon.uf.common.dataplugin.gfe.slice.IGridSlice;
import com.raytheon.uf.common.dataplugin.gfe.slice.ScalarGridSlice;
import com.raytheon.uf.common.dataplugin.gfe.slice.VectorGridSlice;
import com.raytheon.uf.common.dataplugin.gfe.slice.WeatherGridSlice;
import com.raytheon.uf.common.pointdata.PointDataDescription.Type;
import com.raytheon.uf.common.serialization.comm.IRequestHandler;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.time.TimeRange;
import com.raytheon.uf.common.time.util.TimeUtil;

import si.uom.NonSI;
import si.uom.SI;
import tec.uom.se.AbstractUnit;
import tec.uom.se.unit.MetricPrefix;

/**
 * Generates results for Point(s) on requested grid database. Used by AvnFPS to
 * directly retrieve GFE grid data at the closest points to the airports in
 * AvnFPS. This data is then used by the NDFD column on the monitor GUI or the
 * NDFD tab on the guidance GUI.
 * 
 * This replaces the AWIPS 1 approach of running a GFE text formatter to
 * generate rows of data, storing it to a file, and then parsing that data.
 * AWIPS 2 goes direct to GFE.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Aug 11, 2009            njensen     Initial creation
 * Mar 06, 2013 1735       rferrel     Change to retrieve multiple points
 *                                      in a single grid request.
 * Jun 13, 2013     #2044  randerso    Refactored to use IFPServer
 * Oct 31, 2013     #2508  randerso    Change to use DiscreteGridSlice.getKeys()
 * Apr 23, 2014     #3006  randerso    Restructured code to work with multi-hour grids
 * Apr 04, 2016     #5539  randerso    Fixed unsigned byte issues
 * Feb 22, 2018      6937  njensen     Use null for data outside bounds instead of error
 * Feb 23, 2018      7227  njensen     Continue past missing parms
 * 
 * </pre>
 * 
 * @author njensen
 */

public class GetPointDataHandler extends BaseGfeRequestHandler implements
        IRequestHandler<GetPointDataRequest> {

    private static final IUFStatusHandler statusHandler = UFStatus
            .getHandler(GetPointDataHandler.class);

    @Override
    public ServerResponse<GFEPointDataContainers> handleRequest(
            GetPointDataRequest request) throws Exception {
        ServerResponse<GFEPointDataContainers> resp = new ServerResponse<>();

        IFPServer ifpServer = getIfpServer(request);
        DatabaseID dbID = new DatabaseID(request.getDatabaseID());
        GridDatabase db = ifpServer.getGridParmMgr().getDatabase(dbID);
        GridLocation loc = ifpServer.getConfig().dbDomain();

        List<String> parameters = request.getParameters();
        List<ParmID> parmIds = new ArrayList<>(parameters.size());
        for (String p : parameters) {
            parmIds.add(new ParmID(p, dbID));
        }

        int numHours = request.getNumberHours();
        long startTime = request.getStartTime();
        TimeRange overallTr = new TimeRange(new Date(startTime), numHours
                * TimeUtil.MILLIS_PER_HOUR);

        List<Coordinate> coordinates = request.getCoordinates();

        Map<Coordinate, CoordinateInfo> infoMap = new HashMap<>();

        // See if any of the coordinates need the grid slices and set up info
        // map.
        for (Coordinate coordinate : coordinates) {
            CoordinateInfo info = new CoordinateInfo(coordinate, loc);
            infoMap.put(coordinate, info);

            if (!info.containsCoord) {
                /*
                 * Coordinate is outside GFE domain. Use null to signify missing
                 * data.
                 */
                infoMap.put(coordinate, null);
            }
        }
        for (ParmID parmId : parmIds) {
            ServerResponse<List<TimeRange>> invSr = db.getGridInventory(parmId,
                    overallTr);
            if (!invSr.isOkay()) {
                /*
                 * Log as info but continue on, getting the parms we can. The
                 * AvnFPS NDFD code handles missing parms/data.
                 */
                String msg = "AvnFPS NDFD error retrieving inventory for "
                        + parmId
                        + invSr.message();
                statusHandler.info(msg);
                continue;
            }

            String param = parmId.getParmName();

            List<TimeRange> inv = invSr.getPayload();
            ServerResponse<List<IGridSlice>> slicesSR = db.getGridData(parmId,
                    inv);
            if (!slicesSR.isOkay()) {
                String msg = "Error retrieving data for " + parmId + "\n"
                        + slicesSR.message();
                statusHandler.error(msg);
                resp.addMessage(msg);
                continue;
            }
            List<IGridSlice> slices = slicesSR.getPayload();
            Iterator<IGridSlice> sliceIter = slices.iterator();
            IGridSlice slice = null;
            for (int i = 0; i < numHours; i++) {
                Date time = new Date(startTime + (i * TimeUtil.MILLIS_PER_HOUR));
                try {
                    for (Coordinate coordinate : coordinates) {
                        CoordinateInfo info = infoMap.get(coordinate);
                        if (info == null) {
                            // null signifies missing data
                            continue;
                        }
                        boolean containsCoord = info.containsCoord;
                        GFEPointDataView view = info.getView(time);
                        int x = info.x;
                        int y = info.y;

                        // initially set all requested params to missing
                        view.setData(parmId.getParmName(), Type.FLOAT,
                                AbstractUnit.ONE, 999.0f);

                        if (containsCoord) {

                            // find the slice that contains the current time
                            if ((slice == null) && sliceIter.hasNext()) {
                                slice = sliceIter.next();
                            }
                            if ((slice != null)
                                    && (time.getTime() >= slice.getValidTime()
                                            .getEnd().getTime())
                                    && sliceIter.hasNext()) {
                                slice = sliceIter.next();
                            }
                            if ((slice != null)
                                    && slice.getValidTime().contains(time)) {
                                Unit<?> unit = slice.getGridInfo()
                                        .getUnitObject();

                                Type type;
                                GridType gridType = slice.getGridInfo()
                                        .getGridType();
                                switch (gridType) {
                                case VECTOR:
                                    VectorGridSlice vectorSlice = (VectorGridSlice) slice;
                                    type = Type.FLOAT;
                                    view.setData(param + "Dir", type,
                                            NonSI.DEGREE_ANGLE, vectorSlice
                                                    .getDirGrid().get(x, y));
                                    view.setData(param + "Spd", type, unit,
                                            vectorSlice.getMagGrid().get(x, y));
                                    break;
                                case SCALAR:
                                    ScalarGridSlice scalarSlice = (ScalarGridSlice) slice;
                                    float val = scalarSlice.getScalarGrid()
                                            .get(x, y);
                                    type = Type.FLOAT;
                                    view.setData(param, type, unit, val);
                                    break;
                                case DISCRETE:
                                    DiscreteGridSlice discreteSlice = (DiscreteGridSlice) slice;
                                    byte discreteValue = discreteSlice
                                            .getDiscreteGrid().get(x, y);
                                    String discreteKey = discreteSlice
                                            .getKeys()[0xFF & discreteValue]
                                            .toString();
                                    type = Type.STRING;
                                    view.setData(param, type, unit, discreteKey);
                                    break;
                                case WEATHER:
                                    WeatherGridSlice weatherSlice = (WeatherGridSlice) slice;
                                    byte wxValue = weatherSlice
                                            .getWeatherGrid().get(x, y);
                                    String wxKey = weatherSlice.getKeys()[0xFF & wxValue]
                                            .toString();
                                    type = Type.STRING;
                                    view.setData(param, type, unit, wxKey);
                                    break;

                                default:
                                    String msg = "Unknown gridType: "
                                            + gridType + " for " + parmId
                                            + ", data will be ignored.";
                                    statusHandler.error(msg);
                                    resp.addMessage(msg);
                                    break;
                                }
                            }
                        }
                    }
                } catch (Exception e) {
                    resp.addMessage(e.getMessage());
                    statusHandler.error(
                            "Error transforming GFE NDFD grids for AvnFPS", e);
                }
            }
        }

        GFEPointDataContainers gfeContainers = new GFEPointDataContainers();
        List<GFEPointDataContainer> containers = new ArrayList<>(
                coordinates.size());

        /*
         * Keep the results list in the same order as the request's coordinate
         * list.
         */
        for (Coordinate coordinate : coordinates) {
            CoordinateInfo info = infoMap.get(coordinate);
            if (info == null) {
                // use null to signify missing data for this coordinate
                containers.add(null);
                continue;
            }

            List<GFEPointDataView> views = new ArrayList<>(
                    info.viewMap.values());

            GFEPointDataContainer container = new GFEPointDataContainer();
            container.setViews(views);
            containers.add(container);
        }
        gfeContainers.setContainers(containers);
        resp.setPayload(gfeContainers);
        return resp;
    }

    /**
     * Information for a coordinate.
     */
    private class CoordinateInfo {
        private Map<Date, GFEPointDataView> viewMap;

        private boolean containsCoord;

        private int x;

        private int y;

        private Coordinate coordinate;

        public CoordinateInfo(Coordinate coordinate,
                GridLocation gloc) {
            viewMap = new TreeMap<>();
            this.coordinate = coordinate;

            Point gridCell = gloc.gridCoordinate(coordinate);
            x = gridCell.x;
            y = gridCell.y;

            containsCoord = !((x < 0) || (x >= gloc.getNx()) || (y < 0) || (y >= gloc
                    .getNy()));
        }

        public GFEPointDataView getView(Date fcstHour) {
            GFEPointDataView view = viewMap.get(fcstHour);
            if (view == null) {
                view = new GFEPointDataView();
                view.setData("time", Type.LONG, MetricPrefix.MILLI(SI.SECOND),
                        fcstHour.getTime());
                view.setData("lat", Type.FLOAT, null, coordinate.y);
                view.setData("lon", Type.FLOAT, null, coordinate.x);
                viewMap.put(fcstHour, view);
            }

            return view;
        }
    }
}
