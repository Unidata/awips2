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
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import javax.measure.unit.NonSI;
import javax.measure.unit.SI;
import javax.measure.unit.Unit;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.geotools.coverage.grid.GridGeometry2D;

import com.raytheon.edex.plugin.gfe.config.IFPServerConfigManager;
import com.raytheon.edex.plugin.gfe.exception.GfeConfigurationException;
import com.raytheon.edex.plugin.gfe.server.GridParmManager;
import com.raytheon.uf.common.dataplugin.gfe.db.objects.DatabaseID;
import com.raytheon.uf.common.dataplugin.gfe.db.objects.GFERecord;
import com.raytheon.uf.common.dataplugin.gfe.db.objects.GridLocation;
import com.raytheon.uf.common.dataplugin.gfe.db.objects.ParmID;
import com.raytheon.uf.common.dataplugin.gfe.point.GFEPointDataContainer;
import com.raytheon.uf.common.dataplugin.gfe.point.GFEPointDataContainers;
import com.raytheon.uf.common.dataplugin.gfe.point.GFEPointDataView;
import com.raytheon.uf.common.dataplugin.gfe.request.GetPointDataRequest;
import com.raytheon.uf.common.dataplugin.gfe.server.message.ServerResponse;
import com.raytheon.uf.common.dataplugin.gfe.server.request.GetGridRequest;
import com.raytheon.uf.common.dataplugin.gfe.slice.DiscreteGridSlice;
import com.raytheon.uf.common.dataplugin.gfe.slice.IGridSlice;
import com.raytheon.uf.common.dataplugin.gfe.slice.ScalarGridSlice;
import com.raytheon.uf.common.dataplugin.gfe.slice.VectorGridSlice;
import com.raytheon.uf.common.geospatial.MapUtil;
import com.raytheon.uf.common.geospatial.PointUtil;
import com.raytheon.uf.common.pointdata.PointDataDescription.Type;
import com.raytheon.uf.common.serialization.comm.IRequestHandler;
import com.raytheon.uf.common.time.TimeRange;
import com.raytheon.uf.common.time.util.TimeUtil;
import com.vividsolutions.jts.geom.Coordinate;

/**
 * Generates results for Point(s) on requested grid database.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Aug 11, 2009            njensen     Initial creation
 * Mar 06, 2013 1735       rferrel     Change to retrieve multiple points
 *                                      in a single grid request.
 * 
 * </pre>
 * 
 * @author njensen
 * @version 1.0
 */

public class GetPointDataHandler implements
        IRequestHandler<GetPointDataRequest> {

    protected final transient Log logger = LogFactory.getLog(getClass());

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.common.serialization.comm.IRequestHandler#handleRequest
     * (com.raytheon.uf.common.serialization.comm.IServerRequest)
     */
    @SuppressWarnings("unchecked")
    @Override
    public ServerResponse<?> handleRequest(GetPointDataRequest request)
            throws Exception {
        List<String> parameters = request.getParameters();

        DatabaseID db = new DatabaseID(request.getDatabaseID());
        List<ParmID> parmIds = new ArrayList<ParmID>(parameters.size());
        GridLocation loc = null;

        try {
            loc = IFPServerConfigManager.getServerConfig(db.getSiteId())
                    .dbDomain();
        } catch (GfeConfigurationException e) {
            String msg = "Error getting grid location for site "
                    + db.getSiteId();
            logger.error(msg, e);
            ServerResponse<?> error = new ServerResponse<Object>();
            error.addMessage(msg);
            return error;
        }

        GridGeometry2D geom = MapUtil.getGridGeometry(loc);

        for (String p : parameters) {
            parmIds.add(new ParmID(p, db));
        }

        List<TimeRange> times = new ArrayList<TimeRange>();
        for (int i = 0; i < request.getNumberHours(); i++) {
            long iStartTime = request.getStartTime()
                    + (i * TimeUtil.MILLIS_PER_HOUR);
            long iEndTime = iStartTime + TimeUtil.MILLIS_PER_HOUR;
            TimeRange tr = new TimeRange(iStartTime, iEndTime);
            times.add(tr);
        }

        List<Coordinate> coordinates = request.getCoordinates();
        ServerResponse<?> resp = null;
        resp = new ServerResponse<GFEPointDataContainers>();

        Map<Coordinate, CoordinateInfo> infoMap = new HashMap<Coordinate, CoordinateInfo>();

        boolean getSlices = false;

        // See if any of the coordinates need the grid slices and set up info
        // map.
        for (Coordinate coordinate : coordinates) {
            CoordinateInfo info = new CoordinateInfo();
            infoMap.put(coordinate, info);

            info.container = new GFEPointDataContainer();
            Point index = PointUtil.determineIndex(coordinate, loc.getCrs(),
                    geom);
            info.x = index.x;
            info.y = index.y;
            info.containsCoord = !((info.x < 0) || (info.x >= loc.getNx())
                    || (info.y < 0) || (info.y >= loc.getNy()));

            if (!getSlices) {
                getSlices = info.containsCoord;
            }
        }

        for (TimeRange tr : times) {
            List<GetGridRequest> reqList = new ArrayList<GetGridRequest>();
            for (ParmID p : parmIds) {
                GetGridRequest req = new GetGridRequest();
                req.setParmId(p);
                List<GFERecord> reqRecList = new ArrayList<GFERecord>(
                        times.size());
                GFERecord rec = new GFERecord(p, tr);
                reqRecList.add(rec);
                req.setRecords(reqRecList);
                reqList.add(req);
            }

            try {
                ServerResponse<List<IGridSlice>> sr = null;
                if (getSlices) {
                    sr = GridParmManager.getGridData(reqList);
                }

                for (Coordinate coordinate : coordinates) {
                    CoordinateInfo info = infoMap.get(coordinate);
                    boolean containsCoord = info.containsCoord;
                    GFEPointDataContainer container = info.container;
                    GFEPointDataView view = new GFEPointDataView();
                    int x = info.x;
                    int y = info.y;

                    view.setData("time", Type.LONG, SI.MILLI(SI.SECOND), tr
                            .getStart().getTime());
                    view.setData("lat", Type.FLOAT, null, coordinate.y);
                    view.setData("lon", Type.FLOAT, null, coordinate.x);

                    // initially set all requested params to missing
                    for (String param : parameters) {
                        view.setData(param, Type.FLOAT, Unit.ONE, 999.0f);
                    }

                    if (containsCoord) {

                        // set the retrieved data
                        for (IGridSlice slice : sr.getPayload()) {
                            String param = slice.getGridInfo().getParmID()
                                    .getParmName();
                            Unit<?> unit = slice.getGridInfo().getUnitObject();
                            if (slice instanceof VectorGridSlice) {
                                VectorGridSlice gs = (VectorGridSlice) slice;
                                Type type = Type.FLOAT;
                                view.setData(param + "Dir", type,
                                        NonSI.DEGREE_ANGLE, gs.getDirGrid()
                                                .get(x, y));
                                view.setData(param + "Spd", type, unit, gs
                                        .getMagGrid().get(x, y));
                            } else if (slice instanceof ScalarGridSlice) {
                                ScalarGridSlice gs = (ScalarGridSlice) slice;
                                float val = gs.getScalarGrid().get(x, y);
                                Type type = Type.FLOAT;
                                view.setData(param, type, unit, val);
                            } else if (slice instanceof DiscreteGridSlice) {
                                DiscreteGridSlice gs = (DiscreteGridSlice) slice;
                                byte value = gs.getDiscreteGrid().get(x, y);
                                String key = gs.getKey()[value].toString();
                                Type type = Type.STRING;
                                view.setData(param, type, unit, key);
                            }
                        }
                    }
                    container.append(view);
                }
            } catch (Exception e) {
                resp.addMessage(e.getMessage());
            }

        }

        GFEPointDataContainers gfeContainers = new GFEPointDataContainers();
        List<GFEPointDataContainer> containers = new ArrayList<GFEPointDataContainer>(
                coordinates.size());

        // Keep the results list in the same order as the request's
        // coordinate list.
        for (Coordinate coordinate : coordinates) {
            containers.add(infoMap.get(coordinate).container);
        }
        gfeContainers.setContainers(containers);
        ((ServerResponse<GFEPointDataContainers>) resp)
                .setPayload(gfeContainers);
        return resp;
    }

    /**
     * Information for a coordinate.
     */
    private class CoordinateInfo {
        GFEPointDataContainer container;

        boolean containsCoord;

        int x;

        int y;
    }
}
