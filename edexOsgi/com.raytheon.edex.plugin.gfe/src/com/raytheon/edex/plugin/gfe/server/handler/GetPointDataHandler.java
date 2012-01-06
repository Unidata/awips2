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
import java.util.List;

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
import com.vividsolutions.jts.geom.Coordinate;

/**
 * TODO Add Description
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Aug 11, 2009            njensen     Initial creation
 * 
 * </pre>
 * 
 * @author njensen
 * @version 1.0
 */

public class GetPointDataHandler implements
        IRequestHandler<GetPointDataRequest> {

    protected final transient Log logger = LogFactory.getLog(getClass());

    @Override
    public ServerResponse<?> handleRequest(GetPointDataRequest request)
            throws Exception {
        List<String> parameters = request.getParameters();

        DatabaseID db = new DatabaseID(request.getDatabaseID());
        List<ParmID> parmIds = new ArrayList<ParmID>(parameters.size());
        for (String p : parameters) {
            parmIds.add(new ParmID(p, db));
        }

        List<TimeRange> times = new ArrayList<TimeRange>();
        for (int i = 0; i < request.getNumberHours(); i++) {
            TimeRange tr = new TimeRange(request.getStartTime() + (i * 3600)
                    * 1000, request.getStartTime() + ((i + 1) * 3600 * 1000));
            times.add(tr);
        }

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

        GFEPointDataContainer container = new GFEPointDataContainer();
        ServerResponse<GFEPointDataContainer> resp = new ServerResponse<GFEPointDataContainer>();

        Coordinate coordinate = request.getCoordinate();
        Point index = PointUtil.determineIndex(coordinate, loc.getCrs(), geom);
        int x = index.x;
        int y = index.y;
        boolean containsCoord = true;
        if (x < 0 || x >= loc.getNx() || y < 0 || y >= loc.getNy()) {
            containsCoord = false;
        }

        for (TimeRange tr : times) {
            List<GetGridRequest> reqList = new ArrayList<GetGridRequest>();
            for (ParmID p : parmIds) {
                GetGridRequest req = new GetGridRequest();
                req.setParmId(p);
                List<GFERecord> reqRecList = new ArrayList<GFERecord>(times
                        .size());
                GFERecord rec = new GFERecord(p, tr);
                reqRecList.add(rec);
                req.setRecords(reqRecList);
                reqList.add(req);
            }

            try {
                GFEPointDataView view = new GFEPointDataView();
                view.setData("time", Type.LONG, SI.MILLI(SI.SECOND), tr
                        .getStart().getTime());
                view.setData("lat", Type.FLOAT, null, coordinate.y);
                view.setData("lon", Type.FLOAT, null, coordinate.x);

                // initially set all requested params to missing
                for (String param : parameters) {
                    view.setData(param, Type.FLOAT, Unit.ONE, 999.0f);
                }

                if (containsCoord) {
                    // get the data for each requested parm at a single time
                    ServerResponse<List<IGridSlice>> sr = GridParmManager
                            .getGridData(reqList);

                    // set the retrieved data
                    for (IGridSlice slice : sr.getPayload()) {
                        String param = slice.getGridInfo().getParmID()
                                .getParmName();
                        Unit<?> unit = slice.getGridInfo().getUnitObject();
                        if (slice instanceof VectorGridSlice) {
                            VectorGridSlice gs = (VectorGridSlice) slice;
                            Type type = Type.FLOAT;
                            view.setData(param + "Dir", type,
                                    NonSI.DEGREE_ANGLE, gs.getDirGrid().get(x,
                                            y));
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

            } catch (Exception e) {
                resp.addMessage(e.getMessage());
            }

        }

        resp.setPayload(container);
        return resp;
    }
}
