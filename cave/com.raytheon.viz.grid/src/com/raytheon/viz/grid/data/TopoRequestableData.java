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
package com.raytheon.viz.grid.data;

import java.util.HashMap;
import java.util.Map;

import javax.measure.unit.SI;

import org.geotools.coverage.grid.GridGeometry2D;

import com.raytheon.uf.common.comm.CommunicationException;
import com.raytheon.uf.common.dataplugin.level.LevelFactory;
import com.raytheon.uf.common.datastorage.Request;
import com.raytheon.uf.common.datastorage.records.FloatDataRecord;
import com.raytheon.uf.common.gridcoverage.GridCoverage;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.topo.TopoQuery;
import com.raytheon.uf.viz.derivparam.data.AbstractRequestableData;
import com.raytheon.viz.grid.util.SliceUtil;

/**
 * requestable data that queries the topo datastore and transforms the data into
 * the correct coverage.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jan 15, 2010            rjpeter     Initial creation
 * 
 * </pre>
 * 
 * @author rjpeter
 * @version 1.0
 */

public class TopoRequestableData extends AbstractRequestableData {
    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(TopoRequestableData.class);

    // need to move to a static timed cache
    private static Map<GridCoverage, FloatDataRecord> topoCache = new HashMap<GridCoverage, FloatDataRecord>();

    public TopoRequestableData(String modelName) {
        this.source = modelName;
        this.parameter = "staticTopo";
        this.parameterName = "Topography";
        this.unit = SI.METER;
        try {
            this.level = LevelFactory.getInstance().getLevel("SFC", 0.0);
        } catch (CommunicationException e) {
            statusHandler.handle(Priority.PROBLEM, e.getLocalizedMessage(), e);
        }
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.viz.grid.util.AbstractRequestableData#getDataValue()
     */
    @Override
    public FloatDataRecord getDataValue(Object arg) throws VizException {
        GridCoverage coverage = (GridCoverage) this.getSpace();
        FloatDataRecord rval = topoCache.get(coverage);

        if (rval == null) {
            // retrieve topo data and assign record to FloatDataRecord
            GridGeometry2D gridGeom = coverage.getGridGeometry();
            float[] heights = TopoQuery.getInstance().getHeight(gridGeom);

            if (heights != null) {
                rval = new FloatDataRecord(null, null, heights, 2, new long[] {
                        coverage.getNx(), coverage.getNy() });
                topoCache.put(coverage, rval);
            }
        }
        if (arg instanceof Request) {
            return SliceUtil.slice(rval, (Request) arg);
        } else {
            return rval;
        }
    }
}
