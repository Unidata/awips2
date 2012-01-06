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

import com.raytheon.uf.common.dataplugin.grib.spatial.projections.GridCoverage;
import com.raytheon.uf.common.dataplugin.level.LevelFactory;
import com.raytheon.uf.common.datastorage.Request;
import com.raytheon.uf.common.datastorage.records.FloatDataRecord;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.topo.TopoQuery;
import com.raytheon.uf.viz.derivparam.data.AbstractRequestableData;
import com.raytheon.viz.grid.util.CoverageUtils;
import com.raytheon.viz.grid.util.SliceUtil;

/**
 * TODO Add Description
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
    // need to move to a static timed cache
    private static Map<GridCoverage, FloatDataRecord> topoCache = new HashMap<GridCoverage, FloatDataRecord>();

    public TopoRequestableData(String modelName) {
        this.source = modelName;
        this.parameter = "staticTopo";
        this.parameterName = "Topography";
        this.unit = SI.METER;
        this.level = LevelFactory.getInstance().getLevel("SFC", 0.0);
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.viz.grid.util.AbstractRequestableData#getDataValue()
     */
    @Override
    public FloatDataRecord getDataValue(Object arg) throws VizException {
        GridCoverage coverage = CoverageUtils.getInstance().getCoverage(source);
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
        if (arg == null) {
            return rval;
        } else if (arg instanceof Request) {
            return SliceUtil.slice(rval, (Request) arg);
        }
        throw new VizException(this.getClass().getSimpleName()
                + " cannot process request of type: "
                + arg.getClass().getSimpleName());
    }
}
