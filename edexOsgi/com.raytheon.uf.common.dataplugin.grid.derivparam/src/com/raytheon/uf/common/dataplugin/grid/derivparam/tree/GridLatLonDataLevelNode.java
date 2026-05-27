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
package com.raytheon.uf.common.dataplugin.grid.derivparam.tree;

import java.util.Collection;
import java.util.HashSet;
import java.util.Map;
import java.util.Set;

import com.raytheon.uf.common.dataplugin.grid.derivparam.cache.CoverageUtils;
import com.raytheon.uf.common.dataplugin.grid.derivparam.data.SliceUtil;
import com.raytheon.uf.common.dataplugin.level.Level;
import com.raytheon.uf.common.dataquery.requests.RequestConstraint;
import com.raytheon.uf.common.datastorage.Request;
import com.raytheon.uf.common.datastorage.records.FloatDataRecord;
import com.raytheon.uf.common.derivparam.data.LatLonRequestableData;
import com.raytheon.uf.common.derivparam.tree.LatLonDataLevelNode;
import com.raytheon.uf.common.geospatial.IGridGeometryProvider;
import com.raytheon.uf.common.gridcoverage.GridCoverage;
import com.raytheon.uf.common.inventory.TimeAndSpace;
import com.raytheon.uf.common.inventory.data.AbstractRequestableData;
import com.raytheon.uf.common.inventory.exception.DataCubeException;

/**
 * Implementation of {@link LatLonDataLevelNode} for grid. Handles querying
 * for each models spatial availability and also handles subsets.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date          Ticket#  Engineer  Description
 * ------------- -------- --------- -----------------
 * Jul 17, 2017  6345     bsteffen  Initial creation
 * 
 * </pre>
 * 
 * @author bsteffen
 */
public class GridLatLonDataLevelNode extends LatLonDataLevelNode {

    public GridLatLonDataLevelNode(String source, LatOrLon parameter,
            Level level) {
        super(source, parameter, level);
    }

    @Override
    public Set<TimeAndSpace> getAvailability(
            Map<String, RequestConstraint> originalConstraints, Object response)
            throws DataCubeException {
        Set<TimeAndSpace> result = new HashSet<>();
        for (GridCoverage coverage : CoverageUtils.getInstance()
                .getCoverages(source)) {
            result.add(new TimeAndSpace(coverage));
        }
        return result;
    }

    @Override
    public Collection<? extends IGridGeometryProvider> getAvailableSpaces(
            Map<String, RequestConstraint> originalConstraints)
            throws DataCubeException {
        return CoverageUtils.getInstance().getCoverages(source);
    }

    @Override
    protected AbstractRequestableData createData() {
        return new GridLatLonRequestableData(parameter);
    }

    private static class GridLatLonRequestableData
            extends LatLonRequestableData {

        public GridLatLonRequestableData(LatOrLon parameter) {
            super(parameter);
        }

        @Override
        public FloatDataRecord getDataValue(Object arg)
                throws DataCubeException {
            FloatDataRecord record = super.getDataValue(arg);
            if (arg instanceof Request) {
                Request request = (Request) arg;
                record = SliceUtil.slice(record, request);
            }
            return record;
        }

    }

}
