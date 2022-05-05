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
package com.raytheon.uf.common.dataplugin.grid.derivparam.data;

import si.uom.SI;

import org.geotools.coverage.grid.GridGeometry2D;

import com.raytheon.uf.common.dataplugin.grid.dataset.DatasetInfo;
import com.raytheon.uf.common.dataplugin.grid.dataset.DatasetInfoLookup;
import com.raytheon.uf.common.dataplugin.grid.util.StaticGridData;
import com.raytheon.uf.common.dataplugin.grid.util.StaticGridDataType;
import com.raytheon.uf.common.dataplugin.level.LevelFactory;
import com.raytheon.uf.common.datastorage.Request;
import com.raytheon.uf.common.datastorage.records.FloatDataRecord;
import com.raytheon.uf.common.geospatial.util.GridGeometryWrapChecker;
import com.raytheon.uf.common.gridcoverage.GridCoverage;
import com.raytheon.uf.common.inventory.TimeAndSpace;
import com.raytheon.uf.common.inventory.data.AbstractRequestableData;
import com.raytheon.uf.common.inventory.exception.DataCubeException;

/**
 * Requestable data object that returns static data for a GridCoverage dx,dy, or
 * coriolis.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date          Ticket#  Engineer  Description
 * ------------- -------- --------- ------------------------------
 * Jan 15, 2010  3965     rjpeter   Initial creation
 * Sep 09, 2014  3356     njensen   Remove CommunicationException
 * Mar 22, 2016  5439     bsteffen  Move to common
 * Nov 09, 2018  7531     bsteffen  Support worldWrapX
 * 
 * </pre>
 * 
 * @author rjpeter
 */
public class StaticGridRequestableData extends AbstractRequestableData {

    private StaticGridDataType dataType;

    public StaticGridRequestableData(StaticGridDataType dataType, String source,
            GridCoverage coverage) {
        this.dataType = dataType;
        this.source = source;
        this.parameter = dataType.toString();
        this.parameterName = dataType.toString();
        this.space = coverage;
        this.dataTime = TimeAndSpace.TIME_AGNOSTIC;
        if (StaticGridDataType._dt.equals(dataType)) {
            this.unit = SI.SECOND;
        } else {
            this.unit = SI.METRE;
        }
        this.level = LevelFactory.getInstance().getLevel("SFC", 0.0);
    }

    @Override
    public Object getDataValue(Object arg) throws DataCubeException {
        FloatDataRecord rval = null;

        if (StaticGridDataType._dt == dataType) {
            int dTinSeconds = 0;
            DatasetInfo info = DatasetInfoLookup.getInstance().getInfo(source);

            if (info != null) {
                dTinSeconds = info.getDt();

                // dT <= 24 is in hours, need to convert to seconds
                if (Math.abs(dTinSeconds) <= 24) {
                    dTinSeconds *= 3600;
                }
            }

            return new Float(dTinSeconds);
        } else if (StaticGridDataType.worldWrapX == dataType) {
            if (this.space != null) {
                GridGeometry2D gridGeom = this.space.getGridGeometry();
                int nx = GridGeometryWrapChecker.checkForWrapping(gridGeom);
                if (GridGeometryWrapChecker.NO_WRAP == nx) {
                    return Boolean.FALSE;
                } else if (arg instanceof Request) {
                    Request request = (Request) arg;
                    switch (request.getType()) {
                    case ALL:
                    case XLINE:
                        return Boolean.TRUE;
                    case SLAB:
                        int[] max = request.getMaxIndexForSlab();
                        int[] min = request.getMinIndexForSlab();
                        return Boolean.valueOf(max[0] - min[0] == nx);
                    case POINT:
                    case YLINE:
                    default:
                        return Boolean.FALSE;
                    }
                } else {
                    return Boolean.TRUE;
                }
            } else {
                return Boolean.FALSE;
            }
        } else {
            if (this.space instanceof GridCoverage) {
                StaticGridData data = StaticGridData
                        .getInstance((GridCoverage) this.space);
                switch (dataType) {
                case coriolis:
                    rval = data.getCoriolis();
                    break;
                case dx:
                    rval = data.getDx();
                    break;
                case dy:
                    rval = data.getDy();
                    break;
                }
            } else {
                throw new IllegalStateException(
                        "Cannot get static topo for: " + this.space);
            }

        }
        if (arg instanceof Request) {
            return SliceUtil.slice(rval, (Request) arg);
        } else {
            return rval;
        }

    }
}