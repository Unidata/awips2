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

import javax.measure.unit.SI;

import com.raytheon.uf.common.comm.CommunicationException;
import com.raytheon.uf.common.dataplugin.grib.spatial.projections.GridCoverage;
import com.raytheon.uf.common.dataplugin.grib.util.GribModelLookup;
import com.raytheon.uf.common.dataplugin.grib.util.GridModel;
import com.raytheon.uf.common.dataplugin.grib.util.StaticGridData;
import com.raytheon.uf.common.dataplugin.grib.util.StaticGridDataType;
import com.raytheon.uf.common.dataplugin.level.LevelFactory;
import com.raytheon.uf.common.datastorage.Request;
import com.raytheon.uf.common.datastorage.records.FloatDataRecord;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.viz.core.exception.VizException;
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

public class StaticGridRequestableData extends AbstractRequestableData {
    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(StaticGridRequestableData.class);

    private StaticGridDataType dataType;

    public StaticGridRequestableData(StaticGridDataType dataType, String source) {
        this.dataType = dataType;
        this.source = source;
        this.parameter = dataType.toString();
        this.parameterName = dataType.toString();
        if (StaticGridDataType._dt.equals(dataType)) {
            this.unit = SI.SECOND;
        } else {
            this.unit = SI.METER;
        }
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
    public Object getDataValue(Object arg) throws VizException {
        FloatDataRecord rval = null;

        if (StaticGridDataType._dt.equals(dataType)) {
            int dTinSeconds = 0;
            GridModel model = GribModelLookup.getInstance().getModelByName(
                    source);

            if (model != null) {
                dTinSeconds = model.getDt();

                // dT <= 24 is in hours, need to convert to seconds
                if (Math.abs(dTinSeconds) <= 24) {
                    dTinSeconds *= 3600;
                }
            }

            return new Float(dTinSeconds);
        } else {
            GridCoverage coverage = CoverageUtils.getInstance().getCoverage(
                    source);
            StaticGridData data = StaticGridData.getInstance(coverage);

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
        }
        if (arg instanceof Request) {
            return SliceUtil.slice(rval, (Request) arg);
        } else {
            return rval;
        }

    }
}