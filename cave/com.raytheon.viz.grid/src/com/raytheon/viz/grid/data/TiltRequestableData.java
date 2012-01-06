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

import com.raytheon.uf.common.dataplugin.grib.spatial.projections.GridCoverage;
import com.raytheon.uf.common.dataplugin.level.Level;
import com.raytheon.uf.common.datastorage.Request;
import com.raytheon.uf.common.datastorage.records.FloatDataRecord;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.derivparam.data.AbstractRequestableData;
import com.raytheon.viz.grid.util.CoverageUtils;
import com.raytheon.viz.grid.util.SliceUtil;
import com.raytheon.viz.grid.util.TiltUtils;

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

public class TiltRequestableData extends AbstractRequestableData {

    public TiltRequestableData(String modelName, Level tiltAngle) {
        this.source = modelName;
        this.unit = SI.METER;
        this.parameter = "TILT";
        this.parameterName = "TILT";
        this.level = tiltAngle;
    }

    @Override
    public FloatDataRecord getDataValue(Object arg) throws VizException {

        GridCoverage coverage = CoverageUtils.getInstance().getCoverage(source);
        FloatDataRecord fdr = TiltUtils.getInstance().getHeightGrid(coverage,
                level.getLevelonevalue());
        if (arg == null) {
            return fdr;
        } else if (arg instanceof Request) {
            return SliceUtil.slice(fdr, (Request) arg);
        }
        throw new VizException(this.getClass().getSimpleName()
                + " cannot process request of type: "
                + arg.getClass().getSimpleName());
    }

}
