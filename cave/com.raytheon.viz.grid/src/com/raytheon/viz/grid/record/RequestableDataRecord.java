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
package com.raytheon.viz.grid.record;

import java.util.ArrayList;
import java.util.Collection;
import java.util.List;

import javax.measure.unit.Unit;
import javax.measure.unit.UnitFormat;

import com.raytheon.uf.common.dataplugin.PluginException;
import com.raytheon.uf.common.dataplugin.grib.GribModel;
import com.raytheon.uf.common.dataplugin.grib.GribRecord;
import com.raytheon.uf.common.dataplugin.grib.spatial.projections.GridCoverage;
import com.raytheon.uf.common.datastorage.Request;
import com.raytheon.uf.common.datastorage.records.FloatDataRecord;
import com.raytheon.uf.common.datastorage.records.IDataRecord;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.derivparam.data.AbstractRequestableData;
import com.raytheon.viz.grid.data.GribRequestableData;
import com.raytheon.viz.grid.util.CoverageUtils;

/**
 * The RequestableDataRecord Class
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Mar 18, 2010            bsteffen     Initial creation
 * 
 * </pre>
 * 
 * @author bsteffen
 * @version 1.0
 */

public class RequestableDataRecord extends GribRecord {

    private static final long serialVersionUID = 1L;

    private AbstractRequestableData requester;

    public RequestableDataRecord(AbstractRequestableData requester)
            throws VizException {
        this.requester = requester;
        GridCoverage coverage = CoverageUtils.getInstance().getCoverage(
                requester.getSource());
        if (coverage == null && requester instanceof GribRequestableData) {
            coverage = ((GribRequestableData) requester).getGribSource()
                    .getModelInfo().getLocation();
            CoverageUtils.getInstance().setCoverage(requester.getSource(),
                    coverage);
        }
        GribModel modelInfo = new GribModel();
        modelInfo.setModelName(requester.getSource());
        modelInfo.setLocation(coverage);
        modelInfo.setLevel(requester.getLevel());
        modelInfo.setParameterAbbreviation(requester.getParameter());
        modelInfo.setParameterName(requester.getParameterName());
        if (requester.getUnit() != Unit.ONE) {
            modelInfo.setParameterUnit(UnitFormat.getUCUMInstance().format(
                    requester.getUnit()));
        }
        setPluginName("grib");
        setDataTime(requester.getDataTime());
        setModelInfo(modelInfo);
        try {
            constructDataURI();
        } catch (PluginException e) {
            throw new VizException(e);
        }
    }

    public RequestableDataRecord(GribRequestableData requester)
            throws VizException {
        super(requester.getGribSource());
        this.requester = requester;
    }

    public RequestableDataRecord(RequestableDataRecord pdo) {
        super(pdo);
        this.requester = pdo.requester;
    }

    public IDataRecord[] getDataRecord() throws VizException {
        return getDataRecord(Request.ALL);
    }

    public IDataRecord[] getDataRecord(Request request) throws VizException {
        Object obj = requester.getDataValue(request);
        if (obj instanceof IDataRecord[]) {
            return (IDataRecord[]) obj;
        } else if (obj instanceof IDataRecord) {
            return new IDataRecord[] { (IDataRecord) obj };
        } else if (obj instanceof Number) {
            Integer nx = this.getModelInfo().getLocation().getNx();
            Integer ny = this.getModelInfo().getLocation().getNy();
            if (request != null) {
                switch (request.getType()) {
                case POINT:
                    nx = ny = 1;
                    break;
                case SLAB:
                    nx = request.getMaxIndexForSlab()[0]
                            - request.getMinIndexForSlab()[0];
                    ny = request.getMaxIndexForSlab()[1]
                            - request.getMinIndexForSlab()[1];
                    break;
                case XLINE:
                    nx = request.getIndices().length;
                    ny = 1;
                    break;
                case YLINE:
                    ny = request.getIndices().length;
                    nx = 1;
                    break;
                }
            }
            float[] data = new float[nx * ny];
            for (int i = 0; i < nx * ny; i++) {
                data[i] = ((Number) obj).floatValue();
            }
            FloatDataRecord rec = new FloatDataRecord(this.getModelInfo()
                    .getParameterName(), this.getPluginName(), data, 2,
                    new long[] { nx, ny });
            return new IDataRecord[] { rec };
        }
        return null;
    }

    /**
     * @return
     */
    public Collection<GribRequestableData> getGribRequests() {
        List<GribRequestableData> results = new ArrayList<GribRequestableData>();
        List<AbstractRequestableData> list = new ArrayList<AbstractRequestableData>();
        list.add(requester);
        AbstractRequestableData current = null;
        for (int i = 0; i < list.size(); i++) {
            current = list.get(i);
            if (current.getClass().equals(GribRequestableData.class)) {
                results.add((GribRequestableData) current);
            } else {
                for (AbstractRequestableData data : current.getDependencies()) {
                    if (data != null) {
                        list.add(data);
                    }
                }
            }
        }
        return results;
    }
}
