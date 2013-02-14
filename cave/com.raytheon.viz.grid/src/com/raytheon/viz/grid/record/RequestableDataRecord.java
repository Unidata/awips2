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

import com.raytheon.uf.common.dataplugin.PluginException;
import com.raytheon.uf.common.dataplugin.grid.GridConstants;
import com.raytheon.uf.common.dataplugin.grid.GridRecord;
import com.raytheon.uf.common.datastorage.Request;
import com.raytheon.uf.common.datastorage.records.FloatDataRecord;
import com.raytheon.uf.common.datastorage.records.IDataRecord;
import com.raytheon.uf.common.gridcoverage.GridCoverage;
import com.raytheon.uf.common.parameter.Parameter;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.derivparam.data.AbstractRequestableData;
import com.raytheon.viz.grid.data.GridRequestableData;
import com.raytheon.viz.grid.data.TiltRequestableData.TiltCenterPoint;
import com.raytheon.viz.grid.util.TiltRequest;

/**
 * A PDO that extends GridRecord and wraps a AbstractRequestableData to allow
 * derived parameters to be used anywhere GridRecords can be used.
 *
 * <pre>
 *
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Mar 18, 2010            bsteffen     Initial creation
 * 
 *
 * </pre>
 *
 * @author bsteffen
 * @version 1.0
 */

public class RequestableDataRecord extends GridRecord {

    private static final long serialVersionUID = 1L;

    private final AbstractRequestableData requester;

    public RequestableDataRecord(AbstractRequestableData requester)
            throws VizException {
        this.requester = requester;
        GridCoverage coverage = null;
        if (requester.getSpace() instanceof GridCoverage) {
            coverage = (GridCoverage) requester.getSpace();
        if (requester instanceof GridRequestableData) {
            setSecondaryId(((GridRequestableData) requester).getGridSource().getSecondaryId());
        }
        }
        setDatasetId(requester.getSource());
        setLocation(coverage);
        setLevel(requester.getLevel());
        Parameter parameter = new Parameter(requester.getParameter(),
                requester.getParameterName(), requester.getUnit());

        setParameter(parameter);
        setPluginName(GridConstants.GRID);
        setDataTime(requester.getDataTime());
        try {
            constructDataURI();
        } catch (PluginException e) {
            throw new VizException(e);
        }
    }

    public RequestableDataRecord(GridRequestableData requester)
            throws VizException {
        super(requester.getGridSource());
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
        Object obj = null;
        if (request instanceof TiltRequest) {
            obj = requester.getDataValue(new TiltCenterPoint(
                    ((TiltRequest) request).getTiltLocation()));
        } else {
            obj = requester.getDataValue(request);
        }
        if (obj instanceof IDataRecord[]) {
            return (IDataRecord[]) obj;
        } else if (obj instanceof IDataRecord) {
            return new IDataRecord[] { (IDataRecord) obj };
        } else if (obj instanceof Number) {
            Integer nx = 0;
            Integer ny = 0;
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
                default:
                    nx = this.getLocation().getNx();
                    ny = this.getLocation().getNy();
                }
            }
            float[] data = new float[nx * ny];
            for (int i = 0; i < nx * ny; i++) {
                data[i] = ((Number) obj).floatValue();
            }
            FloatDataRecord rec = new FloatDataRecord(this.getParameter()
                    .getName(), this.getPluginName(), data, 2, new long[] { nx,
                    ny });
            return new IDataRecord[] { rec };
        }
        return null;
    }

    /**
     * @return
     */
    public Collection<GridRequestableData> getGribRequests() {
        List<GridRequestableData> results = new ArrayList<GridRequestableData>();
        List<AbstractRequestableData> list = new ArrayList<AbstractRequestableData>();
        list.add(requester);
        AbstractRequestableData current = null;
        for (int i = 0; i < list.size(); i++) {
            current = list.get(i);
            if (current.getClass().equals(GridRequestableData.class)) {
                results.add((GridRequestableData) current);
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
