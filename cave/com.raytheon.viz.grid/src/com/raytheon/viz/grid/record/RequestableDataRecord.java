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
import java.util.HashSet;
import java.util.List;
import java.util.Set;

import com.raytheon.uf.common.dataplugin.grid.GridRecord;
import com.raytheon.uf.common.dataplugin.grid.derivparam.data.GridRequestableData;
import com.raytheon.uf.common.dataplugin.grid.derivparam.data.ImportRequestableData;
import com.raytheon.uf.common.datastorage.Request;
import com.raytheon.uf.common.datastorage.records.FloatDataRecord;
import com.raytheon.uf.common.datastorage.records.IDataRecord;
import com.raytheon.uf.common.gridcoverage.GridCoverage;
import com.raytheon.uf.common.inventory.data.AbstractRequestableData;
import com.raytheon.uf.common.inventory.exception.DataCubeException;
import com.raytheon.uf.common.parameter.Parameter;
import com.raytheon.uf.viz.core.exception.VizException;

/**
 * A PDO that extends GridRecord and wraps a AbstractRequestableData to allow
 * derived parameters to be used anywhere GridRecords can be used.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date          Ticket#  Engineer     Description
 * ------------- -------- ------------ -----------------------------------------
 * Mar 18, 2010  4646     bsteffen     Initial creation
 * Aug 30, 2013  2298     rjpeter      Make getPluginName abstract
 * Nov 21, 2014  3026     mpduff       Set secondary ID if it exists.
 * Mar 15, 2016  18657    MPorricelli  Do not process dependency of Import data
 *                                     to avoid coverage mismatch with base data
 * Apr 01, 2016  5439     bsteffen     Move import node to common.
 * Apr 20, 2017  6046     bsteffen     Change grib to grid.
 * Aug 15, 2017  6332     bsteffen     Move radar specific logic to extension
 * 
 * </pre>
 * 
 * @author bsteffen
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
            Set<String> secondaryIds = new HashSet<>();
            for (GridRequestableData data : getGridRequests()) {
                secondaryIds.add(data.getGridSource().getSecondaryId());
            }
            if (secondaryIds.size() == 1) {
                setSecondaryId(secondaryIds.iterator().next());
            }
        }
        setDatasetId(requester.getSource());
        setLocation(coverage);
        setLevel(requester.getLevel());
        Parameter parameter = new Parameter(requester.getParameter(),
                requester.getParameterName(), requester.getUnit());

        setParameter(parameter);
        setDataTime(requester.getDataTime());
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

    public IDataRecord[] getDataRecord() throws DataCubeException {
        return getDataRecord(Request.ALL);
    }

    public IDataRecord[] getDataRecord(Request request)
            throws DataCubeException {
        Object obj = requester.getDataValue(request);
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
            for (int i = 0; i < (nx * ny); i++) {
                data[i] = ((Number) obj).floatValue();
            }
            FloatDataRecord rec = new FloatDataRecord(
                    this.getParameter().getName(), this.getPluginName(), data,
                    2, new long[] { nx, ny });
            return new IDataRecord[] { rec };
        }
        return null;
    }

    public Collection<GridRequestableData> getGridRequests() {
        List<GridRequestableData> results = new ArrayList<>();
        List<AbstractRequestableData> list = new ArrayList<>();
        list.add(requester);
        AbstractRequestableData current = null;
        for (int i = 0; i < list.size(); i++) {
            current = list.get(i);
            if (current.getClass().equals(GridRequestableData.class)) {
                results.add((GridRequestableData) current);
            } else {
                for (AbstractRequestableData data : current.getDependencies()) {
                    if (data != null
                            && !(data instanceof ImportRequestableData)) {
                        list.add(data);
                    }
                }
            }
        }
        return results;
    }
}
