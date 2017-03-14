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

import java.lang.ref.SoftReference;
import java.util.Collections;
import java.util.HashMap;
import java.util.Map;

import com.raytheon.uf.common.dataplugin.grid.GridRecord;
import com.raytheon.uf.common.dataplugin.grid.datastorage.GridDataRetriever;
import com.raytheon.uf.common.datastorage.Request;
import com.raytheon.uf.common.datastorage.StorageException;
import com.raytheon.uf.common.datastorage.records.IDataRecord;
import com.raytheon.uf.common.derivparam.library.DerivParamDesc;
import com.raytheon.uf.common.derivparam.library.DerivedParameterGenerator;
import com.raytheon.uf.common.inventory.data.AbstractRequestableData;
import com.raytheon.uf.common.inventory.exception.DataCubeException;

/**
 * A requestable data object for which wraps a GridRecord.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date          Ticket#  Engineer  Description
 * ------------- -------- --------- -----------------
 * Mar 18, 2010  4646     bsteffen  Initial creation
 * Mar 03, 2016  5439     bsteffen  Move to common
 * 
 * </pre>
 * 
 * @author bsteffen
 */
public class GridRequestableData extends AbstractRequestableData {

    protected Map<Request, SoftReference<IDataRecord[]>> cache = Collections
            .synchronizedMap(new HashMap<Request, SoftReference<IDataRecord[]>>());

    protected Map<Request, Long> timeRequested = Collections
            .synchronizedMap(new HashMap<Request, Long>());

    protected GridRecord gridSource;

    protected GridRequestableData() {
    }

    public GridRequestableData(GridRecord source) {
        this.gridSource = source;
        this.source = source.getDatasetId();
        this.dataTime = source.getDataTime();
        this.space = source.getLocation();
        this.level = source.getLevel();
        this.parameter = source.getParameter().getAbbreviation();
        this.unit = source.getParameter().getUnit();
        if (DerivedParameterGenerator.getDerParLibrary().containsKey(
                this.parameter)) {
            DerivParamDesc derivParamDesc = DerivedParameterGenerator
                    .getDerParLibrary().get(this.parameter);
            this.parameterName = derivParamDesc.getName();
        } else {
            this.parameterName = source.getParameter().getName();
        }
    }

    /**
     * @return the source
     */
    public GridRecord getGridSource() {
        return gridSource;
    }

    public void setGridSource(GridRecord gridSource) {
        this.gridSource = gridSource;
    }

    public boolean needsRequest(Request request) {
        if (request == null) {
            return needsRequest(Request.ALL);
        }

        SoftReference<IDataRecord[]> record = cache.get(request);
        boolean notCached = record == null || record.get() == null;
        Long timeLastRequest = timeRequested.get(request);
        long curTime = System.currentTimeMillis();

        if (notCached
                && (timeLastRequest == null || (curTime - timeLastRequest) > 15000)) {
            timeRequested.put(request, curTime);
            return true;
        }

        return false;
    }

    @Override
    public IDataRecord[] getDataValue(Object arg) throws DataCubeException {

        if (arg instanceof Request) {
            Request request = (Request) arg;
            SoftReference<IDataRecord[]> record = cache.get(request);
            if (record != null) {
                IDataRecord[] result = record.get();
                if (result != null) {
                    return result;
                }
            }

            IDataRecord[] result = null;
            synchronized (this) {
                record = cache.get(request);
                if (record != null) {
                    result = record.get();
                    if (result != null) {
                        return result;
                    }
                }

                long time = System.currentTimeMillis();
                Long timeLastRequest = timeRequested.get(request);

                // wait up to 15 seconds
                long waitTime = 0;
                if (timeLastRequest != null) {
                    waitTime = 15000 - (time - timeLastRequest);
                }

                if (waitTime > 0) {
                    try {
                        this.wait(waitTime);
                    } catch (InterruptedException e) {
                        // ignore
                    }

                    record = cache.get(request);
                    if (record != null) {
                        result = record.get();
                        if (result != null) {
                            return result;
                        }
                    }
                }

                try {
                    IDataRecord dataRecord = GridDataRetriever.retrieve(
                            gridSource, (Request) arg);
                    result = new IDataRecord[] { dataRecord };

                    cache.put(request, new SoftReference<>(result));
                } catch (StorageException e) {
                    throw new DataCubeException("Cannot request grid data for "
                            + gridSource, e);
                }
            }

            return result;
        } else {
            return getDataValue(Request.ALL);
        }

    }

    public void setDataValue(Request request, IDataRecord[] records) {

        synchronized (this) {
            cache.put(request, new SoftReference<>(records));
            timeRequested.put(request, null);
            this.notifyAll();
        }
    }

}
