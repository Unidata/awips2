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
import java.util.HashMap;
import java.util.Map;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.ExecutionException;
import java.util.concurrent.Future;
import java.util.concurrent.FutureTask;
import java.util.concurrent.RunnableFuture;

import com.raytheon.uf.common.dataplugin.grid.GridRecord;
import com.raytheon.uf.common.dataplugin.grid.datastorage.GridDataRetriever;
import com.raytheon.uf.common.datastorage.Request;
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
 * ------------- -------- --------- --------------------------------------------
 * Mar 18, 2010  4646     bsteffen  Initial creation
 * Mar 03, 2016  5439     bsteffen  Move to common
 * Aug 15, 2017  6332     bsteffen  Clone incoming request to ensure it is not a
 *                                  subclass.
 * Mar 29, 2021  8374     randerso  Re-implemented copyFrom as shallowCopy.
 * Jul 28, 2021  8611     randerso  Moved cache retrieval into a separate method
 *                                  for reuse in subclasses.
 * Jan 26, 2022  8741     njensen   Renamed setDataValue() to cacheDataValue()
 * Feb 22, 2023  9021     mapeters  Cache data as Futures
 * May 22, 2024  2037092  mapeters  Make gridSource final, remove empty constructor
 *
 * </pre>
 *
 * @author bsteffen
 */
public class GridRequestableData extends AbstractRequestableData {

    /**
     * Cache of requested data. Futures are used because they make it easier to
     * handle concurrent requests and to invalidate in-process data requests (in
     * subclasses).
     */
    protected final Map<Request, SoftReference<Future<IDataRecord[]>>> cache = new HashMap<>();

    protected final GridRecord gridSource;

    public GridRequestableData(GridRecord source) {
        this.gridSource = source;
        this.source = source.getDatasetId();
        this.dataTime = source.getDataTime();
        this.space = source.getLocation();
        this.level = source.getLevel();
        this.parameter = source.getParameter().getAbbreviation();
        this.unit = source.getParameter().getUnit();
        if (DerivedParameterGenerator.getDerParLibrary()
                .containsKey(this.parameter)) {
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

    public boolean needsRequest(Request request) {
        if (request == null) {
            request = Request.ALL;
        }

        return getCachedValue(request) == null;
    }

    protected Future<IDataRecord[]> getCachedValue(Request request) {
        Future<IDataRecord[]> recordFuture = null;

        synchronized (cache) {
            SoftReference<Future<IDataRecord[]>> futureRef = cache.get(request);
            if (futureRef != null) {
                recordFuture = futureRef.get();
            }
        }

        return recordFuture;
    }

    @Override
    public IDataRecord[] getDataValue(Object arg) throws DataCubeException {
        Request request = (arg instanceof Request ? (Request) arg : Request.ALL)
                .shallowCopy();

        Future<IDataRecord[]> recordFuture;
        synchronized (cache) {
            recordFuture = getCachedValue(request);

            if (recordFuture == null) {
                recordFuture = new FutureTask<>(() -> {
                    IDataRecord dataRecord = GridDataRetriever
                            .retrieve(gridSource, request);
                    return new IDataRecord[] { dataRecord };
                });
                cache.put(request, new SoftReference<>(recordFuture));
            }
        }

        try {
            if (recordFuture instanceof RunnableFuture) {
                ((RunnableFuture<IDataRecord[]>) recordFuture).run();
            }
            return recordFuture.get();
        } catch (InterruptedException | ExecutionException e) {
            throw new DataCubeException(
                    "Cannot request grid data for " + gridSource, e);
        }
    }

    public void cacheDataValue(Request request, IDataRecord[] records) {
        synchronized (cache) {
            cache.put(request, new SoftReference<>(
                    CompletableFuture.completedFuture(records)));
        }
    }
}
