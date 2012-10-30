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

import java.lang.ref.SoftReference;
import java.util.Collections;
import java.util.HashMap;
import java.util.Map;

import com.raytheon.uf.common.dataplugin.grid.GridRecord;
import com.raytheon.uf.common.datastorage.Request;
import com.raytheon.uf.common.datastorage.records.IDataRecord;
import com.raytheon.uf.viz.core.datastructure.DataCubeContainer;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.derivparam.data.AbstractRequestableData;
import com.raytheon.uf.viz.derivparam.library.DerivParamDesc;
import com.raytheon.uf.viz.derivparam.library.DerivedParameterGenerator;

/**
 * A requestable data object for which wraps a GridRecord.
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

public class GridRequestableData extends AbstractRequestableData {

    protected Map<Request, SoftReference<IDataRecord[]>> cache = Collections
            .synchronizedMap(new HashMap<Request, SoftReference<IDataRecord[]>>());

    protected Map<Request, Long> timeRequested = Collections
            .synchronizedMap(new HashMap<Request, Long>());

    protected GridRecord gridSource;

    protected GridRequestableData() {
    }

    protected GridRequestableData(GridRecord source) {
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
    public IDataRecord[] getDataValue(Object arg) throws VizException {

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

                // if (timeLastRequest != null) {
                // System.out.println("Cache miss for " + this.parameter + "_"
                // + this.level.toString() + " data bulk requested "
                // + (time - timeLastRequest) + " ms ago");
                // }

                result = DataCubeContainer.getDataRecord(gridSource,
                        (Request) arg, null);

                cache.put(request, new SoftReference<IDataRecord[]>(result));
            }

            return result;
        } else {
            return getDataValue(Request.ALL);
        }

    }

    public void setDataValue(Request request, IDataRecord[] records) {
        // Long timeLastRequest = timeRequested.get(request);
        // if (timeLastRequest != null) {
        // System.out.println("Putting data into cache for " + this.parameter
        // + "_" + this.level.toString() + " took "
        // + (System.currentTimeMillis() - timeLastRequest) + " ms");
        // }

        synchronized (this) {
            cache.put(request, new SoftReference<IDataRecord[]>(records));
            timeRequested.put(request, null);
            this.notifyAll();
        }
    }

}
