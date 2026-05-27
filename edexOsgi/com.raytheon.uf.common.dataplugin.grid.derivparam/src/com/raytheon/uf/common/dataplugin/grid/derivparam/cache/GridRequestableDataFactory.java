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
package com.raytheon.uf.common.dataplugin.grid.derivparam.cache;

import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;

import com.raytheon.uf.common.dataplugin.grid.GridRecord;
import com.raytheon.uf.common.dataplugin.grid.derivparam.cache.GridCacheUpdater.GridUpdateListener;
import com.raytheon.uf.common.dataplugin.grid.derivparam.data.GridRequestableData;

/**
 * Provides a cache of GridRequestableDataObjcts, since these objects have the a
 * SoftReference to the raw data loading a cached object might be able to
 * provide faster data loading.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date          Ticket#  Engineer  Description
 * ------------- -------- --------- -----------------
 * Dec 02, 2011  11750    rjpeter   Initial creation
 * Mar 03, 2016  5439     bsteffen  Move to common
 * 
 * </pre>
 * 
 * @author rjpeter
 */
public class GridRequestableDataFactory implements GridUpdateListener {
    private static GridRequestableDataFactory instance = new GridRequestableDataFactory();

    public static GridRequestableDataFactory getInstance() {
        return instance;
    }

    private boolean updatesEnabled = false;

    private Map<String, GridRequestableData> requestableDataMap = new ConcurrentHashMap<>();

    public GridRequestableDataFactory() {
        GridCacheUpdater.getInstance().addListener(this);
    }

    public GridRequestableData getGridRequestableData(GridRecord record) {
        String uri = record.getDataURI();
        GridRequestableData rval = requestableDataMap.get(uri);

        if (rval == null) {
            synchronized (this) {
                /* double check value still null */
                rval = requestableDataMap.get(uri);
                if (rval == null) {
                    rval = new GridRequestableData(record);
                    if (updatesEnabled) {
                        requestableDataMap.put(uri, rval);
                    }
                }
            }
        }

        return rval;
    }

    @Override
    public void update(GridRecord record) {
        requestableDataMap.remove(record.getDataURI());
    }

    @Override
    public void enableUpdates() {
        updatesEnabled = true;
    }

    @Override
    public void disableUpdates() {
        updatesEnabled = false;
        synchronized (this) {
            requestableDataMap.clear();
        }

    }

}
