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

import java.util.Collection;
import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;

import com.raytheon.uf.common.dataplugin.grid.GridConstants;
import com.raytheon.uf.common.dataplugin.grid.GridRecord;
import com.raytheon.uf.viz.core.alerts.AlertMessage;
import com.raytheon.viz.alerts.IAlertObserver;
import com.raytheon.viz.alerts.observers.ProductAlertObserver;

/**
 * Provides a cache of GridRequestableDataObjcts, since these objects have the a
 * SoftReference to the raw data loading a cached object might be able to
 * provide faster data loading.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Dec 2, 2011            rjpeter     Initial creation
 * 
 * </pre>
 * 
 * @author rjpeter
 * @version 1.0
 */

public class GridRequestableDataFactory implements IAlertObserver {
    private static GridRequestableDataFactory instance = new GridRequestableDataFactory();

    public static GridRequestableDataFactory getInstance() {
        return instance;
    }

    private Map<String, GridRequestableData> requestableDataMap = new ConcurrentHashMap<String, GridRequestableData>();

    public GridRequestableDataFactory() {
        ProductAlertObserver.addObserver(GridConstants.GRID, this);
    }

    public GridRequestableData getGridRequestableData(GridRecord record) {
        String uri = record.getDataURI();
        GridRequestableData rval = requestableDataMap.get(uri);

        if (rval == null) {
            synchronized (this) {
                // double check value still null
                rval = requestableDataMap.get(uri);
                if (rval == null) {
                    rval = new GridRequestableData(record);
                    requestableDataMap.put(uri, rval);
                }
            }
        }

        return rval;
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.viz.alerts.IAlertObserver#alertArrived(java.util.Collection)
     */
    @Override
    public void alertArrived(Collection<AlertMessage> alertMessages) {
        for (AlertMessage mess : alertMessages) {
            requestableDataMap.remove(mess.dataURI);
        }
    }
}
