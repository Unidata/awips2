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
package com.raytheon.edex.plugin.grib.util;

import java.lang.ref.WeakReference;
import java.util.List;
import java.util.Map;
import java.util.WeakHashMap;

import com.raytheon.edex.plugin.grib.dao.GribModelDao;
import com.raytheon.uf.common.dataplugin.grib.GribModel;
import com.raytheon.uf.edex.database.DataAccessLayerException;

/**
 * TODO Add Description
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jan 29, 2010            rjpeter     Initial creation
 * </pre>
 * 
 * @author rjpeter
 * @version 1.0
 */

public class GribModelCache {
    private static GribModelCache instance;

    private Map<Integer, WeakReference<GribModel>> modelMap = new WeakHashMap<Integer, WeakReference<GribModel>>();

    private Map<Integer, Object> threadSyncMap = new WeakHashMap<Integer, Object>();

    private GribModelCache() {

    }

    public static synchronized GribModelCache getInstance() {
        if (instance == null) {
            instance = new GribModelCache();
        }

        return instance;
    }

    public GribModel getModel(GribModel modelToLookup)
            throws DataAccessLayerException {
        if (modelToLookup.getId() == null) {
            modelToLookup.generateId();
        }

        Integer key = modelToLookup.getId();

        GribModel rval = null;
        WeakReference<GribModel> reference = modelMap.get(key);

        if (reference == null || (rval = reference.get()) == null) {
            // quick check for in memory reference failed, grab sync lock and
            // check database
            synchronized (getThreadSyncObject(key)) {
                // due to sync lock need to double check map
                reference = modelMap.get(key);

                if (reference == null || (rval = reference.get()) == null) {
                    // was no prior sync'd thread
                    GribModelDao dao = new GribModelDao();
                    rval = dao.checkModel(modelToLookup);

                    if (rval == null) {
                        rval = modelToLookup;
                        dao.saveOrUpdate(rval);
                    }

                    modelMap.put(rval.getId(), new WeakReference<GribModel>(
                            rval));
                }
            }
        }

        // double check the location information hasn't changed
        if (modelToLookup.getLocation().getId() != rval.getLocation().getId()) {
            synchronized (getThreadSyncObject(key)) {
                rval = modelToLookup;
                GribModelDao dao = new GribModelDao();
                dao.saveOrUpdate(rval);
                modelMap.put(rval.getId(), new WeakReference<GribModel>(rval));
            }
        }

        return rval;
    }

    public void purgeCache(List<Integer> modelKeys) {
        synchronized (modelMap) {
            for (Integer key : modelKeys) {
                modelMap.remove(key);
            }
        }
    }

    private synchronized Object getThreadSyncObject(Integer key) {
        Object rval = threadSyncMap.get(key);

        if (rval == null) {
            rval = new Object();
            threadSyncMap.put(key, rval);
        }

        return rval;
    }
}
