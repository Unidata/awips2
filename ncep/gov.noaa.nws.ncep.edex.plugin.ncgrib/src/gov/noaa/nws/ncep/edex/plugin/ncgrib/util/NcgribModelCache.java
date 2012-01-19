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
package gov.noaa.nws.ncep.edex.plugin.ncgrib.util;

import java.lang.ref.WeakReference;
import java.util.Map;
import java.util.WeakHashMap;

import com.raytheon.uf.edex.database.DataAccessLayerException;

import gov.noaa.nws.ncep.edex.plugin.ncgrib.dao.NcgribModelDao;
import gov.noaa.nws.ncep.common.dataplugin.ncgrib.NcgribModel;


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

public class NcgribModelCache {
    private static NcgribModelCache instance;

    private Map<Integer, WeakReference<NcgribModel>> modelMap = new WeakHashMap<Integer, WeakReference<NcgribModel>>();

    private Map<Integer, Object> threadSyncMap = new WeakHashMap<Integer, Object>();

    private NcgribModelCache() {

    }

    public static synchronized NcgribModelCache getInstance() {
        if (instance == null) {
            instance = new NcgribModelCache();
        }

        return instance;
    }

    public NcgribModel getModel(NcgribModel modelToLookup)
            throws DataAccessLayerException {
        if (modelToLookup.getId() == null) {
            modelToLookup.generateId();
        }

        Integer key = modelToLookup.getId();

        NcgribModel rval = null;
        WeakReference<NcgribModel> reference = modelMap.get(key);

        if (reference == null || (rval = reference.get()) == null) {
            // quick check for in memory reference failed, grab sync lock and
            // check database
            synchronized (getThreadSyncObject(key)) {
                // due to sync lock need to double check map
                reference = modelMap.get(key);

                if (reference == null || (rval = reference.get()) == null) {
                    // was no prior sync'd thread
                    NcgribModelDao dao = new NcgribModelDao();
                    rval = dao.checkModel(modelToLookup);

                    if (rval == null) {
                        rval = modelToLookup;
                        dao.saveOrUpdate(rval);
                    }

                    modelMap.put(rval.getId(), new WeakReference<NcgribModel>(
                            rval));
                }
            }
        }

        // double check the location information hasn't changed
        if (modelToLookup.getLocation().getId() != rval.getLocation().getId()) {
            synchronized (getThreadSyncObject(key)) {
                rval = modelToLookup;
                NcgribModelDao dao = new NcgribModelDao();
                dao.saveOrUpdate(rval);
                modelMap.put(rval.getId(), new WeakReference<NcgribModel>(rval));
            }
        }

        return rval;
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
