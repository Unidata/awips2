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
package com.raytheon.uf.edex.plugin.bufrmos.decoder;

import java.lang.ref.WeakReference;
import java.util.Map;
import java.util.WeakHashMap;

import com.raytheon.uf.common.dataplugin.bufrmos.common.BufrMosDataLocation;
import com.raytheon.uf.edex.database.DataAccessLayerException;
import com.raytheon.uf.edex.plugin.bufrmos.dao.BufrMosLocationDao;

/**
 * TODO Add Description
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * May 24, 2011            rjpeter     Initial creation
 * 
 * </pre>
 * 
 * @author rjpeter
 * @version 1.0
 */

public class BufrMosLocationCache {
    private static final BufrMosLocationCache instance = new BufrMosLocationCache();

    private Map<Integer, WeakReference<BufrMosDataLocation>> locationCache = new WeakHashMap<Integer, WeakReference<BufrMosDataLocation>>();;

    public static BufrMosLocationCache getInstance() {
        return instance;
    }

    private BufrMosLocationCache() {

    }

    public BufrMosDataLocation getLocation(BufrMosDataLocation locationToLookup)
            throws DataAccessLayerException {
        if (locationToLookup.getId() == null) {
            locationToLookup.generateId();
        }

        Integer key = locationToLookup.getId();

        BufrMosDataLocation rval = null;
        WeakReference<BufrMosDataLocation> reference = null;
        synchronized (locationCache) {
            reference = locationCache.get(key);
        }

        if (reference == null || (rval = reference.get()) == null) {
            BufrMosLocationDao dao = new BufrMosLocationDao();
            rval = dao.checkLocation(locationToLookup);

            if (rval == null) {
                rval = locationToLookup;
                dao.saveOrUpdate(rval);
            }

            synchronized (locationCache) {
                locationCache
                        .put(key, new WeakReference<BufrMosDataLocation>(rval));
            }
        }

        return rval;
    }
}
