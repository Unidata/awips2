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
package com.raytheon.uf.viz.thinclient.cave.cache;

import java.io.File;

import com.raytheon.uf.viz.core.spatial.GeometryCache;
import com.raytheon.uf.viz.thinclient.cache.AbstractCachePersistance;
import com.raytheon.uf.viz.thinclient.preferences.ThinClientPreferenceConstants;

/**
 * Thin client geometry cache manager, listens for preference changes to
 * geometry cache dir preference. Has methods for storing and restoring the
 * cache to/from the filesystem
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Oct 20, 2011            mschenke     Initial creation
 * Sep 04, 2014 3365       ccody        Changes for removing Data_Delivery dependencies
 * 
 * </pre>
 * 
 * @author mschenke
 * @version 1.0
 */

public class GeometryCachePersistence extends AbstractCachePersistance {

    public GeometryCachePersistence() {
        super(ThinClientPreferenceConstants.P_CACHE_MAPS, "geometry.cache");
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.viz.thinclient.cache.ThinClientCacheManager.ICachePersistance
     * #store(java.io.File)
     */
    @Override
    public void store(File cacheFile) {
        GeometryCache.storeCache(cacheFile);
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.viz.thinclient.cache.ThinClientCacheManager.ICachePersistance
     * #restore(java.io.File)
     */
    @Override
    public void restore(File cacheFile) {
        GeometryCache.restoreCache(cacheFile);
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.viz.thinclient.cache.AbstractCachePersistance#enable()
     */
    @Override
    protected void enable() {
        // No runtime changes required
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.viz.thinclient.cache.AbstractCachePersistance#disable()
     */
    @Override
    protected void disable() {
        // No runtime changes required
    }

}
