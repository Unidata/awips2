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
package com.raytheon.uf.viz.thinclient.localization;

import java.io.File;

import com.raytheon.uf.common.localization.IPathManager;
import com.raytheon.uf.common.localization.PathManagerFactory;
import com.raytheon.uf.viz.core.localization.CAVELocalizationAdapter;
import com.raytheon.uf.viz.core.localization.CAVELocalizationNotificationObserver;
import com.raytheon.uf.viz.thinclient.cache.AbstractCachePersistance;
import com.raytheon.uf.viz.thinclient.preferences.ThinClientPreferenceConstants;

/**
 * Thin client cache persistence for LocalizationFiles
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Oct 31, 2011            mschenke     Initial creation
 * Jul 24, 2014 3378       bclement     cache serialization done via interface
 * 
 * </pre>
 * 
 * @author mschenke
 * @version 1.0
 */

public class LocalizationCachePersistence extends AbstractCachePersistance {

    public LocalizationCachePersistence() {
        super(ThinClientPreferenceConstants.P_CACHE_LOCALIZATION,
                "localization.cache");
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
        IPathManager pathManager = PathManagerFactory.getPathManager();
        try {
            pathManager.storeCache(cacheFile);
        } catch (Exception e) {
            e.printStackTrace();
        }
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
        IPathManager pathManager = PathManagerFactory.getPathManager();
        try {
            pathManager.restoreCache(cacheFile);
        } catch (Exception e) {
            e.printStackTrace();
        }
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.viz.thinclient.cache.AbstractCachePersistance#enable()
     */
    @Override
    protected void enable() {
        CAVELocalizationNotificationObserver.unregister();
        PathManagerFactory.setAdapter(new ThinClientLocalizationAdapter());
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.viz.thinclient.cache.AbstractCachePersistance#disable()
     */
    @Override
    protected void disable() {
        CAVELocalizationNotificationObserver.register();
        PathManagerFactory.setAdapter(new CAVELocalizationAdapter());
    }
}
