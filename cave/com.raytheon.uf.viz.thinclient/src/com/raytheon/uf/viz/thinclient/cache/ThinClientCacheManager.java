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
package com.raytheon.uf.viz.thinclient.cache;

import java.io.File;

import org.eclipse.jface.preference.IPreferenceStore;
import org.eclipse.jface.util.IPropertyChangeListener;
import org.eclipse.jface.util.PropertyChangeEvent;

import com.raytheon.uf.viz.thinclient.Activator;
import com.raytheon.uf.viz.thinclient.preferences.ThinClientPreferenceConstants;

/**
 * Thin client cache manager. Manages cache storage for thing client caches
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Oct 31, 2011            mschenke     Initial creation
 * 
 * </pre>
 * 
 * @author mschenke
 * @version 1.0
 */

public class ThinClientCacheManager implements IPropertyChangeListener {

    private File thinClientCacheDir;

    private final AbstractCachePersistance[] persistors;

    public ThinClientCacheManager(AbstractCachePersistance... persistors) {
        this.persistors = persistors;
        IPreferenceStore store = Activator.getDefault().getPreferenceStore();
        store.addPropertyChangeListener(this);
        thinClientCacheDir = new File(
                store.getString(ThinClientPreferenceConstants.P_CACHE_DIR));
    }

    public void storeCaches() {
        // We'll always store the cache data to the file system
        long t0 = System.currentTimeMillis();
        File thinClientCacheDir = this.thinClientCacheDir;
        for (AbstractCachePersistance persistance : persistors) {
            String fileName = persistance.getFileName();
            File cacheFile = new File(thinClientCacheDir, fileName);
            try {
                persistance.store(cacheFile);
            } catch (Throwable t) {
                t.printStackTrace();
            }
        }
        System.out.println("Time to store thin client caches: "
                + (System.currentTimeMillis() - t0) + "ms");
    }

    public void restoreCaches() {
        // Only restore and use persisted caches if preference is set
        long t0 = System.currentTimeMillis();
        File thinClientCacheDir = this.thinClientCacheDir;
        IPreferenceStore store = Activator.getDefault().getPreferenceStore();
        for (AbstractCachePersistance persistance : persistors) {
            if (store.getBoolean(persistance.getPreferenceId())) {
                String fileName = persistance.getFileName();
                File cacheFile = new File(thinClientCacheDir, fileName);
                if (cacheFile.exists()) {
                    try {
                        persistance.restore(cacheFile);
                    } catch (Throwable t) {
                        t.printStackTrace();
                    }
                }
            }
        }
        System.out.println("Time to restore thin client caches: "
                + (System.currentTimeMillis() - t0) + "ms");
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * org.eclipse.jface.util.IPropertyChangeListener#propertyChange(org.eclipse
     * .jface.util.PropertyChangeEvent)
     */
    @Override
    public void propertyChange(PropertyChangeEvent event) {
        if (ThinClientPreferenceConstants.P_CACHE_DIR.equals(event
                .getProperty())) {
            // Property changed, move existing cache files to new directory in
            // case we close unexpectedly and do not re-persist
            File newCacheDir = new File(String.valueOf(event.getNewValue()));
            for (AbstractCachePersistance persistance : persistors) {
                String fileName = persistance.getFileName();
                File cacheFile = new File(thinClientCacheDir, fileName);
                if (cacheFile.exists()) {
                    cacheFile.renameTo(new File(newCacheDir, fileName));
                }
            }

            thinClientCacheDir = newCacheDir;
        }
    }

}
