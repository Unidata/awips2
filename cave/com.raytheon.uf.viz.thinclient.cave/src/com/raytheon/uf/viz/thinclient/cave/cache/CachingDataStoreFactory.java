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

import org.eclipse.jface.preference.IPreferenceStore;
import org.eclipse.jface.util.IPropertyChangeListener;
import org.eclipse.jface.util.PropertyChangeEvent;

import com.raytheon.uf.common.datastorage.IDataStore;
import com.raytheon.uf.common.datastorage.IDataStoreFactory;
import com.raytheon.uf.viz.thinclient.Activator;
import com.raytheon.uf.viz.thinclient.preferences.ThinClientPreferenceConstants;

/**
 * IDataStore factory that constructs instances of CachingDataStores
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date          Ticket#  Engineer    Description
 * ------------- -------- ----------- --------------------------
 * Nov 08, 2011           mschenke    Initial creation
 * Sep 18, 2013  2309     bsteffen    Share a single DataStoreCache for all
 *                                    data stores.
 * 
 * </pre>
 * 
 * @author mschenke
 * @version 1.0
 */

public class CachingDataStoreFactory implements IDataStoreFactory,
        IPropertyChangeListener {

    private IDataStoreFactory delegateFactory;

    private boolean cachingData;

    private DataStoreCache cache;

    public CachingDataStoreFactory(IDataStoreFactory delegateFactory) {
        this.delegateFactory = delegateFactory;
        IPreferenceStore store = Activator.getDefault().getPreferenceStore();
        cachingData = store
                .getBoolean(ThinClientPreferenceConstants.P_CACHE_WEATHER);
        File cacheDir = new File(
                store.getString(ThinClientPreferenceConstants.P_CACHE_DIR));
        cache = new DataStoreCache(cacheDir);
        store.addPropertyChangeListener(this);
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.common.datastorage.IDataStoreFactory#getDataStore(java
     * .io.File, boolean)
     */
    @Override
    public IDataStore getDataStore(File file, boolean useLocking) {
        IDataStore dataStore = delegateFactory.getDataStore(file, useLocking);
        if (cachingData) {
            dataStore = new CachingDataStore(dataStore, cache);
        }
        return dataStore;
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
        if (ThinClientPreferenceConstants.P_CACHE_WEATHER.equals(event
                .getProperty())) {
            cachingData = Boolean.valueOf(String.valueOf(event.getNewValue()));
        } else if (ThinClientPreferenceConstants.P_CACHE_DIR.equals(event
                .getProperty())) {
            File cacheDir = new File(String.valueOf(event.getNewValue()));
            cache = new DataStoreCache(cacheDir);
        }
    }

}
