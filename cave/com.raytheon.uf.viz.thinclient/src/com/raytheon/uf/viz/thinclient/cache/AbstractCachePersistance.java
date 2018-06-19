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

/**
 * Abstract cache persistance object, handles preference toggling
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Nov 15, 2011            mschenke     Initial creation
 * 
 * </pre>
 * 
 * @author mschenke
 * @version 1.0
 */

public abstract class AbstractCachePersistance implements
        IPropertyChangeListener {

    private final String preferenceId;

    private final String fileName;

    private boolean useCache;

    protected AbstractCachePersistance(String preferenceId, String fileName) {
        this.preferenceId = preferenceId;
        this.fileName = fileName;
        IPreferenceStore store = Activator.getDefault().getPreferenceStore();
        store.addPropertyChangeListener(this);
        apply(store.getBoolean(preferenceId));
    }

    /**
     * Apply the useCach setting. Will call enable or disable
     * 
     * @param useCache
     */
    private void apply(boolean useCache) {
        if (this.useCache != useCache) {
            this.useCache = useCache;
            if (useCache) {
                enable();
            } else {
                disable();
            }
        }
    }

    public final String getPreferenceId() {
        return preferenceId;
    }

    public final String getFileName() {
        return fileName;
    }

    /**
     * Enable caching for this persistence object
     */
    protected abstract void enable();

    /**
     * Disable caching for this persistence object
     */
    protected abstract void disable();

    /**
     * Store the cache to this file
     */
    public abstract void store(File cacheFile);

    /**
     * Restore cache from this file
     */
    public abstract void restore(File cacheFile);

    /*
     * (non-Javadoc)
     * 
     * @see
     * org.eclipse.jface.util.IPropertyChangeListener#propertyChange(org.eclipse
     * .jface.util.PropertyChangeEvent)
     */
    @Override
    public final void propertyChange(PropertyChangeEvent event) {
        if (getPreferenceId().equals(event.getProperty())) {
            Boolean useCache = Boolean.parseBoolean(String.valueOf(event
                    .getNewValue()));
            apply(useCache);
        }
    }

}
