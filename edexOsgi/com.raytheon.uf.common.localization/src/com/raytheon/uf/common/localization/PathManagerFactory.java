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
package com.raytheon.uf.common.localization;

import java.util.Observable;
import java.util.Observer;

/**
 * Handles {@link IPathManager} object creation depending on the context.
 * 
 * <p>
 * CAVE based code will use a different implementation of the
 * {@link IPathManager} than EDEX based code. In case both implementations are
 * available the CAVE based implementation will take precedence.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#     Engineer    Description
 * ------------ ----------  ----------- --------------------------
 * May 7, 2008              chammack    Initial creation
 * Jul 14, 2008 1250        jelkins     EDEX LocalizationAdapter additions.
 * 
 * </pre>
 * 
 * @author chammack
 * @version 1.0
 */

public class PathManagerFactory {

    private static final String CAVE_ADAPTER_CLASS = "com.raytheon.uf.viz.core.localization.CAVELocalizationAdapter";

    private static final String EDEX_ADAPTER_CLASS = "com.raytheon.edex.utility.EDEXLocalizationAdapter";

    static IPathManager pathManager;

    private static ILocalizationAdapter adapter;

    private static Observable internalObservable = new Observable() {
        @Override
        public synchronized void addObserver(Observer o) {
            super.addObserver(o);
            // mark changed if an observer is added
            setChanged();
        }
    };

    private PathManagerFactory() {

    }

    /**
     * Creates or retrieves the appropriate {@link IPathManager}.
     * 
     * @return the appropriate {@link IPathManager} object for the context.
     */
    public static synchronized IPathManager getPathManager() {
        if (pathManager == null) {
            if (adapter == null) {
                // TODO adapters should be passed in, not found through forName
                try {
                    pathManager = new PathManager((ILocalizationAdapter) Class
                            .forName(CAVE_ADAPTER_CLASS).newInstance());
                } catch (Exception e) {
                    try {
                        pathManager = new PathManager(
                                (ILocalizationAdapter) Class.forName(
                                        EDEX_ADAPTER_CLASS).newInstance());
                    } catch (Exception e1) {
                        throw new RuntimeException(
                                "Unable to load any path manager", e);
                    }
                }
                // notify observers that the path manager adaptor has been
                // initialized.
                internalObservable.notifyObservers();
            } else {
                pathManager = new PathManager(adapter);
            }
        }

        return pathManager;
    }

    /**
     * Add observers for path manager adaptor initialization.
     * 
     * @param o
     *            observer to be notified when the path manager adaptor has been
     *            initialized.
     */
    public static synchronized void addObserver(Observer o) {
        internalObservable.addObserver(o);
    }

    /**
     * Get a path manager using the specified adapter
     * 
     * @param adapter
     * @return the path manager with the specified adapter
     */
    public static IPathManager getPathManager(ILocalizationAdapter adapter) {
        return new PathManager(adapter);
    }

    public static void setAdapter(ILocalizationAdapter adapter) {
        PathManagerFactory.adapter = adapter;
        pathManager = null;
    }
}
