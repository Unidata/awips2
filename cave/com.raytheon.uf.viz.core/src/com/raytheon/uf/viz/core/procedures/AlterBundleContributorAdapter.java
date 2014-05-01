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
package com.raytheon.uf.viz.core.procedures;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;

/**
 * An abstract class that implements IAlterBundleContributor. Default methods
 * are provided for getAlterables(String key), addAlterBundleChangeListner(),
 * and removeAlterBundleChangeListner. All other methods of the interface must
 * be implemented in the subclass.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Oct 3, 2012  1248       rferrel     Initial creation
 * 
 * </pre>
 * 
 * @author rferrel
 * @version 1.0
 */

public abstract class AlterBundleContributorAdapter implements
        IAlterBundleContributor {

    private List<IAlterBundleChangeListener> bundleListeners = new ArrayList<IAlterBundleChangeListener>();

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.viz.core.procedures.IAlterBundleContributor#getAlterables
     * ()
     */
    public abstract Map<String, String[]> getAlterables();

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.viz.core.procedures.IAlterBundleContributor#alterBundle
     * (com.raytheon.uf.viz.core.procedures.Bundle, java.lang.String,
     * java.lang.String)
     */
    public abstract void alterBundle(Bundle bundleToAlter, String alterKey,
            String alterValue);

    public String[] getAlterables(String key) {
        return null;
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.uf.viz.core.procedures.AlterBundleContributorAdapter#
     * addAlterBundleChangeListner
     * (com.raytheon.uf.viz.core.procedures.IAlternateBundleChangeListener)
     */
    @Override
    public final void addAlterBundleChangeListener(
            IAlterBundleChangeListener listener) {
        synchronized (bundleListeners) {
            bundleListeners.add(listener);
        }
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.uf.viz.core.procedures.AlterBundleContributorAdapter#
     * removeAlterBundeChangeListner
     * (com.raytheon.uf.viz.core.procedures.IAlternateBundleChangeListener)
     */
    @Override
    public final void removeAlterBundeChangeListener(
            IAlterBundleChangeListener listener) {
        synchronized (bundleListeners) {
            bundleListeners.remove(listener);
        }
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.viz.core.procedures.IAlterBundleContributor#listenerSetup
     * ()
     */
    @Override
    public void listenerSetup() {
        // Default do nothing.
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.viz.core.procedures.IAlterBundleContributor#listenerShutdown
     * ()
     */
    @Override
    public void listenerShutdown() {
        // Default do nothing.
    }

    protected final void fireAlterBundleChangeListener(
            final AlterBundleChangeEvent event) {
        List<IAlterBundleChangeListener> listeners = null;
        synchronized (bundleListeners) {
            if (bundleListeners.isEmpty()) {
                return;
            }
            listeners = new ArrayList<IAlterBundleChangeListener>(
                    bundleListeners);
        }
        for (IAlterBundleChangeListener listener : listeners) {
            listener.changeBundle(event);
        }
    }
}
