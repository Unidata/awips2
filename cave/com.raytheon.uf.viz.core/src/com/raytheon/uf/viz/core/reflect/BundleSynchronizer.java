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
package com.raytheon.uf.viz.core.reflect;

import org.eclipse.osgi.framework.internal.core.BundleRepository;
import org.eclipse.osgi.internal.loader.BundleLoader;
import org.osgi.framework.Bundle;

/**
 * If a call to BundleWiring.getClassLoader() is invoked on a thread other than
 * main/UI thread, then there is a possible deadlock if the application shuts
 * down while the BundleWiring.getClassLoader() call is still going. The
 * BundleLoader and BundleRepository of the Framework are the primary resources
 * that are in contention in this deadlock scenario, due to the BundleRepository
 * being used as a synchronization lock both deep in
 * bundleWiring.getClassloader() and in Framework shutdown code. The other
 * resource used as a synchronization lock and causing the deadlock is the
 * BundleLoader associated with the bundle. When BundleLoader.findClass() is
 * called, it results in a lock on the BundleLoader and then a lock on the
 * BundleRepository. This happens when the DefaultClassLoader loads a class.
 * 
 * Therefore to avoid this deadlock, if you are going to call
 * BundleWiring.getClassLoader() you should attempt synchronize against the
 * BundleLoader and the BundleRepository. This will ensure the call to
 * getClassLoader() can finish and then release synchronization locks of both
 * the BundleRepository and BundleLoader.
 * 
 * If we fail to get the BundleLoader or BundleRepository, then you should
 * proceed onwards anyway because the odds of the application shutting down at
 * the same time as the call to BundleWiring.getClassLoader() is still running
 * is low. Even if that occurs, the odds are further reduced that the two
 * threads will synchronize against the BundleLoader and the BundleRepository at
 * the same time and deadlock.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Aug 13, 2014 3500       bclement     Initial creation
 * 
 * </pre>
 * 
 * @author bclement
 * @version 1.0
 */
public class BundleSynchronizer {

    private BundleSynchronizer() {
    }

    /**
     * Attempts to synchronize with the bundle's BundleLoader and
     * BundleRepository objects before running the runner. If either the
     * BundleLoader or the BundleRepository are unable to be retrieved from the
     * bundle, the runner is ran anyway since the likelihood of a deadlock is
     * relatively small.
     * 
     * @param runner
     * @param bundle
     * @see BundleLoaderGetter#getBundleLoader(Bundle)
     * @see BundleRepositoryGetter#getFrameworkBundleRepository(Bundle)
     */
    protected static void runSynchedWithBundle(Runnable runner, Bundle bundle) {
        BundleRepository repo = BundleRepositoryGetter
                .getFrameworkBundleRepository(bundle);
        BundleLoader loader = BundleLoaderGetter.getBundleLoader(bundle);
        if (repo != null && loader != null) {
            synchronized (loader) {
                synchronized (repo) {
                    runner.run();
                }
            }
        } else {
            runner.run();
        }
    }
}
