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

import java.lang.reflect.Field;

import org.eclipse.osgi.framework.internal.core.AbstractBundle;
import org.eclipse.osgi.framework.internal.core.BundleRepository;
import org.eclipse.osgi.framework.internal.core.Framework;
import org.osgi.framework.Bundle;

/**
 * Utility class to get the BundleRepository object associated with a Bundle, to
 * potentially synchronize against that object.
 * 
 * Specifically if a call to BundleWiring.getClassLoader() is invoked on a
 * thread other than main/UI thread, then there is a possible deadlock if the
 * application shuts down while the BundleWiring.getClassLoader() call is still
 * going. The BundleRepository of the Framework is the primary resource that is
 * in contention in this deadlock scenario, due to the BundleRepository being
 * used as a synchronization lock both deep in bundleWiring.getClassloader() and
 * in Framework shutdown code. The other resource used as a synchronization lock
 * and causing the deadlock is the BundleLoader associated with the bundle.
 * 
 * Therefore to avoid this deadlock, if you are going to call
 * BundleWiring.getClassLoader() you should attempt to get the BundleRepository
 * and synchronize against it. This will ensure the call to getClassLoader() can
 * finish and then release synchronization locks of both the BundleRepository
 * and BundleLoader.
 * 
 * If we fail to get the BundleRepository due to access restrictions, then you
 * should proceed onwards anyway because the odds of the application shutting
 * down at the same time as the call to BundleWiring.getClassLoader() is still
 * running is low. Even if that occurs, the odds are further reduced that the
 * two threads will synchronize against the BundleRepository at the same time
 * and deadlock.
 * 
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Apr 17, 2014            njensen     Initial creation
 * 
 * </pre>
 * 
 * @author njensen
 * @version 1.0
 */

public class BundleRepositoryGetter {

    private BundleRepositoryGetter() {

    }

    /**
     * Attempts to retrieve the BundleRepository associated with the bundle's
     * framework. Returns the BundleRepository or null if it could not be
     * retrieved.
     * 
     * @param bundle
     *            the bundle to retrieve the associated BundleRepository for
     * @return the BundleRepository or null
     */
    @SuppressWarnings("restriction")
    protected static BundleRepository getFrameworkBundleRepository(Bundle bundle) {
        BundleRepository bundleRepo = null;
        if (bundle instanceof AbstractBundle) {
            try {
                AbstractBundle ab = (AbstractBundle) bundle;
                Field bundleRepoField = Framework.getField(Framework.class,
                        BundleRepository.class, true);
                bundleRepo = (BundleRepository) bundleRepoField.get(ab
                        .getFramework());
            } catch (Throwable t) {
                // intentionally log to console and proceed anyway
                t.printStackTrace();
            }
        }

        return bundleRepo;
    }

}
