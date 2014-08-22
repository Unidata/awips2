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
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Apr 17, 2014            njensen     Initial creation
 * Aug 13, 2014 3500       bclement    moved documentation over to BundleSynchronizer
 * 
 * </pre>
 * 
 * @author njensen
 * @version 1.0
 * @see BundleSynchronizer
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
