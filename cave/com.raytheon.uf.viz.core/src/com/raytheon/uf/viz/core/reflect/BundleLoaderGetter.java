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

import org.eclipse.osgi.framework.internal.core.AbstractBundle;
import org.eclipse.osgi.framework.internal.core.BundleHost;
import org.eclipse.osgi.internal.loader.BundleLoader;
import org.eclipse.osgi.internal.loader.BundleLoaderProxy;
import org.eclipse.osgi.service.resolver.BundleDescription;
import org.osgi.framework.Bundle;
import org.osgi.framework.BundleReference;

/**
 * Utility class to get the BundleLoader object associated with a Bundle, to
 * potentially synchronize against that object.
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
 * @see BundleSynchronizer
 */
public class BundleLoaderGetter {

    private BundleLoaderGetter() {
    }

    /**
     * Attempts to retrieve the BundleLoader associated with the bundle. Returns
     * the BundleLoader or null if it could not be retrieved.
     * 
     * @param bundle
     *            the bundle to retrieve the associated BundleLoader for
     * @return the BundleLoader or null
     */
    @SuppressWarnings("restriction")
    protected static BundleLoader getBundleLoader(Bundle bundle) {
        BundleLoader rval = null;
        if (bundle instanceof AbstractBundle) {
            BundleDescription bundleDesc = ((AbstractBundle) bundle)
                    .getBundleDescription();
            if (bundleDesc != null) {
                Object o = bundleDesc.getUserObject();
                if (!(o instanceof BundleLoaderProxy)) {
                    if (o instanceof BundleReference)
                        o = ((BundleReference) o).getBundle();
                    if (o instanceof BundleHost)
                        o = ((BundleHost) o).getLoaderProxy();
                }
                if (o instanceof BundleLoaderProxy) {
                    rval = ((BundleLoaderProxy) o).getBundleLoader();
                }
            }
        }
        return rval;
    }

}
