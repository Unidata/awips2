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
package com.raytheon.uf.viz.spring.dm;

import java.util.Collection;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;

import org.osgi.framework.Bundle;
import org.osgi.framework.namespace.BundleNamespace;
import org.osgi.framework.namespace.PackageNamespace;
import org.osgi.framework.wiring.BundleWire;
import org.osgi.framework.wiring.BundleWiring;

/**
 * Resolve required bundles. Adapted from
 * com.raytheon.uf.viz.core.reflect.SubClassLocator
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Apr 3, 2014            ekladstrup     Initial creation
 * 
 * </pre>
 * 
 * @author ekladstrup
 * @version 1.0
 */
public class BundleResolver {

    protected final Map<String, Collection<Bundle>> requiredBundles = new HashMap<String, Collection<Bundle>>();

    public BundleResolver() {

    }

    /**
     * Get back all the bundles this bundle depends on.
     * 
     * @param bundle
     *            the bundle
     * @return bundles required by bundle.
     */
    public Collection<Bundle> getRequiredBundles(Bundle bundle) {
        String bundleName = bundle.getSymbolicName();
        Collection<Bundle> required = requiredBundles.get(bundleName);
        if (required == null) {
            required = new HashSet<Bundle>();
            BundleWiring bundleWiring = bundle.adapt(BundleWiring.class);
            if (bundleWiring != null) {
                /* Get Required bundles */
                for (BundleWire bw : bundleWiring
                        .getRequiredWires(BundleNamespace.BUNDLE_NAMESPACE)) {
                    required.add(bw.getProviderWiring().getBundle());
                }
                /* Get Bundles through import package */
                for (BundleWire bw : bundleWiring
                        .getRequiredWires(PackageNamespace.PACKAGE_NAMESPACE)) {
                    required.add(bw.getProviderWiring().getBundle());
                }
            }
            /* Avoid recursion */
            required.remove(bundle);
            requiredBundles.put(bundleName, required);

        }
        return required;
    }

}
