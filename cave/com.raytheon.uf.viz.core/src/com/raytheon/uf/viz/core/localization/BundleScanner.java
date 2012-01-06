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
package com.raytheon.uf.viz.core.localization;

import java.io.File;
import java.io.IOException;
import java.net.URL;
import java.util.Collections;
import java.util.HashMap;
import java.util.Map;
import java.util.Set;

import org.eclipse.core.runtime.FileLocator;
import org.eclipse.core.runtime.Path;
import org.osgi.framework.Bundle;

import com.raytheon.uf.viz.core.Activator;

/**
 * Allows files to be present in UNPACKED bundles.
 * 
 * Hopefully in the future, we can add the capability to leave the files packed
 * in the jar.
 * 
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Mar 26, 2009            chammack     Initial creation
 * 
 * </pre>
 * 
 * @author chammack
 * @version 1.0
 */

public class BundleScanner {

    private static Map<String, Bundle> CAVE_STATIC_BUNDLES;

    private static Map<String, Bundle> CAVE_BUNDLES;

    static {
        scan();
    }

    private static synchronized void scan() {
        if (CAVE_STATIC_BUNDLES != null) {
            return;
        }

        CAVE_STATIC_BUNDLES = new HashMap<String, Bundle>();
        CAVE_BUNDLES = new HashMap<String, Bundle>();

        Bundle[] bndls = Activator.getDefault().getContext().getBundles();
        for (Bundle bndl : bndls) {
            CAVE_BUNDLES.put(bndl.getSymbolicName(), bndl);
            URL url = FileLocator.find(bndl, new Path("localization"), null);
            if (url != null) {
                CAVE_STATIC_BUNDLES.put(bndl.getSymbolicName(), bndl);
            }
        }

    }

    public static Set<String> getListOfBundles() {
        return getListOfBundles(true);
    }

    public static Set<String> getListOfBundles(boolean caveStaticOnly) {
        return Collections.unmodifiableSet(caveStaticOnly ? CAVE_STATIC_BUNDLES
                .keySet() : CAVE_BUNDLES.keySet());
    }

    public static File searchInBundle(String bundleToSearch,
            String pathToLookFor) {
        return searchInBundle(bundleToSearch, "localization", pathToLookFor,
                CAVE_STATIC_BUNDLES);
    }

    public static File searchInBundle(String bundleToSearch, String basePath,
            String pathToLookFor) {
        return searchInBundle(bundleToSearch, basePath, pathToLookFor,
                CAVE_BUNDLES);
    }

    public static File searchInBundle(String bundleToSearch, String basePath,
            String pathToLookFor, Map<String, Bundle> toSearch) {
        File file = null;
        Bundle b = toSearch.get(bundleToSearch);
        if (b != null) {
            URL url = FileLocator.find(b, new Path(basePath + File.separator
                    + pathToLookFor), null);
            if (url != null) {
                URL resolvedURL = null;
                try {
                    resolvedURL = FileLocator.toFileURL(url);
                } catch (IOException e) {

                }
                if (resolvedURL != null) {
                    file = new File(resolvedURL.getPath());
                }
            }
        }
        return file;
    }
}
