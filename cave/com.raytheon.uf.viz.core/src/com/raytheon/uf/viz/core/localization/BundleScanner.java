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
import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.LinkedHashMap;
import java.util.Map;

import org.eclipse.core.runtime.FileLocator;
import org.eclipse.core.runtime.Path;
import org.osgi.framework.Bundle;

import com.raytheon.uf.common.localization.IPathManager;
import com.raytheon.uf.viz.core.Activator;

/**
 * Allows files to be present in UNPACKED bundles.
 * 
 * Hopefully in the future, we can add the capability to leave the files packed
 * in the jar. Path separation should use {@link IPathManager#SEPARATOR}
 * 
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Mar 26, 2009            chammack    Initial creation
 * Aug 13, 2013       2033 mschenke    Generalized bundle scanner instead of
 *                                     CAVE_STATIC vs CAVE_CONFIG
 * 
 * </pre>
 * 
 * @author chammack
 * @version 1.0
 */

public class BundleScanner {

    private static Map<String, BundleScanner> scanners = new HashMap<String, BundleScanner>();

    private static BundleScanner getBundleScanner(String path) {
        if (path == null) {
            path = IPathManager.SEPARATOR;
        }
        path = path.trim();
        if (path.isEmpty() || ".".equals(path)) {
            path = IPathManager.SEPARATOR;
        }
        BundleScanner scanner;
        synchronized (scanners) {
            scanner = scanners.get(path);
            if (scanner == null) {
                scanner = new BundleScanner(path);
                scanners.put(path, scanner);
            }
        }
        return scanner;
    }

    private final String basePath;

    private Map<String, Bundle> bundles = new LinkedHashMap<String, Bundle>();

    public BundleScanner(String basePath) {
        this.basePath = basePath;
        Path path = new Path(basePath);
        Bundle[] bundles = Activator.getDefault().getContext().getBundles();
        for (Bundle bundle : bundles) {
            String bundleName = bundle.getSymbolicName();
            URL url = FileLocator.find(bundle, path, null);
            if (url != null) {
                this.bundles.put(bundleName, bundle);
            }
        }
    }

    public Collection<String> getContributingBundles() {
        return new ArrayList<String>(bundles.keySet());
    }

    public Bundle getBundle(String symbolicName) {
        return bundles.get(symbolicName);
    }

    public File searchInBundles(String path) {
        for (String bundle : bundles.keySet()) {
            File file = searchInBundle(bundle, path);
            if (file != null) {
                return file;
            }
        }
        return null;
    }

    public File searchInBundle(String bundleToSearch, String pathToLookFor) {
        File file = null;
        Bundle bundle = getBundle(bundleToSearch);
        if (bundle != null && bundle.getState() != Bundle.UNINSTALLED) {
            String path = basePath;
            if (pathToLookFor != null
                    && pathToLookFor.trim().isEmpty() == false) {
                path += IPathManager.SEPARATOR + pathToLookFor;
            }
            URL url = FileLocator.find(bundle, new Path(path), null);
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

    /**
     * Gets a list of all bundles
     * 
     * @return
     */
    public static Collection<String> getListOfBundles() {
        return getListOfBundles(IPathManager.SEPARATOR);
    }

    /**
     * Gets the bundles that contain the path passed in in their bundle. Null,
     * empty string, ".", or "/" will return all bundles.
     * 
     * @param path
     * @return
     */
    public static Collection<String> getListOfBundles(String path) {
        BundleScanner scanner = getBundleScanner(path);
        return scanner.getContributingBundles();
    }

    /**
     * Search in the specified bundle for the pathToLookFor relative to basePath
     * 
     * @param bundleToSearch
     * @param basePath
     * @param pathToLookFor
     * @return
     */
    public static File searchInBundle(String bundleToSearch, String basePath,
            String pathToLookFor) {
        BundleScanner scanner = getBundleScanner(basePath);
        return scanner.searchInBundle(bundleToSearch, pathToLookFor);
    }

}
