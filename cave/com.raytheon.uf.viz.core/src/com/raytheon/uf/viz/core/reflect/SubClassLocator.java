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

import java.io.File;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.regex.Pattern;

import org.osgi.framework.Bundle;
import org.osgi.framework.Constants;
import org.osgi.framework.wiring.BundleWiring;
import org.reflections.scanners.SubTypesScanner;

import com.raytheon.uf.common.serialization.reflect.ISubClassLocator;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.viz.core.Activator;

/**
 * Loads all subclasses of any class using all installed OSGi bundles and the
 * Reflections package. Results are cached using a {@link BundleClassCache}. to
 * save time on startup.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date          Ticket#  Engineer    Description
 * ------------- -------- ----------- -----------------------------------------
 * Oct 18, 2013  2491     bsteffen    Initial creation
 * Dec 10, 2013  2602     bsteffen    Add null checks to detect unloaded
 *                                    bundles.
 * 
 * </pre>
 * 
 * @author bsteffen
 * @version 1.0
 */
public class SubClassLocator implements ISubClassLocator {
    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(SubClassLocator.class);

    private static final String CACHE_FILENAME = "subclassCache.txt";

    private static final Pattern COMMA_SPLIT = Pattern.compile("[,]");

    private static final Pattern SEMICOLON_SPLIT = Pattern.compile("[;]");

    private final Map<String, BundleReflections> reflectionLookup = new HashMap<String, BundleReflections>();

    private final Map<String, Bundle> bundleLookup = new HashMap<String, Bundle>();

    private final Map<String, List<Bundle>> requiredBundles = new HashMap<String, List<Bundle>>();

    private final BundleClassCache cache;

    /**
     * Create a new SubClassLocator.
     */
    public SubClassLocator() {
        Bundle[] bundles = Activator.getDefault().getBundle()
                .getBundleContext().getBundles();
        for (Bundle b : bundles) {
            bundleLookup.put(b.getSymbolicName(), b);
        }
        File stateDir = Activator.getDefault().getStateLocation().toFile();
        cache = new BundleClassCache(new File(stateDir, CACHE_FILENAME));
    }

    /**
     * Locate all subclasses in all bundles of a given class
     * 
     * @param base
     * @return
     */
    public Collection<Class<?>> locateSubClasses(Class<?> base) {
        Map<String, Set<Class<?>>> recursiveClasses = new HashMap<String, Set<Class<?>>>(
                bundleLookup.size(), 1.0f);
        Set<Class<?>> result = new HashSet<Class<?>>(512);
        for (Bundle bundle : bundleLookup.values()) {
            result.addAll(lookupRecursiveSubClasses(base, bundle, true,
                    recursiveClasses));
        }
        return result;
    }

    /**
     * Store the cache to disk.
     */
    public void save() {
        cache.save();
    }

    /**
     * The lookup must occur recursively because otherwise sub classes of sub
     * classes of base types are not correctly located.
     * 
     * @param base
     *            base class
     * @param bundle
     *            bundle to search
     * @param includeRequiredSubclasses
     *            when false only subclasses of base found in this bundle are
     *            returned, when true subclasses found in other bundles required
     *            by this bundle are also returned.
     * @param recursiveClasses
     *            map of already searched bundles to avoid recursing the same
     *            bundles multiple time.
     * @return the sub classes contained in the bundle.
     */
    private Set<Class<?>> lookupRecursiveSubClasses(Class<?> base,
            Bundle bundle, boolean includeRequiredSubclasses,
            Map<String, Set<Class<?>>> recursiveClasses) {
        String bundleName = bundle.getSymbolicName();
        if (bundleName.startsWith("org.eclipse")) {
            /*
             * org.eclipse.osgi has no class loader and must be skipped,
             * skipping the rest of org.eclipse just saves time.
             */
            return Collections.emptySet();
        }

        if (includeRequiredSubclasses) {
            /* Short circut if we already did this. */
            Set<Class<?>> result = recursiveClasses.get(bundleName);
            if (result != null) {
                return result;
            }
        }

        String[] ownedNames = cache.getTypes(bundle, base.getName());
        if (ownedNames == null) {
            Set<Class<?>> dependencies = getRequiredSubclasses(base, bundle,
                    recursiveClasses);
            /* Must pass dependencies in so type heirarchy is complete. */
            Set<Class<?>> owned = loadSubClassesReflectively(bundle, dependencies);
            /* populate the cache */
            ownedNames = new String[owned.size()];
            int index = 0;
            for (Class<?> clazz : owned) {
                ownedNames[index++] = clazz.getName();
            }
            cache.putTypes(bundle, base.getName(), ownedNames);
            Set<Class<?>> all = new HashSet<Class<?>>(dependencies);
            all.addAll(owned);
            recursiveClasses.put(bundleName, all);
            if (includeRequiredSubclasses) {
                return all;
            } else {
                return owned;
            }
        } else {
            Set<Class<?>> owned = loadClassesFromCache(bundle,
                    Arrays.asList(ownedNames));
            if (includeRequiredSubclasses) {
                Set<Class<?>> dependencies = getRequiredSubclasses(base,
                        bundle,
                        recursiveClasses);
                Set<Class<?>> all = new HashSet<Class<?>>(dependencies);
                all.addAll(owned);
                recursiveClasses.put(bundleName, all);
                return all;
            } else {
                return owned;
            }
        }
    }

    /**
     * Locate all subclasses of base that are found in the bundles required by
     * this bundle.
     * 
     * @param base
     *            base class
     * @param bundle
     *            bundle to search
     * @param recursiveClasses
     *            map of already searched bundles to avoid recursing the same
     *            bundles multiple time.
     * @return the sub classes contained in required bundles.
     */
    private Set<Class<?>> getRequiredSubclasses(Class<?> base, Bundle bundle,
            Map<String, Set<Class<?>>> recursiveClasses) {
        Set<Class<?>> dependencies = new HashSet<Class<?>>();
        dependencies.add(base);
        for (Bundle reqBundle : getRequiredBundles(bundle)) {
            dependencies.addAll(lookupRecursiveSubClasses(base, reqBundle,
                    true, recursiveClasses));
        }
        return dependencies;
    }

    /**
     * Load all subclasses of a set of classes that are found within a bundle.
     * 
     * @param bundle
     *            bundle to search
     * @param baseClasses
     *            all base classes
     * @return
     */
    private Set<Class<?>> loadSubClassesReflectively(Bundle bundle,
            Collection<Class<?>> baseClasses) {
        String bundleName = bundle.getSymbolicName();

        try {
            BundleReflections reflections = reflectionLookup.get(bundleName);
            if (reflections == null) {
                reflections = new BundleReflections(bundle,
                        new SubTypesScanner());
                reflectionLookup.put(bundleName, reflections);
            }
            return reflections.getSubTypesOf(baseClasses
                    .toArray(new Class<?>[0]));
        } catch (Throwable e) {
            statusHandler.handle(Priority.PROBLEM,
                    "Error loading classes in bundle(" + bundleName
                            + "), some procedures may not load.", e);
        }
        return Collections.emptySet();
    }

    /**
     * Load classes by name using a specific bundles class loader
     * 
     * @param bundle
     *            the bundle to get a class loader from
     * @param classNames
     *            names of classes to load.
     * @return
     */
    private Set<Class<?>> loadClassesFromCache(Bundle bundle,
            Collection<String> classNames) {
        BundleWiring bundleWiring = bundle.adapt(BundleWiring.class);
        if (bundleWiring == null) {
            return Collections.emptySet();
        }
        ClassLoader loader = bundleWiring.getClassLoader();
        if (loader == null) {
            return Collections.emptySet();
        }
        HashSet<Class<?>> result = new HashSet<Class<?>>(classNames.size(),
                1.0f);
        for (String className : classNames) {
            try {
                result.add(Class.forName(className, false, loader));
            } catch (ClassNotFoundException e) {
                statusHandler.handle(Priority.PROBLEM, "Error loading class("
                        + className + "), some procedures may not load.", e);
            }
        }
        return result;
    }

    /**
     * Parse bundle header to get all required bundles
     * 
     * @param bundle
     *            the bundle
     * @return bundles required by bundle.
     */
    private List<Bundle> getRequiredBundles(Bundle bundle) {
        String bundleName = bundle.getSymbolicName();
        List<Bundle> required = requiredBundles.get(bundle);
        if (required == null) {
            required = new ArrayList<Bundle>();
            String requiredBundlesHeader = bundle.getHeaders().get(
                    Constants.REQUIRE_BUNDLE);
            if (requiredBundlesHeader != null) {
                String[] requiredBundles = COMMA_SPLIT
                        .split(requiredBundlesHeader);
                for (String requiredBundleName : requiredBundles) {
                    String[] nameParts = SEMICOLON_SPLIT
                            .split(requiredBundleName);
                    Bundle reqBundle = bundleLookup.get(nameParts[0]);
                    if (reqBundle != null) {
                        required.add(reqBundle);
                    }
                }
            }
            requiredBundles.put(bundleName, required);

        }
        return required;
    }

}
