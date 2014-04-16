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

import java.io.IOException;
import java.lang.reflect.Field;
import java.util.Collections;
import java.util.HashSet;
import java.util.Set;

import org.eclipse.core.runtime.FileLocator;
import org.eclipse.osgi.framework.internal.core.AbstractBundle;
import org.eclipse.osgi.framework.internal.core.BundleRepository;
import org.eclipse.osgi.framework.internal.core.Framework;
import org.osgi.framework.Bundle;
import org.osgi.framework.wiring.BundleWiring;
import org.reflections.Reflections;
import org.reflections.scanners.Scanner;
import org.reflections.util.ConfigurationBuilder;

/**
 * Provides the capabilities of {@link Reflections} for a single OSGi
 * {@link Bundle}. Uses Reflections internally but populates URL and CLassLoader
 * from the Bundle.
 * 
 * In the future this can be expanded to expose more of the {@link Reflections}
 * capabilities.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date          Ticket#  Engineer    Description
 * ------------- -------- ----------- --------------------------
 * Oct 21, 2013  2491     bsteffen    Initial creation
 * Jan 22, 2014  2062     bsteffen    Handle bundles with no wiring.
 * Apr 16, 2014  3018     njensen     Synchronize against BundleRepository
 * 
 * </pre>
 * 
 * @author bsteffen
 * @version 1.0
 */

public class BundleReflections {

    private final Reflections reflections;

    @SuppressWarnings("restriction")
    public BundleReflections(Bundle bundle, Scanner scanner) throws IOException {
        ConfigurationBuilder cb = new ConfigurationBuilder();
        BundleWiring bundleWiring = bundle.adapt(BundleWiring.class);

        /*
         * If BundleReflection's constructor is invoked on a thread other than
         * main/UI thread, then there is a possible deadlock if the application
         * shuts down while the bundleWiring.getClassloader() call below is
         * still going. The BundleRepository of the Framework is the primary
         * resource that is in contention in this deadlock scenario, due to the
         * BundleRepository being used as a synchronization lock both deep in
         * bundleWiring.getClassloader() and in Framework shutdown code.
         * 
         * Therefore to avoid this deadlock, we attempt to get the
         * BundleRepository and synchronize against it, ensuring the call to
         * getClassLoader() can finish and then release synchronization locks.
         * 
         * If we fail to get the BundleRepository due to access restrictions,
         * then we proceed onwards anyway because the odds of the application
         * shutting down at the same time as this is still running is low, and
         * even if that occurs, the odds are further reduced that the two
         * threads will synchronize against the BundleRepository at the same
         * time and deadlock.
         */
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

        if (bundleWiring != null) {
            if (bundleRepo != null) {
                synchronized (bundleRepo) {
                    cb.addClassLoader(bundleWiring.getClassLoader());
                }
            } else {
                cb.addClassLoader(bundleWiring.getClassLoader());
            }
            cb.addUrls(FileLocator.getBundleFile(bundle).toURI().toURL());
            cb.setScanners(scanner);
            reflections = cb.build();
        } else {
            reflections = null;
        }

    }

    public <T> Set<Class<? extends T>> getSubTypesOf(final Class<T> type) {
        if (reflections == null) {
            return Collections.emptySet();
        } else {
            return reflections.getSubTypesOf(type);
        }
    }

    public Set<Class<?>> getSubTypesOf(Class<?>... types) {
        Set<Class<?>> subTypes = new HashSet<Class<?>>();
        for (Class<?> type : types) {
            subTypes.addAll(getSubTypesOf(type));
        }
        return subTypes;
    }

}
