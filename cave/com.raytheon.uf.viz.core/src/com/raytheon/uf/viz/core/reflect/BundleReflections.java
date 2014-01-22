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
import java.util.Collections;
import java.util.HashSet;
import java.util.Set;

import org.eclipse.core.runtime.FileLocator;
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
 * 
 * </pre>
 * 
 * @author bsteffen
 * @version 1.0
 */

public class BundleReflections {

    private final Reflections reflections;

    public BundleReflections(Bundle bundle, Scanner scanner) throws IOException {
        ConfigurationBuilder cb = new ConfigurationBuilder();
        BundleWiring bundleWiring = bundle.adapt(BundleWiring.class);
        if (bundleWiring != null) {
            cb.addClassLoader(bundleWiring.getClassLoader());
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
