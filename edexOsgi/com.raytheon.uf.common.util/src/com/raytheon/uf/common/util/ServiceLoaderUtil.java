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
package com.raytheon.uf.common.util;

import java.util.Iterator;
import java.util.ServiceLoader;

/**
 * Utilities for interacting with {@link ServiceLoader}.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jan 04, 2013 1451       djohnson     Initial creation
 * 
 * </pre>
 * 
 * @author djohnson
 * @version 1.0
 */

public final class ServiceLoaderUtil {
    /**
     * Prevent construction.
     */
    private ServiceLoaderUtil() {
    }

    /**
     * Loads the first implementation of an interface found using
     * {@link ServiceLoader}. If no results are found, will return the provided
     * default implementation.
     * 
     * @param interfaceClass
     *            the class instance to search for a service loader
     *            implementation of
     * @param defaultImplementation
     *            the default implementation
     * @return the loaded implementation, or the default implementation is no
     *         configuration file found
     */
    public static <T> T load(Class<T> interfaceClass, T defaultImplementation) {
        ServiceLoader<T> overridden = ServiceLoader.load(interfaceClass);

        final Iterator<T> iter = overridden.iterator();
        if (iter.hasNext()) {
            return iter.next();
        }

        return defaultImplementation;
    }
}
