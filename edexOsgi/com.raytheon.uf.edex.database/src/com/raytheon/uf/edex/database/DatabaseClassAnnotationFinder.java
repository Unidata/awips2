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
package com.raytheon.uf.edex.database;

import java.util.Set;

import javax.persistence.Embeddable;
import javax.persistence.Entity;

import org.reflections.Reflections;
import org.reflections.scanners.TypeAnnotationsScanner;
import org.reflections.util.ClasspathHelper;
import org.reflections.util.ConfigurationBuilder;

/**
 * Uses the reflections package to find classes on the classpath that match the
 * start of a package name and will be needed to access the database.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Oct 11, 2013            njensen     Initial creation
 * 
 * </pre>
 * 
 * @author njensen
 * @version 1.0
 */

public class DatabaseClassAnnotationFinder {

    private Set<Class<?>> dbAnnotatedClassSet;

    public DatabaseClassAnnotationFinder(String... packageNames) {
        dbAnnotatedClassSet = findClasses(packageNames);
    }

    /**
     * Searches the classpath for classes that will be needed by the database
     * layer.
     * 
     * @param packageNames
     *            The start of pacakge names to include, e.g. com.raytheon
     * @return
     */
    protected Set<Class<?>> findClasses(String... packageNames) {
        long t0 = System.currentTimeMillis();
        ConfigurationBuilder cb = new ConfigurationBuilder();
        for (String pkg : packageNames) {
            cb.addUrls(ClasspathHelper.forPackage(pkg));
        }
        cb.setScanners(new TypeAnnotationsScanner());
        Reflections reflecs = cb.build();
        Set<Class<?>> set = reflecs.getTypesAnnotatedWith(Entity.class, false);
        set.addAll(reflecs.getTypesAnnotatedWith(Embeddable.class, false));
        long t1 = System.currentTimeMillis();
        System.out.println("Found " + set.size() + " db classes in "
                + (t1 - t0) + " ms");
        return set;
    }

    /**
     * Gets the set of classes that this class finder is aware of that
     * correspond to db classes.
     * 
     * @return
     */
    public Set<Class<?>> getDbAnnotatedClases() {
        return dbAnnotatedClassSet;
    }

}
