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

import java.io.File;
import java.io.FileFilter;
import java.net.MalformedURLException;
import java.util.Set;

import javax.persistence.Embeddable;
import javax.persistence.Entity;

import org.reflections.Reflections;
import org.reflections.scanners.TypeAnnotationsScanner;
import org.reflections.util.ConfigurationBuilder;

import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.edex.core.props.PropertiesFactory;

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
 * Apr 25, 2014 2995       rjpeter     Updated to scan PLUGINDIR for all files.
 * </pre>
 * 
 * @author njensen
 * @version 1.0
 */

public class DatabaseClassAnnotationFinder {

    private final Set<Class<?>> dbAnnotatedClassSet;

    private final IUFStatusHandler statusHandler = UFStatus
            .getHandler(DatabaseClassAnnotationFinder.class);

    public DatabaseClassAnnotationFinder() {
        dbAnnotatedClassSet = findClasses();
    }

    /**
     * Searches the plugin dir for classes that will be needed by the database
     * layer.
     * 
     * @return
     */
    protected Set<Class<?>> findClasses() {
        long t0 = System.currentTimeMillis();
        ConfigurationBuilder cb = new ConfigurationBuilder();
        File pluginDir = new File(PropertiesFactory.getInstance()
                .getEnvProperties().getEnvValue("PLUGINDIR"));

        if (!pluginDir.exists()) {
            throw new AssertionError(
                    "Cannot find Database classes to load.  PluginDir ["
                            + pluginDir.getAbsolutePath() + "] does not exist.");
        }

        File[] pluginJarFiles = pluginDir.listFiles(new FileFilter() {
            @Override
            public boolean accept(File file) {
                if (file.isFile()) {
                    String name = file.getName();
                    if (name.endsWith(".jar")) {
                        return true;
                    }
                }
                return false;
            }

        });

        for (File jarFile : pluginJarFiles) {
            try {
                cb.addUrls(jarFile.toURI().toURL());
            } catch (MalformedURLException e) {
                statusHandler
                        .error("Unable to scan jar file ["
                                + jarFile.getAbsolutePath()
                                + "] for database annotations.  File will be skipped",
                                e);
            }
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
