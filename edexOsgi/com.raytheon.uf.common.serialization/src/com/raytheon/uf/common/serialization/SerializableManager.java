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
 * See the AWIPS II Master Rights File ("Master Rights File.p df") for
 * further licensing information.
 **/
package com.raytheon.uf.common.serialization;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.net.URL;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Enumeration;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.ServiceConfigurationError;
import java.util.Set;

import javax.persistence.Embeddable;
import javax.persistence.Entity;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlRegistry;

import com.raytheon.uf.common.serialization.jaxb.JaxbDummyObject;

/**
 * Determines and organizes the list of ISerializableObjects in the runtime
 * environment.
 * 
 * Uses Java's ServiceLoader and so requires a
 * com.raytheon.edex.ISerializableObject file under the jars' META-INF/services
 * directory that has each ISerializableObject's fully qualified name in it.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date			Ticket#		Engineer	Description
 * ------------	----------	-----------	--------------------------
 * Aug 11, 2008				njensen	    Initial creation
 * Aug 31, 2009 2924        rjpeter     Added Embeddable.
 * Feb 07, 2013 1543        djohnson    Implement IJaxbableClassesLocator.
 * Mar 29, 2013 1841        djohnson    Never return null from hibernatables for plugin.
 * Apr 24, 2013 1939        randerso    Clean up code and attempt to improve speed.
 *                                      Added initializeHibernatables flag to disable
 *                                      processing hibernatables on CAVE
 * </pre>
 * 
 * @author njensen
 * @version 1.0
 */

public class SerializableManager implements IJaxbableClassesLocator {

    private static SerializableManager instance;

    private final Map<String, List<Class<ISerializableObject>>> hibernatables = new HashMap<String, List<Class<ISerializableObject>>>();

    private ArrayList<Class<ISerializableObject>> jaxbables = new ArrayList<Class<ISerializableObject>>();

    /**
     * Private constructor
     */
    private SerializableManager() {
    }

    /**
     * Determines the categories of ISerializableObject classes based on their
     * annotations
     */
    @SuppressWarnings(value = { "unchecked" })
    private synchronized void initialize() {
        // this is here in case in the future we want to re-initialize the lists
        // during runtime, i.e. hot deploy of a new plugin
        hibernatables.clear();
        jaxbables.clear();
        long realStartTime = System.currentTimeMillis();
        Set<Class<ISerializableObject>> clazzSet = new HashSet<Class<ISerializableObject>>(
                500);
        try {
            Enumeration<URL> urls = SerializableManager.class.getClassLoader()
                    .getResources(
                            "META-INF/services/"
                                    + ISerializableObject.class.getName());

            // doHibernate will be false in CAVE since they are not needed
            boolean doHibernate = Boolean.getBoolean("initializeHibernatables");

            // In testing 1 thread is slowest, 2 threads cuts the time down
            // about 50% and 3 threads cuts down another 5% or so, 4 threads
            // shows no benefit over 2. These results are system specific.
            int numThreads = 3;
            LoadSerializableClassesThread[] threads = new LoadSerializableClassesThread[numThreads];
            for (int i = 0; i < numThreads; i++) {
                threads[i] = new LoadSerializableClassesThread(urls,
                        doHibernate);
                threads[i].start();
            }

            for (LoadSerializableClassesThread thread : threads) {
                thread.join();
                clazzSet.addAll(thread.getClazzList());

                if (doHibernate) {
                    for (Entry<String, List<Class<ISerializableObject>>> entry : thread
                            .getHibernatables().entrySet()) {
                        List<Class<ISerializableObject>> list = hibernatables
                                .get(entry.getKey());
                        if (list == null) {
                            list = new ArrayList<Class<ISerializableObject>>();
                            hibernatables.put(entry.getKey(), list);
                        }
                        list.addAll(entry.getValue());
                    }
                }
            }
        } catch (Throwable e) {
            e.printStackTrace();
        }
        jaxbables = new ArrayList<Class<ISerializableObject>>(
                clazzSet.size() + 1);
        // Add jaxb dummy object so jaxb.properties gets picked up immediately
        @SuppressWarnings("rawtypes")
        Class jaxb = JaxbDummyObject.class;
        jaxbables.add(jaxb);
        jaxbables.addAll(clazzSet);
        jaxbables.trimToSize();

        System.out.println("Total time spent loading classes: "
                + (System.currentTimeMillis() - realStartTime) + "ms");

    }

    private static void fail(@SuppressWarnings("rawtypes") Class service,
            String msg, Throwable cause)
            throws ServiceConfigurationError {
        throw new ServiceConfigurationError(service.getName() + ": " + msg,
                cause);
    }

    private static void fail(@SuppressWarnings("rawtypes") Class service,
            String msg)
            throws ServiceConfigurationError {
        throw new ServiceConfigurationError(service.getName() + ": " + msg);
    }

    private static void fail(@SuppressWarnings("rawtypes") Class service,
            URL u, int line, String msg)
            throws ServiceConfigurationError {
        fail(service, u + ":" + line + ": " + msg);
    }

    private static int parseLine(@SuppressWarnings("rawtypes") Class service,
            URL u, BufferedReader r,
            int lc, List<String> names) throws IOException,
            ServiceConfigurationError {
        String ln = r.readLine();
        if (ln == null) {
            return -1;
        }
        int ci = ln.indexOf('#');
        if (ci >= 0) {
            ln = ln.substring(0, ci);
        }
        ln = ln.trim();
        int n = ln.length();
        if (n != 0) {
            if ((ln.indexOf(' ') >= 0) || (ln.indexOf('\t') >= 0)) {
                fail(service, u, lc, "Illegal configuration-file syntax");
            }
            int cp = ln.codePointAt(0);
            if (!Character.isJavaIdentifierStart(cp)) {
                fail(service, u, lc, "Illegal provider-class name: " + ln);
            }
            for (int i = Character.charCount(cp); i < n; i += Character
                    .charCount(cp)) {
                cp = ln.codePointAt(i);
                if (!Character.isJavaIdentifierPart(cp) && (cp != '.')) {
                    fail(service, u, lc, "Illegal provider-class name: " + ln);
                }
            }

            names.add(ln);
        }
        return lc + 1;
    }

    /**
     * Gets the instance of the SerializableManager
     * 
     * @return
     */
    public synchronized static SerializableManager getInstance() {
        if (instance == null) {
            instance = new SerializableManager();
            instance.initialize();
        }
        return instance;
    }

    /**
     * Returns the list of classes at runtime that have Hibernate annotations
     * 
     * @return
     */
    public List<Class<ISerializableObject>> getHibernatablesForPluginFQN(
            String pluginFQN) {
        List<Class<ISerializableObject>> list = hibernatables.get(pluginFQN);
        if (list == null) {
            list = Collections.<Class<ISerializableObject>> emptyList();
        }
        return list;
    }

    /**
     * 
     */
    public Set<String> getHibernatablePluginFQNs() {
        return hibernatables.keySet();
    }

    /**
     * Returns the list of classes at runtime that have Hibernate annotations
     * 
     * @return
     */
    public List<Class<ISerializableObject>> getHibernatables() {
        List<Class<ISerializableObject>> rval = new ArrayList<Class<ISerializableObject>>();
        for (List<Class<ISerializableObject>> list : hibernatables.values()) {
            rval.addAll(list);
        }
        return rval;
    }

    /**
     * Returns the list of classes at runtime that have JaxB annotations
     * 
     * @return
     */
    @Override
    public List<Class<ISerializableObject>> getJaxbables() {
        return jaxbables;
    }

    private static class LoadSerializableClassesThread extends Thread {

        private final Enumeration<URL> urls;

        private final boolean doHibernate;

        private final List<Class<ISerializableObject>> clazzList;

        private final Map<String, List<Class<ISerializableObject>>> hibernatables;

        public LoadSerializableClassesThread(Enumeration<URL> urls,
                boolean doHibernate) {
            this.urls = urls;
            this.doHibernate = doHibernate;
            this.clazzList = new ArrayList<Class<ISerializableObject>>(500);
            this.hibernatables = new HashMap<String, List<Class<ISerializableObject>>>();
        }

        public List<Class<ISerializableObject>> getClazzList() {
            return clazzList;
        }

        public Map<String, List<Class<ISerializableObject>>> getHibernatables() {
            return hibernatables;
        }

        @Override
        public void run() {
            try {
                ClassLoader cl = getClass().getClassLoader();
                Set<Class<ISerializableObject>> pluginHibernateSet = null;
                if (doHibernate) {
                    pluginHibernateSet = new HashSet<Class<ISerializableObject>>();
                }
                List<String> names = new ArrayList<String>();
                URL u = getNextUrl();
                while (u != null) {
                    InputStream in = null;
                    BufferedReader r = null;
                    names.clear();
                    String path = u.getPath();
                    int endIndex = path.indexOf(".jar");
                    if (endIndex < 0) {
                        endIndex = path.length();
                    }
                    path = path.substring(0, endIndex);
                    int startIndex = path.lastIndexOf("/");
                    if (startIndex < 0) {
                        startIndex = path.lastIndexOf(":");
                    }
                    startIndex++; // move past the last char
                    String pluginFQN = path.substring(startIndex);

                    try {
                        in = u.openStream();
                        r = new BufferedReader(new InputStreamReader(in,
                                "utf-8"));
                        int lc = 1;
                        while ((lc = parseLine(ISerializableObject.class, u, r,
                                lc, names)) >= 0) {
                            ;
                        }
                    } catch (IOException x) {
                        fail(ISerializableObject.class,
                                "Error reading configuration file", x);
                    } finally {
                        try {
                            if (r != null) {
                                r.close();
                            }
                            if (in != null) {
                                in.close();
                            }
                        } catch (IOException y) {
                            fail(ISerializableObject.class,
                                    "Error closing configuration file", y);
                        }
                    }

                    if (doHibernate) {
                        pluginHibernateSet.clear();
                    }

                    Iterator<String> iter = names.iterator();
                    while (iter.hasNext()) {
                        String clazz = iter.next();
                        try {
                            long t0 = System.currentTimeMillis();
                            @SuppressWarnings("unchecked")
                            Class<ISerializableObject> c = (Class<ISerializableObject>) Class
                                    .forName(clazz, true, cl);
                            boolean added = false;
                            if (c.getAnnotation(XmlAccessorType.class) != null
                                    || c.getAnnotation(XmlRegistry.class) != null) {
                                clazzList.add(c);
                                added = true;
                            }

                            if (doHibernate) {
                                if (c.getAnnotation(Entity.class) != null
                                        || c.getAnnotation(Embeddable.class) != null) {
                                    pluginHibernateSet.add(c);
                                    added = true;
                                }
                            }

                            long time = (System.currentTimeMillis() - t0);
                            if (doHibernate && !added) {
                                System.out
                                        .println("Class: "
                                                + clazz
                                                + " should not be in ISerializableObject file, wasted "
                                                + time + "ms processing it!");
                            }
                        } catch (ClassNotFoundException e) {
                            System.out
                                    .println("Unable to load class "
                                            + clazz
                                            + ".  Check that class is spelled correctly in ISerializableObject file");
                        }
                    }

                    if (doHibernate && pluginHibernateSet.size() > 0) {
                        hibernatables.put(pluginFQN,
                                new ArrayList<Class<ISerializableObject>>(
                                        pluginHibernateSet));
                    }
                    u = getNextUrl();
                }
            } catch (Throwable e) {
                e.printStackTrace();
            }
        }

        private URL getNextUrl() {
            synchronized (urls) {
                if (urls.hasMoreElements()) {
                    return urls.nextElement();
                }
                return null;
            }
        }
    }
}
