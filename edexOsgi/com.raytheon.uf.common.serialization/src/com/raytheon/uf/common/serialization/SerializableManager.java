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
package com.raytheon.uf.common.serialization;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.net.URL;
import java.util.ArrayList;
import java.util.Enumeration;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
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
 * </pre>
 * 
 * @author njensen
 * @version 1.0
 */

public class SerializableManager {

    private static SerializableManager instance;

    private Map<String, List<Class<ISerializableObject>>> hibernatables = new HashMap<String, List<Class<ISerializableObject>>>();

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
        ClassLoader cl = getClass().getClassLoader();
        long t0 = System.currentTimeMillis(), total = 0;
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
            // In testing 1 thread is slowest, 2 threads cuts the time down
            // about 50% and 3 threads cuts down another 5% or so, 4 threads
            // shows no benefit over 2. These results are system specific.
            int numThreads = 3;
            Thread[] threads = new Thread[numThreads];
            for (int i = 1; i < numThreads; i++) {
                threads[i] = new LoadSerializableClassesThread(urls, clazzSet,
                        hibernatables);
                threads[i].start();
            }
            threads[0] = new LoadSerializableClassesThread(urls, clazzSet,
                    hibernatables);
            // Run this one on the current thread since its not doing anything
            // but waiting and this avoids overhead of starting a new thread
            threads[0].run();
            for (int i = 1; i < numThreads; i++) {
                threads[i].join();
            }
        } catch (Throwable e) {
            e.printStackTrace();
        }
        jaxbables = new ArrayList<Class<ISerializableObject>>(
                clazzSet.size() + 1);
        // Add jaxb dummy object so jaxb.properties gets picked up immediately
        Class jaxb = JaxbDummyObject.class;
        jaxbables.add(jaxb);
        jaxbables.addAll(clazzSet);
        jaxbables.trimToSize();

        System.out.println("Total time spent loading classes: "
                + (System.currentTimeMillis() - realStartTime) + "ms");
    }

    private static void fail(Class service, String msg, Throwable cause)
            throws ServiceConfigurationError {
        throw new ServiceConfigurationError(service.getName() + ": " + msg,
                cause);
    }

    private static void fail(Class service, String msg)
            throws ServiceConfigurationError {
        throw new ServiceConfigurationError(service.getName() + ": " + msg);
    }

    private static void fail(Class service, URL u, int line, String msg)
            throws ServiceConfigurationError {
        fail(service, u + ":" + line + ": " + msg);
    }

    private static int parseLine(Class service, URL u, BufferedReader r,
            int lc, List<String> names) throws IOException,
            ServiceConfigurationError {
        String ln = r.readLine();
        if (ln == null) {
            return -1;
        }
        int ci = ln.indexOf('#');
        if (ci >= 0)
            ln = ln.substring(0, ci);
        ln = ln.trim();
        int n = ln.length();
        if (n != 0) {
            if ((ln.indexOf(' ') >= 0) || (ln.indexOf('\t') >= 0))
                fail(service, u, lc, "Illegal configuration-file syntax");
            int cp = ln.codePointAt(0);
            if (!Character.isJavaIdentifierStart(cp))
                fail(service, u, lc, "Illegal provider-class name: " + ln);
            for (int i = Character.charCount(cp); i < n; i += Character
                    .charCount(cp)) {
                cp = ln.codePointAt(i);
                if (!Character.isJavaIdentifierPart(cp) && (cp != '.'))
                    fail(service, u, lc, "Illegal provider-class name: " + ln);
            }

            if (!names.contains(ln))
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
        return hibernatables.get(pluginFQN);
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
    public List<Class<ISerializableObject>> getJaxbables() {
        return jaxbables;
    }

    private static class LoadSerializableClassesThread extends Thread {

        private final Enumeration<URL> urls;

        private final Set<Class<ISerializableObject>> clazzSet;

        private final Map<String, List<Class<ISerializableObject>>> hibernatables;

        public LoadSerializableClassesThread(Enumeration<URL> urls,
                Set<Class<ISerializableObject>> clazzSet,
                Map<String, List<Class<ISerializableObject>>> hibernatables) {
            this.urls = urls;
            this.clazzSet = clazzSet;
            this.hibernatables = hibernatables;
        }

        @Override
        public void run() {
            try {
                ClassLoader cl = getClass().getClassLoader();
                Set<Class<ISerializableObject>> pluginHibernateSet = new HashSet<Class<ISerializableObject>>();
                List<String> names = new ArrayList<String>();
                URL u = getNextUrl();
                while (u != null) {
                    InputStream in = null;
                    BufferedReader r = null;
                    names.clear();
                    pluginHibernateSet.clear();
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
                                lc, names)) >= 0)
                            ;
                    } catch (IOException x) {
                        fail(ISerializableObject.class,
                                "Error reading configuration file", x);
                    } finally {
                        try {
                            if (r != null)
                                r.close();
                            if (in != null)
                                in.close();
                        } catch (IOException y) {
                            fail(ISerializableObject.class,
                                    "Error closing configuration file", y);
                        }
                    }
                    Iterator<String> iter = names.iterator();

                    while (iter.hasNext()) {
                        String clazz = iter.next();
                        try {
                            long t0 = System.currentTimeMillis();
                            Class<ISerializableObject> c = (Class<ISerializableObject>) Class
                                    .forName(clazz, true, cl);
                            boolean added = false;
                            if (c.getAnnotation(XmlAccessorType.class) != null
                                    || c.getAnnotation(XmlRegistry.class) != null) {
                                addToClazzSet(c);
                                added = true;
                            }

                            if (c.getAnnotation(Entity.class) != null
                                    || c.getAnnotation(Embeddable.class) != null) {
                                pluginHibernateSet.add(c);
                                added = true;
                            }

                            long time = (System.currentTimeMillis() - t0);
                            if (!added) {
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

                    if (pluginHibernateSet.size() > 0) {
                        addToHibernatables(pluginFQN, pluginHibernateSet);
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

        private void addToClazzSet(Class<ISerializableObject> clazz) {
            synchronized (clazzSet) {
                clazzSet.add(clazz);
            }
        }

        private void addToHibernatables(String pluginFQN,
                Set<Class<ISerializableObject>> pluginHibernateSet) {
            synchronized (hibernatables) {
                hibernatables.put(pluginFQN,
                        new ArrayList<Class<ISerializableObject>>(
                                pluginHibernateSet));
            }
        }
    }
}
