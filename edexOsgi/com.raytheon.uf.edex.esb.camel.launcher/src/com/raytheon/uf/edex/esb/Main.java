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
package com.raytheon.uf.edex.esb;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileReader;
import java.lang.reflect.Method;
import java.net.JarURLConnection;
import java.net.MalformedURLException;
import java.net.URI;
import java.net.URL;
import java.net.URLClassLoader;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Comparator;
import java.util.HashSet;
import java.util.List;
import java.util.Set;
import java.util.StringTokenizer;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * Provides a launcher for starting the ESB
 * 
 * The launcher uses reflection to add all jars in the lib path, and then
 * execute the Executor class
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Nov 14, 2008            chammack    Initial creation.
 * Feb 15, 2013 1638       mschenke    Removed reference to unused "stop" method.
 * Apr 15, 2014 2726       rjpeter     Use slf4j logger.
 * </pre>
 * 
 * @author chammack
 * @version 1.0
 */

public class Main {

    private static final String SYSTEM_PROPERTY = "edex.home";

    private File edexHome;

    private final Set<File> classPath = new HashSet<File>();

    private final Set<File> extensions = new HashSet<File>();

    private ClassLoader classLoader;

    private static final int MAXIMUM_RECURSION_DEPTH = 6;

    private static final Logger logger = LoggerFactory.getLogger(Main.class);

    public static void main(String[] args) {

        boolean valid = true;

        List<String> argList = Arrays.asList(args);

        if (args.length != 1) {
            valid = false;
        } else if (!argList.contains("start")) {
            valid = false;
        }

        if (!valid) {
            StringBuilder msg = new StringBuilder(200);
            msg.append("Invalid or missing arguments.")
                    .append("\nUsage: edex [operation]")
                    .append("\n       where operation is start.");
            logger.error(msg.toString());
            System.exit(0);
        }

        Main app = new Main();

        File confDir = new File(app.getEDEXHome(), "conf");
        app.addClassPath(confDir);

        File baseLibDir = new File(app.getEDEXHome(), "lib");

        app.addExtension(baseLibDir);

        List<File> dirList = new ArrayList<File>();
        buildRecursiveDirList(0, dirList, baseLibDir);

        for (File dir : dirList) {
            app.addExtension(dir);
        }
        ClassLoader cl = app.getClassLoader();
        try {
            Thread.currentThread().setContextClassLoader(cl);
            // Must use reflection here so that startup does not have
            // dependencies
            Class<?> c = cl
                    .loadClass("com.raytheon.uf.edex.esb.camel.Executor");
            if (args[0].equals("start")) {
                // Print the banner
                File banner = new File(app.getEDEXHome().getAbsolutePath()
                        + File.separator + "conf" + File.separator
                        + "banner.txt");
                if (banner.exists()) {
                    FileReader fr = null;
                    BufferedReader br = null;
                    try {
                        fr = new FileReader(banner);
                        br = new BufferedReader(fr);
                        StringBuilder msg = new StringBuilder(250);
                        while (br.ready()) {
                            String line = br.readLine();
                            msg.append("\n").append(line);
                        }
                        logger.info(msg.toString());
                    } catch (Throwable e) {
                        // ignore
                    } finally {
                        if (br != null) {
                            try {
                                br.close();
                            } catch (RuntimeException e) {
                                // ignore
                            }
                        }

                        if (fr != null) {
                            try {
                                fr.close();
                            } catch (RuntimeException e) {
                                // ignore
                            }
                        }

                    }

                }

                Method m = c.getMethod("start", new Class<?>[0]);
                m.invoke(null, new Object[0]);
            }
            System.exit(0);
        } catch (ClassNotFoundException | LinkageError e) {
            logger.error("Could not load class", e);
            if (cl != null) {
                StringBuilder msg = new StringBuilder(1000);
                msg.append("Class  loader setup:");
                printClassLoaderTree(cl, msg);
                logger.info(msg.toString());
            }

            System.exit(1);
        } catch (Throwable e) {
            logger.error("Error occurred during startup: ", e);
            System.exit(1);
        }
    }

    public ClassLoader getClassLoader() {
        if (classLoader == null) {
            classLoader = Main.class.getClassLoader();
            if (!extensions.isEmpty() || !classPath.isEmpty()) {

                ArrayList<URL> urls = new ArrayList<URL>(500);

                for (File dir : classPath) {
                    try {
                        urls.add(dir.toURI().toURL());
                    } catch (MalformedURLException e) {
                        logger.warn("Bad directory entry: " + dir
                                + ":: Skipping.", e);
                    }
                }

                File[] extensionsArray = extensions.toArray(new File[0]);

                // Sort the dirs so that classpath built is
                // consistently in the same order
                Arrays.sort(extensionsArray, new Comparator<File>() {
                    @Override
                    public int compare(File f1, File f2) {
                        return f1.getPath().compareTo(f2.getPath());
                    }
                });

                for (File dir : extensionsArray) {
                    if (dir.isDirectory()) {
                        File[] files = dir.listFiles();
                        if (files != null) {

                            // Sort the jars so that classpath built is
                            // consistently in the same order
                            Arrays.sort(files, new Comparator<File>() {
                                @Override
                                public int compare(File f1, File f2) {
                                    return f1.getName().compareTo(f2.getName());
                                }
                            });

                            for (File file : files) {
                                if (file.getName().endsWith(".zip")
                                        || file.getName().endsWith(".jar")) {
                                    try {
                                        urls.add(file.toURI().toURL());
                                    } catch (MalformedURLException e) {
                                        logger.warn("Bad jar entry: " + dir
                                                + ":: Skipping.", e);
                                    }
                                }
                            }
                        }
                    }
                }
                URL u[] = new URL[urls.size()];
                urls.toArray(u);
                classLoader = new URLClassLoader(u, classLoader);
            }
            Thread.currentThread().setContextClassLoader(classLoader);
        }
        return classLoader;

    }

    private static void buildRecursiveDirList(int depth, List<File> dirList,
            File file) {
        if (depth < MAXIMUM_RECURSION_DEPTH) {
            File[] files = file.listFiles();
            for (File f : files) {
                if (f.isDirectory()) {
                    dirList.add(f);
                    buildRecursiveDirList(depth + 1, dirList, f);
                }
            }
        } else {
            logger.warn(file
                    + ": exceeded maximum recursion depth.  Not traversing further.");
        }
    }

    public File getEDEXHome() {
        if (edexHome == null) {
            if (System.getProperty(SYSTEM_PROPERTY) != null) {
                edexHome = new File(System.getProperty(SYSTEM_PROPERTY));
            }

            if (edexHome == null) {
                URL url = Main.class.getClassLoader().getResource(
                        Main.class.getName());
                if (url != null) {
                    try {
                        JarURLConnection jarConnection = (JarURLConnection) url
                                .openConnection();
                        url = jarConnection.getJarFileURL();
                        URI baseURI = new URI(url.toString()).resolve("..");
                        edexHome = new File(baseURI).getCanonicalFile();
                        System.setProperty(SYSTEM_PROPERTY,
                                edexHome.getAbsolutePath());
                    } catch (Exception ignored) {
                    }
                }
            }

            if (edexHome == null) {
                edexHome = new File("../.");
                System.setProperty(SYSTEM_PROPERTY, edexHome.getAbsolutePath());
            }
        }

        return edexHome;
    }

    public void addClassPathList(String fileList) {
        if ((fileList != null) && (fileList.length() > 0)) {
            StringTokenizer tokenizer = new StringTokenizer(fileList, ";");
            while (tokenizer.hasMoreTokens()) {
                addClassPath(new File(tokenizer.nextToken()));
            }
        }
    }

    public void addExtension(File extensionDir) {
        this.extensions.add(extensionDir);
    }

    public void addClassPath(File classpath) {
        this.classPath.add(classpath);
    }

    /**
     * Print out what's in the classloader tree being used.
     * 
     * @param cl
     * @return depth
     */
    private static int printClassLoaderTree(ClassLoader cl, StringBuilder msg) {
        int depth = 0;
        if (cl.getParent() != null) {
            depth = printClassLoaderTree(cl.getParent(), msg) + 1;
        }

        StringBuilder indent = new StringBuilder(2 * depth);
        for (int i = 0; i < depth; i++) {
            indent.append("  ");
        }

        if (cl instanceof URLClassLoader) {
            URLClassLoader ucl = (URLClassLoader) cl;
            msg.append("\n").append(indent).append(cl.getClass().getName())
                    .append(" {");
            URL[] urls = ucl.getURLs();
            for (URL url : urls) {
                msg.append("\n").append(indent).append("  ").append(url);
            }
            msg.append("\n").append(indent).append("}");
        } else {
            msg.append("\n").append(indent).append(cl.getClass().getName());
        }

        return depth;
    }

}
