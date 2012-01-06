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
import java.util.Iterator;
import java.util.List;
import java.util.Set;
import java.util.StringTokenizer;

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
 * Nov 14, 2008            chammack     Initial creation
 * </pre>
 * 
 * @author chammack
 * @version 1.0
 */

public class Main {

    private static final String SYSTEM_PROPERTY = "edex.home";

    private File edexHome;

    private Set<File> classPath = new HashSet<File>();

    private Set<File> extensions = new HashSet<File>();

    private ClassLoader classLoader;

    private static final int MAXIMUM_RECURSION_DEPTH = 6;

    public static void main(String[] args) {

        boolean valid = true;

        List<String> argList = Arrays.asList(args);

        if (args.length != 1) {
            valid = false;
        } else if (!argList.contains("start") && !argList.contains("stop")) {
            valid = false;
        }

        if (!valid) {
            System.out.println("Invalid or missing arguments.");
            System.out.println("Usage: edex [operation]");
            System.out.println("       where operation is start or stop.");
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
                        while (br.ready()) {
                            String line = br.readLine();
                            System.out.println(line);
                        }
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

                        if (fr != null)
                            try {
                                fr.close();
                            } catch (RuntimeException e) {
                                // ignore
                            }

                    }

                }

                Method m = c.getMethod("start", new Class<?>[0]);
                m.invoke(null, new Object[0]);
            } else if (args[0].equals("stop")) {
                Method m = c.getMethod("stop", new Class<?>[0]);
                m.invoke(null, new Object[0]);
            }
            System.exit(0);
        } catch (ClassNotFoundException e) {
            System.out.println("Could not load class: " + e.getMessage());
            if (cl != null) {
                System.out.println("Class loader setup: ");
                printClassLoaderTree(cl);
            }

            System.exit(1);
        } catch (Throwable e) {
            e.printStackTrace();
            printClassLoaderTree(cl);
            System.exit(1);
        }
    }

    public ClassLoader getClassLoader() {
        if (classLoader == null) {
            classLoader = Main.class.getClassLoader();
            if (!extensions.isEmpty() || !classPath.isEmpty()) {

                ArrayList<URL> urls = new ArrayList<URL>(500);

                for (Iterator<File> iter = classPath.iterator(); iter.hasNext();) {
                    File dir = iter.next();
                    try {
                        urls.add(dir.toURI().toURL());
                    } catch (MalformedURLException e) {
                        System.out.println("Bad directory entry: " + dir
                                + ":: Skipping.");
                    }
                }

                File[] extensionsArray = extensions.toArray(new File[0]);

                // Sort the dirs so that classpath built is
                // consistently in the same order
                Arrays.sort(extensionsArray, new Comparator<File>() {
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
                                public int compare(File f1, File f2) {
                                    return f1.getName().compareTo(f2.getName());
                                }
                            });

                            for (int j = 0; j < files.length; j++) {
                                if (files[j].getName().endsWith(".zip")
                                        || files[j].getName().endsWith(".jar")) {
                                    try {
                                        urls.add(files[j].toURI().toURL());
                                    } catch (MalformedURLException e) {
                                        System.out.println("Bad jar entry: "
                                                + dir + ":: Skipping.");
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
            System.out
                    .println("WARNING: "
                            + file
                            + " exceeded maximum recursion depth.  Not traversing further.");
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
                        System.setProperty(SYSTEM_PROPERTY, edexHome
                                .getAbsolutePath());
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
        if (fileList != null && fileList.length() > 0) {
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
    private static int printClassLoaderTree(ClassLoader cl) {
        int depth = 0;
        if (cl.getParent() != null) {
            depth = printClassLoaderTree(cl.getParent()) + 1;
        }

        StringBuffer indent = new StringBuffer();
        for (int i = 0; i < depth; i++) {
            indent.append("  ");
        }

        if (cl instanceof URLClassLoader) {
            URLClassLoader ucl = (URLClassLoader) cl;
            System.out.println(indent + cl.getClass().getName() + " {");
            URL[] urls = ucl.getURLs();
            for (int i = 0; i < urls.length; i++) {
                System.out.println(indent + "  " + urls[i]);
            }
            System.out.println(indent + "}");
        } else {
            System.out.println(indent + cl.getClass().getName());
        }
        return depth;
    }

}
