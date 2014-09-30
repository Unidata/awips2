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

package com.raytheon.edex.uengine;

import java.io.File;
import java.net.URL;
import java.net.URLClassLoader;
import java.util.Enumeration;
import java.util.HashSet;
import java.util.Set;
import java.util.zip.ZipEntry;
import java.util.zip.ZipFile;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;

import com.raytheon.edex.util.Util;
import com.raytheon.uf.common.util.FileUtil;
import com.raytheon.uf.common.util.JarUtil;
import com.raytheon.uf.edex.core.EDEXUtil;

/**
 * Determines the classes that extend ScriptTask. Derived from original
 * METaskList of original uEngine.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date             PR#             Engineer            Description
 * -----------      ----------      ------------        --------------------------
 * Apr 2, 2007                      njensen             Initial Creation
 * Jul 10, 2014     2914            garmendariz         Remove EnvProperties
 * </PRE>
 * 
 */
public class MicroEngineTaskManager {
    /**
     * the logger.
     */
    private static final Log theLogger = LogFactory
            .getLog(MicroEngineTaskManager.class);

    /**
     * name of the base class for uEngine tasks.
     */
    private static final String taskName = "com.raytheon.edex.uengine.tasks.ScriptTask";

    /**
     * Hash Table containing the task list.
     */
    private static Set<String> packages = new HashSet<String>();

    /**
     * The single instance of METaskList.
     */
    private static MicroEngineTaskManager instance;

    /**
     * Constructor. Accesses the contents of <code>uEngine.jar</code> plus any
     * JARs in the EXEC plugins directory and creates a hash table indexed by
     * the names of valid uEngine tasks.
     * <P>
     * The key is the full class name and the value is the short name of the
     * class file. For example, <code>com/raytheon/edex/uengine/Task</code>
     * results in the entry:<BR>
     * <code>properties{com.raytheon.edex.uengine.Task} = Task.class</code>.
     * <P>
     * If any problems occur, the hash table will be empty.
     */
    private MicroEngineTaskManager() {
        Class<?> task = null;
        URLClassLoader uriLoader;
        ClassLoader loader = ClassLoader.getSystemClassLoader();

        String jarPath = null;
        String jarName = null;
        String pluginPath = null;
        theLogger.debug("creating MicroEngineTaskManager instance");
        try {
            task = Class.forName(taskName);
            /*
             * First, check uEngine.jar for tasks
             */
            jarName = getJarName();
            jarPath = getJarLocation();
            addTasksInJar(loader, task, new File(jarPath + "/" + jarName));

            /*
             * next, get a listing of plugin JARs and search for tasks.
             */
            pluginPath = EDEXUtil.getEdexPlugins();
            pluginPath = FileUtil.convertFilePath(pluginPath);
            File[] files = JarUtil.getJarFiles(new File(pluginPath));
            uriLoader = Util.getLibrariesClassLoader(files);
            for (File file : files) {
                addTasksInJar(uriLoader, task, file);
            }
        } catch (Exception e) {
            /*
             * slimply print a stack trace with the offending JAR file path
             * added
             */
            theLogger.error("unable to create MicroEngineTaskManager: ", e);
            (new Exception(e.getMessage() + ": JAR file = " + jarName))
                    .printStackTrace();
        }
    }

    /**
     * Finds the tasks in the specified JAR and adds them to the properties
     * list.
     * 
     * @param loader
     *            the class loader for the JAR
     * @param task
     *            the basic task class instance
     * @param file
     *            the JAR file to search
     * 
     * @throws Exception
     *             when any problem occurs.
     */
    private void addTasksInJar(ClassLoader loader, Class<?> task, File file)
            throws Exception {
        String match = "^.*\\.class$";
        Class<?> temp = null;
        ZipFile zf = new ZipFile(file.getAbsolutePath());

        Enumeration<? extends ZipEntry> enm = zf.entries();
        while (enm.hasMoreElements()) {
            ZipEntry ze = (ZipEntry) enm.nextElement();
            String name = ze.getName();
            if (!ze.isDirectory() && name.matches(match)) {
                String fullName = pathToClassName(name);
                temp = loader.loadClass(fullName);
                if (!fullName.equals(taskName) &&

                task.isAssignableFrom(temp)) {
                    packages.add(fullName.substring(0,
                            fullName.lastIndexOf(".")));
                }
            }
        }

    }

    /**
     * Retrieves the value associated with the specified key.
     * 
     * @param key
     *            the key
     * @return the value
     */
    public static synchronized Set<String> getPackages() {
        if (instance == null) {
            instance = new MicroEngineTaskManager();
        }
        return packages;
    }

    /**
     * Finds and returns the fully qulaified path name of the JAR containing
     * this class.
     * 
     * @return the fully qualified class name
     */
    private String getJarName() {
        URL url = MicroEngineTaskManager.class.getProtectionDomain()
                .getCodeSource().getLocation();
        File fl = new File(url.getPath());
        String retVal = fl.getName();
        return retVal;
    }

    private String getJarLocation() {
        URL url = MicroEngineTaskManager.class.getProtectionDomain()
                .getCodeSource().getLocation();
        File fl = new File(url.getPath());
        String retVal = fl.getParent();
        return retVal;
    }

    /**
     * Extracts and returns the class name from a class path.
     * 
     * @param path
     *            the class path
     * @return the class name
     */
    // private String classNameFromPath(String path) {
    // return (new File(path)).getName();
    // }

    /**
     * Converts a relative path for a class file to a fully qualified class
     * name.
     * 
     * @param path
     *            the path to the class file
     * @return the fully qualified class name.
     */
    private String pathToClassName(String path) {
        return path.replaceAll("\\/", ".").replaceFirst("\\.class$", "");
    }

}
