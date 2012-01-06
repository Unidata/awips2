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
package com.raytheon.uf.edex.esb.camel;

import java.io.File;
import java.io.FileFilter;
import java.io.FileReader;
import java.io.FilenameFilter;
import java.io.IOException;
import java.lang.management.ManagementFactory;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Enumeration;
import java.util.Iterator;
import java.util.List;
import java.util.jar.JarEntry;
import java.util.jar.JarFile;

import javax.management.MBeanServerConnection;
import javax.management.ObjectInstance;
import javax.management.ObjectName;
import javax.xml.bind.JAXBContext;
import javax.xml.bind.JAXBException;
import javax.xml.bind.Unmarshaller;

import org.apache.activemq.console.util.JmxMBeansUtil;
import org.springframework.context.support.ClassPathXmlApplicationContext;

import com.raytheon.uf.edex.core.props.PropertiesFactory;
import com.raytheon.uf.edex.esb.camel.context.ContextManager;
import com.raytheon.uf.edex.esb.camel.spring.DefaultEdexMode;
import com.raytheon.uf.edex.esb.camel.spring.EdexModesContainer;

/**
 * Provides the central mechanism for starting the ESB
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Nov 14, 2008            chammack     Initial creation
 * Jul 14, 2009  #2950   njensen         Basic spring file ordering
 * Apr 5, 2010   #3857   njensen        Removed file ordering in favor of
 *                                                 spring's depends-on attribute
 * 
 * </pre>
 * 
 * @author chammack
 * @version 1.0
 */

public class Executor {

    public static final String XML = ".xml";

    private static final String MODES_FILE = "modes.xml";

    public static void start() throws Exception {
        long t0 = System.currentTimeMillis();
        Thread.currentThread().setName("EDEXMain");
        System.setProperty("System.status", "Starting");

        final boolean[] shutdown = new boolean[] { false };

        String pluginDirStr = PropertiesFactory.getInstance()
                .getEnvProperties().getEnvValue("PLUGINDIR");

        List<String> xmlFiles = new ArrayList<String>();

        File confDir = new File(System.getProperty("edex.home")
                + File.separator + "conf");
        File springDir = new File(confDir, "spring");
        File[] springFiles = springDir.listFiles(new FileFilter() {

            @Override
            public boolean accept(File pathname) {
                return pathname.getName().endsWith(XML);
            }
        });
        List<String> springList = new ArrayList<String>();
        for (File f : springFiles) {
            xmlFiles.add(f.getName());
            springList.add(f.getName().replace(XML, ""));
        }

        EdexModesContainer emc = getModeFilter(confDir);
        String modeName = System.getProperty("edex.run.mode");
        String highMem = System.getProperty("HighMem");
        boolean highMemEnabled = (highMem != null && highMem.equals("on"));

        if (modeName != null && modeName.length() > 0) {
            System.out.println("EDEX run configuration: " + modeName
                    + ", High Memory Mode: " + highMemEnabled);
        } else {
            System.out
                    .println("No EDEX run configuration specified, defaulting to use all discovered spring XML files");
        }
        System.out.println("EDEX site configuration: "
                + System.getProperty("aw.site.identifier"));
        FilenameFilter mode = emc.getMode(modeName, highMemEnabled);
        if (mode == null) {
            if (modeName == null || modeName.length() == 0) {
                mode = new DefaultEdexMode();
            } else {
                throw new UnsupportedOperationException(
                        "No EDEX run configuration specified in modes.xml for "
                                + modeName);
            }
        }

        List<String> discoveredPlugins = extractSpringXmlFiles(pluginDirStr,
                xmlFiles, mode);

        System.out.println();
        System.out.println(" ");
        System.out.println("EDEX configuration files: ");
        System.out.println("-----------------------");
        System.out.println(printList(springList));
        System.out.println(" ");
        System.out.println(" ");
        System.out.println("Spring-enabled Plugins:");
        System.out.println("-----------------------");
        System.out.println(printList(discoveredPlugins));
        System.out.println(" ");
        System.out.println(" ");

        ClassPathXmlApplicationContext context = new ClassPathXmlApplicationContext(
                xmlFiles.toArray(new String[xmlFiles.size()]));
        ContextManager ctxMgr = (ContextManager) context
                .getBean("contextManager");
        // start final routes
        ctxMgr.startContexts();

        long t1 = System.currentTimeMillis();
        System.out
                .println("**************************************************");
        System.out
                .println("* EDEX ESB is now operational                    *");
        System.out.println("* Total startup time: " + ((t1 - t0) / 1000)
                + " seconds");
        System.out
                .println("**************************************************");
        System.setProperty("System.status", "Operational");
        Runtime.getRuntime().addShutdownHook(new Thread() {
            @Override
            public void run() {
                try {

                    shutdown[0] = true;
                } catch (Exception e) {
                }

            }
        });

        synchronized (shutdown) {
            while (!shutdown[0]) {
                try {
                    shutdown.wait();
                } catch (InterruptedException e) {
                }
            }
        }
    }

    private static String printList(List<String> components) {
        StringBuffer sb = new StringBuffer();
        boolean first = true;

        Collections.sort(components);
        Iterator<String> iterator = components.iterator();
        while (iterator.hasNext()) {
            if (first) {
                first = false;
            } else {
                sb.append(", ");
            }

            sb.append(iterator.next());
        }

        return sb.toString();
    }

    /**
     * Populates files with a list of files that match in the specified
     * directory
     * 
     * Returns a list of plugins, etc
     * 
     * @param jarDir
     * @param files
     * @return
     * @throws IOException
     */
    private static List<String> extractSpringXmlFiles(String jarDir,
            List<String> files, FilenameFilter filter) throws IOException {
        List<String> retVal = new ArrayList<String>();
        File jarDirFile = new File(jarDir);
        File[] jars = jarDirFile.listFiles();

        List<JarFile> jarList = new ArrayList<JarFile>();
        for (File p : jars) {
            if (p.getName().endsWith(".jar")) {
                JarFile jar = new JarFile(p);
                jarList.add(jar);
            }
        }

        for (JarFile jar : jarList) {
            Enumeration<JarEntry> entries = jar.entries();
            while (entries.hasMoreElements()) {
                JarEntry e = entries.nextElement();
                String name = e.getName();
                if (filter.accept(null, name)) {
                    files.add(name);
                    retVal.add(name.replace(XML, "").replace("res/spring/", ""));
                }
            }

        }

        return retVal;
    }

    private static EdexModesContainer getModeFilter(File confDir)
            throws IOException, JAXBException {
        String path = confDir.getPath() + File.separator + MODES_FILE;
        File file = new File(path);

        FileReader reader = null;
        Unmarshaller msh = null;
        try {
            JAXBContext jaxbContext = JAXBContext
                    .newInstance(EdexModesContainer.class);
            msh = jaxbContext.createUnmarshaller();
            reader = new FileReader(file);
            EdexModesContainer emc = (EdexModesContainer) msh.unmarshal(reader);
            return emc;
        } finally {
            if (reader != null) {
                try {
                    reader.close();
                } catch (IOException e) {
                    // ignore
                }
            }
        }
    }

    public static void stop() throws Exception {
        MBeanServerConnection conn = ManagementFactory.getPlatformMBeanServer();

        List<?> beans = JmxMBeansUtil.getAllBrokers(conn);
        for (Iterator<?> i = beans.iterator(); i.hasNext();) {
            ObjectName brokerObjName = ((ObjectInstance) i.next())
                    .getObjectName();

            String brokerName = brokerObjName.getKeyProperty("BrokerName");
            System.out.println("Stopping broker: " + brokerName);

            try {
                conn.invoke(brokerObjName, "terminateJVM",
                        new Object[] { Integer.valueOf(0) },
                        new String[] { "int" });
            } catch (Exception e) {

            }
        }
    }
}
