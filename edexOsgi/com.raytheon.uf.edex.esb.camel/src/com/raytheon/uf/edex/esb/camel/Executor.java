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
import java.util.Arrays;
import java.util.Collections;
import java.util.Enumeration;
import java.util.Iterator;
import java.util.List;
import java.util.Properties;
import java.util.jar.JarEntry;
import java.util.jar.JarFile;
import java.util.regex.Pattern;

import javax.management.MBeanServerConnection;
import javax.management.ObjectInstance;
import javax.management.ObjectName;
import javax.xml.bind.JAXBContext;
import javax.xml.bind.JAXBException;
import javax.xml.bind.Unmarshaller;

import org.apache.activemq.console.util.JmxMBeansUtil;
import org.springframework.context.support.ClassPathXmlApplicationContext;

import com.raytheon.uf.common.util.PropertiesUtil;
import com.raytheon.uf.edex.core.EDEXUtil;
import com.raytheon.uf.edex.core.props.PropertiesFactory;
import com.raytheon.uf.edex.esb.camel.context.ContextManager;
import com.raytheon.uf.edex.esb.camel.spring.DefaultEdexMode;
import com.raytheon.uf.edex.esb.camel.spring.EdexMode;
import com.raytheon.uf.edex.esb.camel.spring.EdexModesContainer;

/**
 * Provides the central mechanism for starting the ESB
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Nov 14, 2008            chammack     Initial creation
 * Jul 14, 2009  #2950     njensen      Basic spring file ordering
 * Apr 05, 2010  #3857     njensen      Removed file ordering in favor of
 *                                          spring's depends-on attribute
 * Jun 12, 2012  #0609     djohnson     Use EDEXUtil for EDEX_HOME.
 * Jul 09, 2012  #0643     djohnson     Read plugin provided resources into system properties.
 * Jul 17, 2012  #0740     djohnson     Redo changes since the decomposed repositories lost them.
 * Oct 19, 2012  #1274     bgonzale     Load properties from files in conf 
 *                                         resources directory.
 * 
 * </pre>
 * 
 * @author chammack
 * @version 1.0
 */

public class Executor {

    public static final String XML = ".xml";

    private static final Pattern XML_PATTERN = Pattern.compile("\\" + XML);

    private static final Pattern RES_SPRING_PATTERN = Pattern
            .compile("res/spring/");

    private static final String MODES_FILE = "modes.xml";

    public static void start() throws Exception {
        long t0 = System.currentTimeMillis();
        Thread.currentThread().setName("EDEXMain");
        System.setProperty("System.status", "Starting");

        final boolean[] shutdown = new boolean[] { false };

        String pluginDirStr = PropertiesFactory.getInstance()
                .getEnvProperties().getEnvValue("PLUGINDIR");

        List<String> xmlFiles = new ArrayList<String>();

        List<File> propertiesFiles = new ArrayList<File>();
        File confDir = new File(EDEXUtil.EDEX_HOME
                + File.separator + "conf");
        File resourcesDir = new File(confDir, "resources");
        propertiesFiles.addAll(Arrays.asList(findFiles(resourcesDir,
                ".properties")));
        // load site files after loading the config files so that their
        // properties take precedence.
        String site = System.getProperty("aw.site.identifier");
        File siteResourcesDir = new File(confDir, "resources" + File.separator
                + "site" + File.separator + site);
        propertiesFiles.addAll(Arrays.asList(findFiles(siteResourcesDir,
                ".properties")));

        // Add each file to the system properties
        for (File propertiesFile : propertiesFiles) {
            Properties properties = PropertiesUtil.read(propertiesFile);
            System.getProperties().putAll(properties);
        }

        File springDir = new File(confDir, "spring");
        File[] springFiles = findFiles(springDir, XML);

        List<String> springList = new ArrayList<String>();
        for (File f : springFiles) {
            String name = f.getName();

            xmlFiles.add(name);
            springList.add(XML_PATTERN.matcher(name).replaceAll(""));
        }

        EdexModesContainer emc = getModeFilter(confDir);
        String modeName = System.getProperty("edex.run.mode");
        String highMem = System.getProperty("HighMem");
        boolean highMemEnabled = "on".equals(highMem);

        if (modeName != null && modeName.length() > 0) {
            System.out.println("EDEX run configuration: " + modeName
                    + ", High Memory Mode: " + highMemEnabled);
        } else {
            System.out
                    .println("No EDEX run configuration specified, defaulting to use all discovered spring XML files");
        }
        System.out.println("EDEX site configuration: "
                + System.getProperty("aw.site.identifier"));

        EdexMode edexMode = emc.getMode(modeName, highMemEnabled);

        if (edexMode != null && edexMode.isTemplate()) {
            throw new UnsupportedOperationException(modeName
                    + " is a template mode, and is not bootable.");
        }

        FilenameFilter mode = edexMode;

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

    /**
     * Finds all files in the specified directory with specified extension.
     * 
     * @param directory
     *            the directory
     * @param extension
     *            file extension
     * @return the file array
     */
    private static File[] findFiles(File directory, final String extension) {
        File[] files = directory.listFiles(new FileFilter() {
            @Override
            public boolean accept(File pathname) {
                return pathname.getName().endsWith(extension);
            }
        });

        // If no files were found return an empty array
        return (files == null) ? new File[0] : files;
    }

    private static String printList(List<String> components) {
        StringBuffer sb = new StringBuffer();

        Collections.sort(components);
        Iterator<String> iterator = components.iterator();
        while (iterator.hasNext()) {
            sb.append(iterator.next());
            sb.append(", ");
        }

        int length = sb.length();
        sb.delete(length - 2, length);

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
                    retVal.add(RES_SPRING_PATTERN.matcher(
                            XML_PATTERN.matcher(name).replaceAll(""))
                            .replaceAll(""));
                }
            }

        }

        return retVal;
    }

    private static EdexModesContainer getModeFilter(File confDir)
            throws IOException, JAXBException {
        File file = new File(confDir.getPath(), MODES_FILE);

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
                PropertiesUtil.close(reader);
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
