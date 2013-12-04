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
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.Iterator;
import java.util.List;
import java.util.Properties;
import java.util.concurrent.CountDownLatch;

import org.springframework.context.support.ClassPathXmlApplicationContext;

import com.raytheon.uf.common.util.PropertiesUtil;
import com.raytheon.uf.edex.core.modes.EDEXModesUtil;
import com.raytheon.uf.edex.esb.camel.context.ContextManager;

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
 * Feb 14, 2013  1638      mschenke     Removing activemq reference in stop
 * Apr 22, 2013  #1932     djohnson     Use countdown latch for a shutdown hook.
 * Dec 04, 2013  2566      bgonzale     refactored mode methods to a utility in edex.core.
 * 
 * </pre>
 * 
 * @author chammack
 * @version 1.0
 */

public class Executor {

    private static final CountDownLatch shutdownLatch = new CountDownLatch(1);

    public static void start() throws Exception {

        Runtime.getRuntime().addShutdownHook(new Thread() {
            @Override
            public void run() {
                shutdownLatch.countDown();
            }
        });

        long t0 = System.currentTimeMillis();
        Thread.currentThread().setName("EDEXMain");
        System.setProperty("System.status", "Starting");

        List<String> xmlFiles = new ArrayList<String>();

        List<File> propertiesFiles = new ArrayList<File>();
        File confDir = new File(EDEXModesUtil.CONF_DIR);
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
        File[] springFiles = findFiles(springDir, EDEXModesUtil.XML);

        List<String> springList = new ArrayList<String>();
        for (File f : springFiles) {
            String name = f.getName();

            xmlFiles.add(name);
            springList.add(EDEXModesUtil.XML_PATTERN.matcher(name).replaceAll(
                    ""));
        }

        String modeName = System.getProperty("edex.run.mode");

        if (modeName != null && modeName.length() > 0) {
            System.out.println("EDEX run configuration: " + modeName);
        } else {
            System.out
                    .println("No EDEX run configuration specified, defaulting to use all discovered spring XML files");
        }
        System.out.println("EDEX site configuration: "
                + System.getProperty("aw.site.identifier"));

        List<String> discoveredPlugins = EDEXModesUtil.extractSpringXmlFiles(
                xmlFiles,
                modeName);

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

        shutdownLatch.await();
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

}
