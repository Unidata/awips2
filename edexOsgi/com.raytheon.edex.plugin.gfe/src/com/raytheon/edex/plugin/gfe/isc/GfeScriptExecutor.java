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
package com.raytheon.edex.plugin.gfe.isc;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.FileReader;
import java.io.FilenameFilter;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.concurrent.ConcurrentHashMap;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import com.raytheon.edex.plugin.gfe.config.IFPServerConfigManager;
import com.raytheon.edex.plugin.gfe.exception.GfeConfigurationException;
import com.raytheon.edex.site.SiteUtil;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.util.FileUtil;
import com.raytheon.uf.edex.site.SiteAwareRegistry;

/**
 * Executes gfe related python Scripts
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Oct 9, 2009             bphillip    Initial creation
 * 
 * </pre>
 * 
 * @author bphillip
 * @version 1.0
 */
public class GfeScriptExecutor {

    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(GfeScriptExecutor.class);

    public static final String SUCCESS = "Success";

    private static Map<String, GfeScript> scriptMap = new ConcurrentHashMap<String, GfeScript>();

    private static final FilenameFilter docFileFilter = new FilenameFilter() {
        @Override
        public boolean accept(File dir, String name) {
            return name.endsWith(".doc");
        }
    };

    /**
     * Creates a new GfeScriptExecutor
     */
    public GfeScriptExecutor() {

    }

    /**
     * Executes a command
     * 
     * @param command
     *            The command to execute
     * @return Null on success, else returns the exception message
     */
    public String execute(String command) {

        String retVal = SUCCESS;
        String[] argv = command.trim().split(" ");
        String[] arguments = new String[argv.length - 1];

        String scriptName = argv[0];
        System.arraycopy(argv, 1, arguments, 0, arguments.length);

        Map<String, String[]> sites = null;
        try {
            sites = getSite(scriptName, arguments);
        } catch (IOException e) {
            return "Error getting sites for script\n" + e.getMessage();
        } catch (InterruptedException e) {
            return "Error copying received ISC file\n" + e.getMessage();
        } catch (GfeConfigurationException e) {
            return "Error getting database configuration\n" + e.getMessage();
        }

        for (String site : sites.keySet()) {
            GfeScript theScript = null;
            String key = scriptName + site;
            // synchronize on the map here so multiple threads don't try
            // creating the same GfeScript object
            synchronized (scriptMap) {
                theScript = scriptMap.get(key);
                if (theScript == null) {
                    theScript = new GfeScript(scriptName, site);
                    theScript.start();
                    scriptMap.put(key, theScript);
                }
            }

            String scriptRetVal = null;
            // synchronize on the GfeScript object here so multiple threads
            // can't try to execute the same instance at once
            synchronized (theScript) {
                theScript.execute(sites.get(site));
                scriptRetVal = theScript.waitFor();
            }
            if (!scriptRetVal.equals(SUCCESS)) {
                if (retVal.equals(SUCCESS)) {
                    retVal = scriptRetVal;
                } else {
                    retVal += scriptRetVal;
                }
                statusHandler.error(scriptName + " failed for site " + site);
            }
        }
        return retVal;
    }

    private static Map<String, String[]> getSite(String scriptName,
            String[] args) throws IOException, InterruptedException,
            GfeConfigurationException {

        List<String> siteList = new ArrayList<String>();
        Map<String, String[]> siteMap = new HashMap<String, String[]>();
        if (scriptName.equals("iscDataRec")) {
            String[] tokens = args[2].split(",");
            String xmlFileName = "";
            String dataFileName = null;
            if (tokens.length == 1) {
                xmlFileName = tokens[0];
            } else {
                dataFileName = tokens[0];
                xmlFileName = tokens[1];

            }

            String fileContents = getFileContents(xmlFileName);
            Pattern pat = Pattern.compile("<site>(.*?)</site>");
            Matcher matcher = pat.matcher(fileContents);

            while (matcher.find()) {
                siteList.add(matcher.group(1));
            }
            List<String> activeSites = Arrays.asList(SiteAwareRegistry
                    .getInstance().getActiveSites());
            if (fileContents.contains("<iscrequest>")) {
                // Need to copy the request file if more than 1 site is active
                // on this EDEX server. Otherwise, the file will be deleted
                // after the first site has processed the request file
                siteList.remove(siteList.size() - 1);

                for (int i = 0; i < siteList.size(); i++) {
                    if (activeSites.contains(siteList.get(i))) {
                        if (IFPServerConfigManager.getServerConfig(
                                siteList.get(i)).requestISC()) {
                            String[] newArgs = new String[args.length];
                            System.arraycopy(args, 0, newArgs, 0, args.length);
                            String newXmlFileName = copyFile(xmlFileName,
                                    siteList.get(i));
                            newArgs[2] = newXmlFileName;
                            siteMap.put(siteList.get(i), newArgs);
                        }
                    }
                }
                new File(xmlFileName).delete();
            } else {
                // Remove the source site
                siteList.remove(0);
                Set<String> siteSet = new HashSet<String>();
                siteSet.addAll(siteList);
                try {
                    for (String site : siteSet) {
                        if (activeSites.contains(site)
                                && IFPServerConfigManager.getServerConfig(site)
                                        .requestISC()) {
                            String newFileName = null;
                            String newXmlFileName = null;
                            try {
                                newFileName = copyFile(dataFileName, site);
                                newXmlFileName = copyFile(xmlFileName, site);
                            } catch (IOException e) {
                                statusHandler
                                        .error("Failed to copy: ["
                                                + dataFileName
                                                + "] to "
                                                + newFileName
                                                + ".  Unable to execute iscDataRec for "
                                                + site, e);
                                continue;
                            }
                            if (!new File(newFileName).exists()) {
                                statusHandler
                                        .error("Failed to copy: ["
                                                + dataFileName
                                                + "] to "
                                                + newFileName
                                                + ".  Unable to execute iscDataRec for "
                                                + site);
                                continue;
                            }

                            if (!new File(newXmlFileName).exists()) {
                                statusHandler
                                        .error("Failed to copy: ["
                                                + xmlFileName
                                                + "] to "
                                                + newXmlFileName
                                                + ".  Unable to execute iscDataRec for "
                                                + site);
                                continue;
                            }

                            String[] modifiedArgs = new String[args.length];
                            System.arraycopy(args, 0, modifiedArgs, 0,
                                    args.length);
                            modifiedArgs[2] = modifiedArgs[2].replace(
                                    dataFileName, newFileName);
                            modifiedArgs[2] = modifiedArgs[2].replace(
                                    xmlFileName, newXmlFileName);
                            siteMap.put(site, modifiedArgs);

                        }
                    }
                } finally {
                    File dataFile = new File(dataFileName);
                    File xmlFile = new File(xmlFileName);

                    if (dataFile.exists()) {
                        if (!dataFile.delete()) {
                            statusHandler.error("Unable to delete "
                                    + dataFileName);
                        }
                    }
                    if (xmlFile.exists()) {
                        if (!xmlFile.delete()) {
                            statusHandler.error("Unable to delete "
                                    + xmlFileName);
                        }
                    }
                    ArrayList<File> docFiles = FileUtil.listFiles(
                            xmlFile.getParentFile(), docFileFilter, false);
                    for (File docFile : docFiles) {
                        docFile.delete();
                    }
                }
            }
        } else {
            siteMap.put(SiteUtil.getSite(), args);
        }
        return siteMap;
    }

    private static String getFileContents(String file) throws IOException {
        BufferedReader in = null;
        String fileContents = "";
        try {
            in = new BufferedReader(new FileReader(file));
            String line = null;
            while ((line = in.readLine()) != null) {
                fileContents += line;
            }
        } finally {
            if (in != null) {
                in.close();

            }
        }
        return fileContents;
    }

    private static String copyFile(String src, String site) throws IOException {
        String dst = src + "." + site;
        InputStream in = null;
        OutputStream out = null;
        try {
            in = new FileInputStream(src);
            out = new FileOutputStream(dst);

            // Transfer bytes from in to out
            byte[] buf = new byte[1024];
            int len;
            while ((len = in.read(buf)) > 0) {
                out.write(buf, 0, len);
            }
        } finally {
            if (in != null) {
                try {
                    in.close();
                } catch (IOException e) {
                    statusHandler
                            .error("Error closing input stream while copying isc file!",
                                    e);
                }
            }
            if (out != null) {
                try {
                    out.close();
                } catch (IOException e) {
                    statusHandler
                            .error("Error closing file output stream while copying isc file!",
                                    e);
                }
            }
        }
        return dst;
    }
}
