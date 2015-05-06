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

import java.io.File;
import java.io.FilenameFilter;
import java.io.IOException;
import java.util.Arrays;
import java.util.Collection;
import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Set;

import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.DocumentBuilderFactory;
import javax.xml.parsers.ParserConfigurationException;

import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.w3c.dom.Node;
import org.w3c.dom.NodeList;
import org.xml.sax.SAXException;

import com.raytheon.edex.plugin.gfe.config.IFPServerConfigManager;
import com.raytheon.edex.plugin.gfe.exception.GfeConfigurationException;
import com.raytheon.edex.plugin.gfe.server.IFPServer;
import com.raytheon.uf.common.dataplugin.gfe.request.IscDataRecRequest;
import com.raytheon.uf.common.python.concurrent.IPythonJobListener;
import com.raytheon.uf.common.python.concurrent.PythonJobCoordinator;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.common.util.FileUtil;
import com.raytheon.uf.common.util.file.FilenameFilters;

/**
 * ISC data receive service. Takes incoming request and executes iscDataRec
 * script using provided parameters.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Mar 05, 2012   #361     dgilling    Initial creation
 * Mar 12, 2013   #1759    dgilling    Re-implement using IscScript.
 * Mar 14, 2013   #1794    djohnson    Consolidate common FilenameFilter implementations.
 * Dec 10, 2014   #4953    randerso    Properly handle single file reception
 * May 06, 2015   #4383    dgilling    Properly XML parse incoming XML file.
 * 
 * </pre>
 * 
 * @author dgilling
 * @version 1.0
 */

public class IscReceiveSrv {

    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(IscReceiveSrv.class);

    private static final String ISC_REQUEST = "iscrequest";

    private static final String METHOD_NAME = "main";

    private static final FilenameFilter docFileFilter = FilenameFilters
            .byFileExtension(".doc");

    private static final IPythonJobListener<String> jobListener = new IPythonJobListener<String>() {

        @Override
        public void jobFinished(String result) {
            if (result != null) {
                statusHandler.error("Error encountered executing iscDataRec: "
                        + result);
            }
        }

        @Override
        public void jobFailed(Throwable e) {
            statusHandler.error("Error encountered executing iscDataRec: ", e);
        }
    };

    private final PythonJobCoordinator<IscScript> threadPool;

    public IscReceiveSrv(PythonJobCoordinator<IscScript> threadPool) {
        this.threadPool = threadPool;
    }

    public void processRequest(IscDataRecRequest request) {
        String[] origArgs = request.getArgString().trim().split(" ");

        Map<String, String[]> siteArgMap = Collections.emptyMap();
        try {
            siteArgMap = prepareIscDataRec(origArgs);
        } catch (IOException e) {
            statusHandler.error("Error reading received XML file", e);
            return;
        } catch (InterruptedException e) {
            statusHandler.error("Error copying received ISC file", e);
            return;
        } catch (GfeConfigurationException e) {
            statusHandler.error("Error getting GFE configuration", e);
            return;
        } catch (SAXException | ParserConfigurationException e) {
            statusHandler.error("Error parsing received XML file.", e);
            return;
        }

        for (Entry<String, String[]> siteArgs : siteArgMap.entrySet()) {
            Map<String, Object> args = new HashMap<String, Object>();
            args.put("argv", Arrays.asList(siteArgs.getValue()));
            IscScriptExecutor executor = new IscScriptExecutor(METHOD_NAME,
                    siteArgs.getKey(), args);
            try {
                threadPool.submitAsyncJob(executor, jobListener);
            } catch (Exception e) {
                statusHandler
                        .handle(Priority.PROBLEM,
                                "Could not submit execution job to Python thread pool for iscDataRec",
                                e);
            }
        }
    }

    private Map<String, String[]> prepareIscDataRec(String[] args)
            throws IOException, InterruptedException,
            GfeConfigurationException, SAXException,
            ParserConfigurationException {
        Map<String, String[]> siteMap = new HashMap<String, String[]>();

        String[] incomingFiles = args[2].split(",");
        String xmlFileName = "";
        String dataFileName = null;
        if (incomingFiles.length == 1) {
            xmlFileName = incomingFiles[0];
        } else {
            dataFileName = incomingFiles[0];
            xmlFileName = incomingFiles[1];
        }

        final File incomingXMLFile = new File(xmlFileName);
        DocumentBuilderFactory dbf = DocumentBuilderFactory.newInstance();
        DocumentBuilder db = dbf.newDocumentBuilder();
        Document doc = db.parse(incomingXMLFile);
        doc.getDocumentElement().normalize();

        Collection<String> siteList = getXMLDestinations(doc);
        Set<String> activeSites = IFPServer.getActiveSites();

        if (ISC_REQUEST.equals(doc.getDocumentElement().getNodeName())) {
            /*
             * This case is for processing an ISC Request/Reply message
             * requesting our site's grids.
             */
            for (String siteId : siteList) {
                if (activeSites.contains(siteId)) {
                    if (IFPServerConfigManager.getServerConfig(siteId)
                            .requestISC()) {
                        String[] newArgs = new String[args.length];
                        System.arraycopy(args, 0, newArgs, 0, args.length);
                        String newXmlFileName = xmlFileName + "." + siteId;
                        FileUtil.copyFile(incomingXMLFile, new File(
                                newXmlFileName));
                        newArgs[2] = newXmlFileName;
                        siteMap.put(siteId, newArgs);
                    }
                }
            }
            incomingXMLFile.delete();
        } else {
            try {
                for (String site : siteList) {
                    if (activeSites.contains(site)
                            && IFPServerConfigManager.getServerConfig(site)
                                    .requestISC()) {
                        String[] modifiedArgs = new String[args.length];
                        System.arraycopy(args, 0, modifiedArgs, 0, args.length);

                        if (dataFileName != null) {
                            String newFileName = dataFileName + "." + site;
                            try {
                                FileUtil.copyFile(new File(dataFileName),
                                        new File(newFileName));
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
                            modifiedArgs[2] = modifiedArgs[2].replace(
                                    dataFileName, newFileName);
                        }

                        String newXmlFileName = xmlFileName + "." + site;
                        try {
                            FileUtil.copyFile(new File(xmlFileName), new File(
                                    newXmlFileName));
                        } catch (IOException e) {
                            statusHandler.error("Failed to copy: ["
                                    + xmlFileName + "] to " + newXmlFileName
                                    + ".  Unable to execute iscDataRec for "
                                    + site, e);
                            continue;
                        }
                        if (!new File(newXmlFileName).exists()) {
                            statusHandler.error("Failed to copy: ["
                                    + xmlFileName + "] to " + newXmlFileName
                                    + ".  Unable to execute iscDataRec for "
                                    + site);
                            continue;
                        }

                        modifiedArgs[2] = modifiedArgs[2].replace(xmlFileName,
                                newXmlFileName);
                        siteMap.put(site, modifiedArgs);
                    }
                }
            } finally {
                if (dataFileName != null) {
                    File dataFile = new File(dataFileName);
                    if (dataFile.exists()) {
                        if (!dataFile.delete()) {
                            statusHandler.error("Unable to delete "
                                    + dataFileName);
                        }
                    }
                }

                File xmlFile = incomingXMLFile;
                if (xmlFile.exists()) {
                    if (!xmlFile.delete()) {
                        statusHandler.error("Unable to delete " + xmlFileName);
                    }
                }
                List<File> docFiles = FileUtil.listFiles(
                        xmlFile.getParentFile(), docFileFilter, false);
                for (File docFile : docFiles) {
                    docFile.delete();
                }
            }
        }

        return siteMap;
    }

    private Collection<String> getXMLDestinations(final Document doc)
            throws SAXException, IOException, ParserConfigurationException {
        Collection<String> destinations = new HashSet<>();

        // Expected XML format:
        // <isc>
        // <source></source>
        // <destinations>
        // <address>
        // <site>SITE_ID</site>
        // </address>
        // </destinations>
        // </isc>
        NodeList destNodes = doc.getElementsByTagName("destinations");
        if (destNodes.getLength() > 0) {
            Node destNode = destNodes.item(0);

            if (destNode.getNodeType() == Node.ELEMENT_NODE) {
                Element destElement = (Element) destNode;

                NodeList addrNodes = destElement
                        .getElementsByTagName("address");
                for (int i = 0; i < addrNodes.getLength(); i++) {
                    Node addrNode = addrNodes.item(i);
                    if (addrNode.getNodeType() == Node.ELEMENT_NODE) {
                        Element addrElement = (Element) addrNode;

                        NodeList siteIdNodes = addrElement
                                .getElementsByTagName("site");
                        if (siteIdNodes.getLength() > 0) {
                            Node siteIDNode = siteIdNodes.item(0);
                            String siteID = siteIDNode.getTextContent();
                            destinations.add(siteID);
                        }
                    }
                }
            }
        }

        return destinations;
    }
}
