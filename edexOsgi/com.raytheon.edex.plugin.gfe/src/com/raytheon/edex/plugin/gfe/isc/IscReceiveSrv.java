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

import java.io.IOException;
import java.nio.file.DirectoryStream;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.nio.file.StandardCopyOption;
import java.util.Arrays;
import java.util.Collection;
import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
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
 * May 20, 2015   #4491    dgilling    Remediate path manipulation possibilities.
 * 
 * </pre>
 * 
 * @author dgilling
 * @version 1.0
 */

public class IscReceiveSrv {

    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(IscReceiveSrv.class);

    private static final String METHOD_NAME = "main";

    /*
     * TODO: determine if this constant and the cleanup of DOC files in the
     * finally block of prepareIscDataRec() is still necessary.
     */
    private static final String DOC_FILE_FILTER = "*.doc";

    private static final Path ISC_PRODUCTS_DIR = Paths.get("/awips2",
            "GFESuite", "products", "ISC");

    private static final Collection<Path> WHITELISTED_PATHS = Arrays
            .asList(ISC_PRODUCTS_DIR);

    private static final String COPY_ERROR_MSG = "Failed to copy: [%s] to %s. Unable to execute iscDataRec for %s.";

    private static final String UNSAFE_PATH_MSG = "Skipping iscDataRec processing because file %s comes from an unsafe location.";

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
        Map<String, String[]> siteMap = new HashMap<>();

        String[] incomingFiles = args[2].split(",");
        String xmlPathString = (incomingFiles.length == 1) ? incomingFiles[0]
                : incomingFiles[1];
        String dataPathString = (incomingFiles.length == 1) ? null
                : incomingFiles[0];
        Path xmlFilePath = Paths.get(xmlPathString);
        Path dataFilePath = (dataPathString != null) ? Paths
                .get(dataPathString) : null;

        try {
            if (!isSafePathToProcess(xmlFilePath)) {
                statusHandler.warn(String.format(UNSAFE_PATH_MSG, xmlFilePath));
                return Collections.emptyMap();
            }

            if ((dataFilePath != null) && (!isSafePathToProcess(dataFilePath))) {
                statusHandler
                        .warn(String.format(UNSAFE_PATH_MSG, dataFilePath));
                return Collections.emptyMap();
            }

            String xmlFileName = xmlFilePath.getFileName().toString();
            String dataFileName = (dataPathString != null) ? dataFilePath
                    .getFileName().toString() : null;

            Collection<String> destinations = getXMLDestinations(xmlFilePath);
            Set<String> activeSites = IFPServer.getActiveSites();
            Set<String> activeDestinations = new HashSet<>(activeSites);
            activeDestinations.retainAll(destinations);

            for (String siteId : activeDestinations) {
                if (IFPServerConfigManager.getServerConfig(siteId).requestISC()) {
                    String[] modifiedArgs = new String[args.length];
                    System.arraycopy(args, 0, modifiedArgs, 0, args.length);

                    if (dataFilePath != null) {
                        String newDataFileName = dataFileName + "." + siteId;
                        Path newDataFilePath = dataFilePath
                                .resolveSibling(newDataFileName);

                        try {
                            Files.copy(dataFilePath, newDataFilePath,
                                    StandardCopyOption.REPLACE_EXISTING);
                        } catch (IOException e) {
                            statusHandler.error(String.format(COPY_ERROR_MSG,
                                    dataFilePath, newDataFilePath, siteId), e);
                            continue;
                        }

                        modifiedArgs[2] = modifiedArgs[2].replace(
                                dataPathString, newDataFilePath.toString());
                    }

                    String newXmlFileName = xmlFileName + "." + siteId;
                    Path newXmlFilePath = xmlFilePath
                            .resolveSibling(newXmlFileName);

                    try {
                        Files.copy(xmlFilePath, newXmlFilePath,
                                StandardCopyOption.REPLACE_EXISTING);
                    } catch (IOException e) {
                        statusHandler.error(String.format(COPY_ERROR_MSG,
                                xmlFilePath, newXmlFilePath, siteId), e);
                        continue;
                    }

                    modifiedArgs[2] = modifiedArgs[2].replace(xmlPathString,
                            newXmlFilePath.toString());

                    siteMap.put(siteId, modifiedArgs);
                }
            }
        } finally {
            Collection<Path> filesToDelete = new HashSet<>();

            filesToDelete.add(xmlFilePath);
            if (dataFilePath != null) {
                filesToDelete.add(dataFilePath);
            }
            try (DirectoryStream<Path> stream = Files.newDirectoryStream(
                    xmlFilePath.getParent(), DOC_FILE_FILTER)) {
                for (Path entry : stream) {
                    filesToDelete.add(entry);
                }
            } catch (IOException e) {
                statusHandler.error("Unable to list .doc files in directory "
                        + xmlFilePath.getParent(), e);
            }

            for (Path toDelete : filesToDelete) {
                try {
                    Files.deleteIfExists(toDelete);
                } catch (IOException e) {
                    statusHandler.error("Unable to delete file " + toDelete, e);
                }
            }
        }

        return siteMap;
    }

    private boolean isSafePathToProcess(Path path) {
        try {
            Path realPath = path.toRealPath();
            for (Path safePath : WHITELISTED_PATHS) {
                if (realPath.startsWith(safePath)) {
                    return true;
                }
            }
        } catch (IOException e) {
            statusHandler.error("Unable to resolve the real path for " + path,
                    e);
        }

        return false;
    }

    private Collection<String> getXMLDestinations(final Path xmlDocumentPath)
            throws SAXException, IOException, ParserConfigurationException {
        DocumentBuilderFactory dbf = DocumentBuilderFactory.newInstance();
        dbf.setExpandEntityReferences(false);
        DocumentBuilder db = dbf.newDocumentBuilder();
        Document doc = db.parse(xmlDocumentPath.toFile());
        doc.getDocumentElement().normalize();

        // Expected XML format:
        // <isc>
        // <source></source>
        // <destinations>
        // <address>
        // <site>SITE_ID</site>
        // </address>
        // </destinations>
        // </isc>
        Collection<String> destinations = new HashSet<>();
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
