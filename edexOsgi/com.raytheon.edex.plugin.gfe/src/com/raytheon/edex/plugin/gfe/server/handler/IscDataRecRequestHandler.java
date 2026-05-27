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
package com.raytheon.edex.plugin.gfe.server.handler;

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

import com.raytheon.edex.plugin.gfe.exception.GfeConfigurationException;
import com.raytheon.edex.plugin.gfe.server.IFPServer;
import com.raytheon.uf.common.dataplugin.gfe.request.IscDataRecRequest;
import com.raytheon.uf.common.dataplugin.gfe.server.message.ServerResponse;
import com.raytheon.uf.common.serialization.SerializationUtil;
import com.raytheon.uf.common.serialization.comm.IRequestHandler;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.edex.core.EDEXUtil;

/**
 * Thrift request handler for <code>IscDataRecRequest</code>. Takes request and
 * places it on a queue to be executed by <code>IscReceiveSrv</code> .
 *
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date          Ticket#  Engineer  Description
 * ------------- -------- --------- --------------------------------------------
 * Oct 26, 2010           dgilling  Initial creation
 * Mar 05, 2012  361      dgilling  Make call to iscDataRec asynchronous.
 * Oct 19, 2017  6279     randerso  Moved prepareIscDataRec from IscReceiveSrv.
 *                                  Queues a separate request for each site.
 *
 * </pre>
 *
 * @author dgilling
 */

public class IscDataRecRequestHandler
        implements IRequestHandler<IscDataRecRequest> {
    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(IscDataRecRequestHandler.class);

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

    @Override
    public ServerResponse<String> handleRequest(IscDataRecRequest request)
            throws Exception {
        ServerResponse<String> sr = new ServerResponse<>();

        String[] origArgs = request.getArgString().trim().split(" ");

        Map<String, String[]> siteArgMap = Collections.emptyMap();
        String msg = null;
        Exception exception = null;
        try {
            siteArgMap = prepareIscDataRec(origArgs);
        } catch (IOException e) {
            exception = e;
            msg = "Error reading received XML file";
        } catch (InterruptedException e) {
            exception = e;
            msg = "Error copying received ISC file";
        } catch (GfeConfigurationException e) {
            exception = e;
            msg = "Error getting GFE configuration";
        } catch (SAXException | ParserConfigurationException e) {
            exception = e;
            msg = "Error parsing received XML file.";
        }

        if (msg == null) {
            for (Entry<String, String[]> entry : siteArgMap.entrySet()) {
                IscDataRecRequest req = new IscDataRecRequest();
                req.setSiteID(entry.getKey());
                req.setArgString(String.join(" ", entry.getValue()));
                req.setWorkstationID(request.getWorkstationID());
                byte[] message = SerializationUtil.transformToThrift(req);
                EDEXUtil.getMessageProducer().sendAsync("iscReceiveRoute",
                        message);
            }
        } else {
            statusHandler.error(msg, exception);
            sr.addMessage(msg);
        }

        return sr;
    }

    private Map<String, String[]> prepareIscDataRec(String[] args)
            throws IOException, InterruptedException, GfeConfigurationException,
            SAXException, ParserConfigurationException {
        Map<String, String[]> siteMap = new HashMap<>();

        String[] incomingFiles = args[2].split(",");
        String xmlPathString = (incomingFiles.length == 1) ? incomingFiles[0]
                : incomingFiles[1];
        String dataPathString = (incomingFiles.length == 1) ? null
                : incomingFiles[0];
        Path xmlFilePath = Paths.get(xmlPathString);
        Path dataFilePath = (dataPathString != null) ? Paths.get(dataPathString)
                : null;

        try {
            if (!isSafePathToProcess(xmlFilePath)) {
                statusHandler.warn(String.format(UNSAFE_PATH_MSG, xmlFilePath));
                return Collections.emptyMap();
            }

            if ((dataFilePath != null)
                    && (!isSafePathToProcess(dataFilePath))) {
                statusHandler
                        .warn(String.format(UNSAFE_PATH_MSG, dataFilePath));
                return Collections.emptyMap();
            }

            String xmlFileName = xmlFilePath.getFileName().toString();
            String dataFileName = (dataPathString != null)
                    ? dataFilePath.getFileName().toString() : null;

            Collection<String> destinations = getXMLDestinations(xmlFilePath);
            Set<String> activeSites = IFPServer.getActiveSites();
            Set<String> activeDestinations = new HashSet<>(activeSites);
            activeDestinations.retainAll(destinations);

            for (String siteId : activeDestinations) {
                IFPServer ifpServer = IFPServer.getActiveServer(siteId);
                if (ifpServer != null) {
                    if (ifpServer.getConfig().requestISC()) {
                        String[] modifiedArgs = new String[args.length];
                        System.arraycopy(args, 0, modifiedArgs, 0, args.length);

                        if (dataFilePath != null) {
                            String newDataFileName = dataFileName + "."
                                    + siteId;
                            Path newDataFilePath = dataFilePath
                                    .resolveSibling(newDataFileName);

                            try {
                                Files.copy(dataFilePath, newDataFilePath,
                                        StandardCopyOption.REPLACE_EXISTING);
                            } catch (IOException e) {
                                statusHandler.error(String.format(
                                        COPY_ERROR_MSG, dataFilePath,
                                        newDataFilePath, siteId), e);
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
