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
package com.raytheon.uf.edex.registry.acp.xacml.util;

import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.io.InputStream;
import java.io.StringReader;
import java.util.Scanner;

import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.DocumentBuilderFactory;
import javax.xml.validation.Schema;

import org.opensaml.core.config.ConfigurationService;
import org.opensaml.core.xml.config.XMLObjectProviderRegistry;
import org.opensaml.core.xml.io.Unmarshaller;
import org.opensaml.core.xml.io.UnmarshallerFactory;
import org.opensaml.core.xml.io.UnmarshallingException;
import org.opensaml.xacml.XACMLObject;
import org.opensaml.xacml.ctx.RequestType;
import org.opensaml.xacml.ctx.ResponseType;
import org.opensaml.xacml.policy.PolicySetType;
import org.opensaml.xacml.policy.PolicyType;
import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.xml.sax.InputSource;

import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.edex.registry.ebxml.exception.EbxmlRegistryException;

/**
 * 
 * Parser for XACML objects.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#     Engineer    Description
 * ------------ ----------  ----------- --------------------------
 * ??/??/????   ???         bphillip    Initial Coding
 * 2/25/2016    5380        tjensen     Update to support newer FOSS versions
 * 6/21/2016    5698        dhladky     Removed initializer service.  Not needed.
 * </pre>
 * 
 * @author bphillip
 * @version 1
 */

public class XACMLParser {

    /** The logger */
    private static final IUFStatusHandler statusHandler = UFStatus
            .getHandler(XACMLParser.class);

    private static XACMLParser instance = null;

    private DocumentBuilder builder;

    private Schema schema;

    public static XACMLParser getInstance() throws EbxmlRegistryException {
        if (instance == null) {
            instance = new XACMLParser();
        }
        return instance;
    }

    private XACMLParser() throws EbxmlRegistryException {
        // Initialize the doc builder factory
        try {
            DocumentBuilderFactory factory = DocumentBuilderFactory
                    .newInstance();
            factory.setNamespaceAware(true);
            builder = factory.newDocumentBuilder();
        } catch (Exception e) {
            statusHandler.error("Unable to initialize XACMLParser.", e);
            throw new EbxmlRegistryException(e);
        }
    }

    public Schema getSchema() {
        return schema;
    }

    public PolicyType unmarshalPolicy(String fileName)
            throws EbxmlRegistryException {
        return (PolicyType) unmarshalXacmlObject(fileName);
    }

    public PolicySetType unmarshalPolicySet(String fileName)
            throws EbxmlRegistryException {
        return (PolicySetType) unmarshalXacmlObject(fileName);
    }

    public RequestType unmarshalRequest(String fileName)
            throws EbxmlRegistryException {
        return (RequestType) unmarshalXacmlObject(fileName);
    }

    public ResponseType unmarshalResponse(String fileName)
            throws EbxmlRegistryException {
        return (ResponseType) unmarshalXacmlObject(fileName);
    }

    public XACMLObject unmarshalXacmlObject(String fileName)
            throws EbxmlRegistryException {
        return unmarshalXacmlObject(new File(fileName));
    }

    public XACMLObject unmarshalXacmlObject(File file)
            throws EbxmlRegistryException {
        return parseDomDocument(getDOM(file));
    }

    public Document getDOM(File file) throws EbxmlRegistryException {
        InputStream in = null;
        Document domDocument = null;
        try {
            in = new FileInputStream(file);
            domDocument = builder.parse(in);
        } catch (Exception e) {
            throw new EbxmlRegistryException("Error parsing XACML Document", e);
        } finally {
            if (in != null) {
                try {
                    in.close();
                } catch (IOException e) {
                    throw new EbxmlRegistryException(
                            "Error parsing XACML Document", e);
                }
            }
        }
        return domDocument;
    }

    public Document getDOM(String fileName) throws EbxmlRegistryException {
        return getDOM(new File(fileName));
    }

    public Object unmarshallXacmlObjectFromText(String text)
            throws EbxmlRegistryException {
        StringReader reader = null;
        Document domDocument = null;
        try {
            reader = new StringReader(text);
            domDocument = builder.parse(new InputSource(reader));
        } catch (Exception e) {
            throw new EbxmlRegistryException("Error parsing XACML Document", e);
        } finally {
            if (reader != null) {
                reader.close();
            }
        }
        return parseDomDocument(domDocument);
    }

    private XACMLObject parseDomDocument(Document domDocument)
            throws EbxmlRegistryException {
        Element root = domDocument.getDocumentElement();
        UnmarshallerFactory unmarshallerFactory = ConfigurationService.get(
                XMLObjectProviderRegistry.class).getUnmarshallerFactory();
        Unmarshaller unmarshaller = unmarshallerFactory.getUnmarshaller(root);
        XACMLObject object = null;
        try {
            object = (XACMLObject) unmarshaller.unmarshall(root);
        } catch (UnmarshallingException e) {
            throw new EbxmlRegistryException(
                    "Error unmarshalling XACML document", e);
        }
        return object;
    }

    public String readFileContents(String fileName)
            throws EbxmlRegistryException {
        return readFileContents(new File(fileName));
    }

    public String readFileContents(File file) throws EbxmlRegistryException {
        StringBuilder fileContents = new StringBuilder();
        String NEW_LINE = System.getProperty("line.separator");
        Scanner scanner;
        try {
            scanner = new Scanner(new FileInputStream(file));
        } catch (FileNotFoundException e) {
            throw new EbxmlRegistryException("Error reading contents of file!",
                    e);
        }
        try {
            while (scanner.hasNextLine()) {
                fileContents.append(scanner.nextLine() + NEW_LINE);
            }
        } catch (RuntimeException e) {
            throw new EbxmlRegistryException("Error scanning XACML document.",
                    e);
        } finally {
            scanner.close();
        }
        return fileContents.toString();
    }

}
