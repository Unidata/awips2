package com.raytheon.uf.edex.registry.acp.xacml.util;

import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.io.InputStream;
import java.io.StringReader;
import java.util.Scanner;

import javax.xml.validation.Schema;

import org.opensaml.DefaultBootstrap;
import org.opensaml.xacml.XACMLObject;
import org.opensaml.xacml.ctx.RequestType;
import org.opensaml.xacml.ctx.ResponseType;
import org.opensaml.xacml.policy.PolicySetType;
import org.opensaml.xacml.policy.PolicyType;
import org.opensaml.xml.Configuration;
import org.opensaml.xml.ConfigurationException;
import org.opensaml.xml.io.Unmarshaller;
import org.opensaml.xml.io.UnmarshallerFactory;
import org.opensaml.xml.io.UnmarshallingException;
import org.opensaml.xml.parse.BasicParserPool;
import org.w3c.dom.Document;
import org.w3c.dom.Element;

import com.raytheon.uf.edex.registry.ebxml.exception.EbxmlRegistryException;

public class XACMLParser {

    private static XACMLParser instance = new XACMLParser();

    private BasicParserPool parserPool;

    private Schema schema;

    public static XACMLParser getInstance() {
        return instance;
    }

    private XACMLParser() {
        // Initialize the library
        try {
            DefaultBootstrap.bootstrap();
        } catch (ConfigurationException e) {
            e.printStackTrace();
        }
        parserPool = new BasicParserPool();
        parserPool.setNamespaceAware(true);
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
            domDocument = parserPool.parse(in);
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
            domDocument = parserPool.parse(reader);
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
        UnmarshallerFactory unmarshallerFactory = Configuration
                .getUnmarshallerFactory();
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
