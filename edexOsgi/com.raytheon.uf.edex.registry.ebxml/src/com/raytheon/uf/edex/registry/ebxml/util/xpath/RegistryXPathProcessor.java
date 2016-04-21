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
package com.raytheon.uf.edex.registry.ebxml.util.xpath;

import java.io.ByteArrayInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.StringWriter;
import java.util.Collections;
import java.util.HashSet;
import java.util.Set;

import javax.xml.bind.JAXBElement;
import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.DocumentBuilderFactory;
import javax.xml.parsers.ParserConfigurationException;
import javax.xml.transform.Transformer;
import javax.xml.transform.TransformerConfigurationException;
import javax.xml.transform.TransformerFactory;
import javax.xml.transform.dom.DOMSource;
import javax.xml.transform.stream.StreamResult;
import javax.xml.xpath.XPath;
import javax.xml.xpath.XPathConstants;
import javax.xml.xpath.XPathExpressionException;
import javax.xml.xpath.XPathFactory;

import oasis.names.tc.ebxml.regrep.wsdl.registry.services.v4.MsgRegistryException;

import org.w3c.dom.Attr;
import org.w3c.dom.Document;
import org.w3c.dom.Node;
import org.w3c.dom.NodeList;

import com.raytheon.uf.common.registry.RegistryJaxbManager;
import com.raytheon.uf.common.registry.RegistryNamespaceMapper;
import com.raytheon.uf.edex.registry.ebxml.exception.EbxmlRegistryException;
import com.raytheon.uf.edex.registry.ebxml.services.lifecycle.LifecycleManagerImpl;
import com.raytheon.uf.edex.registry.ebxml.util.EbxmlExceptionUtil;

/**
 * 
 * Xpath expression processor used by the registry for updating objects.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#     Engineer    Description
 * ------------ ----------  ----------- --------------------------
 * 8/8/2013     1692        bphillip    Initial implementation
 * 06/02/2015   4499        dhladky     Security comment on xpath requests.
 * </pre>
 * 
 * @author bphillip
 * @version 1
 */
public class RegistryXPathProcessor {

    /** Attributes that cannot be updated */
    private static final Set<String> PROHIBITED_ATTRIBUTES = initializeProhibitedAttributes();

    /** The DOM document builder */
    private DocumentBuilder documentBuilder;

    /** Transformer used to transform the DOM document back to XML */
    private Transformer transformer;

    /** Jaxb manager used to convert objects to and from XML */
    private RegistryJaxbManager jaxbManager;

    /** The XPath processor used for evaluating XPath expressions */
    private XPath xpath;

    /**
     * Creates a new RegistryXPathProcessor
     */
    protected RegistryXPathProcessor() {

    }

    /**
     * Creates a new RegistryXPathProcessor
     * 
     * @param jaxbManager
     *            The jaxb manager used for marshalling and unmarshalling
     *            objects
     * @param registryNamespaceMapper
     *            The namespace mapper used for correctly mapping namespaces in
     *            the registry object schemas
     * @throws ParserConfigurationException
     *             If errors occur while initializing the DOM Document parser
     * @throws TransformerConfigurationException
     *             If errors occur while initializing the DOM transformer
     */
    public RegistryXPathProcessor(RegistryJaxbManager jaxbManager,
            RegistryNamespaceMapper registryNamespaceMapper)
            throws ParserConfigurationException,
            TransformerConfigurationException {
        this.jaxbManager = jaxbManager;
        DocumentBuilderFactory factory = DocumentBuilderFactory.newInstance();
        factory.setNamespaceAware(true);
        documentBuilder = factory.newDocumentBuilder();
        transformer = TransformerFactory.newInstance().newTransformer();
        xpath = XPathFactory.newInstance().newXPath();
        xpath.setNamespaceContext(registryNamespaceMapper);
    }

    /**
     * Inserts an object into the object provided
     * 
     * @param obj
     *            The object to insert into
     * @param xpathExpression
     *            The xpath expression used to select where to insert the new
     *            object
     * @param addNode
     *            The object to be added to the target object
     * @return The updated object
     * @throws EbxmlRegistryException
     *             If errors occur while converting the target object to and
     *             from DOM
     * @throws MsgRegistryException
     */
    public Object insert(Object obj, String xpathExpression, Object addNode)
            throws EbxmlRegistryException, MsgRegistryException {
        // Convert the object to a DOM document
        Document domDocument = toDom(obj);

        // Evaluates the XPATH Expression using the DOM document
        NodeList nodeList = evaluateXpathExpression(domDocument,
                xpathExpression);

        // Iterate over the selected nodes and add the new node to all
        // applicable locations
        for (int i = 0; i < nodeList.getLength(); i++) {
            Node currentNode = nodeList.item(i);
            checkForProhibitedUpdates(currentNode);
            Node newNode = toDom(addNode).getDocumentElement();
            Node importedNode = domDocument.importNode(newNode, true);
            currentNode.appendChild(importedNode);

        }
        // Converts the updated object back from DOM
        return fromDom(domDocument);
    }

    /**
     * Updates an object specified by the xpath expression
     * 
     * @param obj
     *            The object to be updated
     * @param xpathExpression
     *            The xpath expression used to select which node to update
     * @param updateNode
     *            The value to update the selected nodes to
     * @return The updated object
     * @throws EbxmlRegistryException
     *             If errors occur while converting the target object to and
     *             from DOM
     * @throws MsgRegistryException
     */
    public Object update(Object obj, String xpathExpression, Object updateNode)
            throws EbxmlRegistryException, MsgRegistryException {
        // Convert the object to a DOM document
        Document domDocument = toDom(obj);

        // Evaluates the XPATH Expression using the DOM document
        NodeList nodeList = evaluateXpathExpression(domDocument,
                xpathExpression);

        // Iterate over the selected nodes and update the selected nodes
        for (int i = 0; i < nodeList.getLength(); i++) {
            Node currentNode = nodeList.item(i);

            if (currentNode.getNodeType() == Node.ATTRIBUTE_NODE) {
                checkForProhibitedUpdates(currentNode);
                currentNode.setNodeValue(String.valueOf(updateNode));

            } else {
                currentNode.getParentNode().replaceChild(
                        domDocument.importNode(toDom(updateNode)
                                .getDocumentElement(), true), currentNode);
            }
        }
        // Converts the updated object back from DOM
        return fromDom(domDocument);
    }

    /**
     * Deletes the nodes specified by the given xpath expression on the target
     * object
     * 
     * @param obj
     *            The object delete from
     * @param xpathExpression
     *            The xpath expression used to select which nodes to delete
     * @return The updated object
     * @throws EbxmlRegistryException
     *             If errors occur while converting the target object to and
     *             from DOM
     * @throws MsgRegistryException
     */
    public Object delete(Object obj, String xpathExpression)
            throws EbxmlRegistryException, MsgRegistryException {
        Document domDocument = toDom(obj);

        // Evaluates the XPATH Expression using the DOM document
        NodeList nodeList = evaluateXpathExpression(domDocument,
                xpathExpression);

        for (int i = 0; i < nodeList.getLength(); i++) {
            Node currentNode = nodeList.item(i);
            if (currentNode.getNodeType() == Node.ATTRIBUTE_NODE) {
                Node pNode = ((Attr) currentNode).getOwnerElement();
                checkForProhibitedUpdates(currentNode);
                pNode.getAttributes()
                        .removeNamedItem(currentNode.getNodeName());

            } else {
                currentNode.getParentNode().removeChild(currentNode);
            }
        }
        return fromDom(domDocument);
    }

    /**
     * Check the security blacklist of nodes not allowed for update.
     * 
     * @param node
     * @throws MsgRegistryException
     */
    private void checkForProhibitedUpdates(Node node)
            throws MsgRegistryException {
        String nodeName = node.getNodeName();
        if (PROHIBITED_ATTRIBUTES.contains(nodeName)) {
            throw EbxmlExceptionUtil
                    .createInvalidRequestExceptionType(
                            "Attempt to update prohibited attribute: "
                                    + nodeName,
                            "Cannot update the following attributes: id,lid,objectType");
        }
    }

    /**
     * Method used to evaluate XPATH expressions on a DOM Document
     * 
     * @param document
     *            The DOM document to apply the xpath expression to
     * @param xpathExpression
     *            The XPATH expression to evaluate
     * @return The nodes selected from the XPATH evaluation
     * @throws MsgRegistryException
     *             If there are syntax errors or errors evaluating the xpath
     *             expression
     */
    private NodeList evaluateXpathExpression(Document document,
            String xpathExpression) throws MsgRegistryException {
        /*
         * Uses the first child of the document which should be the
         * RegistryObject tag. This is so relative xpath queries will be based
         * off the RegistryObjectTag not the root
         */
        NodeList list = null;
        xpath.reset();

        try {
            /********************* Security Comment ***********************
             * The entire xpathExpression was provided by the user and the 
             * document is user stored content, not a system level document. 
             * The authenticated user has full CRUD operation access to the document. 
             * XPath injection is not considered to be a security risk in this scenario
             * since the query was provided in entirety by the user and the query
             * is running against their own previously stored document.
             */
            Node firstNode = document.getFirstChild();
            list = (NodeList) xpath.evaluate(xpathExpression, firstNode,
                    XPathConstants.NODESET);

        } catch (XPathExpressionException e) {
            throw EbxmlExceptionUtil.createMsgRegistryException(
                    LifecycleManagerImpl.UPDATE_OBJECTS_ERROR_MSG,
                    "Invalid Expression Syntax Exception",
                    new EbxmlRegistryException(
                            "Error evaluating XPath expression", e));
        }

        return list;
    }

    /**
     * Converts an object to a DOM Document
     * 
     * @param registryObject
     *            The object to convert
     * @return The DOM Document representation of the object
     * @throws EbxmlRegistryException
     *             If errors occur while converting the object
     */
    private Document toDom(Object registryObject) throws EbxmlRegistryException {
        Document domDocument = null;
        InputStream stream = null;
        try {
            stream = new ByteArrayInputStream(jaxbManager.marshalToXml(
                    registryObject).getBytes("UTF-8"));
            documentBuilder.reset();
            domDocument = documentBuilder.parse(stream);
        } catch (Exception e) {
            throw new EbxmlRegistryException("Error parsing object to DOM", e);
        } finally {
            if (stream != null) {
                try {
                    stream.close();
                } catch (IOException e) {
                    throw new EbxmlRegistryException(
                            "Error closing input stream!", e);
                }
            }
        }
        return domDocument;
    }

    /**
     * Converts a DOM Document back into an object
     * 
     * @param doc
     *            The DOM Document to convert
     * @return The converted object
     * @throws EbxmlRegistryException
     *             If errors occur while converting the object
     */
    private Object fromDom(Document doc) throws EbxmlRegistryException {
        StringWriter writer = new StringWriter();
        StreamResult result = new StreamResult(writer);
        try {
            transformer.reset();
            transformer.transform(new DOMSource(doc), result);

            return ((JAXBElement<?>) jaxbManager.unmarshalFromXml(writer
                    .toString())).getValue();
        } catch (Exception e) {
            throw new EbxmlRegistryException(
                    "Error converting object from dom", e);
        }
    }

    /**
     * Initializes the set of attributes that cannot be modified
     * 
     * @return Unmodifiable view of the prohibited attribute set
     */
    private static Set<String> initializeProhibitedAttributes() {
        Set<String> prohibitedAttributeSet = new HashSet<String>();
        prohibitedAttributeSet.add("id");
        prohibitedAttributeSet.add("lid");
        prohibitedAttributeSet.add("objectType");
        return Collections.unmodifiableSet(prohibitedAttributeSet);
    }
}
