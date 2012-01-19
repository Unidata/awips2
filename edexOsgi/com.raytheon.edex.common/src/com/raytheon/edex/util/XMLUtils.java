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

/**
 * 
 */
package com.raytheon.edex.util;

import java.io.StringReader;
import java.io.StringWriter;
import java.util.ArrayList;
import java.util.Enumeration;
import java.util.Hashtable;
import java.util.List;
import java.util.Map;

import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.DocumentBuilderFactory;
import javax.xml.transform.Transformer;
import javax.xml.transform.TransformerFactory;
import javax.xml.transform.dom.DOMSource;
import javax.xml.transform.stream.StreamResult;

import org.w3c.dom.Attr;
import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.w3c.dom.Node;
import org.w3c.dom.NodeList;
import org.xml.sax.InputSource;

import com.sun.org.apache.xerces.internal.jaxp.DocumentBuilderFactoryImpl;

/**
 * Contains static methods for manipulating XML. Methods are included
 * to allow parsing, extracting, transforming and building XML strings.
 * All methods are static to allow for library style calls. All methods
 * throw an Exception when errors occur. The caller can check the actual
 * exception class to determine the type of excption that occurred.
 * <pre>
 * SOFTWARE HISTORY
 *
 * Date             PR#             Engineer            Description
 * -----------      ----------      ------------        --------------------------
 * 09Aug2006                        MW Fegan            Initial Creation
 * 09Aug2006        Task 19         MW Fegan            Added method to check for
 *                                                       a tag in a document.
 * </PRE>
 * @author mfegan
 *
 */
public final class XMLUtils {
    private static final String PROPERTY = "property";
    public static final String QUERY = "query";
    public static final String NAME = "name";
    public static final String VALUE = "value";
    public static final String TERMQUERY_TAG = "termQuery";
    public static final String QUERY_TAG = "query";
    public static final String PROPERTY_URI_KEY = "uri";

    /**
     * Determines if the specified document contains the requested tag.
     * 
     * @param document the document to check
     * @param tagName the name of the tag to check
     * 
     * @return true if the document has the specified tag
     */
    public static boolean checkForTag(Document document,
                                      String tagName) 
    throws Exception {
        return document.getElementsByTagName(tagName).getLength() != 0;
    }
    /**
     * Creates a generic XML message by wrapping the message XML around the
     * specified contents. The final form of the message is
     * <PRE>
     *    {@literal <message> }
     *       {@literal <header> }
     *          {@literal <!-- properties specifies inserted here --> }
     *          {@literal <property name="<<name>>" value="<<value>>" /> }
     *       {@literal </header> }
     *       {@literal <body> }
     *           {@literal <!-- <<contents>> inserted here --> }
     *       {@literal </body> }
     *    {@literal </message> }
     * </PRE>
     * The <code>properties</code> argument contains a mapping of property
     * names to values and has the following structure:
     * <PRE>
     *    properties
     *          |---name1=value1
     *          |---name2=value2
     *          | ...
     *          |---namen=valuen 
     * </PRE>
     * For example, if the <code>properties</code> argument contains
     * <PRE>
     *    properties
     *          |---id="Test Script"
     *          |---time="20060809122800"
     *          |---function="validate"
     * </PRE>
     * and the <code>contents</code> argument contains
     * <PRE>
     *    {@literal <action name=""TestScript> }
     *       {@literal <jython> }
     *          {@literal <![CDATA[ }
     *          {@literal print "Hello EDEX World\n" }
     *          {@literal ]]> }
     *       {@literal </jython> }
     *    {@literal </action> }
     * </PRE>
     * the message created is
     * <pre>
     *    {@literal <message> }
     *       {@literal <header> }
     *          {@literal <property name="id" value="Test Script" /> }
     *          {@literal <property name="time" value="20060809122800" /> }
     *          {@literal <property name="function" value="validate" /> }
     *       {@literal </header> }
     *       {@literal <body> }
     *          {@literal <action name="TestScript"> }
     *             {@literal <jython> }
     *                {@literal <![CDATA[ }
     *                {@literal print "Hello EDEX World\n" }
     *                {@literal ]]> }
     *             {@literal </jython> }
     *          {@literal </action> }
     *       {@literal </body> }
     *    {@literal </message> }
     * </PRE>
     * <B>Note:</B> use {@link com.raytheon.edex.util.XMLUtils#transformXMLDocument(org.w3c.dom.Document) } to transform the
     * DOM to an XML string.
     * 
     * @param contents the contents of the message body
     * @param properties contains the properties
     * @return the completed message
     * 
     * @throws Exception if any error occurred
     * 
     * @see com.raytheon.edex.util.XMLUtils#transformXMLDocument(org.w3c.dom.Document)
     */
    public static Document createXMLMessage(Document contents,
                                            Hashtable<String,String> properties) 
    throws Exception {
        Document document = createNewDocument();
        /*
         * create the message, header and body tags
         */
        Node message = document.createElement("message");
        Node header = document.createElement("header");
        Node body = document.createElement("body");
        /*
         * add the header properties
         */
        Enumeration<String> keys = properties.keys();
        while (keys.hasMoreElements()) {
            String key = keys.nextElement();
            header.appendChild((Node)makeProperty(document,key,properties.get(key)));
        }
        /*
         * add the contents to the body
         */
        body.appendChild(document.importNode(contents.getFirstChild(),true));
        /*
         * add the header and body to the message
         * and the message to the document.
         */
        message.appendChild(header);
        message.appendChild(body);
        document.appendChild(message);
        /*
         * return the document
         */
        return document;
    }
    /**
     * Creates a canonical XML message document. The body of the message contains the tags
     * contained in {@code contents}. The message header properties are contained in 
     * {@code properties}.
     * 
     * @param properties contains the message header properties.
     * @param contents contains the message body contents.
     * 
     * @return the complete canonical message as a document.
     *  
     * @throws Exception if any error occurs.
     */
    public static Document createXMLMessage(Hashtable<String,String> properties,
                                            String[] contents) 
    throws Exception {
        Document content = scanXMLtoDOM((String)contents[0]);
        Document document = createXMLMessage(content,properties);
        for (int i = 1; i < contents.length; i++) {
            if (contents[i] != null) {
                Document result = scanXMLtoDOM((String)contents[i]);
                Node body = document.getElementsByTagName("body").item(0);
                body.appendChild(document.importNode(result.getFirstChild(),true));
            }
        }
        return document;
    }
    /**
     * Creates an ANT style property tag with the specified name and
     * value. For example, if <code>name</code> is "function" and 
     * <code>value</code> is "validate", then the property tag created
     * is
     * <PRE>
     *    {@literal <property name="function" value="validate" /> }
     * </PRE>
     * @param document the document that is to contain the property tag
     * @param name the name for the property
     * @param value the value for the property
     * 
     * @return the newly created property tag
     */
    private static Element makeProperty(Document document,
                                        String name, 
                                        String value) {
        Element property = document.createElement(PROPERTY);
        property.setAttribute(NAME, name);
        property.setAttribute(VALUE, value);
        return property;
    }
    
    /**
     * Extracts a sub tree from a document. The subdocument consists
     * of the specified tag and its children. For example, if
     * <code>document</code> contains
     * <PRE>
     *    {@literal <action name="TestScript"> }
     *       {@literal <jython> }
     *          {@literal <![CDATA[ }
     *          {@literal print "Hello EDEX World\n" }
     *          {@literal ]]> }
     *       {@literal </jython> }
     *    {@literal </action> }
     * </PRE>
     * and <code>tagName</code> contains "jython", the subdocument returned
     * contains
     * <PRE>
     *    {@literal <jython> }
     *       {@literal <![CDATA[ }
     *       {@literal print "Hello EDEX World\n" }
     *       {@literal ]]> }
     *    {@literal </jython> }
     * </PRE>
     * <DL>
     * <DT><B>Limitation</B>
     * <DD>If multiple occurrances of the tag are present, only the
     *     first occurrance is returned.
     * </DL>
     * 
     * @param document the document containing the subdocument
     * @param tagName the name of the tag to start the subdocument
     * @return the subdocument
     * 
     * @throws Exception if any error occurred
     */
    public static Document getSubDocument(Document document,
                                          String tagName) 
    throws Exception {
        Document subDoc = createNewDocument();
        Node head = document.getElementsByTagName(tagName).item(0);
        subDoc.appendChild(subDoc.importNode(head,true));
        return subDoc;
    }
    /**
     * Extracts all occurances of a perticular sub tree from a document.
     * The subdocument consists of the specified tag and its children. For
     * example, if <code>document</code> contains
     * <PRE>
     *    {@literal <action name="TestScript"> }
     *          {@literal <fileIn using="a" />}
     *          {@literal <fileIn using="a" />}
     *          {@literal <fileIn using="a" />}
     *    {@literal </action> }
     * </PRE>
     * and <code>tagName</code> contains "fileIn", the subdocument list returned
     * contains the three "fileIn" tags.
     * @param document the document containing the subdocument
     * @param tagName the name of the tag to start the subdocument
     * 
     * @return the subdocuments
     * 
     * @throws Exception if any error occurred
     */
    public static List<Document> getAllSubDocumentsByTag(Document document, 
                                                         String tagName) 
    throws Exception {
        List<Document> retVal = new ArrayList<Document>();
        NodeList nodes = document.getElementsByTagName(tagName);
        for (int counter = 0; counter < nodes.getLength(); counter++) {
            Document subDoc = createNewDocument();
            subDoc.appendChild(subDoc.importNode(nodes.item(counter),true));
            retVal.add(subDoc);
        }
        return retVal;
    }
    /**
     * Removes a tag and its contents from the document. If the tag exists
     * more than once in the document, only the first instance is removed.
     * 
     * @param document the original document.
     * @param tagName the name of the tag to remove.
     * 
     * @return the modified document.
     * 
     * @throws Exception if an error occurs.
     */
    public static Document removeSubDocument(Document document, 
                                             String tagName) 
    throws Exception {
        Node node = document.getElementsByTagName(tagName).item(0);
        Node parent = node.getParentNode();
        parent.removeChild(node);
        return document;            
    }
    /**
     * Removes all instances of the tag and it's contents from the document.
     * <B>Caution<B> if multiple instances of the tag exist, all are removed.
     * 
     * @param document the original document.
     * @param tagName the name of the tag to remove.
     * 
     * @return the modified document.
     * 
     * @throws Exception if an error occurs.
     */
    public static Document removeSubDocumentAll(Document document, 
                                                String tagName)
    throws Exception {
        NodeList nodes = document.getElementsByTagName(tagName);
        while (nodes.getLength() > 0) {
            Node node = nodes.item(0);
            Node parent = node.getParentNode();
            parent.removeChild(node);            
        }
//        for (int i = nodes.getLength() - 1; i > -1; i--) {
//            Node node = nodes.item(i);
//            Node parent = node.getParentNode();
//            parent.removeChild(node);
//        }
        return document;
    }
    /**
     * Extracts the value of a {@code <property /> } tag from a document. The property
     * tag is similar to ANT's {@code <property /> } tag. For example, if the document
     * contains
     * <P>
     *    {@code <property name="function" value="execute" /> }
     * <P>
     * calling {@code getPropertyFromXML(document,"function") } returns value
     * {@code execute }.
     * <P>
     * <DT><B>Limitation</B>
     * <DD>If multiple occurrances of the same named property are present, only the
     *     first occurrance is returned.
     * </DL>
     * 
     * @param document the document containing the property tag
     * @param property the "name" of the property to extract
     * @return the value of the property
     * @throws Exception if any error occurred
     */
    public static String getPropertyFromXML(Document document,
                                            String property) 
    throws Exception {
        NodeList nodes = document.getElementsByTagName(PROPERTY);
        String retVal = null;
        for (int i = 0; i < nodes.getLength(); i++) {
            Node node = nodes.item(i);
            Node name = node.getAttributes().getNamedItem(NAME);
            if (name.getNodeValue().equals(property)) {
                Node value = node.getAttributes().getNamedItem(VALUE);
                retVal = value.getNodeValue();
            }
        }
        return retVal;
    }
    /**
     * Sets an attribute on an XML tag in a document. If the attribute
     * already exists, it is given a new value, otherwise it is created
     * with the specified value.
     * 
     * @param document the document containing the tag
     * @param tagName the name of the tag to modify
     * @param attrName the name of the attribute to add or modify.
     * @param value the new value for the attribute
     * 
     * @return true if an attribute was set
     * 
     * @throws Exception if any error occurred.
     */
    public static boolean setValueIntoXML(Document document, 
                                          String tagName,
                                          String attrName,
                                          String value)
    throws Exception {
        boolean retval = false;
        NodeList nodes = document.getElementsByTagName(tagName);
        for (int i = 0; i < nodes.getLength(); i++) {
            Node node = nodes.item(i);
            Node attribute = node.getAttributes().getNamedItem(attrName);
            if (attribute != null) {
                /*
                 * modify the attribute
                 */
                attribute.setNodeValue(value);
            } else {
                /*
                 * create a new attribute
                 */
                Attr attr = document.createAttribute(attrName);
                attr.setValue(value);
                node.getAttributes().setNamedItem(attr);
            }
            retval =  true;
        }
        return retval;
    }
    /**
     * Creates a new tag in the specified {@code document}. The specified
     * {@code parent} is used as the parent node. The tag name and contents
     * are specified by {@code tagName} and {@code tagValue}. The tag that
     * is created has no attributes. To add attributes, use 
     * {@link #setValueIntoXML(Document, String, String, String)}.
     * <P>
     * <DT><B>Limitation</B>
     * <DD>If multiple occurrances of the same named {@code parent} are 
     *     present, the tag is added to the first {@code parent}.
     * </DL>
     * 
     * @param document the document to get the new tag
     * @param parent the parent of the new tag
     * @param tagName the name of the new tag
     * @param tagValue the contents of the new tag
     * 
     * @return the newly created tag as a node
     *  
     * @throws Exception if any problems were encountered
     */
    public static Node setTagIntoXML(Document document,
                                     String parent, 
                                     String tagName, 
                                     String tagValue) 
    throws Exception {
        NodeList nodes = document.getElementsByTagName(parent);
        Node parentNode = nodes.item(0);
        Node newNode = document.createElement(tagName);
        Node textNode = document.createTextNode(tagValue);
        newNode.appendChild(textNode);
        parentNode.appendChild(newNode);
        return newNode;
    }
    /**
     * Extracts the value for a named tag from a document. This tag structure
     * is similar to Ant's {@code <property />} tag. For example, if the document
     * contains
     * <P>
     *    {@code <query name="function" value="execute" /> }
     * <P>
     * calling {@code getPropertyFromXML(document,"function") } returns value
     * {@code execute }.
     * <P>
     * <DT><B>Limitation</B>
     * <DD>If multiple occurrances of the same named property are present, only the
     *     first occurrance is returned.
     * </DL>
     * 
     * @param document the document containing the tag
     * @param tagName the name of the tag of interest
     * @param name the name attribute of the tag
     * 
     * @return the value attribute of the tag
     * 
     * @throws Exception if any error orrucced
     */
    public static String getValueFromXMLTag(Document document, 
                                            String tagName,
                                            String name) 
    throws Exception {
        NodeList nodes = document.getElementsByTagName(tagName);
        String retVal = null;
        for (int i = 0; i < nodes.getLength(); i++) {
            Node node = nodes.item(i);
            Node attribute = node.getAttributes().getNamedItem(NAME);
            if (attribute.getNodeValue().equals(name)) {
                Node value = node.getAttributes().getNamedItem(VALUE);
                retVal = value.getNodeValue();
                break;
            }
        }
        return retVal;
    }
    /**
     * Adds the specified child tag a tag of a document. 
     * <P>
     * <B>Caution:</B> If the tag occurs more than once, the child is added to
     * only the first instance of the tag.
     * 
     * @param document the document containing the tag
     * @param tagName the name of the tag
     * @param childName the name of the child tag
     * @param attributes the attributes for the child tag
     * 
     * @throws Exception when an error occurs 
     */
    public static void addChildToTag(Document document, 
                                     String tagName, 
                                     String childName,
                                     Map<String,String> attributes) 
    throws Exception {
        Element child = document.createElement(childName);
        for (String key : attributes.keySet()) {
            String value = attributes.get(key);
            child.setAttribute(key, value);
        }
        document.getElementsByTagName(tagName).item(0).appendChild(child);
    }
    /**
     * Retrieves the value of the specified attributefrom a tag in the specified document.
     *  
     * @param document the document containing the tag
     * @param tagName the name of the tag of interest
     * @param name the name of the attribute of the tag
     * 
     * @return the value attribute of the tag
     * 
     * @throws Exception if any error orrucced
     */
    public static String getAttributeValueFromTag(Document document,
                                                  String tagName,
                                                  String name) 
    throws Exception {
        NodeList nodes = document.getElementsByTagName(tagName);
        if(nodes == null) {
            throw new Exception("Unable to find tag " + Util.printString(tagName));
        }
        Node node = nodes.item(0);
        Node attribute = node.getAttributes().getNamedItem(name);
        if (attribute == null) {
            throw new Exception("Unable to find attribute " + Util.printString(name) +
                                " for tag " + Util.printString(tagName));
        }
        String retVal = attribute.getNodeValue();
        return retVal;
    }
    /**
     * Converts a string containing XML into a DOM Document object.
     * This is basically a convenience method that hides some of the
     * implementation details.
     *  
     * @param xml the string containing the XML
     * @return the XML as a dcoument
     * @throws Exception if any error occurred
     */
    public static Document scanXMLtoDOM(String xml) 
    throws Exception {
        Document document = null;
        DocumentBuilderFactory dbf = new DocumentBuilderFactoryImpl();
        DocumentBuilder db = dbf.newDocumentBuilder();
        document = db.parse(new InputSource(new StringReader(xml)));
        return document;
    }

    /**
     * Transforms a {@code Document } containing XML into a String.
     * This is a convenience method that hides some of the details
     * required for the transformation.
     * 
     * @param document the XML document to transform
     * @return the XML as a String
     * @throws Exception if any error occurred
     */
    public static String transformXMLDocument(Document document) 
    throws Exception {
        // Use a Transformer for output
        TransformerFactory tFactory = TransformerFactory.newInstance();
        Transformer transformer = tFactory.newTransformer();
        // transform the document to a string
        DOMSource source = new DOMSource(document);
        StringWriter string = new StringWriter();
        StreamResult result = new StreamResult(string);
        transformer.transform(source, result);
        return result.getWriter().toString();
    }
    /**
     * Creates a document which may be used to mainpulate XML. This
     * is a wrapper designed to hide some of the document creation
     * details.
     * 
     * @return the newly created document.
     * @throws Exception if any error occurred
     */
    private static Document createNewDocument() throws Exception{
        DocumentBuilderFactory dbf = new DocumentBuilderFactoryImpl();
        DocumentBuilder db = dbf.newDocumentBuilder();
        return db.newDocument();

    }
}
