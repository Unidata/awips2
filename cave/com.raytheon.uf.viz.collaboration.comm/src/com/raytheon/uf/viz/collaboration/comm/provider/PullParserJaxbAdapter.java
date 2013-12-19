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
package com.raytheon.uf.viz.collaboration.comm.provider;

import java.io.IOException;
import java.util.HashSet;
import java.util.Set;

import javax.xml.bind.JAXBException;
import javax.xml.bind.UnmarshallerHandler;

import org.xml.sax.Attributes;
import org.xml.sax.Locator;
import org.xml.sax.SAXException;
import org.xmlpull.v1.XmlPullParser;
import org.xmlpull.v1.XmlPullParserException;

/**
 * Adapts xml pull parser to JAXB content handler. Treats XML input as fragment
 * (parsed tags do not inherit namespace of tags higher in parse stack)
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Dec 17, 2013 2562       bclement     Initial creation
 * 
 * </pre>
 * 
 * @author bclement
 * @version 1.0
 */

public class PullParserJaxbAdapter {

    private final XmlPullParser parser;

    private final UnmarshallerHandler handler;

    private final Set<String> parentNamespaces = new HashSet<String>();

    private final int[] startAndLength = new int[2];

    /**
     * Adapts pull parser to sax locator
     */
    private final Locator locator = new Locator() {

        @Override
        public String getSystemId() {
            return null;
        }

        @Override
        public String getPublicId() {
            return null;
        }

        @Override
        public int getLineNumber() {
            return parser.getLineNumber();
        }

        @Override
        public int getColumnNumber() {
            return parser.getColumnNumber();
        }
    };

    /**
     * Adapts pull parser to sax attributes
     */
    private final Attributes attributes = new Attributes() {

        @Override
        public String getValue(String uri, String localName) {
            int index = getIndex(uri, localName);
            return index < 0 ? null : getValue(index);
        }

        @Override
        public String getValue(String qName) {
            int index = getIndex(qName);
            return index < 0 ? null : getValue(index);
        }

        @Override
        public String getValue(int index) {
            return parser.getAttributeValue(index);
        }

        @Override
        public String getURI(int index) {
            return parser.getAttributeNamespace(index);
        }

        @Override
        public String getType(String uri, String localName) {
            int index = getIndex(uri, localName);
            return index < 0 ? null : getType(index);
        }

        @Override
        public String getType(String qName) {
            int index = getIndex(qName);
            return index < 0 ? null : getType(index);
        }

        @Override
        public String getType(int index) {
            return parser.getAttributeType(index);
        }

        @Override
        public String getQName(int index) {
            String name = parser.getAttributeName(index);
            String prefix = parser.getAttributePrefix(index);
            if (prefix != null) {
                name = prefix + ":" + name;
            }
            return name;
        }

        @Override
        public String getLocalName(int index) {
            return parser.getAttributeName(index);
        }

        @Override
        public int getLength() {
            return parser.getAttributeCount();
        }

        @Override
        public int getIndex(String uri, String localName) {
            for (int i = 0, len = getLength(); i < len; ++i) {
                if (getLocalName(i).equals(localName) && getURI(i).equals(uri)) {
                    return i;
                }
            }
            return -1;
        }

        @Override
        public int getIndex(String qName) {
            for (int i = 0, len = getLength(); i < len; ++i) {
                if (getQName(i).equals(qName)) {
                    return i;
                }
            }
            return -1;
        }
    };

    /**
     * @param parser
     * @param handler
     *            JAXB unmarshaller content handler
     */
    public PullParserJaxbAdapter(XmlPullParser parser, UnmarshallerHandler handler) {
        this.parser = parser;
        this.handler = handler;
        handler.setDocumentLocator(locator);
    }

    /**
     * Unmarshal XML from pull parser and get the result from the handler.
     * Treats XML input as fragment (parsed tags do not inherit namespace of
     * tags higher in parse stack)
     * 
     * @throws SAXException
     * @throws XmlPullParserException
     * @throws IOException
     * @throws JAXBException
     * @throws IllegalStateException
     */
    public Object unmarshal() throws SAXException, XmlPullParserException,
            IOException, IllegalStateException, JAXBException {
        for (int i = 0, count = parser.getNamespaceCount(parser.getDepth() - 1); i < count; ++i) {
            parentNamespaces.add(parser.getNamespaceUri(i));
        }

        handler.startDocument();
        parseElement();
        handler.endDocument();
        return handler.getResult();
    }

    /**
     * Begin namespace prefix mapping
     * 
     * @param start
     *            starting index in parser for mapping
     * @param end
     *            ending index in parser (not included in mapping)
     * @throws XmlPullParserException
     * @throws SAXException
     */
    private void startPrefix(int start, int end) throws XmlPullParserException,
            SAXException {
        for (int i = start; i < end; ++i) {
            String prefix = parser.getNamespacePrefix(i);
            if (prefix == null) {
                prefix = "";
            }
            String namespaceUri = parser.getNamespaceUri(i);
            handler.startPrefixMapping(prefix, namespaceUri);
        }
    }

    /**
     * End namespace prefix mapping, ends mapping in reverse order
     * 
     * @param start
     *            starting index in parser for mapping
     * @param end
     *            ending index in parser (not included in mapping)
     * @throws XmlPullParserException
     * @throws SAXException
     */
    private void endPrefix(int start, int end) throws XmlPullParserException,
            SAXException {
        for (int i = end - 1; i >= start; --i) {
            String prefix = parser.getNamespacePrefix(i);
            if (prefix == null) {
                prefix = "";
            }
            handler.endPrefixMapping(prefix);
        }
    }

    /**
     * Recursive parsing method
     * 
     * @throws XmlPullParserException
     * @throws SAXException
     * @throws IOException
     */
    private void parseElement() throws XmlPullParserException, SAXException,
            IOException {

        int startNs = parser.getNamespaceCount(parser.getDepth() - 1);
        int endNs = parser.getNamespaceCount(parser.getDepth());
        
        String name = parser.getName();
        String prefix = parser.getPrefix();
        String qname = (prefix == null ? name : prefix + ":" + name);
        String ns = parser.getNamespace();
        if (parentNamespaces.contains(ns)) {
            // namespace inherited from parent tag, ignore to treat xml as
            // fragment
            ns = "";
        }
        startPrefix(startNs, endNs);
        handler.startElement(ns, name, qname, attributes);

        while (true) {
            switch (parser.next()) {
            case XmlPullParser.TEXT:
                char[] text = parser.getTextCharacters(startAndLength);
                handler.characters(text, startAndLength[0], startAndLength[1]);
                break;
            case XmlPullParser.START_TAG:
                parseElement();
                break;
            case XmlPullParser.END_TAG:
                handler.endElement(ns, name, qname);
                endPrefix(startNs, endNs);
                return;
            }
        }
    }
}
