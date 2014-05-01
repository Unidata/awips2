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
package com.raytheon.uf.common.serialization.jaxb;

import javax.xml.bind.JAXBException;

import org.xml.sax.Attributes;
import org.xml.sax.ContentHandler;
import org.xml.sax.Locator;
import org.xml.sax.SAXException;

import com.sun.xml.internal.bind.v2.runtime.unmarshaller.SAXConnector;

/**
 * JAXB Content handler, delegates work to SAXConnector. Ignores exceptions in
 * startElement to allow for continuing of parsing on errors
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Sep 13, 2011            mschenke     Initial creation
 * 
 * </pre>
 * 
 * @author mschenke
 * @version 1.0
 */

public class CustomContentHandler implements ContentHandler {

    private SAXConnector connector;

    /**
     * @param connector
     */
    public CustomContentHandler(SAXConnector connector) {
        this.connector = connector;
    }

    /**
     * @param arg0
     * @param arg1
     * @param arg2
     * @see com.sun.xml.internal.bind.v2.runtime.unmarshaller.SAXConnector#characters(char[],
     *      int, int)
     */
    public final void characters(char[] arg0, int arg1, int arg2) {
        connector.characters(arg0, arg1, arg2);
    }

    /**
     * @throws SAXException
     * @see com.sun.xml.internal.bind.v2.runtime.unmarshaller.SAXConnector#endDocument()
     */
    public void endDocument() throws SAXException {
        connector.endDocument();
    }

    /**
     * @param arg0
     * @param arg1
     * @param arg2
     * @throws SAXException
     * @see com.sun.xml.internal.bind.v2.runtime.unmarshaller.SAXConnector#endElement(java.lang.String,
     *      java.lang.String, java.lang.String)
     */
    public void endElement(String arg0, String arg1, String arg2)
            throws SAXException {
        connector.endElement(arg0, arg1, arg2);
    }

    /**
     * @param arg0
     * @throws SAXException
     * @see com.sun.xml.internal.bind.v2.runtime.unmarshaller.SAXConnector#endPrefixMapping(java.lang.String)
     */
    public void endPrefixMapping(String arg0) throws SAXException {
        connector.endPrefixMapping(arg0);
    }

    /**
     * @param obj
     * @return
     * @see java.lang.Object#equals(java.lang.Object)
     */
    public boolean equals(Object obj) {
        return connector.equals(obj);
    }

    /**
     * @return
     * @throws JAXBException
     * @throws IllegalStateException
     * @see com.sun.xml.internal.bind.v2.runtime.unmarshaller.SAXConnector#getResult()
     */
    public Object getResult() throws JAXBException, IllegalStateException {
        return connector.getResult();
    }

    /**
     * @return
     * @see java.lang.Object#hashCode()
     */
    public int hashCode() {
        return connector.hashCode();
    }

    /**
     * @param arg0
     * @param arg1
     * @param arg2
     * @see com.sun.xml.internal.bind.v2.runtime.unmarshaller.SAXConnector#ignorableWhitespace(char[],
     *      int, int)
     */
    public final void ignorableWhitespace(char[] arg0, int arg1, int arg2) {
        connector.ignorableWhitespace(arg0, arg1, arg2);
    }

    /**
     * @param arg0
     * @param arg1
     * @see com.sun.xml.internal.bind.v2.runtime.unmarshaller.SAXConnector#processingInstruction(java.lang.String,
     *      java.lang.String)
     */
    public void processingInstruction(String arg0, String arg1) {
        connector.processingInstruction(arg0, arg1);
    }

    /**
     * @param arg0
     * @see com.sun.xml.internal.bind.v2.runtime.unmarshaller.SAXConnector#setDocumentLocator(org.xml.sax.Locator)
     */
    public void setDocumentLocator(Locator arg0) {
        connector.setDocumentLocator(arg0);
    }

    /**
     * @param arg0
     * @see com.sun.xml.internal.bind.v2.runtime.unmarshaller.SAXConnector#skippedEntity(java.lang.String)
     */
    public void skippedEntity(String arg0) {
        connector.skippedEntity(arg0);
    }

    /**
     * @throws SAXException
     * @see com.sun.xml.internal.bind.v2.runtime.unmarshaller.SAXConnector#startDocument()
     */
    public void startDocument() throws SAXException {
        connector.startDocument();
    }

    /**
     * @param arg0
     * @param arg1
     * @param arg2
     * @param arg3
     * @throws SAXException
     * @see com.sun.xml.internal.bind.v2.runtime.unmarshaller.SAXConnector#startElement(java.lang.String,
     *      java.lang.String, java.lang.String, org.xml.sax.Attributes)
     */
    public void startElement(String arg0, String arg1, final String arg2,
            Attributes arg3) throws SAXException {
        try {
            connector.startElement(arg0, arg1, arg2, arg3);
        } catch (Throwable t) {
            // Ignore so we continue processing, we can notify from our event
            // handler
        }
    }

    /**
     * @param arg0
     * @param arg1
     * @throws SAXException
     * @see com.sun.xml.internal.bind.v2.runtime.unmarshaller.SAXConnector#startPrefixMapping(java.lang.String,
     *      java.lang.String)
     */
    public void startPrefixMapping(String arg0, String arg1)
            throws SAXException {
        connector.startPrefixMapping(arg0, arg1);
    }

}
