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

import java.io.IOException;

import javax.xml.bind.JAXBException;
import javax.xml.bind.UnmarshalException;
import javax.xml.bind.UnmarshallerHandler;
import javax.xml.bind.ValidationEventHandler;
import javax.xml.bind.helpers.AbstractUnmarshallerImpl;
import javax.xml.validation.Schema;

import org.w3c.dom.Node;
import org.xml.sax.InputSource;
import org.xml.sax.SAXException;
import org.xml.sax.XMLReader;
import org.xml.sax.helpers.DefaultHandler;

import com.sun.xml.internal.bind.v2.runtime.JaxBeanInfo;
import com.sun.xml.internal.bind.v2.runtime.unmarshaller.InterningXmlVisitor;
import com.sun.xml.internal.bind.v2.runtime.unmarshaller.SAXConnector;
import com.sun.xml.internal.bind.v2.runtime.unmarshaller.UnmarshallerImpl;
import com.sun.xml.internal.bind.v2.runtime.unmarshaller.UnmarshallingContext;
import com.sun.xml.internal.bind.v2.runtime.unmarshaller.XmlVisitor;

/**
 * Custom JAXB Unmarshaller, used to set custom content handler. Delegates
 * everything else
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

public class CustomJAXBUnmarshaller extends AbstractUnmarshallerImpl {

    private static final DefaultHandler dummyHandler = new DefaultHandler();

    private UnmarshallerImpl delegate;

    private UnmarshallingContext coordinator;

    /**
     * @param delegate
     */
    public CustomJAXBUnmarshaller(UnmarshallerImpl delegate) {
        this.delegate = delegate;
        coordinator = new UnmarshallingContext(delegate, null);
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * javax.xml.bind.helpers.AbstractUnmarshallerImpl#setEventHandler(javax
     * .xml.bind.ValidationEventHandler)
     */
    @Override
    public void setEventHandler(ValidationEventHandler handler)
            throws JAXBException {
        delegate.setEventHandler(handler);
    }

    /*
     * (non-Javadoc)
     * 
     * @see javax.xml.bind.helpers.AbstractUnmarshallerImpl#getEventHandler()
     */
    @Override
    public ValidationEventHandler getEventHandler() throws JAXBException {
        return delegate.getEventHandler();
    }

    /*
     * (non-Javadoc)
     * 
     * @see javax.xml.bind.Unmarshaller#getUnmarshallerHandler()
     */
    @Override
    public UnmarshallerHandler getUnmarshallerHandler() {
        return delegate.getUnmarshallerHandler();
    }

    /*
     * (non-Javadoc)
     * 
     * @see javax.xml.bind.Unmarshaller#unmarshal(org.w3c.dom.Node)
     */
    @Override
    public Object unmarshal(Node arg0) throws JAXBException {
        return delegate.unmarshal(arg0);
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * javax.xml.bind.helpers.AbstractUnmarshallerImpl#unmarshal(org.xml.sax
     * .XMLReader, org.xml.sax.InputSource)
     */
    @Override
    protected Object unmarshal(XMLReader reader, InputSource source)
            throws JAXBException {
        CustomContentHandler connector = new CustomContentHandler(
                getUnmarshallerHandler(UnmarshallerImpl.needsInterning(reader),
                        null));

        reader.setContentHandler(connector);
        reader.setErrorHandler(coordinator);

        try {
            reader.parse(source);
        } catch (IOException e) {
            throw new UnmarshalException(e);
        } catch (SAXException e) {
            throw createUnmarshalException(e);
        }

        Object result = connector.getResult();

        reader.setContentHandler(dummyHandler);
        reader.setErrorHandler(dummyHandler);

        return result;
    }

    private SAXConnector getUnmarshallerHandler(boolean intern,
            JaxBeanInfo expectedType) {
        XmlVisitor h = delegate.createUnmarshallerHandler(null, false,
                expectedType);
        if (intern)
            h = new InterningXmlVisitor(h);
        return new SAXConnector(h, null);
    }

    /*
     * (non-Javadoc)
     * 
     * @see javax.xml.bind.helpers.AbstractUnmarshallerImpl#getSchema()
     */
    @Override
    public Schema getSchema() {
        return delegate.getSchema();
    }

    /*
     * (non-Javadoc)
     * 
     * @see javax.xml.bind.helpers.AbstractUnmarshallerImpl#setSchema(javax.xml.
     * validation.Schema)
     */
    @Override
    public void setSchema(Schema schema) {
        delegate.setSchema(schema);
    }

}
