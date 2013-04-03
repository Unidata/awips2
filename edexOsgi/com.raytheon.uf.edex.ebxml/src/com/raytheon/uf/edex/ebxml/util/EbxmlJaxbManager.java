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
package com.raytheon.uf.edex.ebxml.util;

import java.io.IOException;
import java.io.InputStream;
import java.io.StringReader;
import java.io.StringWriter;
import java.util.ArrayList;
import java.util.Map;

import javax.xml.bind.JAXBContext;
import javax.xml.bind.JAXBElement;
import javax.xml.bind.JAXBException;
import javax.xml.bind.Marshaller;
import javax.xml.bind.Unmarshaller;

import org.apache.cxf.helpers.IOUtils;

/**
 * TODO Add Description
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Dec 14, 2011            bphillip     Initial creation
 * 
 * </pre>
 * 
 * @author bphillip
 * @version 1.0
 */

public class EbxmlJaxbManager {

    public static final String JAXB_NAMESPACE_MAPPER = "com.sun.xml.bind.namespacePrefixMapper";

    private static JAXBContext defaultCtx = null;

    private JAXBContext context = null;

    // private NamespacePrefixMapper mapper;

    /**
     * Private constructor
     * 
     * @param prefixmap
     *            A map of URI to namespace prefix to use when marshalling or
     *            umarshalling.
     */
    private EbxmlJaxbManager(final Map<String, String> prefixmap) {
        this.context = getDefaultContext();
        // this.mapper = new NamespacePrefixMapper() {
        // @Override
        // public String getPreferredPrefix(String namespaceUri,
        // String suggestion, boolean requirePrefix) {
        // return prefixmap.get(namespaceUri);
        // }
        // };
    }

    /** Private default constructor. */
    private EbxmlJaxbManager() {
        this.context = getDefaultContext();
    }

    /**
     * @return A new instance of {@link EbxmlJaxbManager} that does not use a
     *         prefix map.
     */
    public static EbxmlJaxbManager getInstance() {
        return new EbxmlJaxbManager();
    }

    /**
     * @param prefixmap
     *            A map of URI to namespace prefix to use when marshalling or
     *            umarshalling.
     * @return A new instance of {@link EbxmlJaxbManager} that uses a prefix
     *         map.
     */
    public static EbxmlJaxbManager getInstance(
            final Map<String, String> prefixmap) {
        return new EbxmlJaxbManager(prefixmap);
    }

    /**
     * @return The default context.
     */
    private JAXBContext getDefaultContext() {
        if (defaultCtx == null) {
            defaultCtx = createContext();
        }
        return defaultCtx;
    }

    /** Create the context based on ObjectFactories that will be needed. */
    private JAXBContext createContext() {
        ArrayList<Class<?>> classList = new ArrayList<Class<?>>();
        classList
                .add(oasis.names.tc.ebxml.regrep.xsd.lcm.v4.ObjectFactory.class);
        classList
                .add(oasis.names.tc.ebxml.regrep.xsd.query.v4.ObjectFactory.class);
        classList
                .add(oasis.names.tc.ebxml.regrep.xsd.rim.v4.ObjectFactory.class);
        classList
                .add(oasis.names.tc.ebxml.regrep.xsd.rs.v4.ObjectFactory.class);
        classList
                .add(oasis.names.tc.ebxml.regrep.xsd.spi.v4.ObjectFactory.class);

        JAXBContext rval = null;
        try {
            rval = JAXBContext.newInstance(classList
                    .toArray(new Class[classList.size()]));
        } catch (JAXBException e) {
            e.printStackTrace();
        }
        return rval;
    }

    /**
     * Create Java objects from the given XML string.
     * 
     * @param xml
     *            The xml to unmarshal.
     * @return The Java objects created from the XML.
     * @throws JAXBException
     *             if a problem occurred.
     */
    public Object unmarshal(String xml) throws JAXBException {
        Object rval = null;
        Unmarshaller u = context.createUnmarshaller();
        rval = u.unmarshal(new StringReader(xml));
        if (rval instanceof JAXBElement<?>) {
            rval = ((JAXBElement<?>) rval).getValue();
        }
        return rval;
    }

    /**
     * Create an xml string from the given object.
     * 
     * @param obj
     *            The object to marshal.
     * @return The XML string created from the Java.
     * @throws JAXBException
     *             if a problem occured.
     */
    public String marshal(Object obj) throws JAXBException {
        return marshal(obj, true);
    }

    /**
     * Create an xml string from the given object.
     * 
     * @param obj
     *            The object to marshal.
     * @param formatted
     *            whether or not the output should be formatted.
     * @return The XML string created from the Java.
     * @throws JAXBException
     *             if a problem occured.
     */
    public String marshal(Object obj, boolean formatted) throws JAXBException {
        return marshal(obj, null, formatted);
    }

    /**
     * Create an xml string from the given object.
     * 
     * @param obj
     *            The object to marshal.
     * @param schemaLocation
     *            The location of a schema to use. May be null.
     * @param formatted
     *            whether or not the output should be formatted.
     * @return The XML string created from the Java.
     * @throws JAXBException
     *             if a problem occured.
     */
    public String marshal(Object obj, String schemaLocation, boolean formatted)
            throws JAXBException {
        String rval = null;
        Marshaller m = context.createMarshaller();
        StringWriter writer = new StringWriter();

        m.setProperty(Marshaller.JAXB_FORMATTED_OUTPUT, formatted);
        // if (mapper != null) {
        // m.setProperty(JAXB_NAMESPACE_MAPPER, mapper);
        // }
        if (schemaLocation != null && !schemaLocation.isEmpty()) {
            m.setProperty(Marshaller.JAXB_SCHEMA_LOCATION, schemaLocation);
        }
        m.marshal(obj, writer);
        rval = writer.toString();
        return rval;
    }

    /**
     * @return The current context.
     */
    public JAXBContext getContext() {
        return context;
    }

    /**
     * Create Java objects from the given {@link InputStream} containing XML.
     * 
     * @param stream
     *            The stream to umarshal from.
     * @return The Java objects created from the XMl stream.
     * @throws IOException
     *             if there was a problem with the stream.
     * @throws JAXBException
     *             if a problem occured.
     */
    public Object unmarshal(InputStream stream) throws JAXBException,
            IOException {
        return unmarshal(IOUtils.toString(stream));
    }

}
