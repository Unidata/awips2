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
package com.raytheon.uf.common.serialization;

import java.io.File;
import java.io.FileOutputStream;
import java.io.FileReader;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.io.StringReader;
import java.io.StringWriter;
import java.util.ArrayList;
import java.util.Queue;
import java.util.concurrent.ConcurrentLinkedQueue;

import javax.xml.bind.JAXBContext;
import javax.xml.bind.JAXBException;
import javax.xml.bind.Marshaller;
import javax.xml.bind.Unmarshaller;
import javax.xml.bind.ValidationEvent;
import javax.xml.bind.ValidationEventHandler;

import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;

/**
 * Provides an easy and convenient layer to marshal or unmarshal objects to and
 * from XML using JAXB. An instance of this class is thread-safe, it will use
 * separate marshallers and unmarshallers if used simultaneously by different
 * threads.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Sep 24, 2008            chammack     Initial creation
 * Nov 13, 2008            njensen      Added thrift methods
 * May 22, 2013 1917       rjpeter      Added non-pretty print option to jaxb serialize methods.
 * Aug 18, 2013 #2097      dhladky      Allowed extension by OGCJAXBManager
 * Sep 30, 2013 2361       njensen      Refactored for cleanliness
 * Nov 14, 2013 2361       njensen      Added lazy init option, improved unmarshal error message
 * Apr 16, 2014 2928       rjpeter      Updated marshalToStream to not close the stream.
 * Apr 25, 2014 2060       njensen      Improved printout
 * </pre>
 * 
 * @author chammack
 * @version 1.0
 */

public class JAXBManager {

    private static final int QUEUE_SIZE = 10;

    /**
     * 
     * Saves all validation events so if an error is caught handlers have an
     * option of getting more accurate information about what happened
     * 
     * <pre>
     * 
     * SOFTWARE HISTORY
     * 
     * Date         Ticket#    Engineer    Description
     * ------------ ---------- ----------- --------------------------
     * Sep 2, 2011            ekladstrup     Initial creation
     * 
     * </pre>
     * 
     * @author ekladstrup
     * @version 1.0
     */
    private static class MaintainEventsValidationHandler implements
            ValidationEventHandler {

        private final ArrayList<ValidationEvent> events = new ArrayList<ValidationEvent>(
                0);

        @Override
        public boolean handleEvent(ValidationEvent event) {
            events.add(event);
            return true;
        }

        public ArrayList<ValidationEvent> getEvents() {
            synchronized (events) {
                return new ArrayList<ValidationEvent>(events);
            }
        }

        public void clearEvents() {
            synchronized (events) {
                events.clear();
            }
        }
    }

    private volatile JAXBContext jaxbContext;

    private Class<?>[] clazz;

    protected final Queue<Unmarshaller> unmarshallers = new ConcurrentLinkedQueue<Unmarshaller>();

    protected final Queue<Marshaller> marshallers = new ConcurrentLinkedQueue<Marshaller>();

    /**
     * Constructor. Clazz should include any classes that this JAXBManager needs
     * to marshal to XML or unmarshal from XML. Does not need to include classes
     * contained as fields or inner classes of other classes already passed to
     * the constructor.
     * 
     * @param clazz
     *            classes that this instance must know about for
     *            marshalling/unmarshalling
     * @throws JAXBException
     */
    public JAXBManager(Class<?>... clazz) throws JAXBException {
        this(false, clazz);
    }

    /**
     * Constructor. Clazz should include any classes that this JAXBManager needs
     * to marshal to XML or unmarshal from XML. Does not need to include classes
     * contained as fields or inner classes of other classes already passed to
     * the constructor.
     * 
     * If lazyInit is true, then the underlying JAXBContext (a potentially slow
     * operation) will be constructed when first used, ie the first marshal or
     * unmarshal operation.
     * 
     * @param lazyInit
     *            whether or not to immediately initialize the underlying
     *            JAXBContext
     * @param clazz
     *            classes that this instance must know about for
     *            marshalling/unmarshalling
     * @throws JAXBException
     */
    public JAXBManager(boolean lazyInit, Class<?>... clazz)
            throws JAXBException {
        this.clazz = clazz;
        if (!lazyInit) {
            getJaxbContext();
        }
    }

    /**
     * Returns the JAXB Context behind this JAXBManager.
     * 
     * @return the JAXBContext
     * @throws JAXBException
     * @Deprecated TODO This method should be protected and the JAXBContext
     *             should be hidden from outside libraries. Any options needing
     *             to be applied to the context or its marshallers/unmarshallers
     *             should either have convenience methods or flags on
     *             JAXBManager to provide that functionality.
     */
    @Deprecated
    public JAXBContext getJaxbContext() throws JAXBException {
        if (jaxbContext == null) {
            synchronized (this) {
                if (jaxbContext == null) {
                    long t0 = System.currentTimeMillis();
                    jaxbContext = JAXBContext.newInstance(clazz);
                    if (clazz.length == 1) {
                        System.out.println("JAXB context for "
                                + clazz[0].getSimpleName() + " inited in: "
                                + (System.currentTimeMillis() - t0) + "ms");
                    }
                    clazz = null;
                }
            }
        }
        return jaxbContext;
    }

    /**
     * Gets an unmarshaller, creating one if one is not currently available.
     * 
     * @return an unmarshaller
     * @throws JAXBException
     */
    protected Unmarshaller getUnmarshaller() throws JAXBException {
        Unmarshaller m = unmarshallers.poll();
        if (m == null) {
            m = getJaxbContext().createUnmarshaller();
            // set event handler to be able to retrieve ValidationEvents
            m.setEventHandler(new MaintainEventsValidationHandler());
        } else {
            // clear events in event handler ( just in case it was missed, don't
            // intentionally rely on this path to clear the events for you, they
            // don't need to live that long )
            ValidationEventHandler h = m.getEventHandler();
            if (h instanceof MaintainEventsValidationHandler) {
                MaintainEventsValidationHandler sh = (MaintainEventsValidationHandler) h;
                sh.clearEvents();
            }
        }

        return m;
    }

    /**
     * Gets a marshaller, creating one if one is not currently available.
     * 
     * @return
     * @throws JAXBException
     */
    protected Marshaller getMarshaller() throws JAXBException {
        Marshaller m = marshallers.poll();
        if (m == null) {
            m = getJaxbContext().createMarshaller();
        }

        return m;
    }

    /**
     * Instantiates an object from the XML representation in a string.
     * 
     * @param xml
     *            The XML representation
     * @return A new instance from the XML representation
     * @throws JAXBException
     */
    public Object unmarshalFromXml(String xml) throws JAXBException {
        Unmarshaller msh = null;
        try {
            msh = getUnmarshaller();
            StringReader reader = new StringReader(xml);
            Object obj = msh.unmarshal(reader);
            return obj;
        } finally {
            handleEvents(msh, null);
            if ((msh != null) && (unmarshallers.size() < QUEUE_SIZE)) {
                unmarshallers.add(msh);
            }
        }

    }

    /**
     * Processes the events received by an unmarshaller when parsing XML.
     * 
     * @param msh
     *            the unmarshaller
     */
    private void handleEvents(Unmarshaller msh, String name) {
        try {
            ValidationEventHandler h = msh.getEventHandler();
            if (h instanceof MaintainEventsValidationHandler) {
                boolean allInfo = true;
                MaintainEventsValidationHandler mh = (MaintainEventsValidationHandler) h;
                for (ValidationEvent event : mh.getEvents()) {
                    if (event.getSeverity() == ValidationEvent.FATAL_ERROR) {
                        // If we had a fatal error, report events at their
                        // native severity, otherwise use all info as the
                        // unmarshalling didn't fail
                        allInfo = false;
                        break;
                    }
                }
                for (ValidationEvent event : mh.getEvents()) {
                    Priority p = Priority.INFO;
                    if (!allInfo) {
                        switch (event.getSeverity()) {
                        case ValidationEvent.FATAL_ERROR:
                            p = Priority.SIGNIFICANT;
                            break;
                        case ValidationEvent.ERROR:
                            p = Priority.PROBLEM;
                            break;
                        case ValidationEvent.WARNING:
                            p = Priority.WARN;
                            break;
                        }
                    }
                    UFStatus.getHandler().handle(
                            p,
                            (name != null ? name : "") + ": "
                                    + event.getMessage() + " on line "
                                    + event.getLocator().getLineNumber()
                                    + " column "
                                    + event.getLocator().getColumnNumber(),
                            event.getLinkedException());
                }
                mh.clearEvents();
            }
        } catch (JAXBException e) {
            // Ignore, unable to get handler
        }
    }

    /**
     * Convert an instance of a class to an XML pretty print representation in a
     * string.
     * 
     * @param obj
     *            Object being marshalled
     * @return XML string representation of the object
     * @throws JAXBException
     */
    public String marshalToXml(Object obj) throws JAXBException {
        return marshalToXml(obj, true);
    }

    /**
     * Convert an instance of a class to an XML representation in a string.
     * 
     * @param obj
     *            Object being marshalled
     * @param formattedOutput
     *            True if the output should be xml pretty print.
     * @return XML string representation of the object
     * @throws JAXBException
     */
    public String marshalToXml(Object obj, boolean formattedOutput)
            throws JAXBException {
        Marshaller msh = getMarshaller();
        try {
            StringWriter writer = new StringWriter();
            msh.setProperty(Marshaller.JAXB_FORMATTED_OUTPUT, new Boolean(
                    formattedOutput));
            msh.marshal(obj, writer);
            return writer.toString();
        } finally {
            if ((msh != null) && (marshallers.size() < QUEUE_SIZE)) {
                marshallers.add(msh);
            }
        }
    }

    /**
     * Convert an instance of a class to an XML representation and writes pretty
     * print formatted XML to file.
     * 
     * @param obj
     *            Object to be marshaled
     * @param filePath
     *            Path to the output file
     * @throws SerializationException
     */
    public void marshalToXmlFile(Object obj, String filePath)
            throws SerializationException {
        marshalToXmlFile(obj, filePath, true);
    }

    /**
     * Convert an instance of a class to an XML representation and writes XML to
     * file.
     * 
     * @param obj
     *            Object to be marshaled
     * @param filePath
     *            Path to the output file
     * @param formattedOutput
     *            True for pretty print xml.
     * @throws SerializationException
     */
    public void marshalToXmlFile(Object obj, String filePath,
            boolean formattedOutput) throws SerializationException {
        OutputStream os = null;
        try {
            os = new FileOutputStream(new File(filePath));
            marshalToStream(obj, os, formattedOutput);
        } catch (SerializationException e) {
            throw e;
        } catch (Exception e) {
            throw new SerializationException(e);
        } finally {
            if (os != null) {
                try {
                    os.close();
                } catch (IOException e) {
                    // ignore
                }
            }
        }
    }

    /**
     * Convert an instance of a class to an XML representation and writes pretty
     * print formatted XML to output stream.
     * 
     * @param obj
     * @param out
     * @throws SerializationException
     */
    public void marshalToStream(Object obj, OutputStream out)
            throws SerializationException {
        marshalToStream(obj, out, true);
    }

    /**
     * Convert an instance of a class to an XML representation and writes XML to
     * output stream.
     * 
     * @param obj
     * @param out
     * @param formattedOutput
     * 
     * @throws SerializationException
     */
    public void marshalToStream(Object obj, OutputStream out,
            boolean formattedOutput) throws SerializationException {
        Marshaller msh = null;
        try {
            msh = getMarshaller();
            msh.setProperty(Marshaller.JAXB_FORMATTED_OUTPUT, new Boolean(
                    formattedOutput));
            msh.marshal(obj, out);
        } catch (Exception e) {
            throw new SerializationException(e);
        } finally {
            if ((msh != null) && (marshallers.size() < QUEUE_SIZE)) {
                marshallers.add(msh);
            }
        }
    }

    /**
     * Instantiates an object from the XML representation in a File.
     * 
     * @param filePath
     *            The path to the XML file
     * @return A new instance from the XML representation
     * @throws SerializationException
     * @Deprecated Use unmarshalFromXmlFile(Class<?>, String) instead
     */
    @Deprecated
    public Object unmarshalFromXmlFile(String filePath)
            throws SerializationException {
        return unmarshalFromXmlFile(new File(filePath));
    }

    /**
     * Instantiates an object from the XML representation in a File.
     * 
     * @param file
     *            The XML file
     * @return A new instance from the XML representation
     * @throws SerializationException
     * @Deprecated Use unmarshalFromXmlFile(Class<?>, File) instead
     */
    @Deprecated
    public Object unmarshalFromXmlFile(File file) throws SerializationException {
        return unmarshalFromXmlFile(Object.class, file);
    }

    /**
     * Instantiates an object of the specified type from the XML representation
     * in a File.
     * 
     * @param clazz
     *            The class to cast the Object in the file to
     * @param filePath
     *            The path to the XML file
     * @return A new instance from the XML representation
     * @throws SerializationException
     */
    public <T> T unmarshalFromXmlFile(Class<T> clazz, String filePath)
            throws SerializationException {
        return unmarshalFromXmlFile(clazz, new File(filePath));
    }

    /**
     * Instantiates an object from the XML representation in a File.
     * 
     * @param clazz
     *            The class to cast the Object in the file to
     * @param file
     *            The XML file
     * @return A new instance from the XML representation
     * @throws SerializationException
     */
    public <T> T unmarshalFromXmlFile(Class<T> clazz, File file)
            throws SerializationException {
        try {
            return clazz.cast(internalUnmarshalFromXmlFile(file));
        } catch (ClassCastException cce) {
            throw new SerializationException(cce);
        }
    }

    /**
     * Instantiates an object from the XML representation in a stream.
     * 
     * @param is
     *            The input stream. The stream will be closed by this operation.
     * @return A new instance from the XML representation
     * @throws SerializationException
     */
    public Object unmarshalFromInputStream(InputStream is)
            throws SerializationException {
        Unmarshaller msh = null;
        try {
            msh = getUnmarshaller();
            Object obj = msh.unmarshal(is);
            return obj;
        } catch (Exception e) {
            throw new SerializationException(e.getLocalizedMessage(), e);
        } finally {
            if (msh != null) {
                handleEvents(msh, null);
            }
            if ((msh != null) && (unmarshallers.size() < QUEUE_SIZE)) {
                unmarshallers.add(msh);
            }
            if (is != null) {
                try {
                    is.close();
                } catch (IOException e) {
                    // ignore
                }
            }
        }
    }

    /**
     * Unmarshals an object from an xml file.
     * 
     * @param file
     *            the file to unmarshal and object from.
     * @return the object from the file
     * @throws SerializationException
     */
    protected Object internalUnmarshalFromXmlFile(File file)
            throws SerializationException {
        FileReader reader = null;
        Unmarshaller msh = null;
        try {
            msh = getUnmarshaller();
            reader = new FileReader(file);
            Object obj = msh.unmarshal(reader);
            return obj;
        } catch (Exception e) {
            throw new SerializationException("Error reading " + file.getName()
                    + "\n" + e.getLocalizedMessage(), e);
        } finally {
            if (msh != null) {
                handleEvents(msh, file.getName());
            }
            if ((msh != null) && (unmarshallers.size() < QUEUE_SIZE)) {
                unmarshallers.add(msh);
            }
            if (reader != null) {
                try {
                    reader.close();
                } catch (IOException e) {
                    // ignore
                }
            }
        }
    }

}
