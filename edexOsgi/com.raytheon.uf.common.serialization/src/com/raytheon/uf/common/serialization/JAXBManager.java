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
import java.io.FileReader;
import java.io.FileWriter;
import java.io.IOException;
import java.io.InputStream;
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
 * Provides utilities for serialization support
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Sep 24, 2008            chammack     Initial creation
 * Nov 13, 2008            njensen      Added thrift methods
 * May 22, 2013 1917       rjpeter      Added non-pretty print option to jaxb serialize methods.
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

    private final JAXBContext jaxbContext;

    private final Queue<Unmarshaller> unmarshallers = new ConcurrentLinkedQueue<Unmarshaller>();

    private final Queue<Marshaller> marshallers = new ConcurrentLinkedQueue<Marshaller>();

    public JAXBManager(Class<?>... clazz) throws JAXBException {
        jaxbContext = JAXBContext.newInstance(clazz);
    }

    public JAXBContext getJaxbContext() throws JAXBException {
        return jaxbContext;
    }

    private Unmarshaller getUnmarshaller() throws JAXBException {
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

    private Marshaller getMarshaller() throws JAXBException {
        Marshaller m = marshallers.poll();
        if (m == null) {
            m = getJaxbContext().createMarshaller();
        }

        return m;
    }

    /**
     * Instantiates an object from the XML representation in a string. Uses
     * JAXB.
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
     * @param msh
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
     * string. Uses JAXB.
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
     * Convert an instance of a class to an XML representation in a string. Uses
     * JAXB.
     * 
     * @param obj
     *            Object being marshalled
     * @param formattedOutput
     *            True if the output should be xml pretty print.
     * @return XML string representation of the object
     * @throws JAXBException
     */
    public String marshalToXml(Object obj, boolean formatedOutput)
            throws JAXBException {
        Marshaller msh = getMarshaller();
        try {
            StringWriter writer = new StringWriter();
            msh.setProperty(Marshaller.JAXB_FORMATTED_OUTPUT, new Boolean(
                    formatedOutput));
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
     * print formatted XML to file. Uses JAXB.
     * 
     * @param obj
     *            Object to be marshaled
     * @param filePath
     *            Path to the output file
     * @throws SerializationException
     */
    public void jaxbMarshalToXmlFile(Object obj, String filePath)
            throws SerializationException {
        jaxbMarshalToXmlFile(obj, filePath, true);
    }

    /**
     * Convert an instance of a class to an XML representation and write XML to
     * file. Uses JAXB.
     * 
     * @param obj
     *            Object to be marshaled
     * @param filePath
     *            Path to the output file
     * @param formattedOutput
     *            True if the output should be xml pretty print.
     * @throws SerializationException
     */
    public void jaxbMarshalToXmlFile(Object obj, String filePath,
            boolean formattedOutput) throws SerializationException {
        FileWriter writer = null;
        Marshaller msh = null;
        try {
            msh = getMarshaller();
            msh.setProperty(Marshaller.JAXB_FORMATTED_OUTPUT, new Boolean(
                    formattedOutput));
            writer = new FileWriter(new File(filePath));
            msh.marshal(obj, writer);
        } catch (Exception e) {
            throw new SerializationException(e);
        } finally {
            if ((msh != null) && (marshallers.size() < QUEUE_SIZE)) {
                marshallers.add(msh);
            }
            if (writer != null) {
                try {
                    writer.close();
                } catch (IOException e) {
                    // ignore
                }
            }
        }

    }

    /**
     * Instantiates an object from the XML representation in a File. Uses JAXB.
     * 
     * @param filePath
     *            The path to the XML file
     * @return A new instance from the XML representation
     * @throws SerializationException
     */
    public Object jaxbUnmarshalFromXmlFile(String filePath)
            throws SerializationException {
        return jaxbUnmarshalFromXmlFile(new File(filePath));
    }

    /**
     * Instantiates an object from the XML representation in a File. Uses JAXB.
     * 
     * @param filePath
     *            The XML file
     * @return A new instance from the XML representation
     * @throws SerializationException
     */
    public Object jaxbUnmarshalFromXmlFile(File file)
            throws SerializationException {
        FileReader reader = null;
        Unmarshaller msh = null;
        try {
            msh = getUnmarshaller();
            reader = new FileReader(file);
            Object obj = msh.unmarshal(reader);
            return obj;
        } catch (Exception e) {
            throw new SerializationException(e.getLocalizedMessage(), e);
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

    /**
     * Instantiates an object from the XML representation in a stream. Uses
     * JAXB.
     * 
     * @param is
     *            The input stream. The stream will be closed by this operation.
     * @return A new instance from the XML representation
     * @throws SerializationException
     */
    public Object jaxbUnmarshalFromInputStream(InputStream is)
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

}
