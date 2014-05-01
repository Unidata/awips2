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
 *
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Mar 30, 2011            bclement     Initial creation
 * Aug 18, 2013  #2097     dhladky      extended JAXBManager
 *
 */
package com.raytheon.uf.edex.ogc.common.jaxb;

import java.io.ByteArrayOutputStream;
import java.io.FileReader;
import java.io.InputStream;
import java.io.OutputStream;
import java.io.StringReader;
import java.util.HashMap;
import java.util.Map;
import java.util.Queue;
import java.util.concurrent.ConcurrentLinkedQueue;
import java.util.concurrent.locks.Lock;
import java.util.concurrent.locks.ReadWriteLock;
import java.util.concurrent.locks.ReentrantReadWriteLock;

import javax.xml.bind.JAXBContext;
import javax.xml.bind.JAXBElement;
import javax.xml.bind.JAXBException;
import javax.xml.bind.Marshaller;
import javax.xml.bind.Unmarshaller;
import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.DocumentBuilderFactory;
import javax.xml.parsers.ParserConfigurationException;

import org.w3c.dom.Document;
import org.w3c.dom.Node;

import com.raytheon.uf.common.serialization.JAXBManager;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.sun.xml.bind.api.JAXBRIContext;
import com.sun.xml.bind.marshaller.NamespacePrefixMapper;

/**
 * Cache and utility class for OGC JAXB
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 2011            bclement     Initial creation
 * 
 * </pre>
 * 
 * @author bclement
 * @version 1.0
 */
public class OgcJaxbManager extends JAXBManager {

    protected final JAXBContext jaxbContext;

    protected static final int QUEUE_SIZE = 10;

	protected final Queue<Unmarshaller> unmarshallers = new ConcurrentLinkedQueue<Unmarshaller>();

	protected final Queue<Marshaller> marshallers = new ConcurrentLinkedQueue<Marshaller>();

	protected volatile int unmarshallersCreated = 0;

	protected volatile int marshallersCreated = 0;

	protected final IUFStatusHandler log = UFStatus.getHandler(this.getClass());

    private volatile NamespacePrefixMapper mapper;

    protected final ReadWriteLock prefixLock = new ReentrantReadWriteLock();

	protected static final String JAXB_NAMESPACE_MAPPER = "com.sun.xml.bind.namespacePrefixMapper";

	public OgcJaxbManager(Class<?>[] classes) throws JAXBException {
        jaxbContext = JAXBContext.newInstance(classes, getJaxbConfig());
    }

    private static Map<String, Object> getJaxbConfig() throws JAXBException {
        Map<String, Object> jaxbConfig = new HashMap<String, Object>();
        TransientAnnotationReader reader = new TransientAnnotationReader();
        try {
            reader.addTransientField(Throwable.class
                    .getDeclaredField("stackTrace"));
            reader.addTransientMethod(Throwable.class
                    .getDeclaredMethod("getStackTrace"));
        } catch (Exception e) {
            throw new JAXBException("Unable to add transient members", e);
        }
        jaxbConfig.put(JAXBRIContext.ANNOTATION_READER, reader);
        return jaxbConfig;
	}

	protected Unmarshaller getUnmarshaller() throws JAXBException {
		Unmarshaller m = unmarshallers.poll();
		if (m == null) {
			if (unmarshallersCreated < QUEUE_SIZE) {
                synchronized (unmarshallers) {
                    m = jaxbContext.createUnmarshaller();
                    ++unmarshallersCreated;
                }
			} else {
				int tries = 0;
				do {
					try {
						Thread.sleep(50);
					} catch (InterruptedException e) {
						// ignore
					}
					m = unmarshallers.poll();
					tries++;
					if (tries >= 20) {
						log.debug("Unable to get jaxb unmarshaller from pool after "
								+ tries + " tries. Growing pool size.");
                        synchronized (unmarshallers) {
                            m = jaxbContext.createUnmarshaller();
                            ++unmarshallersCreated;
                        }
					}
				} while (m == null);
			}
		}
		return m;
	}

	protected Marshaller getMarshaller() throws JAXBException {
		Marshaller m = marshallers.poll();
		if (m == null) {
			if (marshallersCreated < QUEUE_SIZE) {
                synchronized (marshallers) {
                    m = jaxbContext.createMarshaller();
                    ++marshallersCreated;
                }
			} else {
				int tries = 0;
				do {
					try {
						Thread.sleep(50);
					} catch (InterruptedException e) {
						// ignore
					}
					m = marshallers.poll();
					tries++;
					if (tries >= 20) {
						log.debug("Unable to get jaxb marshaller from pool after "
								+ tries + " tries. Growing pool size.");
                        synchronized (marshallers) {
                            m = jaxbContext.createMarshaller();
                            ++marshallersCreated;
                        }
					}
				} while (m == null);
			}
		}
		return m;
	}

	public Object unmarshal(String xml) throws JAXBException {
		Unmarshaller msh = null;
		try {
			msh = getUnmarshaller();
			StringReader reader = new StringReader(xml);
			Object obj = msh.unmarshal(reader);
			if (obj instanceof JAXBElement<?>) {
				obj = ((JAXBElement<?>) obj).getValue();
			}
			return obj;
		} finally {
			if (msh != null) {
				unmarshallers.add(msh);
			}
		}
	}

    public Object unmarshal(Node node) throws JAXBException {
        Unmarshaller msh = null;
        try {
            msh = getUnmarshaller();
            Object obj = msh.unmarshal(node);
            if (obj instanceof JAXBElement<?>) {
                obj = ((JAXBElement<?>) obj).getValue();
            }
            return obj;
        } finally {
            if (msh != null) {
                unmarshallers.add(msh);
            }
        }
    }

    public Object unmarshal(FileReader reader) throws JAXBException {
        Unmarshaller msh = null;
        try {
            msh = getUnmarshaller();
            Object obj = msh.unmarshal(reader);
            if (obj instanceof JAXBElement<?>) {
                obj = ((JAXBElement<?>) obj).getValue();
            }
            return obj;
        } finally {
            if (msh != null) {
                unmarshallers.add(msh);
            }
        }
    }

	public Object unmarshal(InputStream xml) throws JAXBException {
		Unmarshaller msh = null;
		try {
			msh = getUnmarshaller();
			Object obj = msh.unmarshal(xml);
			if (obj instanceof JAXBElement<?>) {
				obj = ((JAXBElement<?>) obj).getValue();
			}
			return obj;
		} finally {
			if (msh != null) {
				unmarshallers.add(msh);
			}
		}
	}

	public String marshal(Object obj) throws JAXBException {
		return marshal(obj, true);
	}

	public String marshal(Object obj, boolean formatted) throws JAXBException {
        return marshal(obj, null, formatted, false);
	}

    public String marshal(Object obj, boolean formatted, boolean fragment)
            throws JAXBException {
        return marshal(obj, null, formatted, fragment);
    }

    public Node marshalToNode(Object obj) throws JAXBException,
            ParserConfigurationException {
        Marshaller msh = getMarshaller();
        try {
            DocumentBuilderFactory dbf = DocumentBuilderFactory.newInstance();
            dbf.setNamespaceAware(true);
            DocumentBuilder db = dbf.newDocumentBuilder();
            Document doc = db.newDocument();
            msh.marshal(obj, doc);
            return doc.getFirstChild();
        } finally {
            marshallers.add(msh);
        }
    }

	public void marshal(Object obj, OutputStream out, String schemaLocation,
            boolean formatted, boolean fragment) throws JAXBException {
		Marshaller msh = getMarshaller();
		try {

			msh.setProperty(Marshaller.JAXB_FORMATTED_OUTPUT, formatted);
            msh.setProperty(Marshaller.JAXB_FRAGMENT, fragment);
			if (mapper != null) {
                Lock read = prefixLock.readLock();
                read.lock();
                try {
                    msh.setProperty(JAXB_NAMESPACE_MAPPER, mapper);
                } finally {
                    read.unlock();
                }
			}
			if (schemaLocation != null && !schemaLocation.isEmpty()) {
				msh.setProperty(Marshaller.JAXB_SCHEMA_LOCATION, schemaLocation);
			}
			msh.marshal(obj, out);
		} finally {
			marshallers.add(msh);
		}
	}

    public String marshal(Object obj, String schemaLocation, boolean formatted,
            boolean fragment)
			throws JAXBException {
		ByteArrayOutputStream out = new ByteArrayOutputStream();
        marshal(obj, out, schemaLocation, formatted, fragment);
		return out.toString();
	}

	public void setPrefixMap(final Map<String, String> prefixMap) {
        Lock write = prefixLock.writeLock();
        write.lock();
        try {
            this.mapper = new NamespacePrefixMapper() {
                @Override
                public String getPreferredPrefix(String namespaceUri,
                        String suggestion, boolean requirePrefix) {
                    return prefixMap.get(namespaceUri);
                }
            };
        } finally {
            write.unlock();
        }
	}

}
