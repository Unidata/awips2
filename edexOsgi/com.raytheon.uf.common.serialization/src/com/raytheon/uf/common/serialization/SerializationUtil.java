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

import java.io.ByteArrayInputStream;
import java.io.File;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.util.List;

import javax.xml.bind.JAXBContext;
import javax.xml.bind.JAXBException;

import com.raytheon.uf.common.serialization.DynamicSerializationManager.SerializationType;

/**
 * Provides utilities for serialization support
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Sep 24, 2008            chammack     Initial creation
 * Nov 13, 2008            njensen      Added thrift methods
 * Sep 07, 2012 1102       djohnson     Overload jaxbUnmarshall and transformFromThrift methods 
 *                                      to accept class parameter, deprecate old versions.  Improve performance
 *                                      of getJaxbManager().
 * 
 * </pre>
 * 
 * @author chammack
 * @version 1.0
 */

public final class SerializationUtil {

    // @VisibleForTesting
    static volatile JAXBManager jaxbManager;

	private SerializationUtil() {

	}

    /**
     * Retrieve the {@link JAXBManager} instance. Lazily-initialized using
     * proper double-checked locking. This version performs better than
     * synchronizing the entire method.
     * 
     * @return the {@link JAXBManager}
     * @see http://en.wikipedia.org/wiki/Double-checked_locking
     * @throws JAXBException
     */
    public static JAXBManager getJaxbManager() throws JAXBException {
        JAXBManager result = jaxbManager;
        if (result == null) {
            synchronized (SerializationUtil.class) {
                result = jaxbManager;
                if (result == null) {
                    List<Class<ISerializableObject>> jaxbClasses = SerializableManager
                            .getInstance().getJaxbables();
                    jaxbManager = result = new JAXBManager(
                            jaxbClasses.toArray(new Class[jaxbClasses.size()]));

                }
            }
        }
        return result;
    }

	public static JAXBContext getJaxbContext() throws JAXBException {
		return getJaxbManager().getJaxbContext();
	}

    /**
     * Instantiates an object from the XML representation in a string. Uses
     * JAXB.
     * 
     * @param xml
     *            The XML representation
     * @return A new instance from the XML representation
     * @throws JAXBException
     * @deprecated Use {@link #unmarshalFromXml(Class, String)} which performs
     *             the cast for you, and wraps any {@link ClassCastException}s
     *             in a serialization exception
     */
    @Deprecated
	public static Object unmarshalFromXml(String xml) throws JAXBException {
        return unmarshalFromXml(Object.class, xml);

	}

    /**
     * Instantiates an object from the XML representation in a string. Uses
     * JAXB.
     * 
     * @param clazz
     *            the class object of the result type
     * @param xml
     *            The XML representation
     * @return A new instance from the XML representation
     * @throws JAXBException
     */
    public static <T> T unmarshalFromXml(Class<T> clazz, String xml)
            throws JAXBException {
        return clazz.cast(getJaxbManager().unmarshalFromXml(xml));
    }

	/**
	 * Convert an instance of a class to an XML representation in a string. Uses
	 * JAXB.
	 * 
	 * @param obj
	 *            Object being marshalled
	 * @return XML string representation of the object
	 * @throws JAXBException
	 */
	public static String marshalToXml(Object obj) throws JAXBException {
		return getJaxbManager().marshalToXml(obj);
	}

	/**
	 * Convert an instance of a class to an XML representation and write XML to
	 * file. Uses JAXB.
	 * 
	 * @param obj
	 *            Object to be marshaled
	 * @param filePath
	 *            Path to the output file
	 * @throws SerializationException
	 */
	public static void jaxbMarshalToXmlFile(Object obj, String filePath)
			throws SerializationException {
		try {
			getJaxbManager().jaxbMarshalToXmlFile(obj, filePath);
		} catch (JAXBException e) {
			throw new SerializationException(e);
		}

	}

    /**
     * Instantiates an object from the XML representation in a File. Uses JAXB.
     * 
     * @param filePath
     *            The path to the XML file
     * @return A new instance from the XML representation
     * @throws SerializationException
     * @deprecated Use {@link #jaxbUnmarshalFromXmlFile(Class, String)} which
     *             performs the cast for you, and wraps any
     *             {@link ClassCastException}s in a serialization exception
     */
    @Deprecated
	public static Object jaxbUnmarshalFromXmlFile(String filePath)
			throws SerializationException {
        return jaxbUnmarshalFromXmlFile(Object.class, filePath);
	}

    /**
     * Instantiates an object from the XML representation in a File. Uses JAXB.
     * 
     * @param clazz
     *            the result type class
     * @param filePath
     *            The path to the XML file
     * @return A new instance from the XML representation
     * @throws SerializationException
     * 
     */
    public static <T> T jaxbUnmarshalFromXmlFile(Class<T> clazz, String filePath)
            throws SerializationException {
        try {
            return clazz.cast(getJaxbManager().jaxbUnmarshalFromXmlFile(
                    filePath));
        } catch (JAXBException e) {
            throw new SerializationException(e);
        }
    }

    /**
     * Instantiates an object from the XML representation in a File. Uses JAXB.
     * 
     * @param filePath
     *            The XML file
     * @return A new instance from the XML representation
     * @throws SerializationException
     * @deprecated Use {@link #jaxbUnmarshalFromXmlFile(Class, File)} which
     *             performs the cast for you, and wraps any
     *             {@link ClassCastException}s in a serialization exception
     */
	@Deprecated
	public static Object jaxbUnmarshalFromXmlFile(File file)
			throws SerializationException {
        return jaxbUnmarshalFromXmlFile(Object.class, file);
	}

    /**
     * Instantiates an object from the XML representation in a File. Uses JAXB.
     * 
     * @param clazz
     *            the clazz object used to dynamically cast the result
     * @param filePath
     *            The XML file
     * @return A new instance from the XML representation
     * @throws SerializationException
     *             if JAXB encounters an exception, or a
     *             {@link ClassCastException} occurs
     */
    public static <T> T jaxbUnmarshalFromXmlFile(Class<T> clazz, File file)
            throws SerializationException {
        try {
            return clazz.cast(getJaxbManager().jaxbUnmarshalFromXmlFile(file));
        } catch (Exception e) {
            throw new SerializationException(e.getLocalizedMessage(), e);
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
     * @deprecated Use {@link #jaxbUnmarshalFromInputStream(Class, InputStream)}
     *             which performs the cast for you, and wraps any
     *             {@link ClassCastException}s in a serialization exception
     */
    @Deprecated
	public static Object jaxbUnmarshalFromInputStream(InputStream is)
			throws SerializationException {
        return jaxbUnmarshalFromInputStream(Object.class, is);
	}

    /**
     * Instantiates an object from the XML representation in a stream. Uses
     * JAXB.
     * 
     * @param clazz
     *            the class object of the result type
     * @param is
     *            The input stream. The stream will be closed by this operation.
     * @return A new instance from the XML representation
     * @throws SerializationException
     * 
     */
    public static <T> T jaxbUnmarshalFromInputStream(Class<T> clazz,
            InputStream is)
            throws SerializationException {
        try {
            return clazz
                    .cast(getJaxbManager().jaxbUnmarshalFromInputStream(is));
        } catch (Exception e) {
            throw new SerializationException(e.getLocalizedMessage(), e);
        }
    }

	/**
	 * Transforms an object to the thrift protocol using DynamicSerialize. The
	 * object will exist in memory 3 times during this process: once in its
	 * native object form, once in its serialized form in a byte stream, and
	 * then as an array of bytes returned from byte stream.
	 * 
	 * @param obj
	 *            the object to convert to bytes
	 * @return the object as bytes
	 * @throws SerializationException
	 */
	public static byte[] transformToThrift(Object obj)
			throws SerializationException {
		DynamicSerializationManager dsm = DynamicSerializationManager
				.getManager(SerializationType.Thrift);
		return dsm.serialize(obj);
	}

	/**
	 * Transforms an object to the thrift protocol using DynamicSerialize.
	 * Object will be written directly to stream reducing memory usage compared
	 * to returning a byte array. Named differently from standard
	 * transformToThrift to avoid ambiguity in camel routes.
	 * 
	 * @param obj
	 *            the object to convert to bytes
	 * @param os
	 *            the output stream to write the bytes to
	 * @return the object as bytes
	 * @throws SerializationException
	 */
	public static void transformToThriftUsingStream(Object obj, OutputStream os)
			throws SerializationException {
		DynamicSerializationManager dsm = DynamicSerializationManager
				.getManager(SerializationType.Thrift);
		dsm.serialize(obj, os);
	}

    /**
     * Transforms a byte array from the thrift protocol to an object using
     * DynamicSerialize
     * 
     * @param bytes
     *            the object as bytes
     * @return the Java object
     * @throws SerializationException
     * @deprecated Use {@link #transformFromThrift(Class, byte[]) which performs
     *             the cast for you, and wraps any {@link ClassCastException}s
     *             in a serialization exception
     */
	@Deprecated
    public static Object transformFromThrift(byte[] bytes)
			throws SerializationException {
        return transformFromThrift(Object.class, bytes);
	}

    /**
     * Transforms a byte array from the thrift protocol to an object using
     * DynamicSerialize
     * 
     * @param bytes
     *            the object as bytes
     * @return the Java object
     * @throws SerializationException
     *             if a serialization or class cast exception occurs
     */
    public static <T> T transformFromThrift(Class<T> clazz, byte[] bytes)
            throws SerializationException {
        DynamicSerializationManager dsm = DynamicSerializationManager
                .getManager(SerializationType.Thrift);
        ByteArrayInputStream bais = null;
        try {
            bais = new ByteArrayInputStream(bytes);
            return clazz.cast(dsm.deserialize(bais));
        } catch (ClassCastException cce) {
            throw new SerializationException(cce);
        } finally {
            if (bais != null) {
                try {
                    bais.close();
                } catch (IOException e) {
                    // ignore
                }
            }
        }
    }
}
