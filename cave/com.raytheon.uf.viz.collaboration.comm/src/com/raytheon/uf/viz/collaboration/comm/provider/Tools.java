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

import javax.xml.bind.JAXBException;

import org.eclipse.ecf.core.IContainer;
import org.eclipse.ecf.core.util.Base64;

import com.raytheon.uf.common.serialization.SerializationException;
import com.raytheon.uf.common.serialization.SerializationUtil;
import com.raytheon.uf.viz.collaboration.comm.compression.CompressionUtil;
import com.raytheon.uf.viz.collaboration.comm.identity.CollaborationException;

/**
 * TODO Add Description
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Mar 7, 2012            jkorman     Initial creation
 * 
 * </pre>
 * 
 * @author jkorman
 * @version 1.0
 */

public abstract class Tools {

    public static final String TAG_INVITE = "[[INVITEID#";

    public static final String TAG_INVITE_ID = TAG_INVITE + "%s]]%s";

    public static final String PROP_SESSION_ID = "sessionId";

    public static final String CMD_PREAMBLE = "[[COMMAND#";
    
    public static final String CONFIG_PREAMBLE = "[[CONFIG#";

    private static final String ENV_THRIFT = CMD_PREAMBLE
            + SerializationMode.THRIFT.name() + "]]";

    private static final String ENV_THRIFT_COMPRESSED = CMD_PREAMBLE
            + SerializationMode.THRIFT.name() + "-COMPRESSED]]";

    private static final String ENV_JAXB = CMD_PREAMBLE
            + SerializationMode.JAXB.name() + "]]";

    private static final String ENV_JAXB_COMPRESSED = CMD_PREAMBLE
            + SerializationMode.JAXB.name() + "-COMPRESSED]]";

    private static final String ENV_STRING = CMD_PREAMBLE
            + SerializationMode.STRING.name() + "]]";

    private static final String ENV_JAVA = CMD_PREAMBLE
            + SerializationMode.JAVA.name() + "]]";

    private static final String ENV_NONE = CMD_PREAMBLE
            + SerializationMode.NONE.name() + "]]";

    public static final String VENUE_SUBJECT_PROP = "subject";

    public static final String NAME_DELIM = "@";

    public static final String PORT_DELIM = ":";

    public static final String RESOURCE_DELIM = "/";

    public static boolean COMPRESSION_OFF = false;

    static {
        try {
            COMPRESSION_OFF = Boolean
                    .getBoolean("collaboration.compressionOff");
        } catch (Exception e) {
            // must not have permission to access system properties. ignore and
            // use default.
        }
    }

    /**
     * 
     * @param <T>
     * @param container
     * @param c
     * @return
     */
    @SuppressWarnings("unchecked")
    public static <T> T getPresenceContainerAdapter(IContainer container,
            Class<T> c) {
        return (T) container.getAdapter(c);
    }

    /**
     * 
     * @param id
     * @return
     */
    public static String parseName(String id) {
        String name = null;
        if (id != null) {
            int delimPos = id.indexOf(NAME_DELIM);
            if (delimPos >= 0) {
                name = id.substring(0, delimPos);
            }
        }
        return name;
    }

    /**
     * 
     * @param id
     * @return
     */
    public static String parseHost(String id) {
        String host = null;
        if (id != null) {
            int delimPos = id.indexOf(NAME_DELIM);
            if (delimPos >= 0) {
                // Ok we have the start of the host name
                int start = delimPos + 1;
                int stop = start;
                delimPos = id.indexOf(PORT_DELIM);
                if (delimPos > stop) {
                    // ok we have a port so grab anything in between
                    stop = delimPos;
                } else {
                    // no port delimiter, so check for the resource
                    delimPos = id.indexOf(RESOURCE_DELIM);
                    if (delimPos > stop) {
                        // we have the resource delimiter
                        stop = delimPos;
                    } else {
                        stop = id.length();
                    }
                }
                host = id.substring(start, stop);
            }
        }
        return host;
    }

    /**
     * 
     * @param id
     * @return
     */
    public static String parsePort(String id) {
        String host = null;
        if (id != null) {
            int delimPos = id.indexOf(NAME_DELIM);
            if (delimPos >= 0) {
                // Ok we have the start of the host name
                int start = delimPos + 1;
                int stop = start;
                delimPos = id.indexOf(PORT_DELIM);
                if (delimPos > stop) {
                    // ok we have a port so grab anything in between
                    start = delimPos + 1;
                    delimPos = id.indexOf(RESOURCE_DELIM);
                    if (delimPos > start) {
                        stop = delimPos;
                    } else {
                        stop = id.length();
                    }
                    host = id.substring(start, stop);
                }
            }
        }
        return host;
    }

    /**
     * 
     * @param id
     * @return
     */
    public static String parseResource(String id) {
        String resource = null;
        if (id != null) {
            int delimPos = id.indexOf(NAME_DELIM);
            // Ensure that the name delimiter is there first.
            if (delimPos >= 0) {
                int start = delimPos + 1;
                delimPos = id.indexOf(RESOURCE_DELIM);
                if (delimPos > start) {
                    delimPos++;
                    if (delimPos < id.length()) {
                        resource = id.substring(delimPos);
                    }
                }
            }
        }
        return resource;
    }

    /**
     * Decode Base64 encoded String data into a byte array.
     * 
     * @param message
     * @return The decoded byte array data.
     */
    public static byte[] decodeFromBase64(String message) {
        return Base64.decode(message);
    }

    /**
     * Encode byte array data into a Base64 String for transmission.
     * 
     * @param message
     *            The message to encode.
     * @return The encoded data as a String.
     */
    public static String encodeToBase64(byte[] message) {
        return Base64.encode(message);
    }

    /**
     * 
     * @param data
     * @return
     */
    public static String marshallData(Object data)
            throws CollaborationException {
        String marshalledData = null;
        if (data != null) {
            StringBuilder sb = new StringBuilder();
            byte[] marshalledBinary = null;
            SerializationMode mode = SerializationMode.getMode(data);
            switch (mode) {
            case THRIFT: {
                try {
                    if (COMPRESSION_OFF) {
                        marshalledBinary = SerializationUtil
                                .transformToThrift(data);
                        sb.append(ENV_THRIFT);
                    } else {
                        /*
                         * compress(thrift(data))
                         */
                        byte[] marshalledThrift = SerializationUtil
                                .transformToThrift(data);
                        marshalledBinary = CompressionUtil
                                .compress(marshalledThrift);
                        sb.append(ENV_THRIFT_COMPRESSED);
                    }
                } catch (Exception e) {
                    throw new CollaborationException(
                            "[THRIFT] Could not serialize object", e);
                }
                break;
            }
            case JAXB: {
                try {
                    if (COMPRESSION_OFF) {
                        String s = SerializationUtil.marshalToXml(data);
                        if (s != null) {
                            sb.append(ENV_JAXB);
                            sb.append(s);
                        }
                    } else {
                        String rawString = SerializationUtil.marshalToXml(data);
                        marshalledBinary = CompressionUtil.compress(rawString
                                .getBytes());
                        sb.append(ENV_JAXB_COMPRESSED);
                    }
                } catch (Exception je) {
                    throw new CollaborationException(
                            "[JAXB] Could not serialize object", je);
                }
                break;
            }
            case JAVA: {
                break;
            }
            case STRING: {
                sb.append(ENV_STRING);
                sb.append(data);
                break;
            }
            case NONE: {
                throw new CollaborationException("Serialization of "
                        + data.getClass().getName() + " not supported");
            }
            case ISNULL: {
                break;
            }
            }
            if (marshalledBinary != null) {
                sb.append(encodeToBase64(marshalledBinary));
            }
            if (sb.length() > 0) {
                marshalledData = sb.toString();
            }
        }
        return marshalledData;
    }

    /**
     * 
     * @param data
     * @return
     * @throws CollaborationException
     */
    public static Object unMarshallData(String data)
            throws CollaborationException {
        Object unMarshalledData = null;
        if (data != null) {
            // look for the envelope header first
            if (data.startsWith(ENV_THRIFT)) {
                String s = data.substring(ENV_THRIFT.length());
                try {
                    byte[] b = decodeFromBase64(s);
                    unMarshalledData = SerializationUtil.transformFromThrift(b);
                } catch (SerializationException e) {
                    throw new CollaborationException(
                            "Could not deserialize object", e);
                }
            } else if (data.startsWith(ENV_THRIFT_COMPRESSED)) {
                String s = data.substring(ENV_THRIFT_COMPRESSED.length());
                try {
                    byte[] rawBytes = decodeFromBase64(s);
                    byte[] uncompressedBytes = CompressionUtil
                            .uncompress(rawBytes);

                    unMarshalledData = SerializationUtil
                            .transformFromThrift(uncompressedBytes);
                    // unMarshalledData = SerializationUtil
                    // .transformFromThrift(createCompressionInputStream(rawBytes));
                } catch (Exception e) {
                    throw new CollaborationException(
                            "Could not deserialize object", e);
                }
            } else if (data.startsWith(ENV_JAXB)) {
                String s = data.substring(ENV_JAXB.length());
                try {
                    unMarshalledData = SerializationUtil.unmarshalFromXml(s);
                } catch (JAXBException je) {
                    throw new CollaborationException(
                            "[JAXB] Could not deserialize object", je);
                }
            } else if (data.startsWith(ENV_JAXB_COMPRESSED)) {
                String rawString = data.substring(ENV_JAXB_COMPRESSED.length());
                try {
                    byte[] rawBytes = decodeFromBase64(rawString);
                    unMarshalledData = SerializationUtil
                            .unmarshalFromXml(new String(CompressionUtil
                                    .uncompress(rawBytes)));
                    // unMarshalledData = SerializationUtil
                    // .unmarshalFromXml(createCompressionInputStream(rawBytes));
                } catch (Exception je) {
                    throw new CollaborationException(
                            "[JAXB] Could not deserialize object", je);
                }
            } else if (data.startsWith(ENV_STRING)) {
                unMarshalledData = data.substring(ENV_STRING.length());
            } else if (data.startsWith(ENV_JAVA)) {
                throw new CollaborationException("Could not deserialize object");
            } else if (data.startsWith(ENV_NONE)) {
                throw new CollaborationException("Could not deserialize object");
            }
        }
        return unMarshalledData;
    }

}
