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

import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.nio.ByteBuffer;
import java.nio.channels.Channels;
import java.nio.channels.ReadableByteChannel;
import java.nio.channels.WritableByteChannel;
import java.util.zip.DeflaterOutputStream;
import java.util.zip.GZIPInputStream;
import java.util.zip.GZIPOutputStream;
import java.util.zip.InflaterInputStream;

import javax.xml.bind.JAXBException;

import org.eclipse.ecf.core.IContainer;
import org.eclipse.ecf.core.util.Base64;

import com.raytheon.uf.common.serialization.SerializationException;
import com.raytheon.uf.common.serialization.SerializationUtil;
import com.raytheon.uf.viz.collaboration.comm.identity.CollaborationException;
import com.raytheon.uf.viz.collaboration.comm.identity.IPresence;

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

    public static CompressionType COMPRESSION_TYPE = CompressionType.ZLIB;

    private static boolean log_compression = false;

    private enum CompressionType {
        ZLIB, GZIP;

        public byte toByte() {
            return (byte) ordinal();
        }

        public static CompressionType fromByte(byte b) {
            if (b < 0 || b > CompressionType.values().length) {
                throw new IndexOutOfBoundsException(
                        "Unable to determine CompressionType for " + b);
            }
            return CompressionType.values()[b];
        }
    };

    static {
        try {
            COMPRESSION_OFF = Boolean
                    .getBoolean("collaboration.compressionOff");
            String compressionType = System
                    .getProperty("collaboration.compressionType");
            if (compressionType != null) {
                COMPRESSION_TYPE = CompressionType.valueOf(compressionType);
            }
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
     * Converts from an IPresence.Type to ECF Presence Type.
     * 
     * @param type
     *            A type to convert.
     * @return The converted type.
     */
    public static org.eclipse.ecf.presence.Presence.Type convertPresenceType(
            IPresence.Type type) {

        org.eclipse.ecf.presence.Presence.Type sType = null;
        switch (type) {
        case AVAILABLE: {
            sType = org.eclipse.ecf.presence.Presence.Type.AVAILABLE;
            break;
        }
        case ERROR: {
            sType = org.eclipse.ecf.presence.Presence.Type.ERROR;
            break;
        }
        case SUBSCRIBE: {
            sType = org.eclipse.ecf.presence.Presence.Type.SUBSCRIBE;
            break;
        }
        case SUBSCRIBED: {
            sType = org.eclipse.ecf.presence.Presence.Type.SUBSCRIBED;
            break;
        }
        case UNAVAILABLE: {
            sType = org.eclipse.ecf.presence.Presence.Type.UNAVAILABLE;
            break;
        }
        case UNSUBSCRIBE: {
            sType = org.eclipse.ecf.presence.Presence.Type.UNSUBSCRIBE;
            break;
        }
        case UNSUBSCRIBED: {
            sType = org.eclipse.ecf.presence.Presence.Type.UNSUBSCRIBED;
            break;
        }
        case UNKNOWN: {
            sType = org.eclipse.ecf.presence.Presence.Type.UNKNOWN;
            break;
        }
        default: {
            sType = org.eclipse.ecf.presence.Presence.Type.ERROR;
            break;
        }
        }

        return sType;
    }

    /**
     * Converts from an IPresence.Mode to ECF Presence Mode.
     * 
     * @param mode
     *            A mode to convert.
     * @return The converted mode.
     */
    public static org.eclipse.ecf.presence.Presence.Mode convertPresenceMode(
            IPresence.Mode mode) {

        org.eclipse.ecf.presence.Presence.Mode sMode = null;
        switch (mode) {
        case AVAILABLE: {
            sMode = org.eclipse.ecf.presence.Presence.Mode.AVAILABLE;
            break;
        }
        case AWAY: {
            sMode = org.eclipse.ecf.presence.Presence.Mode.AWAY;
            break;
        }
        case CHAT: {
            sMode = org.eclipse.ecf.presence.Presence.Mode.CHAT;
            break;
        }
        case DND: {
            sMode = org.eclipse.ecf.presence.Presence.Mode.DND;
            break;
        }
        case EXTENDED_AWAY: {
            sMode = org.eclipse.ecf.presence.Presence.Mode.EXTENDED_AWAY;
            break;
        }
        case INVISIBLE: {
            sMode = org.eclipse.ecf.presence.Presence.Mode.INVISIBLE;
            break;
        }
        }
        return sMode;
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
                        marshalledBinary = compress(marshalledThrift);
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
                        marshalledBinary = compress(rawString.getBytes());
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
                    byte[] uncompressedBytes = uncompress(rawBytes);

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
                            .unmarshalFromXml(new String(uncompress(rawBytes)));
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

    public static byte[] compress(byte[] bytes) throws CollaborationException {
        ByteArrayOutputStream out = new ByteArrayOutputStream(bytes.length);
        CompressionType cType = COMPRESSION_TYPE;

        out.write(cType.toByte());
        OutputStream compressionStrm = null;
        try {
            compressionStrm = createCompressionOutputStream(out);
            long start = System.currentTimeMillis();
            compressionStrm.write(bytes);
            compressionStrm.flush();
            compressionStrm.close();
            byte[] result = out.toByteArray();
            if (log_compression) {
                System.out.println(cType + " Compression time(milliseconds) "
                        + (System.currentTimeMillis() - start) / 1000F
                        + " to compress " + bytes.length + " bytes to "
                        + result.length + " bytes.");
            }
            return result;
        } catch (IOException e) {
            throw new CollaborationException("Unable to compress data.", e);
        }
    }

    private static OutputStream createCompressionOutputStream(OutputStream out)
            throws IOException {
        OutputStream stream = null;
        switch (COMPRESSION_TYPE) {
        case GZIP:
            stream = new GZIPOutputStream(out);
            break;
        case ZLIB:
        default:
            stream = new DeflaterOutputStream(out);
            break;
        }
        return stream;
    }

    public static byte[] uncompress(byte[] bytes) throws CollaborationException {
        ByteArrayInputStream in = new ByteArrayInputStream(bytes);
        ByteArrayOutputStream out = new ByteArrayOutputStream();

        CompressionType cType = CompressionType.fromByte((byte) in.read());
        long start = System.currentTimeMillis();

        try {
            ReadableByteChannel src = Channels
                    .newChannel(createCompressionInputStream(cType, in));
            WritableByteChannel dest = Channels.newChannel(out);

            final ByteBuffer buffer = ByteBuffer.allocateDirect(16 * 1024);
            while (src.read(buffer) != -1) {
                buffer.flip();
                dest.write(buffer);
                buffer.compact();
            }

            // EOF will leave buffer in fill state
            buffer.flip();

            // make sure the buffer is fully drained.
            while (buffer.hasRemaining()) {
                dest.write(buffer);
            }
            dest.close();
            byte[] resultBuffer = out.toByteArray();
            if (log_compression) {
                System.out.println(cType
                        + " Uncompression time(milliseconds): "
                        + ((System.currentTimeMillis() - start) / 1000F)
                        + " to uncompress " + bytes.length + " bytes to "
                        + resultBuffer.length + " bytes.");
            }
            return resultBuffer;
        } catch (IOException e) {
            throw new CollaborationException("Unable to uncompress data.", e);
        }
    }

    private static InputStream createCompressionInputStream(
            CompressionType cType, InputStream in) throws IOException {
        InputStream stream = null;
        switch (cType) {
        case GZIP:
            stream = new GZIPInputStream(in);
            break;
        case ZLIB:
        default:
            stream = new InflaterInputStream(in);
            break;
        }
        return stream;
    }

}
