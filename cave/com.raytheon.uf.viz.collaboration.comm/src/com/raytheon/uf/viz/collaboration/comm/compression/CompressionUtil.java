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
package com.raytheon.uf.viz.collaboration.comm.compression;

import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.nio.ByteBuffer;
import java.nio.channels.Channels;
import java.nio.channels.ReadableByteChannel;
import java.nio.channels.WritableByteChannel;
import java.util.zip.Deflater;
import java.util.zip.DeflaterOutputStream;
import java.util.zip.GZIPInputStream;
import java.util.zip.GZIPOutputStream;
import java.util.zip.InflaterInputStream;

import com.raytheon.uf.viz.collaboration.comm.identity.CollaborationException;

/**
 * Utilities for compressing or decompressing data.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * May 11, 2012            njensen     Initial creation
 * 
 * </pre>
 * 
 * @author njensen
 * @version 1.0
 */

public class CompressionUtil {

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
            Deflater defl = new Deflater(Deflater.BEST_COMPRESSION);
            stream = new DeflaterOutputStream(out, defl);
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
