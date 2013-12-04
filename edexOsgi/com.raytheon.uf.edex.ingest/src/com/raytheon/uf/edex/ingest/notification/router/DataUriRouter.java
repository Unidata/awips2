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
package com.raytheon.uf.edex.ingest.notification.router;

import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.util.concurrent.ConcurrentLinkedQueue;
import java.util.zip.GZIPOutputStream;

import com.raytheon.uf.common.dataplugin.PluginDataObject;
import com.raytheon.uf.common.dataplugin.message.DataURINotificationMessage;
import com.raytheon.uf.common.serialization.SerializationException;
import com.raytheon.uf.common.serialization.SerializationUtil;
import com.raytheon.uf.common.util.ByteArrayOutputStreamPool;
import com.raytheon.uf.edex.core.EDEXUtil;
import com.raytheon.uf.edex.core.EdexException;
import com.raytheon.uf.edex.ingest.notification.PluginNotifierConfig;
import com.raytheon.uf.edex.ingest.notification.PluginNotifierConfig.EndpointType;

/**
 * Routes DataUri Notifications to a destination uri. Notification msg will be
 * sent immediately to the routes for inner jvm calls, routes over jms will be
 * queued and sent in gzipped batches.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Nov 19, 2013 2170       rjpeter     Initial creation
 * 
 * </pre>
 * 
 * @author rjpeter
 * @version 1.0
 */
public class DataUriRouter implements INotificationRouter {

    /**
     * Buffer size.
     */
    private static final int GZIP_BUFFER_SIZE = 4096;

    /**
     * Data URIs that have not been sent.
     */
    private final ConcurrentLinkedQueue<String> uris = new ConcurrentLinkedQueue<String>();

    /**
     * Flag if this route stays in the jvm.
     */
    private final boolean isInternal;

    /**
     * The destination URI for this router.
     */
    private final String route;

    /**
     * 
     * @param config
     */
    public DataUriRouter(PluginNotifierConfig config) {
        EndpointType type = config.getEndpointType();
        isInternal = EndpointType.DIRECTVM.equals(type)
                || EndpointType.VM.equals(type);
        route = config.getEndpointUri();
    }

    @Override
    public String getRoute() {
        return route;
    }

    @Override
    public void process(PluginDataObject pdo) {
        uris.add(pdo.getDataURI());
    }

    /**
     * Creates a DataURINotificationMessage.
     * 
     * @return
     */
    protected synchronized DataURINotificationMessage createMessage() {
        DataURINotificationMessage msg = null;
        // this is the only point that uris is reduced, safe to grab current
        // size and dequeue that many items
        int size = uris.size();
        if (size > 0) {
            String[] data = new String[size];
            for (int i = 0; i < size; i++) {
                data[i] = uris.poll();
            }

            msg = new DataURINotificationMessage();
            msg.setDataURIs(data);
        }

        return msg;
    }

    @Override
    public void sendImmediateData() throws EdexException {
        // if sending inside jvm, create message and send immediately as the
        // memory object
        if (isInternal) {
            DataURINotificationMessage msg = createMessage();
            if (msg != null) {
                EDEXUtil.getMessageProducer().sendAsyncUri(route, msg);
            }
        }
    }

    @Override
    public void sendQueuedData() throws EdexException {
        if (!isInternal) {
            // if sending outside the jvm, create message, serialize, gzip, and
            // then send
            DataURINotificationMessage msg = createMessage();
            if (msg != null) {
                EDEXUtil.getMessageProducer().sendAsyncUri(route,
                        encodeMessage(msg));
            }
        }
    }

    /**
     * Thrift encodes an object and then gzip's the binary data. Should only be
     * used for sending data outside the jvm.
     * 
     * @param msg
     * @return
     * @throws SerializationException
     */
    private byte[] encodeMessage(DataURINotificationMessage msg)
            throws EdexException {
        ByteArrayOutputStream baos = ByteArrayOutputStreamPool.getInstance()
                .getStream();
        GZIPOutputStream gzippedURIs = null;

        try {
            gzippedURIs = new GZIPOutputStream(baos, GZIP_BUFFER_SIZE);
        } catch (IOException e) {
            throw new EdexException(
                    "Failed to prepare the gzipped data stream", e);
        }

        try {
            SerializationUtil.transformToThriftUsingStream(msg, gzippedURIs);
            gzippedURIs.finish();
            gzippedURIs.flush();
            return baos.toByteArray();
        } catch (IOException e) {
            throw new EdexException("Failed to write the gzipped data stream",
                    e);
        } catch (SerializationException e) {
            throw new EdexException(
                    "Failed to serialize DataURINotificationMessage", e);
        } finally {
            try {
                gzippedURIs.close();
            } catch (IOException e) {
                // ignore, we no longer need the stream
            }
        }
    }
}
